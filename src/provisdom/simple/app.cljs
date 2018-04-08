(ns provisdom.simple.app
  (:require [provisdom.maali.common :as common]
            [provisdom.simple.ai :as ai]
            [provisdom.simple.rules :as simple]
            [provisdom.simple.view :as view]
            [provisdom.maali.rules :refer-macros [defsession] :as rules]
            [cljs.core.async :refer [<! >!] :as async]
            [clojure.set :as set]))

#_(st/instrument)

(enable-console-print!)

(def smart-ai true)
(def ai-latency 800)
(def teaching-mode false)

(defonce session-atom (atom nil))

(defn response-fn
  [spec response]
  (swap! session-atom common/handle-response [spec response]))

(defonce history (atom []))

(defn history-response-fn
  [spec response]
  (swap! history conj [spec response])
  (response-fn spec response))

(defn init-session
  [session]
  (reset! common/request-id 0)
  (-> session
      (rules/insert ::simple/CurrentPlayer {::simple/player :x})
      (rules/insert ::simple/TeachingMode {::simple/teaching-mode teaching-mode})
      (rules/insert ::common/ResponseFunction {::common/response-fn response-fn})
      (rules/fire-rules)))

(defn ai-effect
  [ai-fn session]
  (async/go
   (<! (async/timeout (/ (* ai-latency (+ 1 (rand))) 2)))
   (when (not-empty (rules/query-partial session ::simple/move-request :?player :x))
     (let [moves (map :?move (rules/query-partial session ::simple/move))
           board (simple/squares->board moves)
           next-move (ai-fn board)
           {::simple/keys [position player] :as move-request} (rules/query-one :?request session ::simple/move-request :?position next-move :?player :x)]
       (common/respond-to move-request {::simple/position position ::simple/player player})))))

(def smart-ai-effect (partial ai-effect (fn [board] (ai/choose-move board :x 0))))

(def dumb-ai-effect (partial ai-effect (fn [board] (rand-nth (vec (set/difference (set (range 9)) (set (keys board))))))))

;;; HACK - is there a better way to call init-session and view/run only on load?
(defonce hackorama
         (do
           (add-watch session-atom :move-x
                      (fn [_ _ _ session]
                        (if smart-ai (smart-ai-effect session) (dumb-ai-effect session))))

           (reset! session-atom (init-session simple/session))
           ;;; Comment out view/run to run tests headless
           (view/run session-atom)))

(defn reload
  []
  (time
    (when (not-empty @history)
      (println "REPLAYING")
      (reset! session-atom (init-session simple/session))
      (doseq [[spec response] @history]
        (response-fn spec response))
      (println "REPLAYED" (count @history)))))

;;; TESTING

(defn check-invariants
  [session]
  (let [moves (map :?move (rules/query-partial session ::simple/move))
        move-requests (map :?request (rules/query-partial session ::simple/move-request))
        game-over (rules/query-one :?game-over session ::simple/game-over)
        winner (rules/query-one :?player session ::simple/winner)]
    (let [counts (into {} (map (juxt key (comp count val)) (group-by ::simple/player moves)))
          xs (or (:x counts) 0)
          os (or (:o counts) 0)]
      ; Make sure we don't have any extra moves. :x goes first so should be
      ; either one ahead or equal to :o.
      (when (or (< 1 (- xs os)) (> 0 (- xs os)))
        (throw (ex-info "Invariant violation: extra moves" {:counts counts}))))
    ; If all the squares are full, the game should be over.
    (when (= 9 (count moves))
      (when (not game-over)
        (throw (ex-info "Invariant violation: game should over" {}))))
    ; Smart AI should never lose
    (when smart-ai
      (when (= :o winner)
        (throw (ex-info "Invariant violation: smart AI should never lose" {}))))
    ; In teaching mode, the user should never lose
    (when teaching-mode
      (when (= :x winner)
        (throw (ex-info "Invariant violation: human should never lose in teaching-mode" {}))))
    (let [mr-pos (set (map ::simple/position move-requests))
          m-pos (set (map ::simple/position moves))]
      ; Can't request a move for a square that's already been used.
      (when (not-empty (set/intersection mr-pos m-pos))
        (throw (ex-info "Invariant violation: moves and move requests should not have overlapping positions" {}))))))

(defn abuse-simple
  [session-atom iterations]
  (add-watch session-atom :check-invariants
             (fn [_ _ _ session]
               (check-invariants session)))
  (loop [i 0]
    (if (< i iterations)
      (do
        (if (rules/query-one :?game-over @session-atom ::simple/game-over)
          ; If the game is over, just reset
          (let [req (rules/query-one :?request @session-atom ::simple/reset-request)]
            (if req
              (common/respond-to req)
              (throw (ex-info "Should have reset request for game over" {}))))
          ; if the game is not over, then play
          (if (> 0.01 (rand))
            ; Once in awhile, be a jerk and reset the game.
            (when-let [req (rules/query-one :?request @session-atom ::simple/reset-request)]
              (common/respond-to req))
            (let [reqs (rules/query-partial @session-atom ::simple/move-request)]
              (if (not-empty reqs)
                ; If legal moves exist, choose one at random
                (let [{::simple/keys [position player] :as req} (:?request (rand-nth reqs))]
                  (common/respond-to req {::simple/position position ::simple/player player}))))))
        (recur (inc i)))
      (remove-watch session-atom :check-invariants))))

(defn abuse-async
  [session-atom iterations delay-ms]
  (add-watch session-atom :check-invariants
             (fn [_ _ _ session]
               (check-invariants session)))
  (async/go
   (enable-console-print!)
   (time
     (loop [i 0]
       (if (< i iterations)
         (if (rules/query-one :?game-over @session-atom ::simple/game-over)
           ; If the game is over, just reset
           (let [req (rules/query-one :?request @session-atom ::simple/reset-request)]
             (if req
               (do
                 (async/timeout delay-ms)
                 (common/respond-to req)
                 (<! (async/timeout (* delay-ms (rand))))
                 (recur (inc i)))
               (throw (ex-info "Should have reset request for game over" {}))))
           (let [reqs (rules/query-partial @session-atom ::simple/move-request :?player :o)]
             (if (not-empty reqs)
               (let [req (:?request (rand-nth reqs))]
                 (<! (async/timeout (* delay-ms (rand))))
                 (common/respond-to req req)
                 (<! (async/timeout (* delay-ms (rand))))
                 (recur (inc i)))
               (if (> 0.01 (rand))
                 (do
                   (async/go
                    (<! (async/timeout (* delay-ms (rand))))
                    (when-let [req (rules/query-one :?request @session-atom ::simple/reset-request)]
                      (common/respond-to req)))
                   (<! (async/timeout (* delay-ms (rand))))
                   (recur (inc i)))
                 ; If there were no valid :o moves and we didn't reset, wait and recur.
                 ; :x will get it together and move sooner or later.
                 (do
                   (<! (async/timeout (* delay-ms (rand))))
                   (recur i))))))
         (remove-watch session-atom :check-invariants))))))

;;; Uncomment to run tests. First argument is the number
;;; of app-level responses simulated, second is delay in
;;; milliseconds between
(comment
 (abuse-simple session-atom 10000)
 (abuse-async session-atom 50 ai-latency)
 (add-watch session-atom :check-invariants
            (fn [_ _ _ session]
              (check-invariants session)))
 (remove-watch session-atom :check-invariants))
