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

(def smart-ai false)
(def ai-latency 100)
(def teaching-mode true)

(declare init-session)

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

;;; HACK - is there a better way to call init-session and view/run only on load?
(defonce hackorama
         (do
           (add-watch session-atom :move-x
                      (fn [_ _ _ session]
                        (async/go
                         (<! (async/timeout ai-latency))
                         (when (not-empty (rules/query-partial session ::simple/move-request :?player :x))
                          (let [moves (map :?move (rules/query-partial session ::simple/move))
                                board (simple/squares->board moves)
                                next-move (if smart-ai
                                            (ai/choose-move board :x 0)
                                            (rand-nth (vec (set/difference (set (range 9)) (set (keys board))))))
                                move-request (common/query-one :?request session ::simple/move-request :?position next-move :?player :x)]
                            (common/respond-to move-request move-request))))))

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
        game-over (common/query-one :?game-over session ::simple/game-over)
        winner (common/query-one :?player session ::simple/winner)]
    (let [counts (into {} (map (juxt key (comp count val)) (group-by ::simple/player moves)))
          xs (or (:x counts) 0)
          os (or (:o counts) 0)]
      (when (or (< 1 (- xs os)) (> 0 (- xs os)))
        (throw (ex-info "Invariant violation: extra moves" {:counts counts})))
      (when (= 9 (count moves))
        (when (not game-over)
          (throw (ex-info "Invariant violation: game should over" {}))))
      (when smart-ai
        (when (= :o winner)
          (throw (ex-info "Invariant violation: smart AI should never lose" {}))))
      (when (and teaching-mode)
        (when (= :x winner)
          (throw (ex-info "Invariant violation: human should never lose in teaching-mode" {}))))
      (let [mr-pos (set (map ::simple/position move-requests))
            m-pos (set (map ::simple/position moves))]
        (when (not-empty (set/intersection mr-pos m-pos))
          (throw (ex-info "Invariant violation: moves and move requests should not have overlapping positions" {})))))))

(defn abuse
  [session-atom iterations delay-ms]
  (add-watch session-atom :check-invariants
             (fn [_ _ _ session]
               (check-invariants session)))
  (async/go
   (enable-console-print!)
   (loop [i 0]
     (if (< i iterations)
       (if (common/query-one :?game-over @session-atom ::simple/game-over)
         (let [req (common/query-one :?request @session-atom ::simple/reset-request)]
           (if req
             (do
               (common/respond-to req)
               (<! (async/timeout delay-ms))
               (recur (inc i)))
             (throw (ex-info "Should have reset request for game over" {}))))
         (let [reqs (rules/query-partial @session-atom ::simple/move-request :?player :o)]
           (if (not-empty reqs)
             (let [req (:?request (rand-nth reqs))]
               (common/respond-to req req)
               (<! (async/timeout delay-ms))
               (recur (inc i)))
             (if (> 0.01 (rand))
               (do
                 (async/go
                  (<! (async/timeout (* delay-ms (rand))))
                  (when-let [req (common/query-one :?request @session-atom ::simple/reset-request)]
                    (common/respond-to req)))
                 (<! (async/timeout delay-ms))
                 (recur (inc i)))
               (do
                 (<! (async/timeout (+ ai-latency delay-ms)))
                 (recur i))))))
       (remove-watch session-atom :check-invariants)))))

;;; Uncomment to run tests. First argument is the number
;;; of app-level responses simulated, second is delay in
;;; milliseconds between
(comment
  (abuse session-atom 1000 (* 0.5 ai-latency))
  (add-watch session-atom :check-invariants
             (fn [_ _ _ session]
               (check-invariants session)))
  (remove-watch session-atom :check-invariants))
