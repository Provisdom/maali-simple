(ns provisdom.simple.app
  (:require [provisdom.maali.common :as common]
            [provisdom.simple.ai :as ai]
            [provisdom.simple.rules :as simple]
            [provisdom.simple.view :as view]
            [provisdom.maali.rules :refer-macros [defsession] :as rules]
            [clojure.set :as set]))

#_(st/instrument)

(enable-console-print!)

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
      (rules/insert ::common/ResponseFunction {::common/response-fn response-fn})
      (rules/fire-rules)))

(def smart-ai true)
;;; HACK - is there a better way to call init-session and view/run only on load?
(defonce hackorama
         (do
           (add-watch session-atom :move-x
                      (fn [_ _ _ session]
                        (js/setTimeout
                         (fn [_]
                           (when (not-empty (rules/query-partial session ::simple/move-request :?player :x))
                            (let [moves (map :?move (rules/query-partial session ::simple/move))
                                  board (simple/squares->board moves)
                                  next-move (if smart-ai
                                              (ai/choose-move board :x 0)
                                              (rand-nth (vec (set/difference (set (range 9)) (set (keys board))))))
                                  move-request (common/query-one :?request session ::simple/move-request :?position next-move :?player :x)]
                              (common/respond-to move-request move-request))))
                         1000)))


           (reset! session-atom (init-session simple/session))
           ;;; Comment out view/run to run tests headless
           (view/run session-atom)))


;;; Uncomment to run tests. First argument is the number
;;; of app-level responses simulated, second is delay in
;;; milliseconds between responses.
#_(test/abuse session-atom 1000 20)

(defn reload
  []
  (time
    (when (not-empty @history)
      (println "REPLAYING")
      (reset! session-atom (init-session simple/session))
      (doseq [[spec response] @history]
        (response-fn spec response))
      (println "REPLAYED" (count @history)))))
