(ns provisdom.simple.app
  (:require [provisdom.maali.common :as common]
            [provisdom.simple.ai :as ai]
            [provisdom.simple.rules :as simple]
            [provisdom.simple.view :as view]
            [provisdom.maali.rules :refer-macros [defsession] :as rules]))

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
  (let [session (-> session
                    (rules/insert ::simple/Player {::simple/marker :x})
                    (rules/insert ::simple/Player {::simple/marker :o})
                    (rules/insert ::simple/CurrentPlayer {::simple/Player {::simple/marker :x}})
                    (rules/insert ::common/ResponseFunction {::common/response-fn response-fn}))
        squares (map (fn [p] #::simple{:position p}) (range 9))
        session (apply rules/insert session ::simple/Square squares)]
    (rules/fire-rules session)))

;;; HACK - is there a better way to call init-session and view/run only on load?
(defonce hackorama
         (do
           (add-watch session-atom :move-x
                      (fn [_ _ _ session]
                        (js/setTimeout
                         (fn [_]
                           (when (not-empty (rules/query-partial session ::simple/move-request :?marker :x))
                            (let [moves (map :?move (rules/query-partial session ::simple/move))
                                  board (simple/squares->board moves)
                                  next-move (ai/choose-move board :x 0)
                                  move-request (common/query-one :?request session ::simple/move-request :?position next-move :?marker :x)]
                              (common/respond-to move-request move-request))))
                         2000)))


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
