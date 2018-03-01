(ns provisdom.simple.rules
  (:require [clojure.spec.alpha :as s]
            [provisdom.simple.ai :as ai]
            [clara.rules.accumulators :as acc]
            [provisdom.maali.rules #?(:clj :refer :cljs :refer-macros) [defrules defqueries defsession def-derive] :as rules]
            [provisdom.maali.common :as common]))

(s/def ::marker (s/nilable #{:x :o}))
(s/def ::position (s/int-in 0 9))
(s/def ::Player (s/keys :req [::marker]))
(s/def ::Square (s/keys :req [::marker ::position]))
(s/def ::CurrentPlayer (s/keys :req [::marker]))
(s/def ::Winner (s/keys :req [::Player]))
(s/def ::GameOver (s/keys))

(def-derive ::MoveRequest ::common/Request (s/keys :req [::position]))
(def-derive ::MoveResponse ::common/Response (s/keys :req [::position]))

(defn squares->board
  [squares]
  (reduce (fn [b {::keys [marker position]}] (assoc b position marker)) (sorted-map) squares))

(defn print-board
  [board]
  (println board)
  (doseq [row (partition 3 3 board)]
    (println (map (fn [[_ v]] (or v :.)) row))))

(defrules rules
  [::human-move-request!
   [:not [::GameOver]]
   [?current-player <- ::CurrentPlayer (= :o marker)]
   [::Square (not marker) (= ?position position)]
   [::common/ResponseFunction (= ?response-fn response-fn)]
   =>
   (rules/insert! ::MoveRequest (common/request {::position ?position} ::MoveResponse ?response-fn))]

  [::human-move-response!
   [?request <- ::MoveRequest (= ?position position)]
   [::MoveResponse (= ?request Request) (= ?position position)]
   [?square <- ::Square (= ?position position)]
   [?current-player <- ::CurrentPlayer]
   =>
   (rules/upsert! ::Square ?square assoc ::marker :o)
   (rules/upsert! ::CurrentPlayer ?current-player assoc ::marker :x)]

  [::computer-move!
   [:not [::GameOver]]
   [?current-player <- ::CurrentPlayer (= :x marker)]
   [?squares <- (acc/all) :from [::Square]]
   =>
   (let [next-move (ai/choose-move (squares->board ?squares) :x 0)]
     (rules/upsert! ::CurrentPlayer ?current-player assoc ::marker :o)
     (rules/upsert! ::Square {::position next-move ::marker nil} assoc ::marker :x))]

  [::winner!
   [?squares <- (acc/all) :from [::Square]]
   [?player <- ::Player (= ?marker marker)]
   [:test (< 0 (ai/score-board (squares->board ?squares) ?marker 0))]
   =>
   (rules/insert! ::Winner {::Player ?player})]

  [::game-over!
   [:or
    [::Winner]
    [:and
     [?squares <- (acc/all) :from [::Square (some? marker)]]
     [:test (empty? ?squares)]]]
   =>
   (rules/insert! ::GameOver {})])

(defqueries queries
  [::square [:?position] [?square <- ::Square (= ?position position) (= ?marker marker)]]
  [::winner [] [?winner <- ::Winner]]
  [::game-over [] [?game-over <- ::GameOver]]
  [::move-request [:?position] [?request <- ::MoveRequest (= ?position position)]])

(defsession session [rules queries])
