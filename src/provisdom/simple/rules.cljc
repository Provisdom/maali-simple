(ns provisdom.simple.rules
  (:require [clojure.spec.alpha :as s]
            [clojure.set :as set]
            [provisdom.simple.ai :as ai]
            [clara.rules.accumulators :as acc]
            [provisdom.maali.rules #?(:clj :refer :cljs :refer-macros) [defrules defqueries defsession def-derive] :as rules]
            [provisdom.maali.common :as common]))

(s/def ::marker #{:x :o})
(s/def ::position (s/int-in 0 9))
(s/def ::Square (s/keys :req [::position]))
(s/def ::Player (s/keys :req [::marker]))
(s/def ::Move (s/keys :req [::Player ::Square]))
(s/def ::CurrentPlayer (s/keys :req [::Player]))
(s/def ::Winner (s/keys :req [::Player]))
(s/def ::WinningSquare (s/keys :req [::Square]))
(s/def ::CatsGame (s/keys))
(s/def ::GameOver (s/keys))

(def-derive ::MoveRequest ::common/Request (s/keys :req [::Square ::Player]))
(def-derive ::MoveResponse ::common/Response (s/keys :req [::Square ::Player]))
(def-derive ::ResetBoardRequest ::common/Request)

(defn squares->board
  [squares]
  (reduce (fn [b {{marker ::marker} ::Player {position ::position} ::Square}] (assoc b position marker)) (sorted-map) squares))

(defn print-board
  [board]
  (println board)
  (doseq [row (partition 3 3 board)]
    (println (map (fn [[_ v]] (or v :.)) row))))

(defrules rules
  [::move-request!
   [:not [::GameOver]]
   [?current-player <- ::CurrentPlayer (= ?player Player)]
   [?square <- ::Square]
   [:not [::Move (= ?square Square)]]
   [::common/ResponseFunction (= ?response-fn response-fn)]
   =>
   (rules/insert! ::MoveRequest (common/request {::Square ?square ::Player ?player} ::MoveResponse ?response-fn))]

  [::move-response!
   [?request <- ::MoveRequest (= ?square Square)]
   [::MoveResponse (= ?request Request) (= ?square Square) (= ?player Player)]
   [?player <- ::Player (= ?marker marker)]
   [?current-player <- ::CurrentPlayer]
   [?next-player <- ::Player (not=  ?marker marker)]
   =>
   (rules/insert-unconditional! ::Move {::Square ?square ::Player ?player})
   (rules/upsert! ::CurrentPlayer ?current-player assoc ::Player ?next-player)]

  [::winner!
   [?moves <- (acc/all) :from [::Move]]
   [?player <- ::Player (= ?marker marker)]
   [:test (< 0 (ai/score-board (squares->board ?moves) ?marker 0))]
   =>
   (let [player-positions (set (map (comp ::position ::Square) (filter #(= ?marker (-> % ::Player ::marker )) ?moves)))
         winning-positions (some #(when (= 3 (count %)) %) (map (partial set/intersection player-positions) ai/winning-indices))]
     (doseq [position winning-positions]
       (rules/insert! ::WinningSquare {::Square {::position position}}))
     (rules/insert! ::Winner {::Player ?player}))]

  [::cats-game!
   [:not [::Winner]]
   [?count <- (acc/count) :from [::Move]]
   [:test (= 9 ?count)]
   =>
   (rules/insert! ::CatsGame {})]

  [::game-over!
   [::Winner]
   =>
   (println "GAME OVER")
   (rules/insert! ::GameOver {})]

  [::reset-board-request!
   [?moves <- (acc/all) :from [::Move]]
   [:test (not-empty ?moves)]
   [::common/ResponseFunction (= ?response-fn response-fn)]
   =>
   (rules/insert! ::ResetBoardRequest (common/request ::common/Response ?response-fn))]

  [::reset-board-response!
   [?request <- ::ResetBoardRequest]
   [::common/Response (= ?request Request)]
   [?moves <- (acc/all) :from [::Move]]
   [?move-requests <- (acc/all) :from [::MoveRequest]]
   [?next-player <- ::Player (= :x marker)]
   [?current-player <- ::CurrentPlayer]
   =>
   (apply rules/retract! ::Move ?moves)
   (apply rules/retract! ::MoveRequest ?move-requests)
   (rules/upsert! ::CurrentPlayer ?current-player assoc ::Player ?next-player)])

(defqueries queries
  [::move
   [:?position]
   [?move <- ::Move (= ?square Square) (= ?player Player)]
   [?square <- ::Square (= ?position position)]
   [?player <- ::Player (= ?marker marker)]]
  [::winning-square [:?position] [::WinningSquare (= ?square Square)] [?square <- ::Square (= ?position position)]]
  [::winner [] [?winner <- ::Winner]]
  [::game-over [] [?game-over <- ::GameOver]]
  [::current-player
   []
   [?current-player <- ::CurrentPlayer (= ?player Player)]
   [?player <- ::Player (= ?marker marker)]]
  [::move-request
   [:?position :?marker]
   [?request <- ::MoveRequest (= ?square Square) (= ?player Player)]
   [?square <- ::Square (= ?position position)]
   [?player <- ::Player (= ?marker marker)]]
  [::reset-request [] [?request <- ::ResetBoardRequest]])

(defsession session [common/rules rules queries])
