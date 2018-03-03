(ns provisdom.simple.rules
  (:require [clojure.spec.alpha :as s]
            [clojure.set :as set]
            [provisdom.simple.ai :as ai]
            [clara.rules.accumulators :as acc]
            [provisdom.maali.rules #?(:clj :refer :cljs :refer-macros) [defrules defqueries defsession def-derive] :as rules]
            [provisdom.maali.common :as common]))

(s/def ::player #{:x :o})
(s/def ::position (s/int-in 0 9))
(s/def ::Move (s/keys :req [::player ::position]))
(s/def ::CurrentPlayer (s/keys :req [::player]))
(s/def ::Winner (s/keys :req [::player]))
(s/def ::WinningSquare (s/keys :req [::position]))
(s/def ::CatsGame (s/keys))
(s/def ::GameOver (s/keys))

(def-derive ::MoveRequest ::common/Request (s/keys :req [::position ::player]))
(def-derive ::MoveResponse ::common/Response (s/keys :req [::position ::player]))
(def-derive ::ResetBoardRequest ::common/Request)

(defn squares->board
  [squares]
  (reduce (fn [b {::keys [player position]}] (assoc b position player)) (sorted-map) squares))

(defn print-board
  [board]
  (doseq [row (partition 3 3 board)]
    (println (map (fn [[_ v]] (or v :.)) row))))

(defn next-player
  [player]
  (condp = player :x :o :o :x))

(defrules rules
  [::winner!
   [?moves <- (acc/all) :from [::Move]]
   [::CurrentPlayer (= ?player (next-player player))]
   [:test (< 0 (ai/score-board (squares->board ?moves) ?player 0))]
   =>
   (let [player-positions (set (map ::position (filter #(= ?player (::player %)) ?moves)))
         winning-positions (some #(when (= 3 (count %)) %) (map (partial set/intersection player-positions) ai/winning-indices))]
     (doseq [position winning-positions]
       (rules/insert! ::WinningSquare {::position position}))
     (rules/insert! ::Winner {::player ?player}))]

  [::cats-game!
   [:not [::Winner]]
   [?count <- (acc/count) :from [::Move]]
   [:test (apply = [9 ?count])] ;Not using = here due to clara-rules issue #357
   =>
   (rules/insert! ::CatsGame {})]

  [::game-over!
   [:or [::Winner] [::CatsGame]]
   =>
   (rules/insert! ::GameOver {})]

  [::move-request!
   [:not [::GameOver]]
   [::CurrentPlayer (= ?player player)]
   [?moves <- (acc/all) :from [::Move]]
   [::common/ResponseFunction (= ?response-fn response-fn)]
   =>
   (let [empty-positions (set/difference (set (range 9)) (set (map ::position ?moves)))
         requests (map #(common/request {::position % ::player ?player} ::MoveResponse ?response-fn) empty-positions)]
     #_(apply rules/insert! ::MoveRequest requests)
     (condp = ?player
       :x (apply rules/insert! ::MoveRequest requests)
       :o (when-let [optimal-move (first (ai/optimal-moves (squares->board ?moves) :o 0))]
            (rules/insert! ::MoveRequest (some #(when (= optimal-move (::position %)) %) requests)))))]

  [::move-response!
   [?request <- ::MoveRequest (= ?position position)]
   [::MoveResponse (= ?request Request) (= ?position position) (= ?player player)]
   [?current-player <- ::CurrentPlayer]
   =>
   (rules/insert-unconditional! ::Move {::position ?position ::player ?player})
   (rules/upsert! ::CurrentPlayer ?current-player assoc ::player (next-player ?player))]

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
   [?current-player <- ::CurrentPlayer]
   =>
   (apply rules/retract! ::Move ?moves)
   (apply rules/retract! ::MoveRequest ?move-requests)
   (rules/upsert! ::CurrentPlayer ?current-player assoc ::player :x)])

(defqueries queries
  [::move [:?position] [?move <- ::Move (= ?position position) (= ?player player)]]
  [::winning-square [:?position] [?winning-square <- ::WinningSquare (= ?position position)]]
  [::winner [] [?winner <- ::Winner (= ?player player)]]
  [::game-over [] [?game-over <- ::GameOver]]
  [::current-player [] [::CurrentPlayer (= ?player player)]]
  [::move-request [:?position :?player] [?request <- ::MoveRequest (= ?position position) (= ?player player)]]
  [::reset-request [] [?request <- ::ResetBoardRequest]])

(defsession session [common/rules rules queries])
