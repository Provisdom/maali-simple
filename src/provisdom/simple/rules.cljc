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
(s/def ::teaching-mode boolean?)
(s/def ::TeachingMode (s/keys :req [::teaching-mode]))

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
   "For all the moves made by a player, if any contains a win then set
    the ::Winner fact and mark the winning squares."
   [?moves <- (acc/all) :from [::Move (= ?player player)]]
   [:test (< 0 (ai/score-board (squares->board ?moves) ?player 0))]
   =>
   (let [player-positions (set (map ::position (filter #(= ?player (::player %)) ?moves)))
         winning-positions (some #(when (= 3 (count %)) %) (map (partial set/intersection player-positions) ai/winning-indices))]
     (doseq [position winning-positions]
       (rules/insert! ::WinningSquare {::position position}))
     (rules/insert! ::Winner {::player ?player}))]

  [::cats-game!
   "If nobody won and all of the squares have been used, it's a ::CatsGame."
   [:not [::Winner]]
   [?count <- (acc/count) :from [::Move]]
   [:test (apply = [9 ?count])] ;Not using = here due to clara-rules issue #357
   =>
   (rules/insert! ::CatsGame {})]

  [::game-over!
   "Game is over if either there is a ::Winner or ::CatsGame."
   [:or [::Winner] [::CatsGame]]
   =>
   (rules/insert! ::GameOver {})]

  [::move-request!
   "If the game isn't over, request ::Moves from the ::CurrentPlayer for
    eligible squares."
   [:not [::GameOver]]
   [::CurrentPlayer (= ?player player)]
   [?moves <- (acc/all) :from [::Move]]
   [::TeachingMode (= ?teaching-mode teaching-mode)]
   [::common/ResponseFunction (= ?response-fn response-fn)]
   =>
   (let [all-positions (set (range 9))
         empty-positions (set/difference  all-positions (set (map ::position ?moves)))
         requests (map #(common/request {::position % ::player ?player} ::MoveResponse ?response-fn) empty-positions)]
     (if ?teaching-mode
       (condp = ?player
         :x (apply rules/insert! ::MoveRequest requests)
         :o (when-let [optimal-moves (set (ai/optimal-moves (squares->board ?moves) :o 0))]
              (apply rules/insert! ::MoveRequest (filter (fn [%] (optimal-moves (::position %))) requests))))
       (apply rules/insert! ::MoveRequest requests)))]

  [::move-response!
   "Handle response to ::MoveRequest by inserting a new ::Move and switch
    ::CurrentPlayer to the opponent."
   [?request <- ::MoveRequest (= ?position position)]
   [::MoveResponse (= ?request Request) (= ?position position) (= ?player player)]
   [?current-player <- ::CurrentPlayer]
   =>
   (rules/insert-unconditional! ::Move {::position ?position ::player ?player})
   (rules/upsert! ::CurrentPlayer ?current-player assoc ::player (next-player ?player))]

  [::reset-board-request!
   "Request to reset the game."
   [?moves <- (acc/all) :from [::Move]]
   [::CurrentPlayer (= :o player)]
   [:test (not-empty ?moves)]
   [::common/ResponseFunction (= ?response-fn response-fn)]
   =>
   (rules/insert! ::ResetBoardRequest (common/request ::common/Response ?response-fn))]

  [::reset-board-response!
   "Handle response for game reset, retract all existing ::Move's,
    make :x the ::CurrentPlayer."
   [?request <- ::ResetBoardRequest]
   [::common/Response (= ?request Request)]
   [?moves <- (acc/all) :from [::Move]]
   [?current-player <- ::CurrentPlayer]
   =>
   (apply rules/retract! ::Move ?moves)
   (rules/upsert! ::CurrentPlayer ?current-player assoc ::player :x)])

(defqueries queries
  [::move-request [:?position :?player] [?request <- ::MoveRequest (= ?position position) (= ?player player)]]
  [::reset-request [] [?request <- ::ResetBoardRequest]]
  [::current-player [] [::CurrentPlayer (= ?player player)]]
  [::move [:?position] [?move <- ::Move (= ?position position) (= ?player player)]]
  [::winner [] [?winner <- ::Winner (= ?player player)]]
  [::winning-square [:?position] [?winning-square <- ::WinningSquare (= ?position position)]]
  [::game-over [] [?game-over <- ::GameOver]])

(defsession session [#_common/rules rules queries])
