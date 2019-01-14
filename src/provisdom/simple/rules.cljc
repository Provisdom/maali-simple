(ns provisdom.simple.rules
  (:require [provisdom.simple.ai :as ai]
            [provisdom.maali.common :as common]))

(def schema
  {::square       {:db/cardinality :db.cardinality/many
                   :db/valueType :db.type/ref}
   ::played-by {:db/valueType :db.type/ref}
   ::player {:db/cardinality :db.cardinality/many
             :db/valueType :db.type/ref}
   ::current-player {:db/valueType :db.type/ref}
   ::marker {:db/unique :db.unique/identity}
   ::position     {:db/unique :db.unique/identity}
   ::move-request {:db/cardinality :db.cardinality/many
                   :db/valueType :db.type/ref}})

(def initial-board
  {::player [{::marker :x}
             {::marker :o}]
   ::current-player [::marker :o]
   ::square (mapv (fn [i] {::position i}) (range 9))
   ::response-fn identity})

(defn squares->board
  [squares]
  (reduce (fn [b {::keys [player position]}] (assoc b position player)) (sorted-map) squares))

(defn winner?
  [db marker squares]
  (println "*********" squares)
  (< 0 (ai/score-board (squares->board (map #(hash-map ::position % ::player marker) squares)) marker 0)))

(def rules
  {::empty-squares
   {:query  '[:find ?board (distinct ?square)
              :where
              [?board ::square ?square]
              (not [?square ::played-by _])]
    :rhs-fn (fn [?board empty-squares]
              [{:db/id ?board ::empty-squares empty-squares}])}

   ::player-squares
   {:query  '[:find ?player (distinct ?position)
              :where
              [?board ::player ?player]
              [?board ::square ?square]
              [?square ::position ?position]
              [?square ::played-by ?player]]
    :rhs-fn (fn [?player player-squares]
              [{:db/id ?player ::squares player-squares}])}

   ::winner
   {:query  '[:find ?board ?player
              :where
              [?board ::player ?player]
              [?player ::squares ?squares]
              [?player ::marker ?marker]
              [(provisdom.simple.rules/winner? $ ?marker ?squares)]]
    :rhs-fn (fn [?board ?player]
              [{:db/id ?board ::winner ?player}])}

   ::cats-game
   {:query  '[:find ?board
              :where
              [?board ::empty-count ?empty-count]
              [(= 9 ?empty-count)]
              (not [?board ::winner _])]
    :rhs-fn (fn [?board]
              [{:db/id ?board ::cats-game true}])}

   ::game-over
   {:query  '[:find ?board
              :where
              (or [?board ::winner _]
                  [?board ::cats-game true])]
    :rhs-fn (fn [?board]
              [{:db/id ?board ::game-over true}])}

   ::move-request
   {:query  '[:find ?board ?marker ?position ?response-fn
              :where
              [?board ::current-player ?player]
              [?player ::marker ?marker]
              [?board ::square ?square]
              [?square ::position ?position]
              (not [?square ::played-by _])
              (not [?board ::game-over true])
              [?board ::response-fn ?response-fn]]
    :rhs-fn (fn [?board ?marker ?position ?response-fn]
              [{:db/id         ?board
                ::move-request {:request/marker             ?marker
                                :request/position           ?position
                                ::common/response-fn ?response-fn}}])}

   ::move-response
   {:query  '[:find ?board ?player ?position ?next-player
              :where
              [?response ::common/request ?request]
              [?board ::move-request ?request]
              [?response :response/position ?position]
              [?request :request/marker ?marker]
              [?player ::marker ?marker]
              [?board ::player ?next-player]
              [(not= ?player ?next-player)]]
    :rhs-fn (fn [?board ?player ?position ?next-player]
              [{:unconditional? true
                :db/id      [::position ?position]
                ::played-by ?player}
               {:unconditional? true
                :db/id           ?board
                ::current-player ?next-player}])}

   ::reset-request
   {:query '[:find ?board ?response-fn
             :where
             [?board ::current-player ?player]
             [?player ::marker :o]
             [?board ::empty-squares ?squares]
             [(count ?squares) ?n]
             [(< ?n 9)]
             [?board ::response-fn ?response-fn]]
    :rhs-fn (fn [?board ?response-fn]
              [{:db/id ?board
                ::reset-request {:request/board ?board
                                 ::common/response-fn ?response-fn}}])}

   ::reset-response
   {:query '[:find ?board
             :where
             [?board ::reset-request ?request]
             [?response ::common/request ?request]]
    :rhs-fn (fn [?board]
              [[:db/retractEntity ?board]
               (assoc initial-board :unconditional? true)])}})

#_(defqueries queries
              [::move-request [:?position :?player] [?request <- ::MoveRequest (= ?position position) (= ?player player)]]
              [::reset-request [] [?request <- ::ResetBoardRequest]]
              [::current-player [] [::CurrentPlayer (= ?player player)]]
              [::move [:?position] [?move <- ::Move (= ?position position) (= ?player player)]]
              [::winner [] [?winner <- ::Winner (= ?player player)]]
              [::winning-square [:?position] [?winning-square <- ::WinningSquare (= ?position position)]]
              [::game-over [] [?game-over <- ::GameOver]])

#_(defsession session [#_common/rules rules queries])
