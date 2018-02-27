(ns provisdom.simple.rules
  (:require [clojure.spec.alpha :as s]
            [clojure.set :as set]
            [clara.rules.accumulators :as acc]
            [provisdom.maali.rules #?(:clj :refer :cljs :refer-macros) [defrules defqueries defsession def-derive] :as rules]))

(s/def ::marker (s/nilable #{:x :o}))
(s/def ::position (s/int-in 0 9))
(s/def ::Player (s/keys :req [::marker]))
(s/def ::Square (s/keys :req [::marker ::position]))
(s/def ::CurrentPlayer (s/keys :req [::marker]))
(s/def ::Winner (s/keys :req [::Player]))
(s/def ::GameOver (s/keys))

(def winning-indices [#{0 1 2}
                      #{3 4 5}
                      #{6 7 8}
                      #{0 3 6}
                      #{1 4 7}
                      #{2 5 8}
                      #{0 4 8}
                      #{2 4 6}])

(defn score-board
  [board marker depth]
  (let [opponent-marker (if (= marker :x) :o :x)
        by-mark (group-by val board)
        marks (set (map key (by-mark marker)))
        opponent-marks (set (map key (by-mark opponent-marker)))
        win (some #(set/subset? % marks) winning-indices)
        opponent-win (some #(set/subset? % opponent-marks) winning-indices)]
    (cond
      win (- 10 depth)
      opponent-win (- depth 10)
      :else 0)))

#_(def score-board (memoize score-board*))

(declare choose-move)

(defn score-move
  [board marker position depth]
  (let [board' (assoc board position marker)
        score (score-board board' marker depth)
        board-full (every? (comp some? val) board')]
    (if (or board-full (not= 0 score))
      score
      (let [opponent-marker (if (= marker :x) :o :x)
            opponent-move (choose-move board' opponent-marker (inc depth))
            score (- (score-move board' opponent-marker opponent-move (inc depth)))]
        #_(println opponent-marker opponent-move score)
        score))))

(defn choose-move*
  [board marker depth]
  #_(Thread/sleep 200)
  (let [empty-positions (map key (filter (comp nil? val) board))
        scores (into {} (map (fn [p] [p (score-move board marker p depth)]) empty-positions))]
    (first (reduce (fn [[p-max s-max] [p s]] (if (> s s-max) [p s] [p-max s-max])) [-1 -100] scores))))

(def choose-move (memoize choose-move*))

(defn squares->board
  [squares]
  (reduce (fn [b {::keys [marker position]}] (assoc b position marker)) (sorted-map) squares))

(defn print-board
  [board]
  (println board)
  (doseq [row (partition 3 3 board)]
    (println (map (fn [[_ v]] (or v :.)) row))))

(defrules rules
  ;;; TODO - handle this shit correctly. Request/repsonse?
  [::next-move!
   [:not [::GameOver]]
   [?current-player <- ::CurrentPlayer (= :x marker)]
   [?squares <- (acc/all) :from [::Square]]
   =>
   (let [next-move (choose-move (squares->board ?squares) :x 0)]
     (rules/upsert! ::CurrentPlayer ?current-player assoc ::marker :o)
     (rules/upsert! ::Square {::position next-move ::marker nil} assoc ::marker :x))]

  [::winner!
   [?squares <- (acc/all) :from [::Square]]
   [?player <- ::Player (= ?marker marker)]
   [:test (< 0 (score-board (squares->board ?squares) ?marker 0))]
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
  [::squares [] [?squares <- (acc/all) :from [::Square]]]
  [::winner [] [?winner <- ::Winner]]
  [::game-over [] [?game-over <- ::GameOver]])

(defsession session [rules queries])

(defn init-session
  [session]
  (let [session (-> session
                    (rules/insert ::Player {::marker :x})
                    (rules/insert ::Player {::marker :o})
                    (rules/insert ::CurrentPlayer {::marker :x}))
        squares (map (fn [p] {::position p ::marker nil}) (range 9))
        session (apply rules/insert session ::Square squares)]
    (rules/fire-rules session)))

(defn move-o
  [s p]
  (-> s
      (rules/upsert ::Square {::marker nil ::position p} assoc ::marker :o)
      (rules/upsert ::CurrentPlayer {::marker :o} assoc ::marker :x)
      (rules/fire-rules)))

(comment
 (def s (init-session session))
 (rules/query s ::squares)
 (print-board (squares->board (:?squares (first (rules/query s ::squares)))))
 (rules/query s ::game-over)
 (rules/query s ::winner)
 (def s (move-o s 4))
 (def s (move-o s 2))
 (def s (move-o s 8))
 (def s (move-o s 6))


 (def board (squares->board (:?squares (first (rules/query s ::squares)))))
 foo)
