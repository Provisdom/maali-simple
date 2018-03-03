(ns provisdom.simple.ai
 (:require [clojure.set :as set]))

(def winning-indices #{#{0 1 2}
                       #{3 4 5}
                       #{6 7 8}
                       #{0 3 6}
                       #{1 4 7}
                       #{2 5 8}
                       #{0 4 8}
                       #{2 4 6}})

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
        board-full (= 9 (count board'))]
    (if (or board-full (not= 0 score))
      score
      (let [opponent-marker (if (= marker :x) :o :x)
            opponent-move (choose-move board' opponent-marker (inc depth))
            score (- (score-move board' opponent-marker opponent-move (inc depth)))]
        #_(println opponent-marker opponent-move score)
        score))))

(defn optimal-moves*
  [board marker depth]
  (let [empty-positions (set/difference (set (range 9)) (set (keys board)))
        scores (into {} (map (fn [p] [p (score-move board marker p depth)]) empty-positions))]
    (first (reduce (fn [[p-max s-max] [p s]]
                     (cond
                       (> s s-max) [[p] s]
                       (= s s-max) [(conj p-max p) s]
                       :else [p-max s-max]))
                   [[] -100] scores))))

(def optimal-moves (memoize optimal-moves*))

(defn choose-move
  [board marker depth]
  (when-let [optimal-moves (not-empty (optimal-moves board marker depth))]
    (rand-nth optimal-moves)))
