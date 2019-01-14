(require '[provisdom.simple.rules :refer :all :as rules]
         '[provisdom.maali.common :as common]
         '[provisdom.maali.rules :refer :all]
         '[datascript.core :as d])

(defn print-board
  [board]
  (doseq [row (partition 3 3 board)]
    (println (map (fn [[_ v]] (or v :.)) row))))

(def session (-> (create-session schema common/rules rules)
                 (transact [board])))

(def foo (:db (transact session [{:db/id [::rules/position 0]
                                  ::rules/played-by [::rules/marker :x]}
                                 {:db/id [::rules/position 1]
                                  ::rules/played-by [::rules/marker :x]}
                                 {:db/id [::rules/position 2]
                                  ::rules/played-by [::rules/marker :x]}])))

(def bar (:db (transact session [{::common/request 20
                                  :response/position 5}])))