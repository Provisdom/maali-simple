(ns provisdom.simple.view
  (:require [rum.core :as rum]
            [provisdom.maali.common :as common]
            [provisdom.maali.rules :as rules]
            [provisdom.simple.rules :as simple]))

(defn click-handler
  [request]
  (when request
    #(common/respond-to request request)))

(defn tile
  [session position]
  (let [c (click-handler (common/query-one :?request session ::simple/move-request :?position position))
        marker (common/query-one :?marker session ::simple/square :?position position)]
    [:td {:id (str position)
          :class "tile"
          :on-click c}
     (condp = marker
       :x "X"
       :o "O"
       "")]))

(rum/defc app < rum/reactive [session-atom]
          (println "Farkbottle")
  (let [session (rum/react session-atom)
        tiles (map #(into [:tr] (map (partial tile session)) %) (partition 3 3 (range 9)))]
    (println (into [:tbody] tiles))
    [:div
     [:h1 "Tic-Tac-Toe"]

     [:div {:id "left"}]
     [:div {:id "right"}]
     [:div {:id "top"}]
     [:div {:id "left"}]

     [:table {:id "board"}
      (into [:tbody] tiles)]

     [:button {:id "reset"} "Reset Board"]

     [:p "Welcome to Tic-Tac-Toe! The objective of this game is to get three of your symbol (either X or O) in a row vertically, horizontally, or diagonally."]

     [:p "Good luck!"]]))



(defn ^:export run [session-atom]
  (rum/mount (app session-atom) (js/document.getElementById "app")))
