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
  (let [c (click-handler (common/query-one :?request session ::simple/move-request :?position position :?marker :o))
        marker (common/query-one :?marker session ::simple/move :?position position)
        win? (common/query-one :?square session ::simple/winning-square :?position position)]
    [:td {:id (str position)
          :class (cond-> "tile"
                   win? (str " winningTile")
                   c (str " clickable"))
          :on-click c}
     (condp = marker
       :x "X"
       :o "O"
       "")]))

(rum/defc app < rum/reactive [session-atom]
  (let [session (rum/react session-atom)
        tiles (map #(into [:tr] (map (partial tile session)) %) (partition 3 3 (range 9)))
        reset-request (common/query-one :?request session ::simple/reset-request)
        current-player (common/query-one :?marker session ::simple/current-player)]
    [:div
     [:h1 "Tic-Tac-Toe"]
     [:h2 (condp = current-player
            :x "I'm thinking - don't rush me!"
            :o "Your move hooman."
            "Uh-oh")]

     [:div {:id "left"}]
     [:div {:id "right"}]
     [:div {:id "top"}]
     [:div {:id "left"}]

     [:table {:id "board"}
      (into [:tbody] tiles)]

     [:button (cond-> {:id "reset"}
                      reset-request (assoc :on-click #(common/respond-to reset-request))
                      (not reset-request) (assoc :disabled true))
       "Reset Board"]

     [:p "Welcome to Tic-Tac-Toe! The objective of this game is to get three of your symbol (either X or O) in a row vertically, horizontally, or diagonally."]

     [:p "Good luck!"]]))



(defn ^:export run [session-atom]
  (rum/mount (app session-atom) (js/document.getElementById "app")))
