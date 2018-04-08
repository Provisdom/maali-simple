(ns provisdom.simple.view
  (:require [rum.core :as rum]
            [provisdom.maali.common :as common]
            [provisdom.maali.rules :as rules]
            [provisdom.simple.rules :as simple]))

(defn click-handler
  [request]
  (when request
    #(common/respond-to request request)))

;;; Markup and styling from https://codepen.io/leesharma/pen/XbBGEj

(defn tile
  [session position]
  (let [c (click-handler (rules/query-one :?request session ::simple/move-request :?position position :?player :o))
        marker (rules/query-one :?player session ::simple/move :?position position)
        win? (rules/query-one :?winning-square session ::simple/winning-square :?position position)]
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
        reset-request (rules/query-one :?request session ::simple/reset-request)
        current-player (rules/query-one :?player session ::simple/current-player)
        game-over (rules/query-one :?game-over session ::simple/game-over)
        winner (rules/query-one :?player session ::simple/winner)]
    [:div
     [:h1 "Tic-Tac-Toe"]
     [:h2 (if game-over
            (condp = winner
              :x "X wins - BOW TO YOUR COMPUTER OVERLORD!"
              :o "O wins - I think I've been Kobayashi Maru'd"
              "Tie game. You are most logical, hooman.")
            (condp = current-player
             :x "I'm thinking - don't rush me!"
             :o "Your move hooman."
             "Uh-oh"))]

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
