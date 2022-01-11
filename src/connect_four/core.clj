(ns connect-four.core
  (:gen-class)
  (:require [connect-four.board :refer [new-board]]
            [connect-four.prompts :refer [determine-players play-again?]]
            [connect-four.visual :refer [display-board]]
            [connect-four.game :refer [game]]))

;;
; main
;;
(def default-players [{:id :player-1 :name "Player 1" :board-symbol "o"}
                      {:id :player-2 :name "Player 2" :board-symbol "x"}])

(defn -main []
  (println "Welcome to Connect4!")
  (let [players (determine-players default-players) board (new-board 7 6)]
    (display-board board)
    (game board (cycle players)))
  (if (play-again?) (recur) nil))
