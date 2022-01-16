(ns connect-four.core
  (:gen-class)
  (:require [connect-four.board :refer [new-board]]
            [connect-four.prompts :refer [determine-players play-again?]]
            [connect-four.visual :refer [display-board]]
            [connect-four.game :refer [play]]))

;;
; main
;;
(def default-players [{:name "Player 1" :token "o"}
                      {:name "Player 2" :token "x"}])

(defn -main []
  (println "Welcome to Connect4!")
  (let [[player-1 player-2] (determine-players default-players)
        board                (new-board 7 6)
        game                 {:board board
                              :current-player player-1
                              :next-player player-2}]
    (display-board board)
    (play game))
  (if (play-again?) (recur) nil))
