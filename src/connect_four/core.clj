(ns connect-four.core
  (:gen-class)
  (:require [connect-four.board :refer [new-board]]
            [connect-four.prompts :refer [determine-players play-again?]]
            [connect-four.visual :refer [render-board report]]
            [connect-four.game :refer [play]]))

;;
; main
;;
(def default-players [{:name "Player 1" :token "o"}
                      {:name "Player 2" :token "x"}])

(defn game-turns
  [board players]
  (let [game (zipmap [:board :active-player :passive-player]
                     (cons board players))]
    (rest (play game))))

(defn -main []
  (let [welcome-message     "Welcome to Connect4!\n"
        board               (new-board 7 6)
        players             #(determine-players default-players)]

    (mapv println (lazy-cat [welcome-message (render-board board)]
                            (map report (game-turns board (players))))))
  (if (play-again?) (recur) nil))
