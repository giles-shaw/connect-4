(ns connect-four.core
  (:gen-class)
  (:require [connect-four.board :refer [new-board]]
            [connect-four.prompts :refer [computer-opponent? play-again?]]
            [connect-four.visual :refer [report]]
            [connect-four.game :refer [play]]))

;;
; main
;;
(def human-pair [{:name "Player 1" :token "o"} {:name "Player 2" :token "x"}])

(def hybrid-pair [{:name "Player 1" :token "o"}
                      {:name "✨ AI ✨"  :token "x" :computer? true}])

(defn new-game
  [players]
  (let [[player-1, player-2] players
        board                (new-board 7 6)]
    {:board board :active-player player-1 :passive-player player-2}))

(defn determine-players [computer-opponent]
  (if computer-opponent hybrid-pair human-pair))

(defn -main []
  (let [welcome-message     "Welcome to Connect4!\n"
        players             #(determine-players (computer-opponent?))]
    (mapv println (lazy-cat [welcome-message]
                            (map report (play (new-game (players)))))))
  (if (play-again?) (recur) nil))
