(ns connect-four.core
  (:gen-class)
  (:require [connect-four.board :refer [new-board]]
            [connect-four.game :refer [compute-move play]]
            [connect-four.prompts :refer [ask-move-fn computer-opponent? play-again?]]
            [connect-four.visual :refer [report]]))

;;
; main
;;
(def human-pair (mapv #(assoc % :move-fn (ask-move-fn %))
                      [{:name "Player 1" :token "o"}
                       {:name "Player 2" :token "x"}]))

(def hybrid-pair [(first human-pair)
                  {:name "✨ AI ✨"  :token "x"
                   :computer? true :move-fn compute-move}])

(defn new-game
  [players]
  (let [[player-1, player-2] players
        board                (new-board 7 6)]
    {:board board :active-player player-1 :passive-player player-2}))

(defn determine-players [opponent]
  (if (= opponent :computer) hybrid-pair human-pair))

(defn -main []
  (let [welcome-message     "Welcome to Connect4!\n"
        players             #(determine-players (computer-opponent?))]
    (mapv println (lazy-cat [welcome-message]
                            (map report (play (new-game (players)))))))
  (if (play-again?) (recur) nil))
