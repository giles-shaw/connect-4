(ns connect-four.core
  (:gen-class)
  (:require [connect-four.game :refer [new-game play]]
            [connect-four.prompts :refer [ask-move? choose-opponent? play-again?]]
            [connect-four.strategy :refer [compute-move]]
            [connect-four.visual :refer [report]]))

;;
; main
;;
(def player-1 (let [name "Player 1"]
                {:name name :token "o" :move-fn (ask-move? name)}))

(def player-2 (let [name "Player 2"]
                {:name name :token "x" :move-fn (ask-move? name)}))

(def player-2-computer (assoc player-2 :move-fn compute-move :computer? true))

(def opponents {:human player-2 :computer player-2-computer})

(defn -main []
  (let [welcome-message     "Welcome to Connect4!\n"
        players #(vector player-1 (opponents (choose-opponent?)))]
    (mapv println (lazy-cat [welcome-message]
                            (map report (play (new-game (players)))))))
  (if (play-again?) (recur) nil))
