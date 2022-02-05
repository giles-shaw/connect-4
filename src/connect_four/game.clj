(ns connect-four.game
  (:gen-class)
  (:require [connect-four.board
             :refer [incomplete-columns new-board streak-counts update-board]]))

;;
; game logic
;;
(defn winning-state?
  [board]
  (let [long-enough (fn [[streak-length n_occurrences]]
                     (and (>= streak-length 4) (> n_occurrences 0)))]
  (some long-enough (streak-counts board))))

(defn new-game
  [players]
  (let [[player-1, player-2] players
        board                (new-board 7 6)]
    {:board board :active-player player-1 :passive-player player-2}))

(defn update-game
  [{:keys [active-player passive-player board] :as game} move]
  (let [updated-board (update-board board (active-player :token) move)]
  (assoc game :board updated-board :active-player passive-player
         :passive-player active-player)))

(defn play-turn
  [{{move-fn :move-fn} :active-player :as game}]
  (let [move          (move-fn game)]
     (assoc (update-game game move) :last-move move)))

(defn play
  [game]
  (condp apply [(game :board)]
    winning-state?                   [(assoc game :winner (game :passive-player))]
    (comp empty? incomplete-columns) [(assoc game :draw true)]
    (lazy-cat [game] (play (play-turn game)))))
