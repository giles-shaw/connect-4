(ns connect-four.game
  (:gen-class)
  (:require [connect-four.board
             :refer [incomplete-columns streak-counts update-board]]
            [connect-four.prompts :refer [ask-move]]
            [connect-four.visual :refer [display-board]]))

;;
; game logic
;;
(defn next-turn
  [{:keys [current-player next-player] :as game} updated-board]
  (assoc game :board updated-board :current-player
         next-player :next-player current-player))

(defn mean [col] (if (seq col) (/ (apply + col) (count col)) 0))

(defn winning-state?
  [board]
  (some (fn [[streak-length n_occurrences]]
          (and (>= streak-length 4) (> n_occurrences 0))) (streak-counts board)))

(def streak-score-map {7 1e6 6 1e6 5 1e6 4 1e6 3 1e3 2 1e2 1 0})

(defn token-score
  [score-map board token]
  (let [token-streak-counts (streak-counts board token)
        score-fn (fn [[streak-length n_occurrences]]
                   n_occurrences * (score-map streak-length))]
    (apply + (map score-fn token-streak-counts))))

(defn snapshot-score
  [score-map {board :board :as game} player]
  (let [players    [(game :current-player) (game :next-player)]
        [opponent] (remove #{player} players)
        score-fn   (comp (partial token-score score-map board) :token)
        [player-score, opp-score] (map score-fn [player, opponent])]
  (- player-score opp-score)))

(defn look-ahead-move-scores
  [{board :board {token :token} :current-player :as game} player n-turns]
  (if (or (zero? n-turns) (winning-state? board))
    {nil (snapshot-score streak-score-map game player)}
    (let [moves             (incomplete-columns board)
          updated-boards    (map (partial update-board board token) moves)
          updated-games     (map (partial next-turn game) updated-boards)
          scores            (map #(look-ahead-move-scores % player (dec n-turns))
                                  updated-games)]
      (zipmap moves (map (comp mean vals) scores)))))

(defn look-ahead-strategy
  [{player :current-player :as game} n-turns]
  (let [move-scores (look-ahead-move-scores game player n-turns)]
    (key (apply max-key val move-scores))))

(defn compute-move
  [game]
  (let [horizon 4
        move    (look-ahead-strategy game horizon)]
    (println (get-in game [:current-player :name]) "chose" move) move))

(defn play-turn
  [{:keys [board current-player] :as game}]
  (let [move          (if (:computer? current-player)
                        (compute-move game)
                        (ask-move board current-player))
        updated-board (update-board board (current-player :token) move)]
    (display-board updated-board move) updated-board))

(defn play
  [game]
  (let [updated-board (play-turn game)]
  (condp apply [updated-board]
    winning-state? (println (get-in game [:current-player :name]) "wins!")
    (comp empty? incomplete-columns) (println "It's a draw!")
    (recur (next-turn game updated-board)))))
