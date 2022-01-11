(ns connect-four.game
  (:gen-class)
  (:require [connect-four.board
             :refer [column-not-full? competitor-token full?
                     rotate-board-by-90 update-board upwards-diagonals width]]
            [connect-four.prompts :refer [ask-move]]
            [connect-four.visual :refer [display-board]]))

;;
; game logic
;;
(defn winning-streak
  [column]
  (let [value-streaks    (partition-by identity column)
        non-null-streaks (filter #(some? (first %)) value-streaks)
        streak-counts    (map count non-null-streaks)]
    (if (seq streak-counts) (<= 4 (apply max streak-counts)) nil)))

(defn winning-state?
  [board] (let
           [cols       board
            rows       (rotate-board-by-90 cols)
            up-diags   (upwards-diagonals cols)
            down-diags (upwards-diagonals rows)
            candidates (concat cols rows up-diags down-diags)]
            (some true? (map winning-streak candidates))))

(defn winning-moves
  [board token]
  (let [legal-moves (filter (partial column-not-full? board) (range (width board)))]
  (filter (comp winning-state?
                (partial update-board board token)) legal-moves)))

(defn random-guess-strategy [board player]
  (let [candidate (rand-int (width board))]
    (or (column-not-full? board candidate) (recur board player))))

(defn look-ahead-once-strategy
  [board player]
  (let [player-wins (winning-moves board (player :token))
        opposition (competitor-token (player :token) board)
        opponent-wins (if opposition (winning-moves board opposition) nil)]
    (or (first player-wins) (first opponent-wins) (random-guess-strategy board player))))

(defn compute-move
  [board player]
  (Thread/sleep 1000)
  (let [move (look-ahead-once-strategy board player)]
    (println (:name player) "chose" move) move))

(defn play-turn
  [board player]
  (let [move (if (:computer? player)
               (compute-move board player)
               (ask-move board player))]
    (update-board board (player :token) move)))

(defn game
  [board players]
  (let [player (first players) updated-board (play-turn board player)]
  (display-board updated-board)
  (condp apply [updated-board]
    winning-state? (println (player :name) "wins!")
    full?          (println "It's a draw!")
    (recur updated-board (rest players)))))
