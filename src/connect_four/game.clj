(ns connect-four.game
  (:gen-class)
  (:require [connect-four.board
             :refer [column-not-full? full? rotate-board-by-90 upwards-diagonals width]]
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

(defn update-board
  [board player move]
  (let [column (get board move) idx (count (take-while some? column))]
    (assoc-in board [move idx] (:board-symbol player))))

(defn random-guess-strategy [board player]
  (let [candidate  (rand-int (width board))]
    (or (column-not-full? board candidate) (recur board player))))

(defn compute-move
  [board player]
  (Thread/sleep 1000)
  (let [move (random-guess-strategy board player)]
    (println (:name player) "chose" move) move))

(defn play-turn
  [board player]
  (let [move (if (:computer? player)
               (compute-move board player)
               (ask-move board player))]
    (update-board board player move)))

(defn game
  [board players]
  (display-board board)
  (condp apply [board]
    winning-state? (println "Hurray, you win!")
    full?          (println "It's a draw!")
    (recur (play-turn board (first players)) (rest players))))
