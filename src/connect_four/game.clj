(ns connect-four.game
  (:gen-class)
  (:require [connect-four.board
             :refer [column-not-full? full? new-board rotate-board-by-90
                     update-board upwards-diagonals width]]
            [connect-four.prompts :refer [ask-move]]
            [connect-four.visual :refer [display-board]]))

;;
; game logic
;;
(defn longest-streak
  ([column]
  (let [value-streaks    (partition-by identity column)
        non-null-streaks (filter #(some? (first %)) value-streaks)
        streak-counts    (map count non-null-streaks)]
    (if (seq streak-counts) (apply max streak-counts) nil)))
  ([column token] (longest-streak (map #(if (= % token) token nil) column))))


(defn streak-counts
  [board token]
  (let [ cols           board
        rows           (rotate-board-by-90 cols)
        up-diags       (upwards-diagonals cols)
        down-diags     (upwards-diagonals rows)
        candidates     (concat cols rows up-diags down-diags)
        streaks        (map #(longest-streak % token) candidates)]
    (frequencies streaks)))

(def streak-score-map {4 1000000 3 1000 2 100 1 0})

(defn score
  [score-map game player]
  (let [board                  (game :board)
        opponent               (first (filter #(not= % player) [(game :player-1) (game :player-2)]))
        streak-counts          (streak-counts board (player :token))
        opponent-streak-counts (streak-counts board (opponent :token))
        player-score           (apply + (map #(Math/pow (score-map %)
                                                        (get streak-counts % 0) ) (range 1 5)))
        opponent-score         (apply + (map #(Math/pow (score-map %)
                                                        (get opponent-streak-counts % 0)) (range 1 5)))]
      (- player-score opponent-score)))

(def board (new-board 7 6))
(def board2 (reduce
              #(update-board %1 (first %2) (second %2))
              board
              [["x", 3] ["x", 3] ["x" 3]]))
(streak-counts board2 "x")
(streak-counts board "x")
(score streak-score-map board2 "x")

(defn winning-state?
  [board] (let
           [cols       board
            rows       (rotate-board-by-90 cols)
            up-diags   (upwards-diagonals cols)
            down-diags (upwards-diagonals rows)
            candidates (concat cols rows up-diags down-diags)]
            (some #(>= % 4) (map longest-streak candidates))))

(defn winning-moves
  [board token]
  (let [legal-moves (filter (partial column-not-full? board) (range (width board)))]
  (filter (comp winning-state?
                (partial update-board board token)) legal-moves)))

(defn random-guess-strategy [board player]
  (let [candidate (rand-int (width board))]
    (or (column-not-full? board candidate) (recur board player))))

(defn look-ahead-once-strategy
  [game]
  (let [board         (game :board)
        player        (game :current-player)
        opposition    (game :next-player)
        player-wins   (winning-moves board (player :token))
        opponent-wins (winning-moves board (opposition :token))]
    (or (first player-wins) (first opponent-wins) (random-guess-strategy board player))))

(defn mean [col] (if (seq col) (/ (apply + col) (count col)) 0))

(defn look-ahead-score
  [game player n-turns]
  (let [legal-moves (filter (partial column-not-full? board) (range (width board)))]
    (if (some true? ((= 0 n-turns) (nil? legal-moves) (winning-state? board)))
        (score streak-score-map game player)
        (mean (map #(look-ahead-score (assoc game
                                             :board (update-board board (player :token) %)
                                             :current-player (game :next-player)
                                             :next-player (game :current-player))
                                      player
                                      (dec n-turns))
                   legal-moves)))))

(defn compute-move
  [game]
  (Thread/sleep 1000)
  (let [move (look-ahead-once-strategy game)]
    (println (get-in game [:current-player :name]) "chose" move) move))

(defn play-turn
  [game]
  (let [board  (game :board)
        player (game :current-player)
        move   (if (:computer? player)
               (compute-move game)
               (ask-move board player))]
    (update-board board (player :token) move)))

(defn play
  [game]
  (let [updated-board (play-turn game)]
  (display-board updated-board)
  (condp apply [updated-board]
    winning-state? (println (get-in game [:current-player :name]) "wins!")
    full?          (println "It's a draw!")
    (recur (assoc game
                  :board updated-board
                  :current-player (game :next-player)
                  :next-player (game :current-player))))))
