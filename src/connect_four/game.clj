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
  (let [cols           board
        rows           (rotate-board-by-90 cols)
        up-diags       (upwards-diagonals cols)
        down-diags     (upwards-diagonals rows)
        candidates     (concat cols rows up-diags down-diags)
        streaks        (map #(longest-streak % token) candidates)]
    (frequencies (filter some? streaks))))

(def streak-score-map {4 1e6 3 1e3 2 1e2 1 0})

(defn score
  [score-map game player]
  (let [board                  (game :board)
        opponent               (first (filter #(not= % player)
                                              [(game :current-player) (game :next-player)]))
        player-streak-counts   (streak-counts board (player :token))
        opponent-streak-counts (streak-counts board (opponent :token))
        player-score           (apply + (map #(* (score-map %) (get player-streak-counts % 0))
                                             (range 1 5)))
        opponent-score         (apply + (map #(* (score-map %) (get opponent-streak-counts % 0))
                                             (range 1 5)))]
      (- player-score opponent-score)))


(defn winning-state?
  [board] (let
           [cols       board
            rows       (rotate-board-by-90 cols)
            up-diags   (upwards-diagonals cols)
            down-diags (upwards-diagonals rows)
            candidates (concat cols rows up-diags down-diags)]
            (some #(>= % 4) (filter some? (map longest-streak candidates)))))

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
  (let [board       (game :board)
        legal-moves (filter (partial column-not-full? board) (range (width board)))]
    (if (some true? [(= 0 n-turns) (nil? legal-moves) (winning-state? board)])
        (score streak-score-map game player)
        (mean (map #(look-ahead-score (assoc game
                                             :board (update-board board
                                                                  (get-in game
                                                                          [:current-player :token]) 
                                                                  %)
                                             :current-player (game :next-player)
                                             :next-player (game :current-player))
                                      player
                                      (dec n-turns))
                   legal-moves)))))


(defn look-ahead-strategy
  [game n-turns]
  (let [player        (game :current-player)
        my-board      (game :board)
        legal-moves   (filter (partial column-not-full? my-board) (range (width my-board)))
        future-states (map #(update-board my-board (player :token) %) legal-moves)
        move-scores   (zipmap legal-moves
                              (map #(look-ahead-score (assoc game
                                                             :board %
                                                             :current-player (game :next-player)
                                                             :next-player player)
                                                      player
                                                      (dec n-turns))
                                   future-states))]
    (key (apply max-key val move-scores))))
        
(def board (new-board 7 6))
(def board2 (reduce
              #(update-board %1 (first %2) (second %2))
              board
              [
               ["o" 3]
               ["x" 5]
               ["o" 2]
               ["x" 5]
               ["o" 4]]))

(display-board board2)
; (def board2 (update-board board "o" 3))

(def game {:board board2 ; (update-board board2 "x" 5)
           :current-player {:name "Player 2" :token "x"}
           :next-player {:name "Player 1" :token "o"}})
(def game2 (assoc game
                         :board (update-board (game :board) "x" 0)
                         :current-player (game :next-player)
                         :next-player (game :current-player)))

(def game3 (assoc game2 :board (update-board (game2 :board) "o" 1)
                        :current-player (game2 :next-player)
                        :next-player (game2 :current-player)))
(display-board (game :board))
(game3 :current-player)
(score streak-score-map game (game :next-player))
(look-ahead-strategy game 2)

(let [player        (game :current-player)
      my-board      (game :board)
      legal-moves   (filter (partial column-not-full? my-board) (range (width my-board)))
      future-states (map #(update-board my-board (player :token) %) legal-moves)
      move-scores   (zipmap legal-moves
                            (map #(look-ahead-score (assoc game :board %
                                                           :current-player (game :next-player)
                                                           :next-player (game :current-player))
                                                    {:name "Player 2" :token "x"}
                                                    (dec 2))
                                 future-states))]
  move-scores)
(filter (partial column-not-full? board2) (range 7))
(look-ahead-score (assoc game
                         :board (update-board (game :board) "x" 0)
                         :current-player (game :next-player)
                         :next-player (game :current-player))
                  (game :current-player)
                  1)

(look-ahead-score game {:name "Player 2" :token "x"} 2)
(look-ahead-score game2 {:name "Player 2" :token "x"} 1)
(look-ahead-score game3 {:name "Player 2" :token "x"} 0)
(defn compute-move
  [game]
  (Thread/sleep 1000)
  (let [move (look-ahead-strategy game 4)]
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
