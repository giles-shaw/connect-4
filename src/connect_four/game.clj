(ns connect-four.game
  (:gen-class)
  (:require [connect-four.board
             :refer [column-not-full? new-board rotate-board-by-90
                     update-board upwards-diagonals width]]
            [connect-four.prompts :refer [ask-move]]
            [connect-four.visual :refer [display-board]]))

;;
; game logic
;;
(defn legal-moves [board] (filter (partial column-not-full? board) (range (width board))))

(defn next-turn
  [{:keys [current-player next-player] :as game} updated-board]
  (assoc game :board updated-board :current-player next-player :next-player current-player))

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
  [score-map {board :board :as game} player]
  (let [[opponent]               (filter #(not= % player)
                                         [(game :current-player) (game :next-player)])
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

(defn mean [col] (if (seq col) (/ (apply + col) (count col)) 0))

(defn possible-game-states-next-turn
  [{board :board {token :token} :current-player :as game}]
  (let [moves          (legal-moves board)
        updated-boards (map (partial update-board board token) moves)
        updated-games  (map (partial next-turn game) updated-boards)]
    (zipmap moves updated-games)))

(defn look-ahead-score
  [{board :board :as game} player n-turns]
  (if (some true? [(zero? n-turns) (winning-state? board)])
    (score streak-score-map game player)
    (mean (map #(look-ahead-score % player (dec n-turns))
               (vals (possible-game-states-next-turn game))))))

(defn look-ahead-strategy
  [{player :current-player :as game} n-turns]
  (let [future-states  (possible-game-states-next-turn game)
        [moves games] [(keys future-states) (vals future-states)]
        score-game     (fn [game] (look-ahead-score game player (dec n-turns)))
        move-scores    (zipmap moves (pmap score-game games))]
    (key (apply max-key val move-scores))))
        
(defn compute-move
  [game]
  (Thread/sleep 500)
  (let [future-horizon 4
        move           (look-ahead-strategy game future-horizon)]
    (println (get-in game [:current-player :name]) "chose" move) move))

(defn play-turn
  [{board :board player :current-player :as game}]
  (let [move (if (:computer? player)
               (compute-move game)
               (ask-move board player))]
    (update-board board (player :token) move)))

(defn play
  [game]
  (let [updated-board (play-turn game)]
  (display-board updated-board)
  (condp apply [updated-board]
    winning-state?                     (println (get-in game [:current-player :name]) "wins!")
    (comp empty? legal-moves)          (println "It's a draw!")
    (recur (next-turn game updated-board)))))
