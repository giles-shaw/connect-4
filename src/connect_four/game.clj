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

(defn possible-game-states-next-turn
  [{board :board {token :token} :current-player :as game}]
  (let [legal-moves    (incomplete-columns board)
        updated-boards (map (partial update-board board token) legal-moves)
        updated-games  (map (partial next-turn game) updated-boards)]
    (zipmap legal-moves updated-games)))

(defn mean [col] (if (seq col) (/ (apply + col) (count col)) 0))

(defn winning-state?
  [board]
  (some (fn [[k v]] (and (>= k 4) (> v 0))) (streak-counts board)))

(def streak-score-map {7 1e6 6 1e6 5 1e6 4 1e6 3 1e3 2 1e2 1 0})

(defn token-score
  [score-map board token]
  (let [token-streak-counts (streak-counts board token)
        score-fn (fn [[k v]] v * (score-map k))]
    (apply + (map score-fn token-streak-counts))))

(defn present-score
  [score-map {board :board :as game} player]
  (let [players    [(game :current-player) (game :next-player)]
        [opponent] (remove #{player} players)
        score-fn   (comp (partial token-score score-map board) :token)
        [player-score, opp-score] (map score-fn [player, opponent])]
  (- player-score opp-score)))

(defn look-ahead-score
  [{board :board :as game} player n-turns]
  (if (some true? [(zero? n-turns) (winning-state? board)])
    (present-score streak-score-map game player)
    (mean (map #(look-ahead-score % player (dec n-turns))
               (vals (possible-game-states-next-turn game))))))

(defn look-ahead-strategy
  [{player :current-player :as game} n-turns]
  (let [future-states  (possible-game-states-next-turn game)
        [moves games]  [(keys future-states) (vals future-states)]
        score-game     (fn [game] (look-ahead-score game player (dec n-turns)))
        move-scores    (zipmap moves (pmap score-game games))]
    (key (apply max-key val move-scores))))
        
(defn compute-move
  [game]
  (Thread/sleep 500)
  (let [horizon 4
        move    (look-ahead-strategy game horizon)]
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
    winning-state? (println (get-in game [:current-player :name]) "wins!")
    (comp empty? incomplete-columns) (println "It's a draw!")
    (recur (next-turn game updated-board)))))
