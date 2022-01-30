(ns connect-four.game
  (:gen-class)
  (:require [connect-four.board
             :refer [incomplete-columns streak-counts update-board]]))

;;
; game logic
;;
(defn update-game
  [{:keys [active-player passive-player] :as game} updated-board]
  (assoc game :board updated-board :active-player
         passive-player :passive-player active-player))

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
  (let [players    [(game :active-player) (game :passive-player)]
        [opponent] (remove #{player} players)
        score-fn   (comp (partial token-score score-map board) :token)
        [player-score, opp-score] (map score-fn [player, opponent])]
  (- player-score opp-score)))

(defn look-ahead-move-scores
  ([{board :board {token :token} :active-player :as game} player n-turns mapfn]
  (if (or (zero? n-turns) (winning-state? board))
    {nil (snapshot-score streak-score-map game player)}
    (let [moves             (incomplete-columns board)
          updated-boards    (map (partial update-board board token) moves)
          updated-games     (map (partial update-game game) updated-boards)
          scores            (mapfn #(look-ahead-move-scores % player (dec n-turns))
                                  updated-games)]
      (zipmap moves (map (comp mean vals) scores)))))
  ([game player n-turns] (look-ahead-move-scores game player n-turns map)))

(defn look-ahead-strategy
  [{player :active-player :as game} n-turns]
  (let [move-scores (look-ahead-move-scores game player n-turns pmap)]
    (key (apply max-key val move-scores))))

(defn compute-move
  [game] (let [horizon 4] (look-ahead-strategy game horizon)))

(defn play-turn
  [{board :board {token :token move-fn :move-fn} :active-player :as game}]
  (let [move          (move-fn game)
        updated-board (update-board board token move)]
     (assoc (update-game game updated-board) :last-move move)))

(defn play
  [game]
  (condp apply [(game :board)]
    winning-state?                   [(assoc game :winner (game :passive-player))]
    (comp empty? incomplete-columns) [(assoc game :draw true)]
    (lazy-cat [game] (play (play-turn game)))))
