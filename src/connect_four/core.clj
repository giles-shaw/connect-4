(ns connect-four.core
  (:gen-class)
)

(defn new-board [width height] (vec (repeat width (vec (repeat height nil)))))

(defn row [board ix] (vec (map (fn [column] (get column ix)) board)))

(defn rotate-board-by-90
  "Rotates the board by 90 degrees anticlockwise, so that the first column 
  becomes the first row"
  [board]
  (vec (map (partial row (reverse board)) (range (count (first board))))))

(defn get-diagonal
  [board start_x start_y]
  (let [width (count board) height (count (first board))
        limit (min (- width start_x) (- height start_y))]
    (vec (map #(get-in board [(+ start_x %) (+ start_y %)]) (range limit)))))

(defn get-diagonals
  [board]
  (let [width (count board) height (count (first board))
        starting_points (concat
                         (map vector (repeat height 0) (range height))
                         ; don't double count (0, 0)
                         (map vector (range  1 width) (repeat (- width 1) 0)))]
    (vec (map #(apply get-diagonal board %) starting_points))))

(defn update-board
  [board player move]
  (let [column (get board move) idx (count (take-while some? column))]
    (assoc-in board [move idx] player)))

(defn single-digit-str? [input] (contains? (set (map str (range 10))) input))

(defn validated-move [board move] 
  (if-let [parsed-move (if (single-digit-str? move) (Integer/parseInt move))]
    (when (some nil? (get board parsed-move)) parsed-move)))

(defn ask-move [board player]
  (println (str "Player " player ", please enter a move:"))
  (let [move (read-line)]
    (or (validated-move board move)
        (do
          (println "You fucking idiot," move "isn't a valid move. Try again!")
          (ask-move board player)))))

(defn longest-player-streak
  [column]
  (let [player-streaks (filter
         (fn [group] (some? (first group)))
         (partition-by identity column))
        streak-counts (map count player-streaks)]
    (if (seq streak-counts) (apply max streak-counts) 0)))

(defn vertical-four
  [board] (some (partial = 4) (map longest-player-streak board)))

(defn horizontal-four [board] (vertical-four (rotate-board-by-90 board)))

(defn up-diagonal-four [board] (vertical-four (get-diagonals board)))

(defn down-diagonal-four [board] (up-diagonal-four (rotate-board-by-90 board)))

(defn winning-state?
  [board]
  (some true? (map #(% board)
        [vertical-four horizontal-four up-diagonal-four down-diagonal-four])))

(def print-map {nil "•" 0 "o" 1 "x"})

(defn prettify [row] (apply str (interleave (repeat " ") (map print-map row))))

(defn display-board
  [board]
  (println "")
  (doall (map (comp println prettify (partial row board))
              (reverse (range (count (first board))))))
  nil)

(defn declare-winner [board player]
  (display-board board) (println "Hurray, Player" player "is the winner!"))

(defn play-turn
  [board player]
  (println (display-board board))
  (let
      [move (ask-move board player)
       new-board (update-board board player move)]
    (if-not (winning-state? new-board)
      (recur new-board (mod (+ 1 player) 2))
      (declare-winner new-board player))))

(defn -main [] (println "Welcome to Connect4!") (play-turn (new-board 7 6) 0))
