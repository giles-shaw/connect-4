(ns connect-four.core
  (:gen-class)
)

;;
; board logic
;;
(defn new-board [width height] (vec (repeat width (vec (repeat height nil)))))

(defn width [board] (count board))

(defn height [board] (count (first board)))

(defn transpose [board] (vec (apply map vector board)))

(defn rotate-board-by-90 [board] (vec (reverse (transpose board))))

(defn diagonal
  [board x y]
  (let [infinite-diag (map #(get-in board [(+ % x) (+ % y)] :outside)
                           (iterate inc 0))]
    (vec (take-while (partial not= :outside) infinite-diag))))

(defn get-diagonals
  [board]
  (let [starting_points (concat (map vector (repeat 0) (range (height board)))
                                ; don't double count (0, 0)
                                (map vector (range  1 (width board)) (repeat 0)))]
    (vec (map #(apply diagonal board %) starting_points))))


;;
; display logic
;;
(def print-map {nil "•" 0 "o" 1 "x"})

(defn prettify [row] (apply str (interleave (repeat " ") (map print-map row))))

(defn display-board
  [board]
  (doseq [row (reverse (transpose board))] (println (prettify row)))
  (println ""))


;;
; user interaction
;;
(defn single-digit-str? [input] (contains? (set (map str (range 10))) input))

(defn validated-move [board move] 
  (if-let [parsed-move (if (single-digit-str? move) (Integer/parseInt move))]
    (when (some nil? (get board parsed-move)) parsed-move)))

(defn ask-move [board player]
  (println (str "Player " player ", please enter a move:"))
  (let [move (read-line)]
    (or (validated-move board move)
        (do (println move "isn't a valid move. Try again!")
            (ask-move board player)))))


;;
; game logic
;;
(defn longest-streak
  [column]
  (let [player-streaks (filter
         (fn [group] (some? (first group)))
         (partition-by identity column))
        streak-counts (map count player-streaks)]
    (if (seq streak-counts) (apply max streak-counts) 0)))

(defn winning-state?
  [board] (let
              [columns board
               rows (rotate-board-by-90 columns)
               up-diagonals (get-diagonals columns)
               down-diagonals (get-diagonals rows)]
            (some (partial = 4)
                  (map longest-streak
                       (concat columns rows up-diagonals down-diagonals)))))

(defn update-board
  [board player move]
  (let [column (get board move) idx (count (take-while some? column))]
    (assoc-in board [move idx] player)))

(defn play-turn
  [board player]
  (display-board board)
  (let
      [move (ask-move board player)
       next-board (update-board board player move)]
    (if-not (winning-state? next-board)
      (recur next-board (mod (+ 1 player) 2))
      (do (display-board next-board)
          (println "Hurray, Player" player "is the winner!")))))


;;
; main
;;
(defn -main [] (println "Welcome to Connect4!\n") (play-turn (new-board 7 6) 0))
