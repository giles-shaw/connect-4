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
  [board [x y]]
  (let [infinite-diag (map #(get-in board [(+ % x) (+ % y)] :outside)
                           (iterate inc 0))]
    (vec (take-while (partial not= :outside) infinite-diag))))

(defn get-diagonals
  [board]
  (let [starting_points (concat (map vector (repeat 0) (range (height board)))
                                ; start from (1, 0) to not double count (0, 0)
                                (map vector (range  1 (width board)) (repeat 0)))]
    (vec (map (partial diagonal board) starting_points))))


;;
; display logic
;;

(defn space-out [str-seq] (apply str (rest (interleave (repeat " ") str-seq))))

(defn format_ [row] (space-out (map (fn [s] (or s "•")) row)))

(defn display-board
  [board]
  (println)
  (doseq [row (reverse (transpose board))] (println (format_ row)))
  (println (apply str (repeat (- (* 2 (width board)) 1)  "-")))
  (println (space-out (range (width board))))
  (println))


;;
; user interaction
;;
(defn single-digit-str? [input] (contains? (set (map str (range 10))) input))

(defn legal-move? [board move] (some nil? (get board move)))

(defn validated-move
  [board move] 
  (if-let [parsed-move (if (single-digit-str? move) (Integer/parseInt move))]
    (when (legal-move? board parsed-move) parsed-move)))

(defn ask-move
  [board player]
  (println (str (:name player) ", please enter a move:"))
  (let [move (read-line)]
    (or (validated-move board move)
        (do (println move "isn't a valid move. Try again!")
            (ask-move board player)))))

(defn play-again? []
  (println "Play another game (y / n)?")
  (let [choice (read-line)]
    (condp = choice
      "y" true
      "n" (do (println "Bye!") (System/exit 0))
      (do (println "Unrecognised option - enter 'y' or 'n'") (play-again?)))))

(defn determine-players []
  (println "Are you playing against a real opponent or the computer (r / c)?")
  (let [choice          (read-line)
        default-players [{:id :player-1 :name "Player 1" :board-symbol "o"}
                         {:id :player-2 :name "Player 2" :board-symbol "x"}]]
    (condp = choice
      "r" default-players
      "c" (assoc-in default-players [1 :computer?] true)
      (do (println "Unrecognised option - enter 'r' or 'c'")
          (determine-players)))))

;;
; game logic
;;
(defn winning-streak
  [column]
  (let [value-streaks    (partition-by identity column)
        non-null-streaks (filter #(some? (first %)) value-streaks)
        streak-counts    (map count non-null-streaks)]
    (if (seq streak-counts) (<= 4 (apply max streak-counts)))))

(defn winning-state?
  [board] (let
              [cols       board
               rows       (rotate-board-by-90 cols)
               up-diags   (get-diagonals cols)
               down-diags (get-diagonals rows)
               candidates (concat cols rows up-diags down-diags)]
            (some true? (map winning-streak candidates))))

(defn full? [board] (every? some? (flatten board)))

(defn finished?
  [board]
  (condp apply [board]
    winning-state? (do (println "Hurray, you win!") true)
    full?          (do (println "It's a draw!") true)
    false))

(defn update-board
  [board player move]
  (let [column (get board move) idx (count (take-while some? column))]
    (assoc-in board [move idx] (:board-symbol player))))

(defn random-guess-strategy [board player]
  (let [candidate  (rand-int (width board))]
    (if (legal-move? board candidate) candidate (recur board player))))

(defn compute-move
  [board player]
  (Thread/sleep 3000)
  (let [move (random-guess-strategy board player)]
    (do (println (:name player) "chose" move) move)))

(defn play-turn
  [board player]
  (let [move (if (:computer? player)
               (compute-move board player)
               (ask-move board player))]
    (update-board board player move)))

(defn game [board players]
  (display-board board)
  (if-not (finished? board)
    (recur (play-turn board (first players)) (rest players))))


;;
; main
;;
(defn -main []
  (println "Welcome to Connect4!")
  (let [players (determine-players)] 
    (game (new-board 7 6) (cycle players)))
  (if (play-again?) (recur)))
