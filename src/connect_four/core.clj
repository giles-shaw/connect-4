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

(defn upwards-diagonals
  [board]
  (let [starting_points (concat (map vector (repeat 0) (range (height board)))
                                ; start from (1, 0) to not double count (0, 0)
                                (map vector (range  1 (width board)) (repeat 0)))]
    (vec (map (partial diagonal board) starting_points))))

(defn column-not-full? [board column] (if (some nil? (get board column)) column nil))


;;
; display logic
;;
(defn space-out [str-seq] (apply str (interpose " " str-seq)))

(defn format_ [row] (space-out (map #(or % "•") row)))

(defn display-board
  [board]
  (println)
  (doseq [row (reverse (transpose board))] (println (format_ row)))
  (println (apply str (repeat (- (* 2 (width board)) 1)  "-")))
  (println (space-out (range (width board)))) (println))


;;
; user interaction
;;
(defn parse-move [move] (try (Integer/parseInt move) (catch Exception _ nil)))

(defn error-msg
  [choice guidance]
  (str "'" choice "' is not a valid option- enter " guidance))

;; unsafe, but cool
(defn user-prompt [prompt options guidance]
  (fn [] (println prompt)
    (let [choice (read-line)]
      (or (eval (options choice))
          (do (println (error-msg choice guidance)) (recur))))))

(defn determine-players [default-players]
  ((user-prompt
    "Are you playing against a real opponent or the computer (r / c)?"
    {"r" default-players "c" (assoc-in default-players [1 :computer?] true)}
    "'r' or 'c'")))

(def play-again? (user-prompt
                  "Play another game (y / n)?"
                  {"y" true "n" '(do (println "Bye!") (System/exit 0))}
                  "'y' or 'n'"))

(defn ask-move [board player]
  ((user-prompt (str (:name player) ", please enter a move")
                (comp (partial column-not-full? board) parse-move)
                (str "an index between '0' and '" (width board)
                     "' for a column which is not full"))))


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

(defn full? [board] (every? some? (flatten board)))

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


;;
; main
;;
(def default-players [{:id :player-1 :name "Player 1" :board-symbol "o"}
                      {:id :player-2 :name "Player 2" :board-symbol "x"}])

(defn -main []
  (println "Welcome to Connect4!")
  (let [players (determine-players default-players)] 
    (game (new-board 7 6) (cycle players)))
  (if (play-again?) (recur) nil))
