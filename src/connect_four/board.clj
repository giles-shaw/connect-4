(ns connect-four.board)
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

(defn incomplete-columns
  [board] (filter (partial column-not-full? board) (range (width board))))

(defn update-board
  [board token move]
  (let [column (get board move) idx (count (take-while some? column))]
    (assoc-in board [move idx] token)))

(defn longest-streak
  ([column]
  (let [value-streaks    (partition-by identity column)
        non-null-streaks (filter #(some? (first %)) value-streaks)
        streak-counts    (map count non-null-streaks)]
    (if (seq streak-counts) (apply max streak-counts) nil)))
  ([column token] (longest-streak (map #(if (= % token) token nil) column))))

(defn streak-counts
  [board & token]
  (let [cols           board
        rows           (rotate-board-by-90 cols)
        up-diags       (upwards-diagonals cols)
        down-diags     (upwards-diagonals rows)
        candidates     (concat cols rows up-diags down-diags)
        streaks        (map #(apply longest-streak % token) candidates)]
    (frequencies (filter some? streaks))))
