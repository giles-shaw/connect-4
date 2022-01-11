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

(defn full? [board] (every? some? (flatten board)))

(defn update-board
  [board token move]
  (let [column (get board move) idx (count (take-while some? column))]
    (assoc-in board [move idx] token)))

(defn competitor-token
  [token board]
  (let [non-null-values (filter some? (set (flatten board)))]
    (first (filter #((not= token %)) non-null-values))))
