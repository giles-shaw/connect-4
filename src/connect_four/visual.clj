(ns connect-four.visual (:require [connect-four.board :refer [transpose width]]))
;;
; display logic
;;
(defn space-out [str-seq] (apply str (interpose " " str-seq)))

(defn render [row] (space-out (map #(or % "•") row)))

(defn make-red [char-str] (str "\u001b[31m" char-str "\u001b[0m"))

(defn display-board
  ([board]
   (println)
   (doseq [row (reverse (transpose board))] (println (render row)))
   (println (apply str (repeat (- (* 2 (width board)) 1)  "-")))
   (println (space-out (range (width board)))) (println))

  ([board column]
   (let [head-ix (dec (count (take-while some? (board column))))]
     (display-board (update-in board [column head-ix] make-red)))))
