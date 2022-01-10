(ns connect-four.visual (:require [connect-four.board :refer [transpose width]]))
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
