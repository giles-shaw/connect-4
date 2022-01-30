(ns connect-four.visual (:require [connect-four.board :refer [transpose width]]))
;;
; display logic
;;

(defn make-red [char-str] (str "\u001b[31m" char-str "\u001b[0m"))

(defn make-blue [char-str] (str "\u001b[34m" char-str "\u001b[0m"))

(defn underline [char-str] (str "\u001b[4m" char-str "\u001b[0m"))

(defn space-out [str-seq] (apply str (interpose " " str-seq)))

(defn render-entry
  [entry]
  (condp = entry
    "x" (make-blue "●")
    "o" (make-red "●")
    nil "•"
    entry))

(defn char-board
  ([board] (mapv (partial mapv render-entry) board))
  ([board column]
   (let [head-ix (dec (count (take-while some? (board column))))]
     (update-in (char-board board) [column head-ix] underline))))

(defn render-char-board [char-board_]
  (let [rows (vec (map space-out (reverse (transpose char-board_))))
        divider (apply str (repeat (- (* 2 (width char-board_)) 1) "-"))
        board-indices (space-out (range (width char-board_)))
        lines         (into rows [divider board-indices])
        ]
    (apply str (interleave lines (repeat "\n")))))

(def render-board (comp render-char-board char-board))

(defn preamble
  [{player :passive-player :as game}]
  (if (and (:computer? player) (game :last-move))
    (str (player :name) " chose " (game :last-move))
    nil))

(defn conclusion
  [game]
  (condp apply [game]
    :winner (str (get-in game [:winner :name]) " wins!")
    :draw "It's a draw!"
    nil))

(defn report
  [game]
  (let [rendered-board (if (game :last-move)
                         (render-board (game :board) (game :last-move))
                         (render-board (game :board)))
        messages       [(preamble game) rendered-board (conclusion game)]]
    (apply str (interpose "\n" (filter some? messages)))))
