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
   (let [head-ix (->> column board (take-while some?) count dec)]
     (update-in (char-board board) [column head-ix] underline))))

(defn render-char-board [char-board_]
  (let [rows (->> char-board_ transpose reverse (map space-out) vec)
        divider (apply str (-> char-board_ width (* 2) (- 1) (repeat "-")))
        board-indices (-> char-board_ width range space-out)
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
