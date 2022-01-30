(ns connect-four.prompts (:require [connect-four.board :refer [column-not-full? width]]))
;;
; Ask users to make choices
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

(def choose-opponent?
  (user-prompt
    "Are you playing against a real opponent or the computer (r / c)?"
    {"r" :human "c" :computer} "'r' or 'c'"))

(def play-again? (user-prompt
                  "Play another game (y / n)?"
                  {"y" true "n" '(do (println "Bye!") (System/exit 0))}
                  "'y' or 'n'"))

(defn ask-move? [name]
  (fn [{board :board}]
    ((user-prompt (str name ", please enter a move:")
                (comp (partial column-not-full? board) parse-move)
                (str "an index between '0' and '" (-> board width dec)
                     "' for a column which is not full")))))
