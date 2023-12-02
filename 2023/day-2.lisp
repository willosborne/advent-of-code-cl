(defpackage "aoc/2023/2"
  (:use :cl :aoc :iterate :parachute :alexandria :access)
  (:import-from :serapeum dict)
  (:shadowing-import-from :iterate finish)
  (:shadowing-import-from :alexandria of-type featurep))
(in-package "aoc/2023/2")

(defvar *input* (get-input 2023 2))

(define-test day-2)

(defun kw (string)
  (intern (string-upcase string) 'keyword))

(defun parse-cube-list (input)
  "Parse a list of cubes in the format 1 red, 2 green, 1 blue"
  (let ((cube-pairs (str:split ", " input))
        (out (dict)))
    (iter
      (for pair in cube-pairs)
      (for parsed = (str:split " " pair))
      (setf (access out (kw (cadr parsed))) (parse-integer (car parsed)))
      (finally (return out))
      )
    )
  )

(define-test should-parse-cube-lists
  :parent day-2
  (let ((parsed (parse-cube-list "1 red, 2 green, 1 blue")))
    (is equalp (dict :red 1 :green 2 :blue 1) parsed)))

(defun parse-game (line)
  (destructuring-bind (game rounds) (str:split ": " line)
    (let ((game-id (parse-integer (cl-ppcre:scan-to-strings "\\d+" game)))
            (rounds (mapcar #'parse-cube-list (str:split "; " rounds))))
        (dict :id game-id :rounds rounds)))
  )

(define-test should-parse-games
  :parent day-2
  (is equalp (dict :id 12 :rounds (list (dict :red 1 :green 2 :blue 3)
                                        (dict :green 5)))
      (parse-game "Game 12: 1 red, 2 green, 3 blue; 5 green")))


(defun is-game-possible (game-spec cube-totals)
  (iter (for game in (access game-spec :rounds))
    (iter (for (colour count) in-hashtable cube-totals)
      (when (> (or (access game colour) 0) count)
        (format t "Found game with ~a ~a required when only ~a cubes of that colour are available"
                (access game colour) colour count)
        (return-from is-game-possible nil))
      )
    (finally (return t))))

(define-test "Should check whether games are possible"
  :parent day-2
  (false (is-game-possible (dict :id 12 :rounds (list (dict :red 20 :green 1 :blue 1)))
                           (dict :red 10 :green 1 :blue 1)))
  (false (is-game-possible (dict :id 12 :rounds (list (dict :red 20 :green 1 :blue 1)
                                                      (dict :red 20 :green 1 :blue 2)))
                           (dict :red 20 :green 1 :blue 1)))

  (true (is-game-possible (dict :id 12 :rounds (list (dict :red 20 :green 1 :blue 1)))
                           (dict :red 20 :green 1 :blue 1)))
  )

(defvar *part-1-cubes* (dict :red 12 :green 13 :blue 14))

(defun solution-1 (&optional (input *input*))
  (let* ((games (mapcar #'parse-game (str:lines input)))
         (possible (remove-if-not (lambda (g) (is-game-possible g *part-1-cubes*)) games)))
    (reduce #'+ (mapcar (lambda (g) (access g :id)) possible))))

(define-test "Solution 1 sums ids correctly"
  :parent day-2
  (is = 12 (solution-1  "Game 12: 1 red, 2 green, 3 blue; 5 green"))
  (is = 12 (solution-1  "Game 12: 1 red, 2 green, 3 blue; 5 green
Game 100: 100 red, 100 green, 100 blue"))
  (is = 13 (solution-1  "Game 1: 1 red, 1 green, 1 blue; 5 green
Game 12: 1 red, 2 green, 3 blue; 5 green
Game 100: 100 red, 100 green, 100 blue"))
  )

(defmacro game (id &body rounds)
  `(dict :id ,id
         :rounds (list ,@(mapcar (lambda (round) (push 'dict round)) (car rounds)))))

;; simple macro to make tests easier
(game 12 ((:red 1 :blue 3)
          (:red 3 :blue 2)))

(defun minimum-cubes (game-spec)
  (let ((minimums (dict))
        (rounds (access game-spec :rounds)))
    (iter (for round in rounds)
      (iter (for (colour count) in-hashtable round)
        (when (> count (or (access minimums colour) 0))
          (setf (access minimums colour) count)))
      )
    minimums))


(define-test "Calculates minimum number of cubes"
  :parent day-2
  (is equalp
      (dict :red 100 :green 200 :blue 300)
      (minimum-cubes (game 1 ((:red 100 :green 200 :blue 300)))))

  (is equalp
      (dict :red 100 :green 200 :blue 300)
      (minimum-cubes (game 1 ((:red 100) (:green 200) (:blue 300))))))


(defun cube-set-power (cube-set)
  (iter (for (colour count) in-hashtable cube-set)
    (multiplying count)))

(define-test "Calculate cube set power"
  :parent day-2
  (is = (* 2 3 13) (cube-set-power (dict :red 2 :blue 3 :green 13))))

(defun solution-2 (&optional (input *input*))
  (let* ((games (mapcar #'parse-game (str:lines input)))
         (minima (mapcar #'minimum-cubes games))
         (powers (mapcar #'cube-set-power minima)))
    (reduce #'+ powers)))

(define-test "Solution computes total power of minimums"
  :parent day-2
  (is = (+ 5 15 1000) (solution-2  "Game 1: 1 red, 1 green, 1 blue; 5 green
Game 12: 1 red, 2 green, 3 blue; 5 green
Game 100: 10 red, 10 green, 10 blue")))
