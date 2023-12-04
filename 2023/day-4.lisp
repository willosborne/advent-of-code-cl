(defpackage "aoc/2023/4"
  (:use :cl :aoc :iterate :parachute :alexandria :access)
  (:import-from :serapeum dict)
  (:shadowing-import-from :iterate finish)
  (:shadowing-import-from :alexandria of-type featurep))
(in-package "aoc/2023/4")

(define-test day-4)

(defvar *input* (get-input 2023 4))

(defun parse-number-list (nums)
  (arrows:->> nums
              (str:split " ")
              (remove-if #'str:empty?)
              (mapcar #'parse-integer))
  )

(define-test "parses number list"
  :parent day-4
  (is equalp '(1 12 34 456) (parse-number-list " 1   12   34 456  ")))

(defun parse-line (line)
  (let* ((body (cadr (str:split ":" line)))
         (halves (str:split "|" body))
         (winners (parse-number-list (car halves)))
         (numbers (parse-number-list (cadr halves))))
    (list winners numbers)))

(define-test "parses line to a card spec of (winning nums, numbers)"
  :parent day-4
  (is equalp '((1 2 3 4) (5 6 7 8 9)) (parse-line "Card 1: 1 2 3 4 | 5 6 7 8 9")))

(defvar *sample-input*
  "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

(defvar *sample-card-specs*
  (mapcar #'parse-line (str:lines *sample-input*)))

(defun score (card-spec)
  (destructuring-bind (winners numbers) card-spec
    (let ((winning-nos (intersection winners numbers)))
      (if winning-nos
          (expt 2 (1- (length winning-nos)))
          0))))

(define-test "scores sample input"
  :parent day-4
  (iter (for score in '(8 2 2 1 0 0))
    (for card-spec in (mapcar #'parse-line (str:lines *sample-input*)))
    (is = score (score card-spec))))

(defun solution-1 (&optional (input *input*))
  (let* ((cards (mapcar #'parse-line (str:lines input)))
         (scores (mapcar #'score cards)))
    (reduce #'+ scores)))

(define-test "solves first sample"
  :parent day-4
  (is = 13 (solution-1 *sample-input*)))

;;; PART 2
;;;
;;;
;;;

(defun make-card-counts (card-list)
  (iter (for i in card-list)
    (collect 1)))

(defun bump-card-counts (card-counts start n-cards times)
  (iter (for i from start below (+ start n-cards))
    (incf (nth i card-counts) times))
  card-counts)

(define-test "bumps card counts correctly"
  :parent day-4
  (is equalp '(1 3 3 1) (bump-card-counts (list 1 1 1 1) 1 2 2)))


(defun winners (card-spec)
  (destructuring-bind (winners numbers) card-spec
    (intersection winners numbers)))

(define-test "calculates winners from a card spec"
  :parent day-4
  (is utils:lists-equal-any-order '(1 2) (winners '((1 2 3) (5 6 1 2 8 9)))))

(defun run-game (card-list card-counts)
  (iter (for i from 0 below (length card-list))
    (for card in card-list)
    (for count in card-counts)
    (for winners = (length (winners card)))
    (if (> winners 0)
        (bump-card-counts card-counts (1+ i) winners count))
    (finally (return card-counts))
    ))

(define-test "runs game on sample input correctly"
  :parent day-4
  (let ((card-counts (make-card-counts *sample-card-specs*)))
    (run-game *sample-card-specs* card-counts)
    (is equalp '(1 2 4 8 14 1) card-counts))
  )

(defun solution-2 (&optional (input *input*))
  (let* ((cards (mapcar #'parse-line (str:lines input)))
         (counts (make-card-counts cards)))
    (run-game cards counts)
    (reduce #'+ counts)))
