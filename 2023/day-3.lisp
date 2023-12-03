(defpackage "aoc/2023/3"
  (:use :cl :aoc :iterate :parachute :alexandria :access)
  (:import-from :serapeum dict)
  (:shadowing-import-from :iterate finish)
  (:shadowing-import-from :alexandria of-type featurep))
(in-package "aoc/2023/3")

(defvar *input* (get-input 2023 3))

(define-test day-3)


(defun setup-grid (input)
  (let* ((lines (str:lines input))
         (rows (length lines))
         (cols (length (nth 0 lines)))
         (grid (make-array (list rows cols))))
    (iter (for y from 0 below rows)
      (iter (for x from 0 below cols)
        (for cell = (char (nth y lines) x))
        (setf (aref grid y x) cell))
      (finally (return grid)))))

(defparameter *example-grid-input*
  (format nil "~
111.123...
1*11......
111....12.
..56....+.
.....234.."))

;; (+ 111 1 11 111 12 234)

(defparameter *example-grid* (setup-grid *example-grid-input*))

(define-test "Load data into grid"
  :parent day-3
  (is = 10 (array-dimension (setup-grid *example-grid-input*) 1))
  (is = 5 (array-dimension (setup-grid *example-grid-input*) 0))
  (is char= #\1 (aref (setup-grid *example-grid-input*) 0 0))
  (is char= #\2 (aref (setup-grid *example-grid-input*) 4 5))
  )

(defparameter *input-grid* (setup-grid *input*))

(defun grid-get (grid x y)
  (aref grid y x))

(defun grid-h (grid)
  (array-dimension grid 0))

(defun grid-w (grid)
  (array-dimension grid 1))

(define-test "Get from grid"
  :parent day-3
  (is char= #\2 (grid-get (setup-grid *example-grid-input*) 5 4)))

(defun symbol? (char)
  (not (or (digit-char-p char)
           (char= #\. char))))

(define-test "identifies symbol"
  :parent day-3
  (true (symbol? #\+))
  (false (symbol? #\0))
  (false (symbol? #\.)))

(defparameter adjacencies
  (list '(-1 -1)
        '(0 -1)
        '(1  -1)
        '(-1  0)
        ;; omitting 0  0
        '(1  0)
        '(-1  1)
        '(0  1)
        '(1  1)))

(defun symbol-adjacent? (grid x y)
  (iter (for (dx dy) in adjacencies)
    (for gx = (min (1- (grid-w grid))
                   (max 0 (+ x dx))))
    (for gy = (min (1- (grid-h grid))
                   (max 0 (+ y dy))))
    (if (symbol? (grid-get grid gx gy))
        (return t))
    (finally (return nil))))

(define-test "symbol adjacencies"
  :parent day-3
  ;; all 8 adjacencies
  (true (symbol-adjacent? *example-grid* 0 0))
  (true (symbol-adjacent? *example-grid* 1 0))
  (true (symbol-adjacent? *example-grid* 2 0))
  (true (symbol-adjacent? *example-grid* 0 1))
  (true (symbol-adjacent? *example-grid* 2 1))
  (true (symbol-adjacent? *example-grid* 0 2))
  (true (symbol-adjacent? *example-grid* 1 2))
  (true (symbol-adjacent? *example-grid* 2 2))
  (false (symbol-adjacent? *example-grid* 5 2)))

(defparameter *sample-grid-input* (format nil "~
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."))

(defparameter *sample-grid* (setup-grid *sample-grid-input*))

(defun collect-adjacent-numbers-row (grid y)
  (iter (for x from 0 below (grid-w grid))
    (for ch = (grid-get grid x y))
    (with digits = (list))
    (with out = (list))
    (with was-adjacent = nil)
    (if (digit-char-p ch)
        (progn (push ch digits) ;; if current sym is a digit, add to the list
               ;; also track if we've seen an adjacent symbol for this number
               (when (symbol-adjacent? grid x y)
                 (setf was-adjacent t))) ;; if we see a digit add to the list
        (when digits ;; else if we have seen some digits previously
          (when was-adjacent ;; if one of the digits was adjacent, add it
            (push (parse-integer (concatenate 'string (reverse digits))) out))
          (setf digits (list) ;; reset
                was-adjacent nil)))
    (finally
             (when digits ;; add any more digts left over
               (when was-adjacent
                 (push (parse-integer (concatenate 'string (reverse digits))) out))
               (setf digits (list)))
             (return (reverse out)))))

(define-test "Collects numbers adjacent on a single row"
  :parent day-3
  (is equalp (list 111) (collect-adjacent-numbers-row *example-grid* 0))
  (is equalp (list 1 11) (collect-adjacent-numbers-row *example-grid* 1))

  (is equalp (list 467) (collect-adjacent-numbers-row *sample-grid* 0))
  (is equalp (list) (collect-adjacent-numbers-row *sample-grid* 1))
  (is equalp (list 35 633) (collect-adjacent-numbers-row *sample-grid* 2))
  (is equalp (list 664 598) (collect-adjacent-numbers-row *sample-grid* 9))
  )

(defun collect-adjacent-numbers (grid)
  (iter (for y from 0 below (grid-h grid))
    (summing (reduce #'+ (collect-adjacent-numbers-row grid y)))))

(define-test "Sums up whole grid"
  :parent day-3
  (is = 4361 (collect-adjacent-numbers *sample-grid*))
  (is = 480 (collect-adjacent-numbers *example-grid*)))

(defun solution-1 ()
  (collect-adjacent-numbers *input-grid*))


(defun make-tag-grid (grid)
  (make-array (list (grid-h grid) (grid-w grid)) :initial-element nil))

(defun collect-total (grid start-x end-x y)
  (format t "collecting from ~a to ~a~%" start-x end-x)
  (parse-integer
   (concatenate 'string
                (iter (for x from start-x to end-x)
                  (collecting (grid-get grid x y))))))

(define-test "collect number from row"
  :parent day-3

  (is = 467 (collect-total *sample-grid* 0 2 0)))

(defun tag-cells-with-number-row (grid tag-grid y)
  "For a single row in a grid, sets every cell in the output grid to the full number if appropriate"
  (iter (for x from 0 below (grid-w grid))
    (for ch = (grid-get grid x y))
    (with start = nil)
    (when (and (digit-char-p ch) (not start))
      (format t "setting start to ~a~%" x)
      (setf start x))
    (when (and start (not (digit-char-p ch)))
      (iter (for xx from start below x)
        (with total = (collect-total grid start (1- x) y))
        (setf (aref tag-grid y xx) total))
      (setf start nil))

    (finally
     (when start
       (iter (for xx from start below x)
         (with total = (collect-total grid start (1- x) y))
         (setf (aref tag-grid y xx) total)))
     )
  ))

(define-test "Tags cells with their whole number"
  :parent day-3
  (let ((tag-grid (make-tag-grid *sample-grid*)))
    (tag-cells-with-number-row *sample-grid* tag-grid 0)
    (is = 467 (grid-get tag-grid 0 0))
    (is = 467 (grid-get tag-grid 1 0))
    (is = 467 (grid-get tag-grid 2 0))

    (is = 114 (grid-get tag-grid 5 0))
    (is = 114 (grid-get tag-grid 6 0))
    (is = 114 (grid-get tag-grid 7 0))
    )
  )

(defun tag-whole-grid (grid tag-grid)
  (iter (for y below (grid-h grid))
    (tag-cells-with-number-row grid tag-grid y)
    (finally (return tag-grid))))

(defparameter *sample-tag-grid*
  (let ((tg (make-tag-grid *sample-grid*)))
    (tag-whole-grid *sample-grid* tg)))

(defun adjacent-numbers (grid tag-grid x y)
  (iter (for (dx dy) in adjacencies)
    (with out = ())
    (for gx = (min (1- (grid-w grid))
                   (max 0 (+ x dx))))
    (for gy = (min (1- (grid-h grid))
                   (max 0 (+ y dy))))
    (if (digit-char-p (grid-get grid gx gy))
        (pushnew (grid-get tag-grid gx gy) out))
    (finally (return out))))

(define-test "number adjacencies"
  :parent day-3
  (is equalp (list 35 467) (adjacent-numbers *sample-grid* *sample-tag-grid* 3 1))
  )

(defun get-gears (grid tag-grid)
  (iter (for y from 0 below (grid-h grid))
   (summing
    (iter (for x from 0 below (grid-w grid))
          (when (char= (grid-get grid x y) #\*)
            (let ((adj (adjacent-numbers grid tag-grid x y)))
              (when (= (length adj) 2)
                (sum (reduce #'* adj) into totals))))
      (finally (return totals))))))


(define-test "properly computes gear ratios and sums"
  :parent day-3
  (is = 467835 (get-gears *sample-grid* *sample-tag-grid*)))

(defun solution-2 (&optional (input-grid *input-grid*))
  (let ((tag-grid (make-tag-grid input-grid)))
    (tag-whole-grid input-grid tag-grid)
    (get-gears input-grid tag-grid)))
