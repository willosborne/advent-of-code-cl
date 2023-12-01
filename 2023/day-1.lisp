(defpackage "aoc/2023/1"
  (:use :cl :aoc :iterate :parachute :alexandria)
  (:shadowing-import-from :iterate finish)
  (:shadowing-import-from :alexandria of-type featurep))
(in-package "aoc/2023/1")

(define-test day-1)

(defvar *input* (get-input 2023 1))

(defun get-digits (line)
  (remove-if-not #'digit-char-p line))

(define-test extracts-digits
  :parent day-1
  (is string= "123" (get-digits "1abcd2def3"))
  (is string= "" (get-digits "abcdef")))

(defun process-line (line)
  (let ((digits (get-digits line)))
    (parse-integer (format nil "~a~a" (char digits 0) (char digits (1- (length digits)))))))

(define-test parses-numbers
  :parent day-1
  (is = 23 (process-line "a2sdfef3sdf"))
  (is = 25 (process-line "a2d3s4s5sss")))

(defun solution-1 (&optional (input *input*))
  (iter (with nums = (mapcar #'process-line (str:lines input)))
    (for num in nums)
    (summing num))
  )

(defparameter *example-1* (cons (format nil "~
abcd1b3cd2s
3egg4
sdf5d6")
                                (+ 12 34 56)))


(define-test sol-1
  :parent day-1
  (is = (cdr *example-1*) (solution-1 (car *example-1*))))

(defparameter *sol-1* (solution-1))

(defvar *digit-words*
  (iter (for i from 1 to 9)
    (collecting (format nil "~R" i))))

(defun parse-digit-word (input)
  (iter (for i from 1 to 9)
    (finding i such-that (str:contains? (nth (1- i) *digit-words*) input))))

(define-test parse-word
  :parent day-1
  (false (parse-digit-word ""))
  (false (parse-digit-word "zero"))
  (is = (parse-digit-word "nine") 9)
  (false (parse-digit-word "zer")))

(defun extract-digits-2 (line)
  (iter (for c in-string line)
    (with word = (list))
    (with out = (list))
    (if (digit-char-p c)
        (push (parse-integer (string c)) out)
        (progn
          (push c word)
          (when-let ((digit (parse-digit-word (concatenate 'string (reverse word)))))
            (push digit out)
            (setf word (list (car word)))))) ;; SNEAKY! Overlapping words...

    (finally (return (reverse out)))))

(define-test extract-number-words-in-line
  :parent day-1
  (is equal '(1 2 3) (extract-digits-2 "1twox3"))
  (is equal '(1 2 3) (extract-digits-2 "one2xthree"))
  (is equal '(1 2 3) (extract-digits-2 "onetwothree"))
  )

(defun process-line-2 (line)
  (let ((digits (extract-digits-2 line)))
    (parse-integer (format nil "~a~a" (nth 0 digits) (nth (1- (length digits)) digits)))))

(define-test process-line-2
  :parent day-1
  (is equal 13 (process-line-2 "1two3"))
  (is equal 13 (process-line-2 "one2three"))
  (is equal 13 (process-line-2 "onetwothree"))
  (is equal 83 (process-line-2 "eighthree")))

(defun solution-2 (&optional (input *input*))
  (reduce #'+ (mapcar #'process-line-2 (str:lines input))))


(defparameter *example-2* (cons (format nil "~
abconed1b3cd2s
one3egg4
sdf5d6two
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
")
                                (+ 12 14 52 29 83 13 24 42 14 76)))


(define-test sol-2
  :parent day-1
  (is = (cdr *example-2*) (solution-2 (car *example-2*))))
