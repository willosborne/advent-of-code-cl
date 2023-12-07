(defpackage "aoc/2023/6"
  (:use :cl :aoc :iterate :parachute :alexandria :access)
  (:import-from :serapeum dict)
  (:shadowing-import-from :iterate finish)
  (:shadowing-import-from :alexandria of-type featurep))
(in-package "aoc/2023/6")

(define-test day-6)

(defvar *input* (get-input 2023 6))

(defvar *sample-input*
  "Time:      7  15   30
Distance:  9  40  200")

(defun matches (regex string)
  (multiple-value-bind (str matches) (cl-ppcre:scan-to-strings regex string)
    (declare (ignore str))
    (coerce matches 'list)))

(defun parse-input (input)
  (let* ((lines (str:lines input))
         ;; (times (mapcar #'parse-integer (cdr (str:split " " (first lines)))))
         ;; (distances (mapcar #'parse-integer (cdr (str:split " " (second lines)))))
         (times (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "\\d+" (first lines))))
         (distances (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "\\d+" (second lines))))

         )
    (apply #'mapcar #'list (list times distances))))

(define-test "parses input"
  :parent day-6
  (is equalp '((7 9) (15 40) (30 200)) (parse-input *sample-input*)))


(defun number-ways-to-win (race-spec)
  (destructuring-bind (time record) race-spec
    (let ((solns (solutions -1 time (- record))))
      (print solns)
      (1+ (- (floor (- (apply #'max solns) 0.001)) (ceiling (+ 0.001 (apply #'min solns))))))
    ))

;; distance(wait, time) = (time - wait) * wait
;; winning if d(w, t) > record
;; (t - w) * w - r > 0
;; solutions for w
;; -w^2 + tw - r = 0
;; a = -1
;; b = t
;; c = -r
;; (-b +- sqrt(b^2 - 4ac)) / 2a
;;

(defun solutions (a b c)
  (format t "~a ~a ~a" a b c)
  (when (and (<= (* 4 a c) (* b b))
             (/= a 0))
    (let* ((sq (sqrt (- (* b b) (* 4 a c)))))
      (print sq)
      (list (/ (+ (- b) sq) (* 2 a))
            (/ (- (- b) sq) (* 2 a))))))

(define-test "solves quadratics"
  :parent day-6
  (is equalp '(1 -5) (solutions 2 8 -10))
  (false (solutions 100 2 10))
  (false (solutions 0 1 1)))
