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

;; this worked but i entered it wrong and ended up completely re-solving with a brute force.
;; painful

;; (defun solutions (a b c)
;;   (format t "~a ~a ~a" a b c)
;;   (when (and (<= (* 4 a c) (* b b))
;;              (/= a 0))
;;     (let* ((sq (sqrt (- (* b b) (* 4 a c)))))
;;       (print sq)
;;       (list (floor (/ (+ (- b) sq) (* 2 a)))
;;             (ceiling (/ (- (- b) sq) (* 2 a)))))))

;; (define-test "solves quadratics"
;;   :parent day-6
;;   (is equalp '(1 -5) (solutions 2 8 -10))
;;   (false (solutions 100 2 10))
;;   (false (solutions 0 1 1)))

;; (defun number-ways-to-win (race-spec)
;;   (destructuring-bind (time record) race-spec
;;     (let ((solns (solutions -1 time (- -1 record ))))
;;       (print solns)
;;       ;; (1+ (- (floor (+ -0.001 (apply #'max solns)))
;;       ;;         (ceiling (+ 0.001 (apply #'min solns)))))
;;       (1+ (abs (- (car solns) (cadr solns))))

;;       )
;;     ))

(defun beats-record? (delay time record)
  (> (* delay (- time delay))
     record))



(defun race (race-spec)
  (let* ((time (car race-spec))
         (record (cadr race-spec))
         (lower (iter (for i upfrom 0)
                  (finding i such-that (beats-record? i time record))))
         (upper (iter (for i downfrom time)
                  (finding i such-that (beats-record? i time record)))))
    (1+ (- upper lower))))

(define-test "counts ways to win"
  :parent day-6
  (is = 4 (race '(7 9)))
  (is = 8 (race '(15 40)))
  (is = 9 (race '(30 200)))
  )

(defun solution-1 (&optional (input *input*))
  (reduce #'* (mapcar #'race (parse-input input))))

(defun solution-2 ()
  (race '(61709066 643118413621041)))
