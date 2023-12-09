(defpackage "aoc/2023/8"
  (:use :cl :aoc :iterate :parachute)
  (:import-from :serapeum dict)
  (:import-from :access access)
  (:shadowing-import-from :iterate finish)
  (:shadowing-import-from :alexandria of-type featurep if-let))
(in-package "aoc/2023/8")

(define-test day-8)

(defvar *input*)

(defun pull-input ()
  (setf *input* (get-input 2023 8)))

(defun parse-rule (line)
  (cl-ppcre:all-matches-as-strings "[A-Z]+" line))

(define-test "parses line"
  :parent day-8
  (is equalp '("AAA" "BBB" "CCC") (parse-rule "AAA = (BBB, CCC)")))

(defun parse-input (input)
  (let* ((lines (str:lines input))
         (choices (car lines))
         (rules (mapcar #'parse-rule (cddr lines))))
    (values choices rules)))

(defun follow-rule (rule lr)
  (if (char= lr #\L)
      (second rule)
      (third rule)))

(define-test "follows rule"
  :parent day-8
  (is string= "BBB" (follow-rule '("AAA" "BBB" "CCC") #\L))
  (is string= "CCC" (follow-rule '("AAA" "BBB" "CCC") #\R)))

(defun apply-rules (rules input lr)
  (let ((rule (find-if (lambda (r) (string= (car r) input))
                       rules)))
    (follow-rule rule lr)))

(define-test "follows rules"
  :parent day-8
  (is string= "BBB" (apply-rules '(("AAA" "BBB" "CCC")
                                   ("DDD" "EEE" "FFF")) "AAA" #\L))
  (is string= "EEE" (apply-rules '(("AAA" "BBB" "CCC")
                                   ("DDD" "EEE" "FFF")) "DDD" #\L))
  )

(defun solution-1 (&optional (input *input*))
  (multiple-value-bind (choices rules) (parse-input input)
    (iter
      (with state = "AAA")
      (for i upfrom 0)
      (for lr = (char choices (mod i (length choices))))
      ;; (for lr = (char))
      (setf state (apply-rules rules state lr))
      (when (string= state "ZZZ")
        (return (1+ i))))))


;; part 2
