(defpackage aoc-2019-4
  (:use :cl :arrow-macros :iterate)
  (:import-from :1am :run :test :is)
  (:import-from :lol :nlet))
(in-package :aoc-2019-4)

(defun parse-range (input)
  (mapcar #'parse-integer (str:split "-" input)))

(defun has-consecutive-p (digits)
  (some (lambda (n)
          (= (nth n digits) (nth (- n 1) digits)))
        (iter (for i from 1 below (length digits))
          (collect i))))

(defun monotonically-increasing-p (digits)
  (every (lambda (n)
           (>= (nth n digits) (nth (- n 1) digits)))
         (iter (for i from 1 below (length digits))
           (collect i))))

(defun password-valid-p (number)
  (let ((digits (mapcar #'digit-char-p (coerce (write-to-string number) 'list))))
    (and (has-consecutive-p digits)
         (monotonically-increasing-p digits))))


(defun get-answer-1 ()
  (destructuring-bind (low high) (parse-range (aoc:get-input 2019 4))
    (iter (for i from low to high)
      (counting (password-valid-p i)))))


(defun group-list (list &key (test #'equalp))
  (nlet calc ((vals (cdr list))
              (groups '())
              (last (car list))
              (current-group (list (car list))))
      (if (consp vals)
          (if (funcall test (car vals) last)
              (calc (cdr vals)
                    groups
                    last
                    (nconc current-group (list (car vals))))
              (calc (cdr vals)
                    (nconc groups (list current-group))
                    (car vals)
                    (list (car vals))))
          (nconc groups (list current-group)))))


(defun has-double-group-p (digits)
  (some (lambda (group)
          (= (length group) 2))
        (group-list digits)))

(defun password-valider-p (number)
  (let ((digits (mapcar #'digit-char-p (coerce (write-to-string number) 'list))))
    (and (has-consecutive-p digits)
         (monotonically-increasing-p digits)
         (has-double-group-p digits))))

(defun get-answer-2 ()
  (destructuring-bind (low high) (parse-range (aoc:get-input 2019 4))
    (iter (for i from low to high)
      (counting (password-valider-p i)))))
