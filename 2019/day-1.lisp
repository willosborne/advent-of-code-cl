(defpackage aoc-2019-1
  (:use :cl :lol :arrow-macros))
(in-package :aoc-2019-1)

(defvar *input* (aoc:get-input 2019 1))

(defun calc-fuel (mass)
  (-> mass
    (/ 3)
    (floor)
    (- 2)))

(defun get-answer-1 ()
  (->> *input*
    (str:lines)
    (mapcar #'parse-integer)
    (mapcar #'calc-fuel)
    (reduce #'+)))


(defun calc-fuel-recursive (mass)
  (let* ((fuel (calc-fuel mass))
         (fuel-for-fuel (calc-fuel fuel)))
    (if (<= fuel-for-fuel 0)
        fuel
        (+ fuel (calc-fuel-recursive fuel)))))

(defun calc-fuel-recursive-tail (mass)
  (labels ((func (mass acc)
             (let* ((fuel (calc-fuel mass))
                    (fuel-for-fuel (calc-fuel fuel)))
               (if (<= fuel-for-fuel 0)
                   (+ fuel acc)
                   (func fuel (+ fuel acc))))))
    (func mass 0)))


(defun get-answer-2 ()
  (->> *input*
    (str:lines)
    (mapcar #'parse-integer)
    (mapcar #'calc-fuel-recursive-tail)
    (reduce #'+)))
