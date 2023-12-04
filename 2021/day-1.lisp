(defpackage aoc-2021-1
  (:use :cl :arrow-macros :rutils.bind :rutils.symbol :rutils.readtable :1am))
(in-package :aoc-2021-1)

;; (named-readtables:in-readtable rutils.readtable:rutils-readtable)

;; (defun calc-increases-1 (nums)
;;   (let* ((shifted (cdr nums))
;;          (pairs (mapcar #'list nums shifted)))
;;     (loop for (last curr) in pairs
;;           counting (< last curr))))

;; (defun get-answer-1 ()
;;   (->> (aoc:get-input 2021 1)
;;     (str:lines)
;;     (mapcar #'parse-integer)
;;     (calc-increases-1)))


;; (defun calc-increases-2 (nums)
;;   (let* ((shifted-1 (cdr nums))
;;          (shifted-2 (cddr nums))
;;          (triples (mapcar #'list nums shifted-1 shifted-2))
;;          (sums (mapcar ^(apply #'+ %) triples))
;;          (shifted-sums (cdr sums))
;;          (pairs-sums (mapcar #'list sums shifted-sums)))
;;     (loop for (last curr) in pairs-sums
;;           counting (< last curr))))


;; (defun get-answer-2 ()
;;   (->> (aoc:get-input 2021 1)
;;     (str:lines)
;;     (mapcar #'parse-integer)
;;     (calc-increases-2)))
