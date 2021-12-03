(defpackage aoc-2021-3
  (:use :cl :arrow-macros :rutils.bind :rutils.symbol :rutils.readtable :lol :1am))
(in-package :aoc-2021-3)

(defun count-elements (list &key (test #'eql))
  (loop for elem in list
        with hash = (make-hash-table :test test)
        do (if (gethash elem hash)
               (incf (gethash elem hash))
               (setf (gethash elem hash) 1))
        finally (return
                  (sort
                   (loop for key being the hash-keys of hash
                         for val = (gethash key hash)
                         collect (list key val))
                   ^(> (cadr %) (cadr %%))))))


(defun elem (nth)
  (lambda (string)
    (str:substring nth (1+ nth) string)))

(defun get-positional-strings (list-of-strings)
  (mapcar ))

(defun get-positional (strings n)
  (bind ((len (length (car strings)))
         (bits (loop for position from 0 below len
                     for positional-strings = (mapcar (elem position) strings)
                     for elem-counts = (count-elements positional-strings :test #'equal)
                     collect (car (nth n elem-counts))))
         (concatted (format nil "~{~a~}" bits)))
    (parse-integer concatted :radix 2)))

(defun get-gamma (strings)
  (get-positional strings 0))

(defun get-epsilon (strings)
  (get-positional strings 1))

(defun get-answer-1 ()
  (->> (aoc:get-input 2021 3)
    (str:lines)
    (lambda (lines)
      (* (get-gamma lines) (get-epsilon lines)))))


(defun has-positional-value (string n value)
  (string= (str:substring n (1+ n) string)
           value))

(defun get-bit-criteria (elem-counts n)
  (if (= (length elem-counts) 1)
      (if (= n 0) "1" "0") ; tie breaker; return the opposite
      (bind (((fst snd &rest rest) elem-counts)) ; pattern match first 2 elements
        (if (= (cadr fst) (cadr snd))
            (if (= n 0) "1" "0") ; tie breaker; return the opposite
            (if (= n 0)
                (car fst)
                (car snd))))))

(defun filter-by-positional-match (lines n position)
  (bind ((target-bit (-<>> (mapcar (elem position) lines)
                       (count-elements <> :test #'equal)
                       (get-bit-criteria <> n))))
    (remove-if-not (lambda (s)
                     (has-positional-value s position target-bit))
                   lines)))

(defun get-rating (lines n)
  (loop with len = (length (car lines))
        for position from 0 below len
        do (format t "~a: ~a~%" position lines)
           (setf lines (filter-by-positional-match lines n position))
           (when (= (length lines) 1)
             (return (-<>> (car lines)
                       (parse-integer <> :radix 2))))))

(defun get-answer-2 ()
  (->> (aoc:get-input 2021 3)
    (str:lines)
    (lambda (lines)
      (* (get-rating lines 0) (get-rating lines 1)))))

