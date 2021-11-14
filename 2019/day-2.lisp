(defpackage aoc-2019-2
  (:use :cl :arrow-macros))
(in-package :aoc-2019-2)

;; (defparameter *opcode-lookup* (alexandria:plist-hash-table
;;                                '(1 '(:code 1 :args ()))))

(defconstant +add+ 1)
(defconstant +mul+ 2)
(defconstant +stop+ 99)

(defun load-array (data)
  (coerce (mapcar #'parse-integer (str:split #\, data)) 'vector))

(defparameter *input*  (load-array (aoc:get-input 2019 2)))

(defun get-n-from (arr start n)
  (loop for i from start upto (+ start n)
        collect (aref arr i)))

(defun eval-operation (data pc)
  (let ((op (aref data pc)))
    (cond
      ((= op +stop+) nil)
      ((= op +add+)
       (destructuring-bind (left right out) (coerce (subseq data (+ 1 pc) (+ 4 pc)) 'list)
         (setf (aref data out) (+ (aref data left) (aref data right)))
         (+ 4 pc)))
      ((= op +mul+)
       (destructuring-bind (left right out) (coerce (subseq data (+ 1 pc) (+ 4 pc)) 'list)
         (setf (aref data out) (* (aref data left) (aref data right)))
         (+ 4 pc)))
      (t (error "Invalid operation")))))

(defun run-program (data)
  (let ((pc 0))
    (loop while pc
          do (setf pc (eval-operation data pc))
             ;; (format t "~a~%" data)
          finally (return data))))

(defun get-answer-1 ()
  (let ((input (load-array (aoc:get-input 2019 2))))
    (setf (aref input 1) 12
          (aref input 2) 2)
    (aref (run-program input) 0)))

(defun run-program-inputs (data noun verb)
  (let ((data (alexandria:copy-array data)))
    (setf (aref data 1) noun
          (aref data 2) verb)
    (aref (run-program data) 0)))

(defun get-answer-2 ()
  (let ((input (load-array (aoc:get-input 2019 2)))
        (target 19690720))
    (loop named outer
          for noun from 0 below 100
          do (loop for verb from 0 below 100
                   do (when (= (run-program-inputs input noun verb)
                               target)
                        (return-from outer (+ (* 100 noun) verb)))))))
