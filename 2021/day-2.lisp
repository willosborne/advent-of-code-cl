(defpackage aoc-2021-2
  (:use :cl :arrow-macros :rutils.bind :rutils.symbol :rutils.readtable :1am))
(in-package :aoc-2021-2)

(named-readtables:in-readtable rutils.readtable:rutils-readtable)

(defun parse-command (cmd)
  (bind (((direction amount) (str:split #\  cmd))
         (dir (alexandria:switch (direction :test #'string=)
                ("up" :up)
                ("forward" :forward)
                ("down" :down))))
    (list dir (parse-integer amount))))

(defun run-program (commands)
  (loop with x = 0
        with depth = 0
        for (command amount) in commands
        do (ecase command
             (:up (decf depth amount))
             (:down (incf depth amount))
             (:forward (incf x amount)))
           (format t "~a ~a: (~a, ~a)~%" command amount x depth)
        finally (return (* x depth))))

(defun get-answer-1 ()
  (->> (aoc:get-input 2021 2)
    (str:lines)
    (mapcar #'parse-command)
    (run-program)))


(defun run-program-aim (commands)
  (loop with x = 0
        with depth = 0
        with aim = 0
        for (command amount) in commands
        do (ecase command
             (:up (decf aim amount))
             (:down (incf aim amount))
             (:forward (progn (incf x amount)
                              (incf depth (* aim amount)))))
           (format t "~a ~a: (~a, ~a)~%" command amount x depth)
        finally (return (* x depth))))


(defun get-answer-2 ()
  (->> (aoc:get-input 2021 2)
    (str:lines)
    (mapcar #'parse-command)
    (run-program-aim)))



(loop [a 10
       total 1]
         (if (> a 1)
             (recur (- a 1) (* total a))
             total))
