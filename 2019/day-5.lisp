(defpackage aoc-2019-5
  (:use :cl :arrow-macros :rutils.bind :rutils.symbol :rutils.readtable :lol :1am))
(in-package :aoc-2019-5)

(named-readtables:in-readtable rutils.readtable:rutils-readtable)

(declaim (optimize (debug 3)))
;; (defparameter *opcode-lookup* (alexandria:plist-hash-table
;;                                '(1 '(:code 1 :args ()))))

(defunc symbol->keyword (sym)
  (alexandria:make-keyword (string-upcase (symbol-name sym))))

(defstruct instruction
  (operation nil :type keyword)
  (params nil :type list)
  (param-modes nil :type list))

(defstruct operation
  (code -1 :type integer)
  (name nil :type keyword)
  (num-params -1 :type integer)
  (func nil :type function))

(defparameter *ops-by-code* (make-hash-table))
(defparameter *ops-by-name* (make-hash-table))

(defparameter *input* nil)
(defparameter *output* nil)

(defun read-input ()
  (let ((v (car *input*)))
    (setf *input* (cdr *input*))
    v))

(defun write-output (val)
  (setf *output* (append *output* (list val))))

(defun reset-ops ()
  (setf *ops-by-name* (make-hash-table))
  (setf *ops-by-code* (make-hash-table)))

(defmacro defop (name code params &body body)
  (let ((op (gensym)))
    `(progn
       (let ((,op (make-operation :code ,code
                                  :name ,(symbol->keyword name)
                                  :num-params ,(length params)
                                  :func (lambda (data pc &rest args)
                                          (declare (ignorable data pc))
                                          (destructuring-bind ,params args
                                            (declare (ignorable ,@(flatten params)))
                                            ,@body)))))
         (setf (gethash (operation-name ,op) *ops-by-name*) ,op)
         (setf (gethash ,code *ops-by-code*) ,op)
         ,op))))

(defmacro atat (i data)
  `(aref ,data (aref ,data ,i)))

(defmacro at (i data)
  `(aref ,data ,i))

(defun get-value (i data mode)
  (ecase mode
    (:immediate (at i data))
    (:position (atat i data))))

(defun setup ()
  (reset-ops)
  (defop add 1 ((left-idx left-mode) (right-idx right-mode) (out-idx out-mode))
    (setf (atat out-idx data)
          (+ (get-value left-idx data left-mode)
             (get-value right-idx data right-mode)))
    (+ 4 pc))

  (defop mul 2 ((left-idx left-mode) (right-idx right-mode) (out-idx out-mode))
    (setf (atat out-idx data)
          (* (get-value left-idx data left-mode)
             (get-value right-idx data right-mode)))
    (+ 4 pc))

  (defop stop 99 ()
    -1)

  (defop input 3 ((out-idx out-mode))
    (setf (atat out-idx data) (read-input))
    (+ 2 pc))

  (defop output 4 ((val-idx val-mode))
    (write-output (get-value val-idx data val-mode))
    (+ 2 pc))

  (defop jnz 5 ((test-idx test-mode) (target-idx target-mode))
    (if (not (zerop (get-value test-idx data test-mode)))
        (get-value target-idx data target-mode)
        (+ pc 3)))

  (defop jiz 6 ((test-idx test-mode) (target-idx target-mode))
    (if (zerop (get-value test-idx data test-mode))
        (get-value target-idx data target-mode)
        (+ pc 3)))

  (defop lt 7 ((left-idx left-mode) (right-idx right-mode) (out-idx out-mode))
    (setf (atat out-idx data)
          (if (< (get-value left-idx data left-mode)
                 (get-value right-idx data right-mode))
              1 0))
    (+ 4 pc))

  (defop eq 8 ((left-idx left-mode) (right-idx right-mode) (out-idx out-mode))
    (setf (atat out-idx data)
          (if (= (get-value left-idx data left-mode)
                 (get-value right-idx data right-mode))
              1 0))
    (+ 4 pc)))

(defun resolve-param (param-pair data)
  (destructuring-bind (param mode) param-pair
    (case mode
      (:immediate param)
      (:position (aref data param)))))

(defun run-instruction (instruction data pc)
  "Run an instruction, looking up parameters according to their param modes."
  (let* ((op (gethash (instruction-operation instruction) *ops-by-name*))
         (param-pairs (mapcar #'list
                              (instruction-params instruction)
                              (instruction-param-modes instruction)))
         (aug-params (append (list data pc) param-pairs)))
    (values (apply (operation-func op) aug-params) data)))

(defmacro with-ops (&body forms)
  `(let ((*ops-by-name* nil)
        (*ops-by-code* nil))
     ,@forms))

(test run-add
  (with-ops
    (setup)
    (let ((arr (make-array '(10) :initial-element 0))
          (inst (make-instruction :operation :add :params '(2 3 0) :param-modes '(:immediate :immediate :position))))
      (is (= (run-instruction inst arr 0) 4))
      (is (= (aref arr 0) 5)))))
(test run-mul
  (with-ops
    (setup)
    (let ((arr (make-array '(10) :initial-element 0))
          (inst (make-instruction :operation :mul :params '(2 3 0) :param-modes '(:immediate :immediate :position))))
      (is (= (run-instruction inst arr 0) 4))
      (is (= (aref arr 0) 6)))))
(test run-stop
  (with-ops
    (setup)
    (let ((arr (make-array '(10) :initial-element 0))
          (inst (make-instruction :operation :stop :params '() :param-modes '())))
      (is (= (run-instruction inst arr 0) -1))
      (is (= (aref arr 0) 0)))))
(test run-input
  (with-ops
    (setup)
    (let ((arr (make-array '(6) :initial-contents '(0 2 0 0 0 0)))
          (inst (make-instruction :operation :input :params '(1) :param-modes '(:position)))
          (*input* (list 123)))
      (is (= (run-instruction inst arr 0) 2))
      (is (= (aref arr (aref arr 1)) 123))
      (is (not *input*)))))
(test run-output
  (with-ops
    (setup)
    (let ((arr (make-array '(10) :initial-element 0))
          (inst (make-instruction :operation :output :params '(123) :param-modes '(:immediate)))
          (*output* nil))
      (is (= (run-instruction inst arr 0) 2))
      (is (equalp *output* (list 123))))))

(defun parse-mode-char (char)
  (case char
    (#\0 :position)
    (#\1 :immediate)
    (t (error "Invalid mode: ~a" char))))

(defun get-param-indices (base num-params)
  (loop for i from (+ base 1) below (+ base 1 num-params)
        collect i))

(defun parse-instruction (index data)
  (bind ((opcode (aref data index))
         (padded (str:pad-left 5 (write-to-string opcode) :pad-char #\0))
         (code (parse-integer (str:substring 3 5 padded)))
         (mode-chars (reverse (coerce (str:substring 0 3 padded) 'list)))
         (param-modes (mapcar #'parse-mode-char mode-chars))
         (op (gethash code *ops-by-code*))
         (params (get-param-indices index (operation-num-params op))))
    (make-instruction :operation (operation-name op)
                      :params params
                      :param-modes param-modes)))

(defun load-array (data)
  (coerce (mapcar #'parse-integer (str:split #\, data)) 'vector))

(defparameter *aoc-input*  (load-array (aoc:get-input 2019 5)))

(defun run-program (data input)
  (let ((pc 0)
        (*input* input)
        (*output* nil)
        (inst nil))
    (loop while (not (= pc -1))
          do (setf inst (parse-instruction pc data))
             (setf pc (run-instruction inst data pc))
             ;; (format t "~a~%" data)
          finally (return *output*))))

(defun get-answer-1 ()
  (with-ops
    (setup)
    (bind ((data (load-array (aoc:get-input 2019 5))))
      (run-program data (list 1)))))

(defun get-answer-2 ()
  (with-ops
    (setup)
    (bind ((data (load-array (aoc:get-input 2019 5))))
      (run-program data (list 5)))))
