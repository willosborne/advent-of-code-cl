(in-package :aoc)

(defun build-url (year day)
  (format nil "http://adventofcode.com/~a/day/~a/input" year day))

(defun get-input-http (year day)
  (let ((url (build-url year day)))
    (multiple-value-bind (body code) (drakma:http-request url :cookie-jar *cookie-jar*)
      (if (not (= code 200))
          (format t "No input for this year/day - is it up yet?")
          body))))

(defparameter *db-location* (uiop:native-namestring "~/common-lisp/aoc/inputs.db"))

(defun setup-db ()
  (if (probe-file *db-location*)
      (format t "DB exists already.~%")
      (let ((db (sqlite:connect *db-location*)))
        (sqlite:execute-non-query db "create table input(year integer, day integer, data text)")
        (format t "Created database."))))

(defun get-input-from-db (year day)
  (sqlite:with-open-database (db *db-location*)
    (sqlite:execute-single/named db "select data from input where year = :year and day = :day" ":year" year ":day" day)))

(defun insert-input-into-db (year day data)
  (sqlite:with-open-database (db *db-location*)
    (sqlite:execute-non-query/named db "insert into input (year, day, data) values (:year, :day, :data)"
                                    ":year" year
                                    ":day" day
                                    ":data" data)))

(defun get-input (year day)
  (let ((input (get-input-from-db year day)))
    (if input
        input
        (let ((data (get-input-http year day)))
          (when data 
            (insert-input-into-db year day data)
            data)))))
