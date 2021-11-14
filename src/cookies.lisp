(in-package :aoc)

(defvar *session-cookie*)

(defun get-edge-cookie-db-location ()
  (merge-pathnames #P"/mnt/c/Users/0xzer/AppData/Local/Microsoft/Edge/User Data/Default/Cookies"))

(defvar *cookies-db-location* (get-edge-cookie-db-location))

(defstruct cookie name value)

(defun get-cookie ()
  (sqlite:with-open-database (db *cookies-db-location*)
    (loop for (name value) in (sqlite:execute-to-list db "select name, value from cookies where host_key='.adventofcode.com'")
          do (format t "~a~%" name)
          when (equal name "session")
            return (make-cookie :name "session" :value value)
          finally (format t "No AoC session cookie found, sorry!~%"))))

(defparameter *session-cookie* (make-cookie
                        :name "session"
                        :value "53616c7465645f5f87362f86fcf27ab429f7c50995ddd5b78a4cf85ae3a173b70a519dc4bc125d2d6cbe479727463161"))

(defparameter *cookie-jar* (make-instance 'drakma:cookie-jar
                                          :cookies (list (make-instance 'drakma:cookie
                                                                        :domain ".adventofcode.com"
                                                                        :name "session"
                                                                        :value (cookie-value *session-cookie*)))))
