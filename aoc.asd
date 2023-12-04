(defsystem "aoc"
  :version "0.1.0"
  :author ""
  :license ""
  :serial t
  :depends-on ("sqlite"
               "drakma"
               "fset"
               "alexandria"
               "named-readtables"
               "cl-interpol"
               "iterate"
               "parachute"
               "access"
               "cl-ppcre"
               "maxpc"
               "arrows"
               "rutils"
               "str"
               "cl-json"
               "serapeum")
  :components ((:module "src"
                :serial t
                :components
                ((:file "main")
                 (:file "cookies")
                 (:file "aoc-api")
                 (:file "utils")))
               ;; (:module "2019"
               ;;  :serial t
               ;;  :depends-on ("src")
               ;;  :components
               ;;  ((:file "day-1")
               ;;   (:file "day-2")
               ;;   (:file "day-3")
               ;;   (:file "day-4")
               ;;   (:file "day-5")))

               ;; (:module "2021"
               ;;  :serial t
               ;;  :depends-on ("src")
               ;;  :components
               ;;  ((:file "day-1")
               ;;   ))
               (:module "2023"
                :serial t
                :components
                ((:file "day-1")
                 (:file "day-2")
                 (:file "day-3")
                 (:file "day-4")))
               )
  :description ""
  :in-order-to ((test-op (test-op "aoc/tests"))))

(asdf:defsystem :aoc/tests
  :description ""
  :author ""
  :license  "MIT"
  :version "0.0.1"
  :depends-on (#:aoc)
  :perform (test-op (o c) (uiop:symbol-call :aoc '#:run)))
