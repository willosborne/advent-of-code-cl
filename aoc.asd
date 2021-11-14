(defsystem "aoc"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("sqlite"
               "drakma"
               "fset"
               "alexandria"
               "named-readtables"
               "cl-interpol"
               "1am"
               "cl-ppcre"
               "maxpc"
               "lol"
               "arrow-macros"
               "str")
  :components ((:module "src"
                :serial t
                :components
                ((:file "main")
                 (:file "cookies")
                 (:file "aoc-api")))
               (:module "2019"
                :serial t
                :depends-on ("src")
                :components
                ((:file "day-1")
                 (:file "day-2")
                 (:file "day-3"))))
  :description ""
  :in-order-to ((test-op (test-op "aoc/tests"))))

(defsystem "aoc/tests"
  :author ""
  :license ""
  :depends-on ("aoc"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for aoc"
  :perform (test-op (op c) (symbol-call :rove :run c)))
