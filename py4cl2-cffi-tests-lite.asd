(asdf:defsystem "py4cl2-cffi-tests-lite"
  :serial t
  :description "Unit tests for the py4cl2-cffi library."
  :author "Shubhamkar Ayare <shubhamayare@yahoo.co.in>"
  :license "MIT"
  :version "0.3.0" ; beta
  :depends-on ("py4cl2-cffi"
               "array-operations"
               "alexandria"
               "clunit"
               "float-features"
               "trivial-garbage"
               "trivial-arguments")
  :components ((:file "package")
               (:file "tests"))
  :perform (test-op (o c) (symbol-call :py4cl2-cffi-tests :run)))
