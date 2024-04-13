(in-package :cl-user)

(py4cl2-cffi:defpymodule "math" nil :silent t :continue-ignoring-errors nil)
(py4cl2-cffi:defpymodule "numpy" nil :lisp-package "NP" :silent t
                          :continue-ignoring-errors nil)
(py4cl2-cffi:defpymodule "numpy.random" t :silent t
                         :continue-ignoring-errors nil)

;;; Do not test on ECL on CI just to save some travis time
;;; Do not test on ABCL because it causes some error even though the command
;;; works fine in the REPL
#-(or ecl abcl)
(progn
  (py4cl2-cffi:defpymodule "networkx" nil :lisp-package "NX" :silent t
                           :continue-ignoring-errors nil)
  (py4cl2-cffi:defpymodule "matplotlib.pyplot" nil :lisp-package "PLT" :silent t
                           :continue-ignoring-errors nil))

(defpackage :py4cl2-cffi-tests
  (:use :cl :clunit :py4cl2-cffi :iterate)
  (:shadow :deftest)
  (:export :run))
