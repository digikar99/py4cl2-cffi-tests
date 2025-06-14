(in-package :py4cl2-cffi-tests)

#.(when (find-package :named-readtables)
    `(,(find-symbol "IN-READTABLE" :named-readtables) :common-lisp))

(defsuite py4cl ())
(defsuite callpython-raw (py4cl))
(defsuite callpython-utility (py4cl))
(defsuite callpython-chain (py4cl))
(defsuite callpython-remote (py4cl))
(defsuite import-export (py4cl))
(defsuite embed-basic (py4cl))
(defsuite objects (py4cl))
(defsuite numpy-ufunc (py4cl))
(defsuite element-type (py4cl))
(defsuite array-data (py4cl))

(py4cl2-cffi:pystart :verbose nil)
(defvar *pyversion* (py4cl2-cffi:pyversion-info))
;; so that calling this does not mess up other tests: autostarts in particular
(py4cl2-cffi:pystop)

(defmacro deftest (name declarations required-py4cl2-features &body body)
  `(clunit:deftest ,name ,declarations
     (let ((py4cl2-cffi/config:*pystart-verbosity* nil))
       (py4cl2-cffi:with-pygc
         (if (subsetp ',required-py4cl2-features py4cl2-cffi:*internal-features*)
             (progn
               ,@body)
             (clunit::skip-test-case))))))

(defmacro skip-on (skip-features assert-form)
  `(if (intersection ',skip-features *features*)
       (clunit::skip-test-case)
       ,assert-form))

(defun run (&optional interactive? result-for)
  "Run all the tests for py4cl2."
  (declare (ignore result-for))
  (terpri)
  (princ (run-suite 'py4cl :use-debugger interactive?)))

;; ======================== EMBED-BASIC =====================================

(deftest start-and-stop (embed-basic) nil
  (py4cl2-cffi:pystart)
  (py4cl2-cffi:raw-pyexec "def foo(): return None")
  (assert-true (py4cl2-cffi:pyeval "foo"))
  (py4cl2-cffi:pystop)
  (if (eq +python-call-mode+ :dedicated-thread)
      (clunit::skip-test-case)
      (assert-condition py4cl2-cffi:pyerror (py4cl2-cffi:pyeval "foo"))))

;; ======================== CALLPYTHON-RAW =====================================

(deftest raw-autostart (callpython-raw) nil
  (py4cl2-cffi:pystop)
  (py4cl2-cffi:raw-pyeval "'hello'")
  (assert-true (py4cl2-cffi:python-alive-p))
  (py4cl2-cffi:pystop)
  (py4cl2-cffi:raw-pyexec "import sys")
  (assert-true (py4cl2-cffi:python-alive-p))
  (py4cl2-cffi:pystop))

(deftest raw-io-flush (callpython-raw) (:with-python-output)
  (assert-equalp "hello" (py4cl2-cffi:raw-pyeval "'hello'"))
  (assert-equalp "world" (py4cl2-cffi:raw-pyeval "'world'"))
  (py4cl2-cffi:pyexec "import sys")
  (assert-equalp "hello world"
      (with-python-output
        (py4cl2-cffi:raw-pyexec "sys.stdout.write(\"hello world\")")))
  (assert-equalp "testing"
      (with-python-output
        (py4cl2-cffi:raw-pyexec "sys.stdout.write(\"testing\")"))))

;; If locks and synchronization are not implemented properly, this
;; would likely fail; in fact, SBCL itself seems to stop
;; SBCL can also stop inspite of it being implemented correctly.
(deftest with-python-output-stress-test (callpython-raw) (:with-python-output)
  (iter (repeat #+(or os-windows windows) 100
                #-(or os-windows windows) 10000)
    (string= "hello" (with-python-output (pyexec "print('hello', end = '')")))))

(deftest eval-integer (callpython-raw) (:with-python-output)
  (let ((result (py4cl2-cffi:raw-pyeval "1 + 2 * 3")))
    (assert-true (typep result 'integer))
    (assert-equalp 7 result)))

(deftest eval-malformed (callpython-raw) nil
    (assert-condition py4cl2-cffi:pyerror
      (py4cl2-cffi:raw-pyeval "1 + ")))

(deftest eval-real (callpython-raw) nil
  (let ((result (py4cl2-cffi:raw-pyeval "1.3 + 2.2")))
    (assert-true (typep result 'real))
    (assert-equalp 3.5 result)))

(deftest eval-infinity (callpython-raw) nil
  (py4cl2-cffi:raw-pyexec "import numpy as np")

  (assert-equal float-features:double-float-positive-infinity
      (py4cl2-cffi:raw-pyeval "float('inf')"))
  (assert-equal float-features:double-float-negative-infinity
      (py4cl2-cffi:raw-pyeval "-float('inf')"))
  (assert-equal float-features:double-float-positive-infinity
      (py4cl2-cffi:raw-pyeval "np.float64('inf')"))
  (assert-equal float-features:double-float-negative-infinity
      (py4cl2-cffi:raw-pyeval "-np.float64('inf')"))
  (assert-equal float-features:single-float-positive-infinity
      (py4cl2-cffi:raw-pyeval "np.float32('inf')"))
  (assert-equal float-features:single-float-negative-infinity
      (py4cl2-cffi:raw-pyeval "-np.float32('inf')"))

  (assert-equal float-features:double-float-positive-infinity
      (py4cl2-cffi:pyeval float-features:double-float-positive-infinity))
  (assert-equal float-features:double-float-negative-infinity
      (py4cl2-cffi:pyeval float-features:double-float-negative-infinity))
  (assert-equal float-features:single-float-positive-infinity
      (py4cl2-cffi:pyeval float-features:single-float-positive-infinity))
  (assert-equal float-features:single-float-negative-infinity
      (py4cl2-cffi:pyeval float-features:single-float-negative-infinity)))

(deftest eval-vector (callpython-raw) nil
  (let ((result (py4cl2-cffi:raw-pyeval "[i**2 for i in range(4)]")))
    (assert-true (typep result 'array))
    (assert-equalp #(0 1 4 9) result)))

(deftest eval-list (callpython-raw) nil
  (let ((result (py4cl2-cffi:raw-pyeval "(1,2,3)")))
    (assert-true (typep result 'cons))
    (assert-equalp '(1 2 3) result)))

(deftest eval-nan (callpython-raw) nil
  (raw-pyexec "import numpy as np")
  (assert-true (let ((value (raw-pyeval "float('nan')")))
                 (and (typep value 'double-float)
                      (float-features:float-nan-p value))))
  (assert-true (let ((value (raw-pyeval "np.float32('nan')")))
                 (and (typep value 'single-float)
                      (float-features:float-nan-p value))))
  (assert-true (let ((value (raw-pyeval "np.float64('nan')")))
                 (and (typep value 'double-float)
                      (float-features:float-nan-p value))))
  (assert-true (let ((value (pyeval float-features:single-float-nan)))
                 (and (typep value 'single-float)
                      (float-features:float-nan-p value))))
  (assert-true (let ((value (pyeval float-features:double-float-nan)))
                 (and (typep value 'double-float)
                      (float-features:float-nan-p value)))))

;; Check passing strings, including quote characters which need to be escaped
(deftest eval-string (callpython-raw) nil
  (assert-equalp "say \"hello\" world"
      (py4cl2-cffi:raw-pyeval "'say \"hello\"' + ' world'")))

(deftest eval-string-newline (callpython-raw) nil
  (let ((str "hello
world"))
    #-(or :os-windows :windows)
    (assert-equalp str (py4cl2-cffi:raw-pyeval (py4cl2-cffi::py-repr str)))
    #+(or :os-windows :windows)
    (assert-equalp "hello
world"
        (py4cl2-cffi:raw-pyeval (py4cl2-cffi::pythonize str))))
  #+unix ;; Check that handling this on windows did not affect unix in unexpected ways.
  ;; If this is not handled correctly, the process may wait indefinitely.
  (assert-equalp (uiop:strcat #\return #\newline) (pyeval "'\\r\\n'")))

(deftest eval-format-string (callpython-raw) nil
  (assert-equalp "foo"
      (py4cl2-cffi:pyeval (py4cl2-cffi:pythonize "foo"))))

;; This tests whether outputs to stdout mess up the return stream
(deftest eval-print (callpython-raw) nil
  (unless (= 2 (first *pyversion*))
    ;; Should return the result of print, not the string printed
    (assert-true (py4cl2-cffi::pyobject-wrapper-eq
                  (pyvalue "None")
                  (py4cl2-cffi:raw-pyeval "print(\"hello\")"))
      "This fails with python 2")))

(deftest unicode-string-type (callpython-raw) nil
  ;; Python 2 and python 3 handle unicode differently
  ;; This just catches the use of unicode type strings in python2
  ;; not the use of unicode characters
  (assert-equal "test unicode"
                (py4cl2-cffi:pyeval "u'test unicode'"))
  (assert-equal 3
                (gethash "pizza"
                         (py4cl2-cffi:pyeval "{u'pizza': 3}"))))

(deftest unicode-characters (callpython-raw) nil
  (assert-equal "Ƃ" (py4cl2-cffi:pyeval "'Ƃ'")))

(deftest eval-ratios (callpython-raw) nil
  (assert-equalp 1/2 (py4cl2-cffi:pyeval 1/2)) ; round trip
  (assert-equalp 1/4 (py4cl2-cffi:pyeval 1/2 "/" 2)) ; manipula-ble
  ;; Complex ratios not supported in python so converts to floats
  (assert-equality #'= #C(0.5 1.0)
    (py4cl2-cffi:pyeval #C(1 2) "*" 1/2)))

(deftest eval-nil (callpython-raw) nil
  (assert-equalp +py-empty-tuple+ (raw-pyeval "()")))

(deftest python-exec-scope (callpython-raw) nil
  ;; Local functions are retained in scope
  ;; This changed in python 3.x see e.g. https://stackoverflow.com/a/24734880
  (assert-true
      (progn
        (py4cl2-cffi:pyexec "
a = 5
def foo(): return a
def bar():
  return foo() + foo()
bar()")
        t)))

;; ======================== CALLPYTHON-UTILITY =====================================

(deftest pyeval-params (callpython-utility) nil
  ;; Values are converted into python values
  (let ((a 4)
        (b 7))
    (assert-equalp 11
                   (py4cl2-cffi:pyeval a "+" b)))

  ;; Arrays can also be passed
  (assert-equalp #2A((1 2) (3 4))
                 (py4cl2-cffi:pyeval #2A((1 2) (3 4))))

  (assert-equalp #2A((2 4) (6 8))
                 (py4cl2-cffi:pyeval #2A((1 2) (3 4)) "*" 2))

  (assert-equalp #3A(((2 4) (7 8)) ((8 5) (1 6)))
                 (py4cl2-cffi:pyeval #3A(((1 3) (6 7)) ((7 4) (0 5)))  "+" 1))

  ;; Test handling of real numbers in arrays
  (assert-equalp #(1.0 2.0)
                 (py4cl2-cffi:pyeval (vector 1.0 2.0)))

  ;; Test empty arrays
  (assert-equalp #()
                 (py4cl2-cffi:pyeval #()))

  ;; Unless the values are strings
  (let ((str "hello"))
    (assert-condition py4cl2-cffi:pyerror
                      (py4cl2-cffi:pyeval "len(" str ")")) ; "len(hello)"

    ;; To pass a string to python, run through pythonize:
    (assert-equalp 5
                   (py4cl2-cffi:pyeval "len(" (py4cl2-cffi::pythonize str) ")"))))

(deftest pyeval-complex-values (callpython-utility) nil
  ;; Single values
  (assert-equality #'= #C(1 2)
    (py4cl2-cffi:pyeval #C(1 2)))
  (assert-equality #'= #C(1 -2)
    (py4cl2-cffi:pyeval #C(1 -2)))
  (assert-equality #'= #C(-1 -2)
    (py4cl2-cffi:pyeval #C(-1 -2)))

  ;; Expressions. Tested using multiply to catch things like
  ;; "1+2j * 2+3j -> 1+7j rather than (-4+7j)
  ;; Note: Python doesn't have complex integers, so all returned
  ;;       values could be floats
  (assert-equality #'= #C(-4 7)
    (py4cl2-cffi:pyeval #C(1 2) "*" #C(2 3)))
  (assert-equality #'= #C(4 7)
    (py4cl2-cffi:pyeval #C(1 -2) "*" #C(-2 3)))

  ;; Lists of complex numbers
  (assert-equality #'= #C(6 9)
    (py4cl2-cffi:pyeval "sum(" (list #C(1 2) #C(2 3) #C(3 4))  ")")))

(deftest pyeval-return-numpy-types (callpython-utility) nil
  (py4cl2-cffi:pyexec "import numpy as np")
  (assert-equalp 42.0
      (py4cl2-cffi:pyeval "np.float64(42.0)")))

(deftest pyeval-hash-table-from-dict (callpython-utility) nil
  ;; Simple keys
  (let ((table (py4cl2-cffi:pyeval "{1:2, 2:3}")))
    (assert-equalp 2
                   (gethash 1 table))
    (assert-equalp 3
                   (gethash 2 table)))

  ;; Ensure values are being lispified
  (let ((table (py4cl2-cffi:pyeval "{1:[1,2,3]}")))
    (assert-equalp #(1 2 3)
                   (gethash 1 table)))

  ;; Ensure keys are being lispified and string keys work
  (let ((table (py4cl2-cffi:pyeval "{\"test\":42}")))
    (assert-equalp 42
        (gethash "test" table)))

  ;; Ensure tuple keys and values being lispified and work correctly
  (let ((table (py4cl2-cffi:pyeval "{(1,2):(3,4), (\"hello\",\"world\"):42}")))
    (assert-equalp '(3 4)
        (gethash '(1 2) table))
    (assert-equalp 42
        (gethash '("hello" "world") table))))

(deftest setf-eval (callpython-utility) nil
  (setf (py4cl2-cffi:pyeval "test_value") 42) ; Set a variable
  (assert-equalp 42
                 (py4cl2-cffi:pyeval "test_value")))


(deftest pyexec (callpython-utility) nil
  (unless (= 2 (first *pyversion*))
    (assert-equalp nil
        (multiple-value-list (py4cl2-cffi:pyexec "print(\"hello\")"))
      "This fails with python 2"))
  (assert-equalp nil ; in case someone makes this a macro some day!
      (multiple-value-list (let ((module "sys")) (py4cl2-cffi:pyexec "import " module))))
  (assert-equalp '("hello" 5) ; in case someone makes this a macro some day!
      (let ((a "'hello'") (b 5))
        (py4cl2-cffi:pyexec "temp1 = " a)
        (py4cl2-cffi:pyexec "temp2 = " b)
        (py4cl2-cffi:pyeval "(temp1, temp2,)"))))

(deftest pycall-autostart (callpython-utility) nil
  (py4cl2-cffi:pystop)
  (py4cl2-cffi:pycall "int" "5")
  (assert-true (py4cl2-cffi:python-alive-p))
  (py4cl2-cffi:pystop))

(deftest pycall-io-flush (callpython-utility) (:with-python-output)
  (assert-equalp 5 (py4cl2-cffi:pycall "int" "5"))
  (assert-equalp "world" (py4cl2-cffi:pycall "str" "world"))
  (let ((py4cl2-cffi::*py4cl-tests* t))
    (py4cl2-cffi:pystop)
    (py4cl2-cffi:pyexec "import sys")
    (skip-on (:ci :ccl :ecl)
             (assert-equalp "hello world"
                 (with-python-output (py4cl2-cffi:pycall "sys.stdout.write" "hello world"))))
    (skip-on (:ci :ccl :ecl)
             (assert-equalp "testing"
                 (with-python-output (py4cl2-cffi:pycall "sys.stdout.write" "testing"))))))

(deftest pycall-one-arg-int (callpython-utility) nil
  (assert-equalp 42
      (py4cl2-cffi:pycall "abs" -42)))

(deftest pycall-one-arg-list (callpython-utility) nil
  (assert-equalp 9
      (py4cl2-cffi:pycall "sum" '(3 2 4))))

(deftest pycall-one-arg-string (callpython-utility) nil
  (assert-equalp #("h" "e" "l" "l" "o")
      (py4cl2-cffi:pycall "list" "hello")))

(deftest pycall-dotted-function (callpython-utility) nil
  (py4cl2-cffi:pyexec "import math")
  (assert-equalp (sqrt 42d0)
      (py4cl2-cffi:pycall "math.sqrt" 42)))

(deftest pycall-lambda-function (callpython-utility) nil
  (assert-equalp 16
      (py4cl2-cffi:pycall "lambda x: x*x" 4)))

(deftest pycall-lambda-function-two-args (callpython-utility) nil
  (assert-equalp 10
      (py4cl2-cffi:pycall "lambda x, y: x*y - y" 3 5)))

(deftest pycall-lambda-keywords (callpython-utility) nil
  (assert-equalp -1
      (py4cl2-cffi:pycall "lambda a=0, b=1: a-b" :b 2 :a 1))
  (assert-equalp 1
      (py4cl2-cffi:pycall "lambda a=0, b=1: a-b" :a 2 :b 1)))

(deftest pycall-with-lambda-callback (callpython-utility) nil
  ;; Define a function in python which calls its argument
  (py4cl2-cffi:pyexec "runme = lambda f: f()")
  ;; Pass a lambda function to pycall
  (assert-equalp 42
      (py4cl2-cffi:pycall "runme" (lambda () 42))))

(deftest pycall-string (callpython-utility) nil
  (assert-equalp "hello" (py4cl2-cffi:pycall "str" "hello")))

(deftest pycall-symbol-as-fun-name (callpython-utility) nil
  (let ((py4cl2-cffi::*py4cl-tests* t))
    (py4cl2-cffi:pystop)
    (assert-equalp "5" (py4cl2-cffi:pycall 'str 5))
    (py4cl2-cffi:pyexec "import math")
    (assert-equalp #.(sqrt 5.0d0) (py4cl2-cffi:pycall 'math.sqrt 5))))


(deftest pycall-hash-table-empty (callpython-utility) nil
  (assert-equalp "{}"
      (py4cl2-cffi:pycall "str" (make-hash-table))))

(deftest pycall-hash-table-values (callpython-utility) nil
  (let ((table (make-hash-table)))
    (setf (gethash "test" table) 3
          (gethash "more" table) 42)
    (assert-equalp 42
        (py4cl2-cffi:pycall "lambda d: d[\"more\"]" table))
    (assert-equalp 3
        (py4cl2-cffi:pycall "lambda d: d[\"test\"]" table))
    (assert-equalp 2
        (py4cl2-cffi:pycall "len" table))))

(deftest pymethod-symbol-as-fun-name (callpython-utility) nil
  (assert-equalp 3
      (py4cl2-cffi:pymethod '(1 2 3) '__len__))
  (assert-equalp "hello world"
      (py4cl2-cffi:pymethod "hello {0}" 'format "world")))

(deftest pymethod-string-as-fun-name (callpython-utility) nil
  (assert-equalp 3
      (py4cl2-cffi:pymethod '(1 2 3) "__len__"))
  (assert-equalp "hello world"
      (py4cl2-cffi:pymethod "hello {0}" "format" "world")))

(deftest pymethod-exception-handling (callpython-utility) nil
  (py4cl2-cffi:pyexec "
class Foo:
  def __init__(self):
    return None
  def bar(self):
    raise Exception('This should be raised!')
")
  (assert-condition pyerror
      (py4cl2-cffi:with-pygc (pymethod (pycall "Foo") "bar"))))

(deftest pygenerator (callpython-utility) nil
  (py4cl2-cffi:pyexec "
def foo(gen):
  return list(gen)")
  (assert-equalp #(1 2 3 4)
      (py4cl2-cffi:with-pygc
        (let ((gen (py4cl2-cffi:pygenerator (let ((x 0)) (lambda () (incf x)))
                                            5)))
          (py4cl2-cffi:pycall 'foo gen))))
  (assert-equalp #(#\h #\e #\l #\l #\o)
      (py4cl2-cffi:with-pygc
        (let ((gen (py4cl2-cffi:pygenerator (let ((str (make-string-input-stream "hello")))
                                              (lambda () (read-char str nil)))
                                            nil)))
          (py4cl2-cffi:pycall 'foo gen)))))

(deftest pyslot-value-symbol-as-slot (callpython-utility) nil
  (assert-equalp 5
      (progn
        (py4cl2-cffi:pyexec "a=5")
        (py4cl2-cffi:pyslot-value (py4cl2-cffi:pyvalue "a") 'real)))
  (py4cl2-cffi:pyexec "
class Foo:
  def __init__(self):
    self.a = 5
    self.b = 10
  @property
  def c(self): return self._c
  @c.setter
  def c(self, value): self._c = value
tmp = Foo()")
  (assert-equalp '(5 10 15)
      (let ((s 'b) (temp (py4cl2-cffi:pycall "Foo")))
        (setf (py4cl2-cffi:pyslot-value temp 'c) 15)
        (list (py4cl2-cffi:pyslot-value (py4cl2-cffi:pyvalue "tmp") 'a)
              (py4cl2-cffi:pyslot-value temp s)
              (py4cl2-cffi:pyslot-value temp 'c)))))

(deftest pyslot-value-string-as-slot (callpython-utility) nil
  (assert-equalp 5
      (progn
        (py4cl2-cffi:pyexec "a=5")
        (py4cl2-cffi:pyslot-value (py4cl2-cffi:pyvalue "a") "real")))
  (py4cl2-cffi:pyexec "
class Foo:
  def __init__(self):
    self.a = 5
    self.b = 10
temp = Foo()")
  (assert-equalp 5
      (py4cl2-cffi:pyslot-value (py4cl2-cffi:pyvalue "temp") "a")))

(deftest pythonizers-and-lispifiers (callpython-utility) nil
  (pyexec "import decimal")
  (assert-equalp #.(coerce pi 'long-float)
      (with-lispifiers ((pyobject-wrapper
                         (lambda (o)
                           (if (pycall "isinstance"
                                       o (py4cl2-cffi::pyvalue*
                                          "decimal.Decimal"))
                               (pycall "float" o)
                               o))))
        (with-pythonizers ((real "decimal.Decimal"))
          (pyeval pi)))))

;; ========================= CALLPYTHON-CHAIN ==================================


;; Shorter more convenient slicing
(py4cl2-cffi:defpyfun "slice")

(deftest chain (callpython-chain) nil
  (assert-equalp "Hello world"
      (py4cl2-cffi:chain "hello {0}" (format "world") (capitalize)))
  (assert-equalp "hello world"
      (let ((format-str "hello {0}")
            (argument "world"))
        (py4cl2-cffi:chain* format-str `(format ,argument))))
  (assert-equalp "result: 3"
      (py4cl2-cffi:chain* "result: {0}" `(format ,(+ 1 2))))
  (assert-equalp 3
      (py4cl2-cffi:chain (slice 3) stop))

  ;; Anything not a list or a symbol is put between [] brackets (__getitem__)
  (assert-equalp "o"
      (py4cl2-cffi:chain (aref "hello" 4)))

  ;; [] operator for indexing and slicing (alias for __getitem__)

  (assert-equalp "l"
      (py4cl2-cffi:chain (aref "hello" 3)))
  (assert-equalp 3
      (py4cl2-cffi:chain (aref #2A((1 2) (3 4))
                         1 0)))
  (assert-equalp #(4 5)
      (py4cl2-cffi:chain (aref #2A((1 2 3) (4 5 6))
                               1 (slice 0 2))))

  (let ((dict (py4cl2-cffi:pyeval "{\"hello\":\"world\", \"ping\":\"pong\"}")))
    (assert-equalp "world"
        (py4cl2-cffi:chain* `(aref ,dict "hello")))
    (assert-equalp "pong"
        (py4cl2-cffi:chain* `(aref ,dict "ping")))))

(deftest chain-keywords (callpython-chain) nil
  (py4cl2-cffi:pyexec
   "def test_fn(arg, key=1):
       return arg * key")

  (assert-equalp 3
      (py4cl2-cffi:chain (test_fn 3)))
  (assert-equalp 6
      (py4cl2-cffi:chain (test_fn 3 :key 2)))

  (py4cl2-cffi:pyexec
   "class testclass:
      def run(self, dummy = 1, value = 42):
        return value")

  (assert-equalp 42
      (py4cl2-cffi:chain (testclass) (run)))

  (assert-equalp 31
      (py4cl2-cffi:chain (testclass) (run :value 31))))


(deftest chain-strings (callpython-chain) nil
  (py4cl2-cffi:pyexec
   "class TestClass:
      def doThing(self, dummy = 1, value = 42):
        return value")

  (assert-equalp 42
      (py4cl2-cffi:chain ("TestClass") ("doThing")))

  (assert-equalp 31
      (py4cl2-cffi:chain ("TestClass") ("doThing" :value 31))))

(defclass test-class ()
  ((value :initarg :value)
   (thing :initarg :thing)))

(defmethod python-getattr ((object test-class) slot-name)
  (cond
    ((string= slot-name "value") ; data member
      (slot-value object 'value))
    ((string= slot-name "thing") ; another data member
      (slot-value object 'thing))
    ((string= slot-name "func")         ; method, return a function
     (lambda (arg) (* 2 arg)))
    ((string= slot-name "pyerror")  ; raise an error
      (raw-pyexec "raise Exception(\"raised from PYTHON-GETATTR\")"))
    (t (call-next-method)))) ; Otherwise go to next method
(deftest chain-nested (callpython-chain) nil
  (let ((instance (make-instance 'test-class :value 21)))
    (assert-equal 42 (chain* `((@ ,instance func) (@ ,instance value))))
    (assert-condition pyerror (pyexec instance ".pyerror"))))

(deftest setf-chain (callpython-chain) nil
  ;; Define an empty class which can be modified
  (py4cl2-cffi:pyexec "
class testclass:
  pass")

  (let ((obj (py4cl2-cffi:chain (testclass))))
    (setf (py4cl2-cffi:chain* obj 'data-attrib) 21)
    (assert-equalp 21
        (py4cl2-cffi:chain* obj 'data-attrib))
    (assert-equalp 21
        (pyslot-value obj "data_attrib")))

  (assert-equalp #(4 2 3)
      (with-remote-objects*
        (let ((a (pycall "list" '(1 2 3))))
          (setf (py4cl2-cffi:chain* `(aref ,a 0)) 4)
          a)))

  (assert-equalp #(4 2 3)
      (with-remote-objects*
        (let ((a (pycall "list" '(1 2 3))))
          (setf (py4cl2-cffi:chain* a '(aref 0)) 4)
          a)))

  (assert-equalp (make-array 3
                             :initial-contents
                             (list (alexandria:plist-hash-table '("a" 3) :test #'equal)
                                   5
                                   6))
      (with-remote-objects*
        (let ((a (raw-pyeval "[{}, 5, 6]")))
          (setf (py4cl2-cffi:chain* a '(aref 0) '(aref "a")) 3)
          a))))

;; ========================= CALLPYTHON-REMOTE =================================

(deftest with-remote-objects (callpython-remote) nil
  (assert-equality #'typep
      (py4cl2-cffi:with-remote-objects (py4cl2-cffi:pyeval "1+2"))
      'py4cl2-cffi::pyobject-wrapper)
  (assert-equalp 3
      (py4cl2-cffi:with-remote-objects* (py4cl2-cffi:pyeval "1+2")))
  (assert-equality #'typep
      (py4cl2-cffi:with-remote-objects
        (py4cl2-cffi:with-remote-objects
          (py4cl2-cffi:pyeval "1+2"))
        (py4cl2-cffi:pyeval "1+2"))
      'py4cl2-cffi::pyobject-wrapper))

(deftest callback-in-remote-objects (callpython-remote) nil
  ;; Callbacks send values to lisp in remote-objects environments
  (assert-equalp 6
      (py4cl2-cffi:with-remote-objects*
        (py4cl2-cffi:pycall (lambda (x y) (* x y)) 2 3))))


;; ========================== IMPORT-EXPORT ====================================

(defmacro define-pyfun-with-test (name
                                  (&rest pyexec-forms)
                                     (&rest defpyfun-forms)
                                  &body body)
  `(progn
     (eval-when (:compile-toplevel)
       ,@pyexec-forms)
     ,@defpyfun-forms
     (deftest ,name (import-export) nil
       ,@pyexec-forms
       ,@body)))

(deftest numpy-import-as-np (import-export) nil
  ;; also check whether "all" options as expected
  (skip-on (:ci)
           (progn
             (eval `(defpymodule "numpy" nil :lisp-package "NP" :silent t))
             ;; np. formats are accessible
             (assert-true (pyeval 'np.float32))
             (pystop)
             ;; package is imported as np even after stopping
             (assert-equalp #(5 7 9) (np:add '(1 2 3) '(4 5 6))))))

(deftest numpy-random-import (import-export) nil
  (defpymodule "numpy.random" t :continue-ignoring-errors nil)
  ;; The following tests two bugfixes
  ;; 1. defpysubmodules was previously importing only packages.
  ;; 2. package-import-string was not good for submodules like matplotlib.pyplot
  ;; Note also that some symbols are present in pip numpy not in travis apt numpy.
  ;; py4cl2-tests should not even compile in the case of these bugs.
  (assert-true (numpy.random.mtrand:rand 2)))

(deftest reload-t (import-export) nil
  (flet ((run-with (&key cache)
           (with-standard-io-syntax
             (uiop:with-temporary-file (:stream s :pathname p)
               (write
                `(defpymodule "numpy.linalg" t :lisp-package "LA" :silent t
                              :reload t :cache ,cache
                              :continue-ignoring-errors nil)
                :stream s)
               (force-output s)
               (let ((compiled-file-name (compile-file p :verbose nil)))
                 (if (find-package "LA") (delete-package "LA"))
                 (load compiled-file-name)
                 (assert-equalp
                   #+ccl (read-from-string "(&KEY :A)")
                   #-ccl (read-from-string "(&KEY (LA::A 'NIL))")
                   (trivial-arguments:arglist (read-from-string "LA:DET")))
                 (eval (read-from-string "(DEFUN LA:DET (A) A)"))
                 (assert-equalp (read-from-string "(A)")
                     (trivial-arguments:arglist (read-from-string "LA:DET")))
                 (load compiled-file-name)
                 (assert-equalp
                   #+ccl (read-from-string "(&KEY :A)")
                   #-ccl (read-from-string "(&KEY (LA::A 'NIL))")
                   (trivial-arguments:arglist (read-from-string "LA:DET")))
                 (if (find-package "LA") (delete-package "LA")))))))
    (run-with :cache t)
    (run-with :cache nil)))

(deftest reload-nil (import-export) nil
  (flet ((run-with (&key cache)
           (with-standard-io-syntax
             (uiop:with-temporary-file (:stream s :pathname p)
               (write
                `(defpymodule "numpy.linalg" t :lisp-package "LA" :silent t
                              :reload nil :continue-ignoring-errors nil)
                :stream s)
               (force-output s)
               (let ((compiled-file-name (compile-file p :verbose nil)))
                 (if (find-package "LA") (delete-package "LA"))
                 (load compiled-file-name)
                 (assert-equalp
                   #+ecl :unknown
                   #+ccl (read-from-string "(&KEY :A)")
                   #-(or ecl ccl) (read-from-string "(&KEY (LA::A 'NIL))")
                   (trivial-arguments:arglist (read-from-string "LA:DET")))
                 (eval (read-from-string "(DEFUN LA:DET (A) A)"))
                 (assert-equalp (read-from-string "(A)")
                     (trivial-arguments:arglist (read-from-string "LA:DET")))
                 (load compiled-file-name)
                 (assert-equalp (read-from-string "(A)")
                     (trivial-arguments:arglist (read-from-string "LA:DET")))
                 (if (find-package "LA") (delete-package "LA")))))))
    (run-with :cache t)
    (run-with :cache nil)))

;; more extensive tests for defpyfun and defpymodule are required
(define-pyfun-with-test defpyfun
    ()
    ((py4cl2-cffi:defpyfun "sum" "" :lisp-fun-name "PYSUM")
     (py4cl2-cffi:defpyfun "Fraction" "fractions")
     (py4cl2-cffi:defpyfun "gcd" "math" :as "g"))
  (py4cl2-cffi:pystop)                       ; taking "safety" into account
  (assert-equalp 1/2 (fraction :numerator 1 :denominator 2))
  (py4cl2-cffi:pystop)                       ; taking "safety" into account
  (assert-equalp 1 (g 5 6))
  (assert-equalp 1 (py4cl2-cffi:pycall "g" 5 6)) ; not safe!
  (py4cl2-cffi:pystop)                       ; taking "safety" into account
  (assert-equalp 6 (pysum '(2 1 3))))

(define-pyfun-with-test defpyfun-null
    ((pyexec "def allNulls(a=[], b=(), c=False, d=None):
  assert type(a)==list
  assert type(b)==tuple
  assert type(c)==bool
  assert type(d)==type(None)
  return True")
     (pyexec "def allNullsReturnAll(a=[], b=(), c=False, d=None):
  return (a,b,c,d)"))
    ((defpyfun "allNulls")
     (defpyfun "allNullsReturnAll"))
  (pycall "allNulls")
  (assert-true (all-nulls))
  (assert-true (every (lambda (x y)
                        (or (equalp x y)
                            (py4cl2-cffi::pyobject-wrapper-eq x y)))
                      (list #()
                            py4cl2-cffi:+py-empty-tuple+
                            nil
                            py4cl2-cffi:+py-none+)
                      (all-nulls-return-all)))
  (assert-true (equalp '(5 6 7 8) (all-nulls-return-all :a 5 :b 6 :c 7 :d 8)))
  (assert-true (equalp '("hello" "world" "good" "bye")
                       (all-nulls-return-all :a "hello" :b "world"
                                             :c "good" :d "bye"))))

(define-pyfun-with-test defpyfun-args
    ((pyexec "def noArgFunc(): return True")
     (when (< 7 (nth 1 (pyversion-info)))
       (pyexec "def posArgFunc(a, b, c=5, /): return True"))
     (pyexec "def restArgs(a, b, *args): return True")
     (pyexec "def kwRestArgs(a, b, **kwargs): return True"))
    ((defpyfun "noArgFunc")
     #.(when (< 7 (nth 1 (pyversion-info)))
         `(defpyfun "posArgFunc"))
     (defpyfun "restArgs")
     (defpyfun "kwRestArgs"))
  (assert-true (eq (if (member :ecl *features*)
                       :unknown nil)
                   (trivial-arguments:arglist #'no-arg-func)))
  (when (< 7 (nth 1 (pyversion-info)))
    (assert-true
        (equalp (trivial-arguments:arglist (lambda (a b &optional (c '5))))
                (trivial-arguments:arglist #'pos-arg-func))))
  (assert-true
      (equalp (trivial-arguments:arglist (lambda (&rest py4cl2-cffi::args) ()))
              (trivial-arguments:arglist #'rest-args)))
  (assert-true
      ;; TODO: generalize this
      (or (equalp (trivial-arguments:arglist
                   (lambda (&rest kwargs &key (b 'nil) (a 'nil) &allow-other-keys) ()))
                  (trivial-arguments:arglist #'kw-rest-args))
          (equalp (trivial-arguments:arglist
                   (lambda (&rest kwargs &key (a 'nil) (b 'nil) &allow-other-keys) ()))
                  (trivial-arguments:arglist #'kw-rest-args)))))

(deftest defpymodule-math (import-export) nil
  (assert-equalp (cos 45d0) (math:cos 45)))

(define-pyfun-with-test defpyfun-names
    ((py4cl2-cffi:pyexec "def foo(A, b): return True")
     (py4cl2-cffi:pyexec "def bar(a=1, b=2, **kwargs): return kwargs")
     (py4cl2-cffi:pyexec "def nilAndT(nil, t): return (nil, t)"))
    ((py4cl2-cffi:defpyfun "foo")
     (py4cl2-cffi:defpyfun "bar")
     (py4cl2-cffi:defpyfun "nilAndT"))
  (assert-true (foo :A 4 :b 3))
  (assert-equal '() (alexandria:hash-table-alist (bar)))
  (assert-equal '() (alexandria:hash-table-alist (bar :a 3)))
  (assert-equal '(("c" . 3)) (alexandria:hash-table-alist (bar :c 3)))
  (assert-equalp '("hello" #(5)) (nil-and-t :.nil "hello" :.t #(5))))

;; Call python during callback
(deftest python-during-callback (callpython-utility) nil
  (py4cl2-cffi:export-function
   (lambda () (py4cl2-cffi:pyeval "42"))
   "test")
  (assert-equalp "42"
      (py4cl2-cffi:pyeval "test()")))

;; Simple callback function
(defun test-func ()
  42)

(deftest callback-no-args (import-export) nil
  (py4cl2-cffi:export-function #'test-func "test")
  (assert-equalp 42
      (py4cl2-cffi:pyeval "test()")))

;; Even simpler function returning NIL
(defun nil-func ()
  nil)

(deftest callback-no-args-return-nil (import-export) nil
  (py4cl2-cffi:export-function #'nil-func "test_nil")
  (assert-equalp nil
      (py4cl2-cffi:pyeval "test_nil()")))

;; Python can't eval write-to-string's output "3.141592653589793d0"
(deftest callback-return-double (import-export) nil
  (py4cl2-cffi:export-function (lambda () pi) "test")
  (assert-eql #.(coerce pi 'double-float)
      (py4cl2-cffi:pyeval "test()")))

(deftest callback-one-arg (import-export) nil
  (py4cl2-cffi:export-function (lambda (x) (* 2 x)) "double")
  (assert-equalp 4
      (py4cl2-cffi:pyeval "double(2)")))

(deftest callback-two-args (import-export) nil
  (py4cl2-cffi:export-function (lambda (x y) (/ x y)) "div")
  (assert-equalp 3
      (py4cl2-cffi:pyeval "div(6, 2)")))

(deftest callback-many-args (import-export) nil
  (py4cl2-cffi:export-function #'+ "add")
  (assert-equalp 15
      (py4cl2-cffi:pyeval "add(2, 4, 6, 3)")))

(deftest callback-seq-arg (import-export) nil
  (py4cl2-cffi:export-function #'reverse "reverse")
  (assert-equalp '(3 1 2 4)
      (py4cl2-cffi:pyeval "reverse((4,2,1,3))"))
  (assert-equalp #(3 1 2 4)
      (py4cl2-cffi:pyeval "reverse([4,2,1,3])")))

(deftest callback-keyword-arg (import-export) nil
  (py4cl2-cffi:export-function (lambda (&key setting) setting) "test")
  (assert-equalp nil
      (py4cl2-cffi:pyeval "test()"))
  (assert-equalp 42
      (py4cl2-cffi:pyeval "test(setting=42)")))


;; Call python during callback
(deftest python-during-callback (import-export) nil
  (py4cl2-cffi:export-function
   (lambda () (py4cl2-cffi:pyeval "42"))
   "test")
  (assert-equalp "42"
      (py4cl2-cffi:pyeval "test()")))


;; ============================= OBJECTS =======================================


(deftest pyobject-wrappers (objects) nil
  ;; Define a simple python class containing a value
  (py4cl2-cffi:pystop)
  (py4cl2-cffi:pyexec
   "class Test:
  def __init__(self, value=None):
    self.value = value
  def __repr__(self):
    return \"Test(value={0})\".format(self.value)

a = Test()
a.value = 42")

  ;; Check that the variable has been defined
  (assert-true (= 42
                  (py4cl2-cffi:pyeval "a.value")))

  ;; Evaluate and return a python object
  (let ((var (py4cl2-cffi:pyeval "a")))
    (assert-equalp 'py4cl2-cffi:pyobject-wrapper
        (type-of var))

    ;; Can pass to eval to use dot accessor
    (assert-equalp 42
        (py4cl2-cffi:pyeval var ".value"))

    ;; Can pass as argument to function
    (assert-equal 84
        (py4cl2-cffi:pycall "lambda x : x.value * 2" var))))

;;; Passing unknown lisp objects to python

(defstruct test-struct
  x y)

(deftest lisp-structs (objects) nil
  ;; Create a struct and pass to Python
  (let ((result (py4cl2-cffi:pycall
                 "lambda x: x"
                 (make-test-struct :x 1 :y 2))))

    ;; Check we got back the structure
    (assert-true (typep result 'test-struct))
    (assert-equalp 1
                   (test-struct-x result))
    (assert-equalp 2
                   (test-struct-y result))))

(deftest lisp-class-slots (objects) nil
  (let ((object (make-instance 'test-class :thing 23 :value 42)))
    ;; Slot access
    (assert-equalp 23
                   (py4cl2-cffi:pycall "lambda x : x.thing" object))
    (assert-equalp 42
                   (py4cl2-cffi:chain* object 'value))

    ;; Function (method) call
    (assert-equalp 42
                   (py4cl2-cffi:chain* object `(func 21))))

  ;; The handler should work for other objects of the same class (class-of)
  (let ((object2 (make-instance 'test-class :thing "hello" :value 314)))
    (assert-equalp "hello"
                   (py4cl2-cffi:chain* object2 'thing))))


;; Class inheriting from test-class
(defclass child-class (test-class)
  ((other :initarg :other)))

;; Define method which passes to the next method if slot not recognised
(defmethod py4cl2-cffi:python-getattr ((object child-class) slot-name)
  (cond
    ((string= slot-name "other")
     (slot-value object 'other))
    (t (call-next-method))))

(deftest lisp-class-inherit (objects) nil
  (let ((object (make-instance 'child-class :thing 23 :value 42 :other 3)))
    (assert-equalp 23
                   (py4cl2-cffi:pycall "lambda x : x.thing" object))
    (assert-equalp 42
                   (py4cl2-cffi:chain* object 'value))
    (assert-equalp 3
                   (py4cl2-cffi:chain* object 'other))))

(defmethod py4cl2-cffi:python-setattr ((object test-class) slot-name set-to-value)
  (cond
    ((string= slot-name "value")
     (setf (slot-value object 'value) set-to-value))
    ((string= slot-name "thing")
     (setf (slot-value object 'thing) set-to-value))
    ((string= slot-name "pyerror")
     (raw-pyexec "raise Exception(\"PYTHON-SETATTR raised a python error!\")"))
    (t (call-next-method))))

(deftest lisp-class-set-slots (objects) nil
  (let ((object (make-instance 'test-class :thing 23 :value 42)))
    ;; Set value
    (py4cl2-cffi:pyexec object ".thing = 3")
    (assert-equalp 3 (slot-value object 'thing))
    (assert-equalp 3 (py4cl2-cffi:pyeval object ".thing"))
    ;; Set again
    (setf (py4cl2-cffi:chain* object 'thing) 72)
    (assert-equalp 72 (slot-value object 'thing))
    (assert-equalp 72 (py4cl2-cffi:chain* object 'thing))

    ;; Check for error
    (assert-condition py4cl2-cffi:pyerror
      (py4cl2-cffi:pyexec object ".pyerror = 42"))))

;; ========================= NUMPY-UFUNC =======================================

(py4cl2-cffi:defpyfun "abs" "numpy" :lisp-fun-name "NUMABS")
(deftest numpy-ufunc-abs (numpy-ufunc) nil
  (assert-equalp #(1 2 3) (numabs #(-1 2 -3))))
(py4cl2-cffi:defpyfun "add" "numpy" :lisp-fun-name "NUMADD")
(deftest numpy-ufunc-add (numpy-ufunc) nil
  (assert-equalp #(4 5 6) (numadd #(1 2 3) 3)))

;; =========================== ARRAY-DATA ======================================

(deftest row-major-array-data (array-data) (:typed-arrays)
  (import-module "numpy")
  (let ((rand (aops:rand* 'single-float '(2 3 4))))
    (assert-equalp rand (pycall "numpy.asarray" rand :order "C")))
  (let ((rand (aops:rand '(2 3 4))))
    (assert-equalp rand (pycall "numpy.asarray" rand :order "C" :dtype "object"))))

(deftest column-major-array-data (array-data) (:typed-arrays)
  (import-module "numpy")
  (let ((rand (aops:rand* 'single-float '(2 3 4))))
    (assert-equalp rand (pycall "numpy.asarray" rand :order "F")))
  (let ((rand (aops:rand '(2 3 4))))
    (assert-equalp rand (pycall "numpy.asarray" rand :order "F" :dtype "object"))))

(defun float-close-p (f1 f2)
  (or (= f1 f2)
      (< (abs (- f1 f2))
         (etypecase f1
           (single-float 1f-7)
           (double-float 1d-15)))))

(deftest non-contiguous-array-data (array-data) (:typed-arrays)
  (import-module "numpy")
  (let* ((rand (aops:rand* 'single-float '(2 3 4)))
         (pyrand (pyeval rand "[:, :, ::-2]")))
    (assert-equality 'float-close-p
        (aref rand 0 0 3)
        (aref pyrand 0 0 0))
    (assert-equality 'float-close-p
        (aref rand 0 0 1)
        (aref pyrand 0 0 1))
    (assert-equality 'float-close-p
        (aref rand 0 1 3)
        (aref pyrand 0 1 0))
    (assert-equality 'float-close-p
        (aref rand 0 1 1)
        (aref pyrand 0 1 1)))
  (let* ((rand (aops:rand '(2 3 4)))
         (pyrand (pyeval rand "[:, :, ::-2]")))
    (assert-equality 'float-close-p
        (aref rand 0 0 3)
        (aref pyrand 0 0 0))
    (assert-equality 'float-close-p
        (aref rand 0 0 1)
        (aref pyrand 0 0 1))
    (assert-equality 'float-close-p
        (aref rand 0 1 3)
        (aref pyrand 0 1 0))
    (assert-equality 'float-close-p
        (aref rand 0 1 1)
        (aref pyrand 0 1 1))))

;; ==================== ELEMENT-TYPE ==============================
(deftest float-type (element-type) nil
  (assert-eql 1.0d5 (pyeval 1.0d5))
  (assert-eql 1.0e5 (pyeval 1.0e5))
  (assert-eql 1.0 (pyeval 1.0)))

(deftest simple-vector (element-type) nil
  (assert-equalp #("hello" "world")
      (pyeval #("hello" "world"))))
