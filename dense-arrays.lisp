(in-package :py4cl2-cffi-tests)

(defsuite array-type (py4cl))

;; =========================== ARRAY-TYPE ======================================

;; TODO: Test whether dense-array loads on ECL and ABCL
#-(or :ecl :abcl)
(deftest dense-array (array-type) nil
  ;; Doesn't really matter if they are numcl-arrays or not
  (let ((dense-arrays:*dense-array-class* 'dense-arrays:standard-dense-array))
    (with-lispifiers (((and cl:array (not string)) #'dense-arrays-plus-lite:asarray))
      (destructuring-bind (a b)
          (pyeval "(" #(1 2 3) ", " #2A((1 2 3) (4 5 6)) ")")
        (assert-true (typep a 'dense-arrays:standard-dense-array))
        (assert-true (typep b 'dense-arrays:standard-dense-array))))))
