(defpackage bi-lang-test
  (:use :cl
        :bi-lang
        :prove))
(in-package :bi-lang-test)

;; NOTE: To run this test file, execute `(asdf:test-system :bi-lang)' in your Lisp.

(plan nil)

(princ (parse "fn hoge (a) return a"))(terpri)
(princ (parse "fn hoge (a) return a(1)"))(terpri)
(princ (parse "
fn hoge (a) {
  if a = 0 {
    return []
  } else {
    return ? : hoge(a-1)
  }
}"))(terpri)
(princ (exec "
fn hoge (a) {
  if a = 0 {
    return [] // yo
  } else { /*
hoge */
    return ? : hoge(a-1)
  }
}"))(terpri)
#|
((fn :identifier "hoge" :parameters ((variable "a"))
:body (block-statement
   ((if-statement :condition
(<- (hoge (?a) (?r))
    (or (and (groundp (?a))
             (or (and (= ?a 0)
                      (= ?r ()))
                 (and (not (= ?a 0))
                      (eval ?1 (- ?a 1))
                      (hoge (?1) (?2))
                      (= ?r (?a . ?2)))))
        (and (groundp (?r))
             (= ?r

(<- (makeList ?n ?r0)
    (or (and (groundp ?n)
             (or (and (= ?n 0)
                      (= ?r0 ()))
                 (and (not (= ?n 0))
                      
        (and (groundp ?r0)

|#

(finalize)
