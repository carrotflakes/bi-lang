(defpackage bi-lang
  (:use :cl
        :bi-lang.parser
        :bi-lang.translator
        :bi-lang.translator-2
        :preil)
  (:import-from :snaky
                :defparser)
  (:export :parse
           :exec
           :transpile))
(in-package :bi-lang)


(defparser parse bi-lang)


(defun transpile (source)
  `(let ((preil:*world* (make-world :based (prelude:get-world))))
     (<- (elt ((?x . ?) 0) (?x)))
     (<- (elt ((? . ?xs) ?n) (?x))
         (inc ?n-1 ?n)
         (elt (?xs ?n-1) (?x)))
     (<- (:map (?f ()) (())))
     (<- (:map (?f (?x . ?xs)) ((?y . ?ys)))
         (?f (?x) (?y))
         (:map (?f ?xs) (?ys)))
     ,@(translate-2 (translate-1 (parse source)))
     nil))

(defun exec (source)
  (eval (transpile source)))

(initialize-memory 10000)
