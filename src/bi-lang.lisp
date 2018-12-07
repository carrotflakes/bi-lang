(defpackage bi-lang
  (:use :cl
        :bi-lang.parser
        :bi-lang.translator
        :bi-lang.translator-2
        :preil)
  (:import-from :snaky
                :defparser)
  (:export :parse
           :transpile
           :exec))
(in-package :bi-lang)


(defparser parse bi-lang)


(defun transpile (source)
  `(let ((preil:*world* (make-world)))
     (<- (= ?x ?x))
     (<- (or ?x . ?)
         ?x)
     (<- (or ? . ?xs)
         (or . ?xs))
     (<- (and))
     (<- (and ?x . ?xs)
         ?x
         (and . ?xs))
     (<- (:and* . ?xs)
         (and . ?xs))
     (%- (not ?term)
         ((?term)
          (unless (solvep ?term)
            (satisfy))))
     (%- (:+= ?x ?y ?z)
         ((?x ?y)
          (when (and (integerp ?x) (integerp ?y))
            (satisfy :?z (+ ?x ?y))))
         ((?x ?z)
          (when (and (integerp ?x) (integerp ?z))
            (satisfy :?y (- ?z ?x))))
         ((?y ?z)
          (when (and (integerp ?y) (integerp ?z))
            (satisfy :?x (- ?z ?y)))))
     (%- (:-= ?x ?y ?z)
         ((?x ?y)
          (when (and (integerp ?x) (integerp ?y))
            (satisfy :?z (- ?x ?y))))
         ((?x ?z)
          (when (and (integerp ?x) (integerp ?z))
            (satisfy :?y (- ?x ?z))))
         ((?y ?z)
          (when (and (integerp ?y) (integerp ?z))
            (satisfy :?x (+ ?z ?y)))))
     (<- (:elt ((?x . ?) 0) (?x)))
     (<- (:elt ((? . ?xs) ?n) (?x))
         (:+= ?n-1 1 ?n)
         (:elt (?xs ?n-1) (?x)))
     (<- (:elt! (?x) ((?x . ?) 0)))
     (<- (:elt! (?x) ((? . ?xs) ?n))
         (:elt! (?x) (?xs ?n-1))
         (:+= ?n-1 1 ?n))
     (<- (:map (?f ()) (())))
     (<- (:map (?f (?x . ?xs)) ((?y . ?ys)))
         (?f (?x) (?y))
         (:map (?f ?xs) (?ys)))
     ,@(translate-2 (translate-1 (parse source)))
     nil))

(defun exec (source)
  (eval (transpile source)))

(initialize-memory 10000)
