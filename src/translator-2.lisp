(defpackage bi-lang.translator-2
  (:use :cl
        :optima
        :optima.extra
        :bi-lang.print)
  (:import-from :preil
                :<-
                :do-solve)
  (:export :translate-2))
(in-package :bi-lang.translator-2)

(defun identifier-inverse (identifier)
  (let ((name (symbol-name identifier)))
    (if (string= name "!" :start1 (- (length name) 1))
        (intern (subseq name 0 (- (length name) 1)) (symbol-package identifier))
        (intern (format nil "~a!" name) (symbol-package identifier)))))

(defun expression-subgoals (expression variable)
  (ematch
      expression
    ((list (guard it (string= it 'cons)) expression-1 expression-2)
     (let ((g1 (gensym "?CAR"))
           (g2 (gensym "?CDR")))
       (list* `(= ,variable (,g1 . ,g2))
              (append (expression-subgoals expression-1 g1)
                      (expression-subgoals expression-2 g2)))))
    ((list (guard it (string= it 'logic-variable)))
     (list))
    ((list (guard it (string= it '+)) left right)
     (let ((g1 (gensym "?V1"))
           (g2 (gensym "?V2")))
       (list* `(:+= ,g1 ,g2 ,variable)
              (append (expression-subgoals left g1)
                      (expression-subgoals right g2)))))
    ((list (guard it (string= it '-)) left right)
     (let ((g1 (gensym "?V1"))
           (g2 (gensym "?V2")))
       (list* `(:-= ,g1 ,g2 ,variable)
              (append (expression-subgoals left g1)
                      (expression-subgoals right g2)))))
    ((list (guard it (string= it '|:|)) left right)
     (let ((g1 (gensym "?V1"))
           (g2 (gensym "?V2")))
       (list* `(= ,variable (,g1 . ,g2))
              (append (expression-subgoals left g1)
                      (expression-subgoals right g2)))))
    ((list* (guard it (string= it 'list)) expressions)
     (let ((gs (loop for x in expressions collect (gensym "?"))))
       (list* `(= ,variable ,gs)
              (loop
                for expression in expressions
                for g in gs
                append (expression-subgoals expression g)))))
    ((list 'quote identifier)
     (list `(= ,variable ,identifier)))
    ((list* name args)
     (when (string= name "map") ; FIXME
       (setf name :map))
     (let ((gs (loop for x in args collect (gensym "?"))))
       (list* `(,name ,gs (,variable))
              (loop
                for arg in args
                for g in gs
                append (expression-subgoals arg g)))))
    (a
     (if (and (symbolp a) a (string/= a "?" :end1 1)) ; FIXME
         (list `(= ,variable ,(intern (format nil "?~a" a))))
         (list `(= ,variable ,a))))))

(defun unification (unification)
  (let ((g1 (gensym "?V1"))
        (g2 (gensym "?V2")))
    (values `(= ,g1 ,g2)
            (list* 'and
                   (reverse
                    (append (expression-subgoals (second unification) g1)
                            (expression-subgoals (third unification) g2)))))))

(defun statement (statement)
  (ematch
      statement
    ((list* (guard it (string= it 'progn)) statements)
     `(and ,@(loop
               for statement in statements
               collect (statement statement))))
    ((list (guard it (string= it 'if)) cond then else)
     (multiple-value-bind (cond lemma) (unification cond)
       `(or
         (and ,lemma ,cond
              ,(statement then))
         (and (:and* ,lemma (not ,cond))
              ,(statement else)))))
    ((list* (guard it (string= it 'match)) expression cases)
     (let ((g (gensym "?MATCH-VALUE")))
       `(and ,@(reverse (expression-subgoals expression g))
             (or ,@(loop
                     for (right statement) in cases
                     collect (multiple-value-bind (cond lemma) (unification `(= ,g ,right))
                               `(and (:and* ,lemma ,cond)
                                     ,(statement statement))))))))
    ((list* (guard it (string= it 'return)) expressions)
     (let ((gs (loop for x in expressions collect (gensym "?"))))
       `(and ,@(loop
                 for expression in expressions
                 for g in gs
                 append (reverse (expression-subgoals expression g)))
             (= ?r ,gs))))
    ((list (guard it (string= it '=)) left right)
     (multiple-value-bind (cond lemma) (unification `(= ,left ,right))
       `(and ,lemma ,cond)))))

(defun reduce-and (statement)
  (unless (listp statement)
    (return-from reduce-and statement))
  (case (first statement)
    (and
     (setf statement (cons (car statement) (mapcar #'reduce-and (cdr statement))))
     (when (= (length statement) 2)
       (return-from reduce-and (second statement)))
     (list* 'and
            (loop
              for s in (cdr statement)
              if (and (listp s) (eq (first s) 'and))
              append (cdr s)
              else
              collect s)))
    ((or not)
     (cons (car statement) (mapcar #'reduce-and (cdr statement))))
    (t
     statement)))

(defun inverse (form)
  (cond
    ((eq (first form) 'and)
     `(and ,@(reverse (mapcar #'inverse (cdr form)))))
    ((eq (first form) ':and*)
     `(:and* ,@(mapcar #'inverse (cdr form))))
    ((eq (first form) 'or)
     `(or ,@(mapcar #'inverse (cdr form))))
    ((eq (first form) 'not)
     `(not ,(inverse (second form))))
    ((member (first form) '(:map = :+= :-=))
     form)
    (t
     (list (identifier-inverse (first form)) (third form) (second form)))))

(defun top-level-statement (top-level-statement)
  (ematch
      top-level-statement
   ((list (guard it (string= it 'defun))
          name args statement)
    (let ((gs (loop for arg in args collect (gensym "?ARG"))))
      `((<- (,name ?a ?r)
            (= ?a ,gs)
            ,@(loop
                for arg in args
                for g in gs
                append (reverse (expression-subgoals arg g)))
            ,(reduce-and (statement statement)))
        (<- (,(identifier-inverse name) ?r ?a)
            (= ?a ,gs)
            ,(inverse (reduce-and (statement statement)))
            ,@(loop
                for arg in args
                for g in gs
                append (mapcar #'inverse (expression-subgoals arg g)))))))
   ((list (guard it (string= it 'instraction))
          "printAll" expression)
    `((preil:do-solve ((?x) (printb ?x))
        ,@(mapcar (lambda (x) `',x) (reverse (expression-subgoals expression '?x))))))
   ((list (guard it (string= it 'instraction))
          "print1" expression)
    `((block block
        (preil:do-solve
            ((?x)
             (printb ?x) (return-from block))
         ,@(mapcar (lambda (x) `',x) (reverse (expression-subgoals expression '?x))))
        (format t "fail~%"))))))

(defun top-level-statements (top-level-statements)
  (loop
    for top-level-statement in top-level-statements
    append (top-level-statement top-level-statement)))

(defun translate-2 (ast)
  (top-level-statements ast))
