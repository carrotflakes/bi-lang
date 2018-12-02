(defpackage bi-lang.translator
  (:use :cl
        :optima
        :optima.extra)
  (:export :translate-1))
(in-package :bi-lang.translator)

(defun expression (expression)
  (ematch
      expression
    ((list (guard it (string= it 'variable)) name)
     (intern name))
    ((list (guard it (string= it 'identifier)) identifier)
     `',(intern identifier))
    ((list* (guard it (string= it 'infix))
            (plist :operator operator
                   :left left
                   :right right))
     `(,operator ,(expression left) ,(expression right))) ; |:| + -
    ((list* (guard it (string= it 'call))
            (plist :operator operator
                   :arguments arguments))
     `(,(intern operator) ,@(mapcar #'expression arguments)))
    ((list* (guard it (string= it 'element))
            (plist :list expression
                   :number number))
     `(elt ,(expression expression) ,(expression number)))
    ((list (guard it (string= it 'value)) value)
     value)
    ((list* (guard it (string= it 'list)) expressions)
     `(list ,@(mapcar #'expression expressions)))
    ((list (guard it (string= it 'logic-variable)))
     `(logic-variable))))

(defun statement (statement)
  (ematch
      statement
    ((list* (guard it (string= it 'if-statement))
            (plist :condition condition
                   :then then
                   :else else))
     `(if ,(statement condition)
          ,(statement then)
          ,(statement else)))
    ((list* (guard it (string= it 'match-statement))
            (plist :expression expression
                   :cases cases))
     `(match ,(expression expression)
        ,@(loop
            for case in cases
            for e = (third case)
            for b = (fifth case)
            collect `(,(expression e) ,(statement b)))))
    ((list* (guard it (string= it 'match-statement))
            (plist :cases cases))) ; TODO
    ((list* (guard it (string= it 'return))
            (plist :expressions expressions))
     `(return ,@(mapcar #'expression expressions)))
    ((list (guard it (string= it 'block-statement))
           statements)
     `(progn ,@(mapcar #'statement statements)))
    ((list* (guard it (string= it 'unification))
            (plist :left left
                   :right right))
     `(= ,(expression left) ,(expression right)))
    ((list (guard it (string= it 'expression-statement)) expression)
     `(return ,(expression expression)))))

(defun top-level-statement (top-level-statement)
  (ematch
      top-level-statement
   ((list* (guard it (string= it 'function-definition))
           (plist :identifier identifier
                  :parameters parameters
                  :body body))
    `(defun ,(intern identifier)
         ,(mapcar #'expression parameters)
       ,(statement body)))
   ((list* (guard it (string= it 'instraction-statement))
           (plist :instraction instraction
                  :expression expression))
    `(instraction ,instraction ,(expression expression)))))

(defun top-level-statements (top-level-statements)
  (mapcar #'top-level-statement top-level-statements))

(defun translate-1 (ast)
  (top-level-statements ast))
