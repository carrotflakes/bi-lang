(defpackage bi-lang.parser
  (:use :cl
        :snaky)
  (:export :bi-lang))
(in-package :bi-lang.parser)

(defrule bi-lang (and ws top-level-statements ws))

(defrule top-level-statements
    (@ (? (and top-level-statement (* (and ws top-level-statement))))))
(defrule statements-separated-by-semicolon
    (@ (? (and statement (* (and ws ";" ws statement)) (? (and ws ";"))))))

(defrule top-level-statement
    (or function-definition
        instraction-statement))

(defrule statement
    (or function-definition
        if-statement
        match-statement
        return-statement
        block-statement
        unification
        expression-statement))

(defrule quoted-identifier
    (@ (and (ret 'identifier)
            "'" ws
            identifier)))


(defrule instraction-statement
    (@ (and (ret 'instraction-statement)
            (ret :instraction) (cap (or "printAll" "print1"))
            ws
            (ret :expression) expression)))

(defrule function-definition
    (@ (and (ret 'function-definition)
            "fn"
            ws
            (ret :identifier) identifier
            ws
            (ret :parameters) expressions
            ws
            (ret :body) statement)))

(defrule identifier (cap (and (! reserved) (cc "a-zA-Z") (* (cc "a-zA-Z0-9_")) (? "!"))))

(defrule expression-statement
    (@ (and (ret 'expression-statement)
            expression)))

(defrule expressions
    (@ (or (and "("
                ws
                (? (and expression (* (and ws "," ws expression))))
                ws
                ")")
           expression)))

(defrule if-statement
    (@ (and (ret 'if-statement)
            "if"
            ws
            (ret :condition) unification
            ws
            (ret :then) statement
            (? (and ws
                    "else"
                    ws
                    (ret :else) statement)))))

(defrule match-statement
    (@ (and (ret 'match-statement)
            "match"
            ws
            (ret :expression) expression
            ws "{" ws
            (ret :cases) (@ (? (and match-statement-case
                                    (* (and ws "," ws match-statement-case)))))
            ws "}")))

(defrule match-statement-case
    (@ (and (ret 'match-statement-case)
            (ret :expression) expression
            ws "=>" ws
            (ret :statement) statement)))

(defrule return-statement
    (@ (and (ret 'return)
            "return"
            ws
            (ret :expressions) expressions)))

(defrule block-statement
    (@ (and (ret 'block-statement)
            block*)))

(defrule expression expression-2)

(defmacro def-infixr (operators expression-self expression-next)
  (let ((list (mapcar (lambda (operator)
                        `(and (ret 'infix)
                              (ret :operator) (ret ',operator)
                              (ret :left) ,expression-next
                              ws
                              ,(symbol-name operator)
                              ws
                              (ret :right) ,expression-self))
                      operators)))
  `(defrule ,expression-self
       (or (@ (or ,@list))
           ,expression-next))))

(defmacro def-infixl (operators expression-self expression-next)
  (let ((list (mapcar (lambda (operator)
                        `(and (ret 'infix)
                              (ret :operator) (ret ',operator)
                              (ret :left) ,expression-self
                              ws
                              ,(symbol-name operator)
                              ws
                              (ret :right) ,expression-next))
                      operators)))
  `(defrule ,expression-self
       (or (@ (or ,@list))
           ,expression-next))))

(def-infixr (|:|) expression-2 expression-3)
(def-infixl (+ -) expression-3 expression-4)

(defrule expression-4
    (or function-call
        element
        variable
        quoted-identifier
        logic-variable
        literal
        list-expression
        (and "(" ws
             expression
             ws ")")))

(defrule list-expression
    (@ (and (ret 'list)
            "["
            ws
            (? (and expression
                    (* (and ws "," ws
                            expression))))
             "]")))

(defrule literal
    (or number))

(defrule number
    (@ (and (ret 'value)
            (mod (cap (and (? "-") (+ (cc "0-9"))))
                 #'read-from-string))))

(defrule element
    (@ (and (ret 'element)
            (ret :list) expression
            ws "[" ws
            (ret :number) expression
            ws "]")))

(defrule function-call
    (@ (and (ret 'call)
            (ret :operator) identifier
            ws
            (ret :arguments) (and (& "(")
                                  expressions))))

(defrule variable
    (@ (and (ret 'variable)
            identifier)))

(defrule logic-variable
    (@ (and (ret 'logic-variable)
            "?")))

(defrule block* (and "{" ws statements-separated-by-semicolon ws "}"))

(defrule unification
    (@ (and (ret 'unification)
            (ret :left) expression
            ws
            "="
            ws
            (ret :right) expression)))


(defrule ws (* (or (cc #.(format nil " ~a~a~a" #\cr #\lf #\tab)) comment)))

(defrule comment
    (or (and "//" (* (cc #.(format nil "^~a~a" #\cr #\lf))))
        (and "/*" (* (and (! "*/") (any))) "*/")))

(defrule reserved
    (or "fn"
        "if"
        "else"
        "match"
        "return"))
