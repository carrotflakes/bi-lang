(defpackage bi-lang.print
  (:use :cl)
  (:export :printb))
(in-package :bi-lang.print)

(defun printb (form)
  (labels
      ((f (form)
       (cond
         ((preil::svar-p form) "?")
         ((null form)
          "[]")
         ((atom form)
          (format nil "~a" form))
         ((listp form)
          (format nil "[~{~a~^, ~}]" (mapcar #'f form))))))
    (format t "~a~%" (f form))))
