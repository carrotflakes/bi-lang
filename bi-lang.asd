#|
  This file is a part of bi-lang project.
  Copyright (c) 2018 carrotflakes (carrotflakes@gmail.com)
|#

#|
  Author: carrotflakes (carrotflakes@gmail.com)
|#

(defsystem "bi-lang"
  :version "0.1.0"
  :author "carrotflakes"
  :license "LLGPL"
  :depends-on (:preil
               :preil-prelude
               :snaky
               :optima)
  :components ((:module "src"
                :components
                ((:file "bi-lang" :depends-on ("parser" "translator" "translator-2"))
                 (:file "translator-2")
                 (:file "translator")
                 (:file "parser"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "bi-lang-test"))))
