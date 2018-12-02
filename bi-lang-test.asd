#|
  This file is a part of bi-lang project.
  Copyright (c) 2018 carrotflakes (carrotflakes@gmail.com)
|#

(defsystem "bi-lang-test"
  :defsystem-depends-on ("prove-asdf")
  :author "carrotflakes"
  :license "LLGPL"
  :depends-on ("bi-lang"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "bi-lang"))))
  :description "Test system for bi-lang"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
