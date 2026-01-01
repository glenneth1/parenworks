(in-package #:cl-user)

(asdf:defsystem #:parenworks
  :version "1.0.0"
  :author "ParenWorks Systems"
  :license "MIT"
  :description "Website for ParenWorks Systems - Common Lisp Solutions"
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :components ((:file "module")
               (:file "email" :depends-on ("module"))
               (:file "pages" :depends-on ("module" "email")))
  :depends-on (:r-clip
               :lass
               :parenscript
               :drakma
               :babel
               (:interface :server)))
