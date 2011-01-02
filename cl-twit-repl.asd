(in-package :cl-user)

(defpackage #:cl-twit-repl-asd
  (:use :cl :asdf))

(in-package :cl-twit-repl-asd)

(defsystem cl-twit-repl
    :name "CL-TWIT-REPL"
    :version "0.5"
    :maintainer "Fons Haffmans"
    :author "Ian Eslick"
    :licence "LLGPL"
    :description "An interface to Twitter"
    :components 
    ((:module "cl-twit-repl"
      :serial t
      :components ((:file "package")
		   (:file "serialize-access")
		   (:file "alias")
		   (:file "cl-twit-repl")
		   (:file "twitter"))))
    :depends-on (:cl-twitter))