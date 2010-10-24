(in-package :cl-user)

(defpackage #:cl-twitter-asd
  (:use :cl :asdf))

(in-package :cl-twitter-asd)

(defsystem cl-twitter
    :name "CL-TWITTER"
    :version "0.9"
    :maintainer "Ian Eslick"
    :author "Ian Eslick"
    :licence "LLGPL"
    :description "A simple interface to Twitter"
    :components ((:file "package")
		 (:file "utils")
		 (:file "elements")
		 (:file "commands")
		 (:file "dictionary")
		 (:file "conditions")
		 (:file "twitter"))
    :serial t
    :depends-on (:cl-json :trivial-http :drakma :anaphora :cl-ppcre :closer-mop :cl-oauth))


