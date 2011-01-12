(in-package :cl-user)

(defpackage #:cl-twitter-db-asd
  (:use :cl :asdf))

(in-package :cl-twitter-db-asd)

(defsystem cl-twitter-db
  :name "CL-TWITTER-DB"
  :version "0.5"
  :maintainer "Fons Haffmans"
    :author "Ian Eslick"
    :licence "LLGPL"
    :description "A db interface to Twitter"
    :components 
    ((:module "db"
	      :serial t
	      :components ((:file "package-db")
			   (:file "db"))))
    :depends-on (:cl-twitter :elephant))