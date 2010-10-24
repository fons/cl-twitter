(in-package :cl-user)

(defpackage #:cl-twitter-db-asd
  (:use :cl :asdf))

(in-package :cl-twitter-db-asd)

(defsystem :cl-twitter-db
  :depends-on (:cl-twitter :elephant)
  :components ((:file "package-db")
	       (:file "db"))
  :serial t)


