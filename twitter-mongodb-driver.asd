(in-package :cl-user)

(defpackage #:mongodb-driver-asd
  (:use :cl :asdf))

(in-package :mongodb-driver-asd)

(defsystem twitter-mongodb-driver
  :name "MONGODB-DRIVER"
  :version "0.5"
  :maintainer "Fons Haffmans"
    :author "Fons Haffmans"
    :licence "LLGPL"
    :description "A mongodb driver for cl-twitter"
    :components 
    ((:module "db"
	      :serial t
	      :components (
			   (:file "mongodb-driver-package")
			   (:file "mongodb-driver")
			   )))
    :depends-on (:cl-twitter :cl-mongo))
