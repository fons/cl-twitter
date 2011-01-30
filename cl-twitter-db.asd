(in-package :cl-user)

;;put all the driver packages here

(defpackage #:cl-twitter-db-asd
  (:use :cl :asdf))

(in-package :cl-twitter-db-asd)

(defsystem cl-twitter-db
  :name "TWITTER-DB"
  :version "0.5"
  :maintainer "Fons Haffmans"
    :author "Ian Eslick"
    :licence "LLGPL"
    :description "A berkely db driver for cl-twitter"
    :components 
    ((:module "db"
	      :serial t
	      :components (
;;			   (:file "package")
;;			   (:file "bdb-driver")
			   )))
    :depends-on (:cl-twitter ))


(defpackage #:bdb-driver-asd
  (:use :cl :asdf))

(in-package :bdb-driver-asd)


(defsystem twitter-bdb-driver
  :name "BDB-DRIVER"
  :version "0.5"
  :maintainer "Fons Haffmans"
    :author "Ian Eslick"
    :licence "LLGPL"
    :description "A berkely db driver for cl-twitter"
    :components 
    ((:module "db"
	      :serial t
	      :components (
			   (:file "package")
			   (:file "bdb-driver")
			   )))
    :depends-on (:cl-twitter :elephant))
