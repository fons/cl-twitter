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
;;			   (:file "elephant-driver")
			   )))
    :depends-on (:cl-twitter ))

;;--------------------------------------------------------------------------
;;----------------elephant bd driver ---------------------------------------
(defpackage #:elephant-driver-asd
  (:use :cl :asdf))

(in-package :elephant-driver-asd)

(defsystem twitter-elephant-driver
  :name "ELEPHANT-DRIVER"
  :version "0.5"
  :maintainer "Fons Haffmans"
    :author "Ian Eslick"
    :licence "LLGPL"
    :description "An elephant db driver for cl-twitter"
    :components 
    ((:module "db"
	      :serial t
	      :components (
			   (:file "elephant-driver-package")
			   (:file "elephant-driver")
			   )))
    :depends-on (:cl-twitter :elephant))

;;--------------------------------------------------------------------------
;;----------------mongo dbd driver ---------------------------------------

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
