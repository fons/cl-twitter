(in-package :cl-user)

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

