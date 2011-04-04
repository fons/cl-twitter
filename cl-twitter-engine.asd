(in-package :cl-user)

(defpackage #:cl-twitter-engine-asd
  (:use :cl :asdf))

(in-package :cl-twitter-engine-asd)

(defsystem cl-twitter-engine
    :name "CL-TWITTER-ENGINE"
    :version "0.5"
    :maintainer "Fons Haffmans"
    :author "Fons Haffmans"
    :licence "LLGPL"
    :description "automation of the twitter stream"
    :components 
    ((:module "engine"
	      :serial t
	      :components ((:file "package")
			   ;;(:file "timelines")
			   (:file "scheduler"))))
    :depends-on (:cl-twitter :cl-twit-repl :cl-twitter-db :bordeaux-threads :cl-mongo))