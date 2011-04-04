(in-package :cl-user)

(defpackage :cl-twitter-engine
  (:use #:cl #:cl-twitter #:bordeaux-threads #:cl-mongo); :trivial-http)
  (:nicknames :twitter-engine :twit-engine)
  (:shadow :show) 
  (:export 
   
   ;;--------------------

   #:submit-job

   ))

   

