(in-package :cl-user)

(defpackage :cl-twit-repl
  (:use #:cl #:cl-twitter); :trivial-http)
  (:nicknames :cl-twit-repl :twit-repl)
  (:export 

   ;;--------------------

   #:*access-file*

   ;; Interactive API
   #:authenticate-user
   #:authenticated-user
   #:*twitter-user*

   ;; OAuth
   #:oauth-make-twitter-authorization-uri
   #:oauth-authenticate-user
   #:repl-authenticate-user
   #:get-authenticated-user

   ;; I/O
   #:print-tweets
   #:get-tinyurl
   
   ))

   

