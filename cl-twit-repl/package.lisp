(in-package :cl-user)

(defpackage :cl-twit-repl
  (:use #:cl #:cl-twitter); :trivial-http)
  (:nicknames :cl-twit-repl :twit-repl)
  (:export 

   ;;--------------------

   #:*access-file*
   #:alias
   #:unalias
   #:cl-twit-repl
   #:done-twittering

   #:authenticate-user
   #:authenticated-user
   #:*twitter-user*

   ;; OAuth
   #:oauth-make-twitter-authorization-uri
   #:oauth-authenticate-user
   #:repl-authenticate-user
   #:get-authenticated-user

   ))

   

