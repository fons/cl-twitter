(in-package :cl-user)

(defpackage :twitter-db
  (:use :cl :twitter :elephant)
  (:import-from :twitter :register-twitter-object)
  (:export 
   #:*twitter-db-spec*
   #:open-twitter-db
   #:close-twitter-db
   ;; Users
   #:find-twitter-user
   #:map-twitter-users
   ;; Tweets
   #:get-tweet
   #:map-tweets
   #:user-tweets
   #:map-user-tweets
   ;; Messages
   #:get-tweet-msg
   #:map-tweet-msgs
   #:map-user-msgs
   #:map-user-received-messages))