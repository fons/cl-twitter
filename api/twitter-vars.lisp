(in-package :twitter)

;; debugging
;;(setf drakma:*header-stream* *standard-output*)

;;
;; Main API
;;
;;    ("X-Twitter-Client-URL" . "http://common-lisp.net/project/cl-twitter/"))
(defvar *twitter-app-uri*       "http://api.twitter.com/1/")
(defvar *twitter-search-uri*    "http://search.twitter.com/")
(defvar *twitter-oauth-uri*  "http://api.twitter.com/oauth/")   


(defvar *twitter-user*)

(defvar *twitter-client-headers* 
  '(("X-Twitter-Client" . "CL-Twitter")
    ("X-Twitter-Client-Version" . "1.0")
    ("X-Twitter-Client-URL" . "http://github.com/fons/cl-twitter/"))
  "Default headers sent in requests")

(defvar *dump-response* nil  "dump the response to stdout on error")

(defvar *twitter-client-source-param* "cltwitter"   "The source value for posts; shows up in twitter web as client id")

(defparameter *http-request-function* 'drakma:http-request "Function used to make HTTP requests.  Must conform exactly to Drakma's specifications.")



