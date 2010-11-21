(in-package :twitter)

;; debugging
;;(setf drakma:*header-stream* *standard-output*)

;;
;; Main API
;;
;;    ("X-Twitter-Client-URL" . "http://common-lisp.net/project/cl-twitter/"))

(defvar *twitter-user*)

(defvar *twitter-client-headers* 
  '(("X-Twitter-Client" . "CL-Twitter")
    ("X-Twitter-Client-Version" . "1.0")
    ("X-Twitter-Client-URL" . "http://github.com/fons/cl-twitter/"))
  "Default headers sent in requests")

(defvar *dump-response* nil  "dump the response to stdout on error")

(defvar *twitter-client-source-param* "cltwitter"   "The source value for posts; shows up in twitter web as client id")

(defparameter *http-request-function* 'drakma:http-request "Function used to make HTTP requests.  Must conform exactly to Drakma's specifications.")


(defvar *oauth-request-token-cache-max-length* 100 "Max number of request tokens that can be cached by CL-Twitter.")
(defvar *oauth-request-token-cache* nil  
"A list of request tokens that have been generated for OAuth
authentication by OAUTH-MAKE-TWITTER-AUTHORIZATION-URI.  These are
stored in between when the user is directed to a login URI and when
the user is authenticated.  Automatically pruned every once in a
while (by calls to OAUTH-AUTHENTICATE-USER).")

