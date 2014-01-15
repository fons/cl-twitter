(in-package :twitter)

;; debugging
;;(setf drakma:*header-stream* *standard-output*)

;;
;; Main API
;;
;;    ("X-Twitter-Client-URL" . "http://common-lisp.net/project/cl-twitter/"))
(defvar      *month-strings*         '("xx" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec") "translate month to s string")
(defvar      *day-of-week-strings*   '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") "map day-of-the-week to a string")


(defvar *twitter-app-uri*       "https://api.twitter.com/1.1/")
(defvar *twitter-search-uri*    "https://api.twitter.com/1.1/")
(defvar *twitter-oauth-uri*  "https://api.twitter.com/oauth/")


(defvar *twitter-user*)

(defvar *twitter-client-headers*
  '(("X-Twitter-Client" . "CL-Twitter")
    ("X-Twitter-Client-Version" . "1.0")
    ("X-Twitter-Client-URL" . "http://github.com/fons/cl-twitter/"))
  "Default headers sent in requests")

(defvar *dump-response* nil  "dump the response to stdout on error")

(defvar *twitter-client-source-param* "cltwitter"   "The source value for posts; shows up in twitter web as client id")

(defparameter *http-request-function* 'drakma:http-request "Function used to make HTTP requests.  Must conform exactly to Drakma's specifications.")



