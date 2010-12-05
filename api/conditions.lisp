(in-package :twitter)

;;
;; Errors
;;

(define-condition twitter-api-condition (error)
  ((return-code :reader return-code :initarg :code)
   (short    :reader short-message :initarg :short)
   (long     :reader long-message  :initarg :long)
   (request :reader request-message :initarg :request)
   (uri      :reader request-uri :initarg :uri))
  (:report (lambda (c stream)
	     (format stream "Error code ~A (~A): '~A'~%in request to ~A"
		     (return-code c) (short-message c)
		     (request-message c) (request-uri c)))))

(defparameter *code-messages* 
  '((200 "OK" "Everything went awesome")
    (304 "Not Modified" "No new data to return")
    (400 "Bad request" "your request is invalid, and we'll return an error message that tells you why. This is the status code returned if you've exceeded the rate limit")
    (401 "Not Authorized" "either you need to provide authentication credentials, or the credentials provided aren't valid")
    (403 "Forbidden" "we understand your request, but are refusing to fulfill it.  An accompanying error message should explain why.")
    (404 "Not Found" "either you're requesting an invalid URI or the resource in question doesn't exist (ex: no such user).")
    (500 "Internal Server Error" "Twitter did something wrong.  Please post to the group about it and the Twitter team will investigate.")
    (502 "Bad Gateway" "returned if Twitter is down or being upgraded")
    (503 "Service Unavailable" "the Twitter servers are up, but are overloaded with requests.  Try again later.")))


  
