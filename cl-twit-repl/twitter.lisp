(in-package :cl-twit-repl)


;;
;; Public API for common commands
;;
;;
;;
;;
(defvar *oauth-request-token-cache-max-length* 100 "Max number of request tokens that can be cached by CL-Twitter.")
(defvar *oauth-request-token-cache* nil
"A list of request tokens that have been generated for OAuth
authentication by OAUTH-MAKE-TWITTER-AUTHORIZATION-URI.  These are
stored in between when the user is directed to a login URI and when
the user is authenticated.  Automatically pruned every once in a
while (by calls to OAUTH-AUTHENTICATE-USER).")

(defun oauth-make-twitter-authorization-uri (&key (consumer-key *consumer-key*) (consumer-secret *consumer-secret*))
  "Returns a URL where the user should be directed to authorize the application.  Once the user has authorized the user, she will be
redirected to the web page specified on the https://twitter.com/apps page for the application."
  (let ((request-token
	 (cl-oauth:obtain-request-token (twitter-oauth-uri "request_token")
					(oauth:make-consumer-token :key consumer-key
								   :secret consumer-secret))))
    (push request-token *oauth-request-token-cache*)
    (values
     (cl-oauth:make-authorization-uri (twitter-oauth-uri "authorize") request-token)
     request-token)))


(defun oauth-authenticate-user (request-token)
  "Called after the user has been redirected to some page on a twitter app's web site with a `oauth_token=<request-token-string>' parameter.
Authenticates the user as if by AUTHENTICATE-USER.
If REQUEST-TOKEN is a string, we look up the CL-OAUTH:REQUEST-TOKEN in the *REQUEST-TOKEN-CACHE* list.  Otherwise, if it is a
CL-OAUTH:REQUEST-TOKEN already, we use that to authorize the request token and then obtain an OAuth access token.  Once this is obtained,
the user has been logged in to Twitter via OAuth."
  (when (stringp request-token)
    (setf request-token (find request-token *oauth-request-token-cache* :key 'cl-oauth:token-key :test #'equal)))
  (when (null request-token)
    (error "Invalid request token."))
  (unless (oauth:request-token-authorized-p request-token)
    (oauth:authorize-request-token request-token))
  (let* ((access-token (cl-oauth:obtain-access-token (twitter-oauth-uri "access_token")
						     (oauth:token-consumer request-token)
						     request-token))
	 (user-id (cdr (assoc "user_id" (oauth:token-user-data access-token) :test #'equal)))
	 (username (cdr (assoc "screen_name" (oauth:token-user-data access-token) :test #'equal))))
    (assert username)
    (let ((user (twitter-op :user-show :id user-id  :auth (list :oauth access-token))))
      (setf (twitter-user-access-token user) access-token)
      user)))

(defun repl-authenticate-user (&key (consumer-key *consumer-key*) (consumer-secret *consumer-secret*))
  (let* ((request-token (oauth:obtain-request-token (twitter-oauth-uri "request_token") (oauth:make-consumer-token :key consumer-key :secret consumer-secret)))
	 (uri (cl-oauth:make-authorization-uri (twitter-oauth-uri "authorize") request-token)))
    (format t "please authorize : ~S~%" uri)
    (format t "enter PIN :   ")
    (let (( pin (read)))
      (format t "obtaining access tokens for pin : ~S~%" pin)
      (unless (oauth:request-token-authorized-p request-token) (oauth:authorize-request-token request-token))
      (setf (oauth:request-token-verification-code request-token) (format nil "~A" pin))
      (let* ((access-token (oauth:obtain-access-token (twitter-oauth-uri "access_token") request-token))
	     (user-id (cdr (assoc "user_id" (oauth:token-user-data access-token) :test #'equal)))
	     (username (cdr (assoc "screen_name" (oauth:token-user-data access-token) :test #'equal))))
        ;;; Ugly, ugly hack to get login working in v1.1 API.
        ;;; Basically, create a simple stub twitter-user object and populate enough of it to get the
        ;;; full code working.
        (setf *twitter-user* (make-instance 'twitter-user :access-token access-token))
        (let ((user (get-user username)))
          (setf (twitter-user-access-token user) access-token)
          (setf *twitter-user* user)
          (write-access-info *twitter-user*) )))))

(defun get-authenticated-user (user)
  (handler-case
      (let* ((access-token (get-access-token user))
	     (username (cdr (assoc "screen_name" (oauth:token-user-data access-token) :test #'equal)))
	     )
        ;;; Ugly, ugly hack to get login working in v1.1 API.
        ;;; Basically, create a simple stub twitter-user object and populate enough of it to get the
        ;;; full code working.
        (setf *twitter-user* (make-instance 'twitter-user :access-token access-token))
        (let ((user (cl-twitter:get-user username)))
          (setf (twitter-user-access-token user) access-token)
          (setf *twitter-user* user)))
    (missing-user-credentials (c)
      (format t "We don't have twitter credentials for ~A in ~A ~%" (who c) (access-file))
      (format t "maybe because this is the first time you are using cl-twit-repl on this machine.. ~&")
      (format t "No reason to panic; we'll just get new credentials from twitter~%")
      (repl-authenticate-user))))

(defun authenticated-user () *twitter-user*)


;;---------------------------------------------
