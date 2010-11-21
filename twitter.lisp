(in-package :twitter)


;;
;; Public API for common commands
;;
;;
;;
;;

(defvar *consumer-key* "9hOStbD2Zf7x0mUoo7cYBg"
  "The consumer key for cl-twit-repl, as listed on https://twitter.com/apps. Used for OAuth.")

(defvar *consumer-secret* "PWx9ZBZS9BVbesqlkoyiPzXtucmU7jaWe4ECcC30l0"
  "The consumer secret for cl-twit-repl, as listed on https://twitter.com/apps. Used for OAuth.")


(defun oauth-make-twitter-authorization-uri (&key (consumer-key *consumer-key*) (consumer-secret *consumer-secret*))
  "Returns a URL where the user should be directed to authorize the application.  Once the user has authorized the user, she will be
redirected to the web page specified on the https://twitter.com/apps page for the application."
  (let ((request-token
	 (cl-oauth:obtain-request-token "http://twitter.com/oauth/request_token"
					(oauth:make-consumer-token :key consumer-key
								   :secret consumer-secret))))
    (push request-token *oauth-request-token-cache*)
    (values
     (cl-oauth:make-authorization-uri "http://twitter.com/oauth/authorize" request-token)
     request-token)))


(defun oauth-authenticate-user (request-token)
  "Called after the user has been redirected to some page on a twitter app's web site with a `oauth_token=<request-token-string>' parameter.
Authenticates the user as if by AUTHENTICATE-USER.

If REQUEST-TOKEN is a string, we look up the CL-OAUTH:REQUEST-TOKEN in the *REQUEST-TOKEN-CACHE* list.  Otherwise, if it is a
CL-OAUTH:REQUEST-TOKEN already, we use that to authorize the request token and then obtain an OAuth access token.  Once this is obtained,
the user has been logged in to Twitter via OAuth."
  (when (stringp request-token)
    (setf request-token (find request-token *oauth-request-token-cache*
			      :key 'cl-oauth:token-key :test #'equal)))
  
  (when (null request-token)
    (error "Invalid request token."))
  
  (unless (oauth:request-token-authorized-p request-token)
    (oauth:authorize-request-token request-token))
  
  (let* ((access-token
	 (cl-oauth:obtain-access-token "http://twitter.com/oauth/access_token"
				       (oauth:token-consumer request-token)
				       request-token))
	 (user-id (cdr (assoc "user_id" (oauth:token-user-data access-token) :test #'equal)))
	 (username (cdr (assoc "screen_name" (oauth:token-user-data access-token) :test #'equal))))
    (assert username)
    (let ((user (twitter-op :user-show :id user-id  :auth (list :oauth access-token))))
      (setf (twitter-user-access-token user) access-token)
      user)))

(defun repl-authenticate-user (&key (consumer-key *consumer-key*) (consumer-secret *consumer-secret*))
  (let* ((request-token (oauth:obtain-request-token "http://twitter.com/oauth/request_token" (oauth:make-consumer-token :key consumer-key :secret consumer-secret)))
	 (uri (cl-oauth:make-authorization-uri "http://twitter.com/oauth/authorize" request-token)))
    (format t "please authorize : ~S~%" uri)
    (format t "enter PIN :   ")
    (let (( pin (read)))
      (format t "obtaining access tokens for pin : ~S~%" pin)
      (unless (oauth:request-token-authorized-p request-token)
	(oauth:authorize-request-token request-token))
      (setf (oauth:request-token-verification-code request-token) (format nil "~A" pin))      
      (let* ((access-token (oauth:obtain-access-token "http://twitter.com/oauth/access_token" request-token)) 
	     (user-id (cdr (assoc "user_id" (oauth:token-user-data access-token) :test #'equal)))
	     (username (cdr (assoc "screen_name" (oauth:token-user-data access-token) :test #'equal)))
	     (user (get-user username)))
	(setf (twitter-user-access-token user) access-token)
	(setf *twitter-user* user)
	(write-access-info *twitter-user*)
	(twitter-op :user-show :id user-id  :auth (list :oauth access-token))))))

(defun get-authenticated-user (user)
      (let* ((access-token (get-access-token user))
	     (username (cdr (assoc "screen_name" (oauth:token-user-data access-token) :test #'equal)))
	     (user (get-user username)))
	(setf (twitter-user-access-token user) access-token)
	(setf *twitter-user* user)))

;;	(twitter-op :users/show :user_id user-id  :auth (list :oauth access-token))))

(defun authenticated-user ()
  *twitter-user*)


;;---------------------------------------------

;;
;; Objects
;;

(defun print-tweets (tweets)
  (mapc #'print-tweet tweets))
;;
;; Search API
;;

(defun do-search (query &rest args)
  (let ((result (apply 'twitter-op :search :q query args)))
    (values (search-results result) result)))



  
