(in-package :twitter)

;;
;; Main API
;;

(defvar *twitter-user*)

(defvar *twitter-client-headers* 
  '(("X-Twitter-Client" . "CL-Twitter")
    ("X-Twitter-Client-Version" . "1.0")
    ("X-Twitter-Client-URL" . "http://common-lisp.net/project/cl-twitter/"))
  "Default headers sent in requests")

(defvar *twitter-client-source-param* "cltwitter"
  "The source value for posts; shows up in twitter web if as client id")

(defparameter *http-request-function* 'drakma:http-request
  "Function used to make HTTP requests.  Must conform exactly to Drakma's specifications.")

(defmethod twitter-op (command &rest args)
  (let ((cmd (get-command command)))
    (multiple-value-bind (response code)
	(send-command cmd args)
      (if (eq code 200)
	  (parse-command-response response (command-return-type cmd))
	  (parse-error-response response code)))))

(defun send-command (command args)
  (multiple-value-bind (method url auth post-params) (command-request-arguments command args)
    (let ((socket nil))
      (unwind-protect
	   (multiple-value-bind (response code)
	       (destructuring-bind (&optional auth-method &rest auth-spec) (or auth  (user-http-auth *twitter-user*))
		 (let ((common-drakma-args 
			(list :want-stream t
			      :additional-headers *twitter-client-headers*
			      :external-format-out :utf-8)))
		   (if (member auth-method '(nil :basic-authorization))
		       (apply *http-request-function*
			      url
			      :method method
			      :basic-authorization auth-spec
			      :parameters (plist->alist post-params)
			      common-drakma-args)
		       (destructuring-bind (access-token) auth-spec
			 (oauth:access-protected-resource  url access-token :consumer-token (oauth:token-consumer (twitter-user-access-token *twitter-user*))
				  :request-method method
				  :user-parameters (plist->alist post-params)
				  :drakma-args common-drakma-args)))))
	     (setf socket response)
	     (values (safe-decode-json response) code))
	;; unwind-protect clean-up.  close the socket
	(when socket
	  (close socket))))))

(defun safe-decode-json (response-stream)
  (let ((json:*json-identifier-name-to-lisp* 'convert-from-twitter))
    (declare (special json:*json-identifier-name-to-lisp* 
		      json:*lisp-identifier-name-to-json*))
    (handler-case 
	(decode-json response-stream)
      #+nil
      (t () 
	;; what to do with decoding errors?
	nil))))

(defun parse-command-response (response type)
  (let ((parsed
	 (cond ((consp type)
		(mapcar (lambda (r) 
			  (parse-record r (first type)))
			response))
	       ((null type)
		nil)
	       (t (parse-record response type)))))
    ;;    (format t "Parsed response of type ~S:  ~S~%" type parsed)
    parsed))


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


(defvar *oauth-request-token-cache* nil
  "A list of request tokens that have been generated for OAuth
authentication by OAUTH-MAKE-TWITTER-AUTHORIZATION-URI.  These are
stored in between when the user is directed to a login URI and when
the user is authenticated.  Automatically pruned every once in a
while (by calls to OAUTH-AUTHENTICATE-USER).")

(defvar *oauth-request-token-cache-max-length* 100
  "Max number of request tokens that can be cached by CL-Twitter.")

(defun oauth-make-twitter-authorization-uri (&key (consumer-key *consumer-key*) (consumer-secret *consumer-secret*))
  "Returns a URL where the user should be directed to authorize the
application.  Once the user has authorized the user, she will be
redirected to the web page specified on the https://twitter.com/apps
page for the application."
  (let ((request-token
	 (cl-oauth:obtain-request-token "http://twitter.com/oauth/request_token"
					(oauth:make-consumer-token :key consumer-key
								   :secret consumer-secret))))
    (push request-token *oauth-request-token-cache*)
    (values
      (cl-oauth:make-authorization-uri "http://twitter.com/oauth/authorize" request-token)
      request-token)))


(defun oauth-authenticate-user (request-token)
  "Called after the user has been redirected to some page on a twitter
app's web site with a `oauth_token=<request-token-string>' parameter.
Authenticates the user as if by AUTHENTICATE-USER.

If REQUEST-TOKEN is a string, we look up the CL-OAUTH:REQUEST-TOKEN
in the *REQUEST-TOKEN-CACHE* list.  Otherwise, if it is a
CL-OAUTH:REQUEST-TOKEN already, we use that to authorize the request
token and then obtain an OAuth access token.  Once this is obtained,
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
	     (user-id (cdr (assoc "user_id" (oauth:token-user-data access-token) :test #'equal)))
	     (username (cdr (assoc "screen_name" (oauth:token-user-data access-token) :test #'equal)))
	     (user (get-user username)))
	(setf (twitter-user-access-token user) access-token)
	(setf *twitter-user* user) 
	(twitter-op :user-show :id user-id  :auth (list :oauth access-token))))

(defun authenticated-user ()
  *twitter-user*)

;;
;; Updates
;;

(defun public-timeline (&rest args)
  (print-tweets (apply 'twitter-op :public-timeline args)))

(defun timeline (&rest args)
  (print-tweets (apply 'twitter-op :user-timeline args)))

(defun friends-timeline (&rest args)
(define-command user-followers/ids (:get (:identity))
    "http://twitter.com/followers/ids.json"
    "Returns the authenticating user's friends, each with current status inline. They are ordered by the order in which they were added as friends. It's also possible to request another user's recent friends list via the id parameter below."
  :id "Optional.  The ID or screen name of (the user for whom to request a list of friends."
  :user_id "Optional.  The ID or screen name of (the user for whom to request a list of friends."
  :screen_name "Optional.  The ID or screen name of (the user for whom to request a list of friends."
  :page "Optional. Retrieves the next 5000 ids.")

(define-command user-friends/ids (:get (:identity))
    "http://twitter.com/friends/ids.json"
    "Returns the authenticating user's friends, each with current status inline. They are ordered by the order in which they were added as friends. It's also possible to request another user's recent friends list via the id parameter below."
  :id "Optional.  The ID or screen name of (the user for whom to request a list of friends."
  :user_id "Optional.  The ID or screen name of (the user for whom to request a list of friends."
  :screen_name "Optional.  The ID or screen name of (the user for whom to request a list of friends."
  :page "Optional. Retrieves the next 5000 ids.")
(print-tweets (apply 'twitter-op :friends-timeline args)))

(defun send-tweet (text &rest args &key (tiny-url-p t) &allow-other-keys)
  (let ((newtext (if tiny-url-p (convert-to-tinyurl text) text)))
    (if (<= (length newtext) 140)
	(let ((tweet (apply 'twitter-op :tweet-update :status newtext
			    (rem-keywords args '(:tiny-url-p)))))
	  (print-tweet tweet)
	  tweet)
	(error "Tweet updates must be less than 140 characters.  Length is ~A" (length newtext)))))

(defun update (text &rest args)
  (apply 'send-tweet text args))

(defun reply-to (tweet text &rest args)
  (apply 'send-tweet text :in-reply-to-status-id (tweet-id tweet) args))

(defun @reply-to (tweet text &rest args)
  (let ((fmt (format nil "@~A ~A"
                     (twitter-user-screen-name (tweet-user tweet))
		     text)))
    (apply 'reply-to tweet fmt args)))

;;
;; Messages
;;

(defun messages (&rest args)
  (apply 'twitter-op :messages-received args))

(defun sent-messages (&rest args)
  (apply 'twitter-op :messages-sent args))

(defun send-message (user message &rest args)
  (apply 'twitter-op :message-new 
	 :user (aif (get-user user)
		    (twitter-user-id it)
		    user)
	 :text message
	 args))

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

(defun trends (&rest args)
  (apply 'twitter-op :trends args))

(defun daily-trends (&rest args)
  (apply 'twitter-op :daily-trends args))

(defun weekly-trends (&rest args)
  (apply 'twitter-op :weekly-trends args))

;;
;; Tiny URL API
;;

;;; TinyURL-ize
;;; Using the very simple TinyURL API

(defparameter *tinyurl-url* "http://tinyurl.com/api-create.php")
(defconstant +http-ok+ 200)

(defun get-tinyurl (url)
  "Get a TinyURL for the given URL. Uses the TinyURL API service.
   (c) by Chaitanaya Gupta via cl-twit"
  (multiple-value-bind (body status-code)
      (funcall *http-request-function*
	       *tinyurl-url*
	       :parameters `(("url" . ,url)))
    (if (= status-code +http-ok+)
        body
        (error 'http-error
               :status-code status-code
               :url url
               :body body))))

(defun convert-to-tinyurl (text)
  (let ((result text)
	(regex (ppcre:create-scanner "http:[^\\s\\)\\]\\'\\\"]+")))
    (ppcre:do-matches (start end regex result result)
      (when (> (- end start) 24)
	(setf result (ppcre:regex-replace 
		      regex result 
		      (get-tinyurl (subseq result start end))))))))



  
