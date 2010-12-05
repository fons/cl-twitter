(in-package :twitter)

(defun dump-stream(stream)
  (let ((seq (make-string 6000)))
    (read-sequence seq stream)
    seq))

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

(defun parse-error-response (response code)
  (destructuring-bind (code short long) 
      (assoc code *code-messages*)
    (handler-case 
	(cerror "Ignore response" 'twitter-api-condition 
		:code code
		:short short
		:long long
		:request (get-value :error response)
		:uri (get-value :request response))
      (error (c)
	(cerror "Ignore response" 'twitter-api-condition 
		:code code
		:short short
		:long long 
		:request "request not available"
		:uri response )))))

(defun safe-decode-json (response-stream)
  (let ((json:*json-identifier-name-to-lisp* 'convert-from-twitter))
    (declare (special json:*json-identifier-name-to-lisp* 
		      json:*lisp-identifier-name-to-json*))
    (handler-case 
	(json:decode-json response-stream)
      #+nil
      (t () 
	;; what to do with decoding errors?
	nil))))


(defun send-command (command args)
  ;;(format t "command {~A} args : {~A} ~%" command args)
  (multiple-value-bind (method url auth post-params) (command-request-arguments command args)
    (let ((socket nil))
      ;;(progn
      ;;	(format t "args : {~A} ~%" args)
      ;;	(format t "post params : {~A} ~%" post-params)
      ;;	(format t "method : {~A} ~%" method)
      ;;	(format t "url : {~A} ~%" url)
      ;;	(format t "act post params : {~A} ~%" (plist->alist post-params)))
      (unwind-protect
	   (multiple-value-bind (response code)
	       (destructuring-bind (&optional auth-method &rest auth-spec) (or auth  (user-http-auth *twitter-user*))
		 (let ((common-drakma-args 
			(list :want-stream t
			      :additional-headers *twitter-client-headers*
			      :external-format-out :utf-8)))
		   (if (member auth-method '(nil :basic-authorization))
		       (apply *http-request-function*
			      (puri:parse-uri url)
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
	     (handler-case 
		 (values (safe-decode-json response) code)
	       (error (c)
		 (progn
		   (format t "an error was encountered : ~A ~%" c)
		   (format t "code  : ~A ~%" code)
		   (when *dump-response*
		     (format t "response : {~A}~%" (dump-stream response)))
		   (values response code)
		   )
		 ) ))
	;; unwind-protect clean-up.  close the socket
	(when socket
	  (close socket))))))


(defmethod twitter-op (command &rest args)
  ;;(format t "command : [~S] [~s]~%" command args)
  (let ((cmd (get-command command)))
    (multiple-value-bind (response code)
	(send-command cmd (lisp->twitter-plist args))
      (if (eq code 200)
	  (parse-command-response response (command-return-type cmd))
	  (parse-error-response response code)))))