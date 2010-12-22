(in-package :twitter)

(defvar *twitter-client-source-param*)

(defvar *command-map* (make-hash-table)
  "Table of all the command records")

(defstruct (twitter-command (:conc-name "COMMAND-")) 
  name method return-type base-url description argmap)

(defmethod print-object ((cmd twitter-command) stream)
  (format stream "#<TWITTER-CMD '~A'>" (command-name cmd)))

;;
;; The commands are defined in terms of twitter symbols i.e. a literal rendition
;; of the twitter api args. I want to convert those to lisp commands..
;;

(defun add-conversions-from-twitter (args)
    "Add twitter->lisp conversions for define-element args"
    (progn
      (mapcar #'maybe-add-conversion-from-twitter (mapcar #'car args))
      args))

;;
;; Command definition macro
;;


(defmacro define-command (command (method return-type) base-url description &body args)
  "Capture all the key syntax for Twitter commands:
   command name | method | arguments | return-type | base-url 

   method = { :get | :post | :get-id | get-user-id | :post-id }
   return-type = { :status | (:status) | :user-basic | 
                   :user-ext | :message | (:message) |
                   :id | (:id) | :value"
  (let ((cmd-sym (intern (symbol-name command) :keyword)))
    `(progn
       (setf (gethash ,cmd-sym *command-map*)
	     (make-twitter-command
	      :name ,cmd-sym
	      :method ,method
	      :return-type ',return-type
	      :base-url ,base-url
	      :description ,description
	      :argmap ',(add-conversions-from-twitter (plist->alist args)))))))

;;
;; Command API
;;

(defun get-command (command-ref)
  "Take a command or keyword reference and return command object"
  (if (twitter-command-p command-ref) command-ref
      (gethash command-ref *command-map*)))

(defun list-commands ()
  (format t "Twitter API Commands (command-help command-name) provides help:~%")
  (maphash (lambda (k v)
	     (format t ":~A -> ~A~%    ~A~%"
		     k (command-return-type v)
		     (command-description v)))
           *command-map*))

(defun command-help (&optional command-name)
  "Interactive printing of command arguments"
  (if (null command-name)
      (list-commands)
      (let* ((command (get-command command-name)))
	(if command
	    (progn
	      (format t "~A~%~A~%~%Arguments:~%" command-name (command-description command))
	      (mapcar #'argument-help (command-argmap command))
	      t)
	    (format t "Command ~A not found~%" command-name)))))

(defun argument-help (argument)
  (format t "  ~A: ~A~%" (car argument) (cdr argument)))

;;; HERE 
;;; Fix args here...
;;; reason for this when using the api calls with &rest args an extra nil is passed in
;;;
(defun fix-args (args)
  (if (eql 1 (mod (length args) 2)) 
      (nreverse (cons nil (nreverse args )))
      args))

;; Only post-id-.. seems to rewrite the url
;; plist->uri-params
(defun command-request-arguments (command args)
  "A command reference and a plist of arguments.
   Returns multiple values: url auth post-params parse-type"
  (let ((cmd (get-command command)))
    (check-arguments cmd args)
    (let ((newargs (lisp->twitter-plist args)))
      (case (command-method cmd)
	(:get                    (get-command-request cmd newargs))
	(:post                   (post-command-request cmd (fix-args newargs)))
	(:get-id                 (get-id-command-request cmd newargs))
	(:get-user               (get-user-command-request cmd newargs))
	(:get-user-id            (get-user-id-command-request cmd newargs))
	(:get-user-list-id       (get-user-list-id-command-request cmd newargs))
	(:get-user-list-id-id    (get-user-list-id-id-command-request cmd newargs))
	(:post-user-id           (post-user-id-command-request cmd newargs))
	(:post-user-list-id      (post-user-list-id-command-request cmd newargs))
	(:post-user              (post-user-command-request cmd newargs))
	(:post-id                (post-id-command-request cmd newargs))))))

;;
;; URI generators
;;


(defun get-command-request (cmd args)
  (values 
   :get
   (generate-get-url cmd (strip-keyword-if #'(lambda (k) (member k '(:user :auth))) args))
   (or (getf args :auth) (user-http-auth (get-user (getf args :user nil))))
   nil))

(defun get-id-command-request (cmd args)
  (multiple-value-bind (method url auth)
      (get-command-request cmd (strip-keyword :id args))
    (values method (inject-url-id cmd url args)
	    auth nil)))

(defun get-user-command-request (cmd args)
  (multiple-value-bind (method url auth)
      (get-command-request cmd (strip-keyword :user args))
    (values method (inject-url-user cmd url args)
	    auth nil)))

(defun get-user-id-command-request (cmd args)
  (multiple-value-bind (method url auth)
      (get-command-request cmd (strip-keywords '(:user :id) args))
    (values method (inject-url-user-id cmd url args)
	    auth nil)))

(defun get-user-list-id-command-request (cmd args)
  (multiple-value-bind (method url auth)  (get-command-request cmd (strip-keywords '(:user :list_id) args))
    (values method (inject-url-user-list-id cmd url args) auth nil)))

(defun get-user-list-id-id-command-request (cmd args)
  (multiple-value-bind (method url auth)  (get-command-request cmd (strip-keywords '(:user :list_id :id) args))
    (values method (inject-url-user-list-id-id cmd url args) auth nil)))

(defun post-command-request (cmd args)
  (values
   :post
   (command-base-url cmd)
   (or (getf args :auth) (user-http-auth (get-user (getf args :user nil))))
   (plist->uri-params (append (when *twitter-client-source-param*
				`("source" ,*twitter-client-source-param*))
			      (strip-keyword-if #'(lambda (k) (member k '(:user :auth))) args)))))

(defun post-id-command-request (cmd args)
  (multiple-value-bind (method url auth post)
      (post-command-request cmd (strip-keyword :id args))
    (values method (inject-url-id cmd url args)
	    auth post)))

(defun post-user-command-request (cmd args)
  (multiple-value-bind (method url auth post)
      (post-command-request cmd (strip-keyword :user args))
    (values method (inject-url-user cmd url args)
	    auth post)))

(defun post-user-id-command-request (cmd args)
  (multiple-value-bind (method url auth post)
      (post-command-request cmd (strip-keywords '(:user :id) args))
    (values method (inject-url-user-id cmd url args)
	    auth post)))

(defun post-user-list-id-command-request (cmd args)
  (multiple-value-bind (method url auth post) (post-command-request cmd (strip-keywords '(:user :list_id) args))
    (values method (inject-url-user-list-id cmd url args)  auth post)))

;;
;; Helpers
;;		       

(defun check-arguments (cmd args)
  (let ((argmap (command-argmap cmd))
	(name (command-name cmd)))
    (loop for arg in (plist-keywords args)
       unless (or (member arg argmap :key #'car) (member arg '(:user :auth)))
       do (error "Unknown argument ~A to command ~A" arg name)
       finally (return t))))

(defun get-request-argument (args keyword)
  (to-uri-param (getf args keyword) nil))

(defun get-required-request-argument (cmd args keyword)
  (let ((value (getf args keyword)))
    (unless value
      (error ":~A argument missing for command ~A" keyword (command-name cmd)))
    (to-uri-param value nil)))

(defun inject-url-id (cmd url args)
  (declare (ignorable cmd))
  (ppcre:regex-replace "<id>" url  (or (get-request-argument args :id) "show")))

(defun inject-url-list-id (cmd url args)
  (declare (ignorable cmd))
  (ppcre:regex-replace "<list-id>" url  (or (get-request-argument args :list_id) "show")))

(defun inject-url-user (cmd url args)
  (declare (ignorable cmd))
  (ppcre:regex-replace "<user>" url  (or (get-request-argument args :user) "show")))

(defun inject-url-user-id (cmd url args)
  (declare (ignorable cmd))
  (let ((new-url (ppcre:regex-replace "<user>" url  (or (get-request-argument args :user) "show"))))
    (inject-url-id cmd new-url args)))
	 
(defun inject-url-user-list-id (cmd url args)
  (declare (ignorable cmd))
  (let ((new-url (ppcre:regex-replace "<user>" url  (or (get-request-argument args :user) "show"))))
    (inject-url-list-id cmd new-url args)))
	 
(defun inject-url-user-list-id-id (cmd url args)
  (declare (ignorable cmd))
  (let ((new-url (ppcre:regex-replace "<id>" url  (or (get-request-argument args :id) "show"))))
    (inject-url-user-list-id cmd new-url args)))
	 

(defun generate-get-url (cmd args)
  (format nil "~A?~{~A=~A~^&~}" (command-base-url cmd) (plist->uri-params args t)))
	  

