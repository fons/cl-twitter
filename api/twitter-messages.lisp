(in-package :twitter)

;;
;; Messages
;;

(define-element twitter-message ((sender twitter-user) (recipient twitter-user))
  "This is a twitter message record"
  (id "" nil)
  (created-at "" nil)
  (text "" nil)
  (sender-id "" nil)
  (sender-screen-name "" nil)
  (recipient-id "" nil)
  (recipient-screen-name "" nil)
  (sender "" nil)
  (recipient "" nil))

(defmethod print-object ((msg twitter-message) stream)
  (format stream "#<message '~A' id:~A>" 
	  (twitter-message-sender-screen-name msg)
	  (twitter-message-id msg)))

(defmethod describe-object ((msg twitter-message) stream)
  (format stream "~A: ~A" 
	  (twitter-message-sender-screen-name msg)
	  (twitter-message-text msg)))


;;
;; DIRECT MESSAGES
;;
;;
;;  Direct Messages resources
;;      direct_messages
;;      direct_messages/sent
;;      direct_messages/new
;;      direct_messages/destroy/:id
;;

(define-command direct-messages (:get (:twitter-message))
    (twitter-app-uri "direct_messages.json")
    "Returns a list of the 20 most recent direct messages sent to the authenticating user.  The XML and JSON versions include detailed information about the sending and recipient users."
  :since  "Optional.  Narrows the resulting list of direct messages to just those sent after the specified HTTP-formatted date, up to 24 hours old.  
           The same behavior is available by setting the If-Modified-Since parameter in your HTTP request."
  :max_id "Returns results with an ID less than (that is, older than) or equal to the specified ID."
  :count "Specifies the number of records to retrieve. Must be less than or equal to 200."
  :page "Specifies the page of results to retrieve."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities")

(define-command direct-messages/sent (:get (:twitter-message))
    (twitter-app-uri "direct_messages/sent.json")
    "Returns a list of the 20 most recent direct messages sent by the authenticating user."  
  :since_id  "Optional.  Returns only sent direct messages with an ID greater than 
              (that is, more recent than) the specified ID."
  :max_id "Returns results with an ID less than (that is, older than) or equal to the specified ID."
  :count "Specifies the number of records to retrieve. Must be less than or equal to 200."
  :page "Specifies the page of results to retrieve."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities")


;;messages can only be sent to users that are following you....
(define-command direct-messages/new (:post :twitter-message)
    (twitter-app-uri "direct_messages/new.json")
    "Sends a new direct message to the specified user from the authenticating user."  
  :screen_name "Required. The screen name of the user who should receive the direct message."
  :user_id "Required.  The ID or screen name of the recipient user."
  :text "Required.  The text of your direct message.  Be sure to URL encode as necessary, and keep it under 140 characters."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities")


(define-command direct-messages/destroy/?id (:post-id :twitter-message)
    (twitter-app-uri "direct_messages/destroy/<id>.json")
    "Destroys the direct message specified in the required ID parameter.  The authenticating user must be the recipient of the specified direct message."
  :id "Required.  The ID of the direct message to destroy."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities")

;;----------------------end of direct messages------------------------------------------------------------------------

;;
;; Messages
;;

(defun direct-messages-received (&rest args &key (since nil) (max-id nil) (count nil) (page nil) (include-entities nil))
  (declare (ignore since max-id count page include-entities ))
  (apply 'twitter-op :direct-messages args))

(defun direct-messages-sent (&rest args &key (since nil) (max-id nil) (count nil) (page nil) (include-entities nil))
  (declare (ignore since max-id count page include-entities ))
  (apply 'twitter-op :direct-messages/sent args))

(defun send-direct-message (screen-name text &rest args &key (user-id nil) (include-entities nil))
  (declare (ignore user-id include-entities))
  (apply 'twitter-op :direct-messages/new :screen-name screen-name :text text args))

(defun delete-direct-message (message-id &rest args &key (include-entities nil))
  (declare (ignore include-entities))
  (apply 'twitter-op :direct-messages/destroy/?id :id message-id args))


;;-----------
(defun send-message (user message)
  (let (( user-id (twitter-user-id (show-user user))))
    (send-direct-message user message :user-id user-id)))

(defun delete-message (message)
  (delete-direct-message (twitter-message-id message)))

(defun messages ()
  (direct-messages-received))

(defun sent-messages ()
  (direct-messages-sent))

