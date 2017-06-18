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
	  (twitter-message-id msg)
          (twitter-message-text msg)))

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
  :since_id  "Returns results with an ID greater than (that is, more recent than) the specified ID"
  :max_id "Returns results with an ID less than (that is, older than) or equal to the specified ID."
  :count "Specifies the number of records to retrieve. Must be less than or equal to 200."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities"
  :skip_status "When set to either true, t or 1 statuses will not be included in the returned user objects.")



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
  :text "Required.  The text of your direct message.  Be sure to URL encode as necessary, and keep it under 140 characters.")


(define-command direct-messages/destroy/?id (:post-id :twitter-message)
    (twitter-app-uri "direct_messages/destroy/<id>.json")
    "Destroys the direct message specified in the required ID parameter.  The authenticating user must be the recipient of the specified direct message."
  :id "Required.  The ID of the direct message to destroy."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities")

(define-command direct-messages/show (:get :twitter-message)
    (twitter-app-uri "direct_messages/show.json")
    "Returns a single direct message, specified by an id parameter."
  :id            "The ID of the direct message.")


;;----------------------end of direct messages------------------------------------------------------------------------

;;
;; Messages
;;

(define-twitter-method direct-messages (() &key (since-id nil) (max-id nil) (count nil) (include-entities t) (skip-status nil))   :direct-messages )
(define-twitter-method direct-messages-sent (() &key (since nil) (max-id nil) (count nil) (page nil) (include-entities t))       :direct-messages/sent )
(define-twitter-method direct-messages-new (() &key (screen-name nil) (text nil)  (user-id nil) ) :direct-messages/new )
(define-twitter-method direct-messages-destroy ((message-id ) &key (include-entities t))    :direct-messages/destroy/?id :id)
(define-twitter-method direct-messages-show ((message-id ) )    :direct-messages/show :id)


;;-----------
(defmethod send-message ((screen-name string) (message string))
  (let (( user-id (twitter-user-id (show-user screen-name))))
    (direct-messages-new :screen-name screen-name :text message :user-id user-id)))

(defmethod delete-message ((message twitter-message))
  (direct-messages-destroy (twitter-message-id message)))

(defun messages-received ()
  (direct-messages))

(defun messages-sent ()
  (direct-messages-sent))

(defmethod show-message ((message twitter-message))
  (direct-messages-show (twitter-message-id message)))



