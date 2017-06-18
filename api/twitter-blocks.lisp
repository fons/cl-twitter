(in-package :twitter)
;;
;; Block Methods
;;
;; Block resources
;;    blocks/create
;;    blocks/destroy
;;    blocks/exists
;;    blocks/blocking
;;    blocks/blocking/ids

(define-element cursor-id ((ids (identity)))
  "a cursor element "
  (next-cursor-str     ""  nil)
  (previous-cursor-str ""  nil)
  (next-cursor         ""  nil)
  (ids                 ""  nil)
  (previous-cursor     ""  nil))

(defun limit-length (lst)
  (if (> (length lst) 5)
      (format nil "~{~a~^,~},..." (subseq lst 0 5))
  (format nil "~{~a~^,~}" lst)))

(defmethod print-object ((ref cursor-id) stream)
  (format stream "#<TWITTER-CURSOR-ID '~A:~A'>" (length (cursor-id-ids ref)) (limit-length (cursor-id-ids ref))))

(defun cursor-id (ref)
  (format t "~A: ~A ~A~%" (cursor-id-previous-cursor ref) (cursor-id-next-cursor ref) (length (cursor-id-ids ref))))


;;Destroys a friendship to the blocked user if it exists. Returns the blocked user in the requested format when successful.
(define-command blocks/create (:post :twitter-user)
    (twitter-app-uri "blocks/create.json")
    "Blocks the user specified in the ID parameter as the authenticating user. "
    :user_id "The ID of the user for whom to return results for. Helpful for disambiguating when a valid user ID is also a valid screen name."
    :screen_name "The screen name of the user for whom to return results for. Helpful for disambiguating when a valid screen name is also a user ID."
    :include_entities "When set to either true, t or 1, each tweet will include a node called entities"
    :skip_status "When set to either true, t or 1 statuses will not be included in the returned user objects.")

(define-twitter-method blocks-create (() &key (screen-name) (user-id nil) (include-entities t) (skip-status nil)) :blocks/create )

(define-command blocks/list (:get :cursor-user)
    (twitter-app-uri "blocks/list.json")
    "Returns a collection of user objects that the authenticating user is blocking."
  :cursor "semi-optional:Causes the list of blocked users to be broken into pages of no more than 5000 IDs at a time"
  :skip_status "Optional :When set to either true, t or 1 statuses will not be included in the returned user objects."
  :include_entities "Optional: When set to either true, t or 1, each tweet will include a node called entities")

(define-twitter-method blocks-list (() &key (cursor nil) (include-entities t) (skip-status nil)) :blocks/list )

(define-command blocks/ids (:get :cursor-id)
    (twitter-app-uri "blocks/ids.json")
    "Returns an array of numeric user ids the authenticating user is blocking"    
  :cursor "semi-optional:Causes the list of blocked users to be broken into pages of no more than 5000 IDs at a time"
  :stringify_ids "Optional:Many programming environments will not consume our ids due to their size.")

(define-twitter-method blocks-ids (() &key (cursor nil) (stringify-ids nil)) :blocks/ids )

(define-command blocks/destroy (:post :twitter-user)
    (twitter-app-uri "blocks/destroy.json")
    "Un-blocks the user specified in the ID parameter as the authenticating user.  Returns the un-blocked user in the requested format when successful."
  :user_id "Optional: The ID of the user for whom to return results for. Helpful for disambiguating when a valid user ID is also a valid screen name."
  :screen_name "Optional: The screen name of the user for whom to return results for. Helpful for disambiguating when a valid screen name is also a user ID."
  :skip_status "Optional: When set to either true, t or 1 statuses will not be included in the returned user objects"
  :include_entities "Optional: When set to either true, t or 1, each tweet will include a node called entities")

(define-twitter-method blocks-destroy (() &key (user-id nil) (screen-name nil) (skip-status nil) (include-entities nil)) :blocks/destroy )


 
;;--------------------- end of blocks resources -----------------------------------------------


;; TODO : helper methods

