(in-package :twitter)

;;================================================================================================================================================================================
(define-command lists/members (:get :cursor-user)
    (twitter-app-uri "lists/members.json")
    "Returns the members of the specified list."
  :list_id "Required: The numerical id of the list."
  :slug "Required: You can identify a list by its slug instead of its numerical id. You’ll also have to specify the list owner using the owner_id or owner_screen_name parameters."
  :owner_screen_name "Optional: The screen name of the user who owns the list being requested by a slug."
  :owner_id "Optional :The user ID of the user who owns the list being requested by a slug."
  :count "Optional: Specifies the number of results to return per page (see cursor below). The default is 20, with a maximum of 5,000."
  :cursor "Semi-optional: Causes the collection of list members to be broken into “pages” of consistent sizes (specified by the count parameter)."
  :include_entities "Optional: The entities node will be disincluded when set to false."
  :skip_status "Optional: When set to either true, t or 1 statuses will not be included in the returned user objects.")

(define-twitter-method lists-members (() &key (list-id nil) (slug nil) (owner-screen-name nil) (owner-id nil) (count nil) (cursor nil) (include-entities nil) (skip-status nil) ) :lists/members)

(define-command lists/members/show (:get :twitter-user)
    (twitter-app-uri "lists/members/show.json")
    "Check if a user is a member of the specified list."
  :list_id "Required : The id or slug of the list."
  :slug "Required: You can identify a list by its slug instead of its numerical id. If you decide to do so, note that you’ll also have to specify the list owner using the owner_id or owner_screen_name parameters."
  :user_id "Required : The ID of the user for whom to return results for."
  :screen_name "Required: The screen name of the user for whom to return results for."
  :owner_screen_name "Optional: The screen name of the user who owns the list being requested by a slug."
  :owner_id "Optional: The user ID of the user who owns the list being requested by a slug."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities.")

(define-twitter-method lists-members-show (() &key (list-id nil) (slug nil) (user-id nil) (screen-name nil) (owner-screen-name nil) (owner-id nil) (include-entities nil)  ) :lists/members/show)


(define-command lists/members/destroy (:post :list-type)
    (twitter-app-uri "lists/members/destroy.json")
    "Removes the member from the specified list."
  :user_id "user" 
  :slug "Optional: You can identify a list by its slug instead of its numerical id."
  :list_id "Required : The id or slug of the list."
  :screen_name   "Optional: screen name of the user to be removed"
  :owner_screen_name "ptional: The screen name of the user who owns the list being requested by a slug"
  :owner_id "Optional: user ID of the user who owns the list being requested by a slug.")

(define-twitter-method lists-members-destroy (() &key (user-id nil) (slug nil) (list-id nil) (screen-name nil) (owner-screen-name nil) (owner-id nil)) :lists/members/destroy)

(define-command lists/members/create (:post :list-type)
    (twitter-app-uri "lists/members/create.json")
    "Add a member to the specified list."
  :user_id "user" 
  :slug "Optional: You can identify a list by its slug instead of its numerical id."
  :list_id "Required : The id or slug of the list."
  :screen_name   "Optional: screen name of the user to be removed"
  :owner_screen_name "ptional: The screen name of the user who owns the list being requested by a slug"
  :owner_id "Optional: user ID of the user who owns the list being requested by a slug.")

(define-twitter-method lists-members-create (() &key (user-id nil) (slug nil) (list-id nil) (screen-name nil) (owner-screen-name nil) (owner-id nil)) :lists/members/create)

(define-command lists/members/destroy_all (:post :list-type)
    (twitter-app-uri "lists/members/destroy_all.json")
    "Removes multiple members from a list, by specifying a comma-separated list of member ids or screen names."
  :user_id "user" 
  :slug "Optional: You can identify a list by its slug instead of its numerical id."
  :list_id "Required : The id or slug of the list."
  :screen_name   "Optional: screen name of the user to be removed"
  :owner_screen_name "ptional: The screen name of the user who owns the list being requested by a slug"
  :owner_id "Optional: user ID of the user who owns the list being requested by a slug.")

(define-twitter-method lists-members-destroy-all (() &key (user-id nil) (slug nil) (list-id nil) (screen-name nil) (owner-screen-name nil) (owner-id nil)) :lists/members/destroy_all)


(define-command lists/members/create_all (:post :list-type)
    (twitter-app-uri "lists/members/create_all.json")
    "Add multiple members to a list, by specifying a comma-separated list of member ids or screen names."
  :user_id "user" 
  :slug "Optional: You can identify a list by its slug instead of its numerical id."
  :list_id "Required : The id or slug of the list."
  :screen_name   "Optional: screen name of the user to be removed"
  :owner_screen_name "ptional: The screen name of the user who owns the list being requested by a slug"
  :owner_id "Optional: user ID of the user who owns the list being requested by a slug.")

(define-twitter-method lists-members-create-all (() &key (user-id nil) (slug nil) (list-id nil) (screen-name nil) (owner-screen-name nil) (owner-id nil)) :lists/members/create_all)

;;================================================================================================================================================================================

;;;TODO: Needs to be update....
(defun collect-user-list-members (list-owner list-id &key (max -1) (skip 0))
  (let ((ht (make-hash-table  :test 'equal :size 100)))
    (labels ((collect-it (lst)
	       (dolist (item lst)
		 (setf (gethash (twitter-user-id item) ht) item))))
      (with-cursor (:extractor #'cursor-user-users :controller #'cursor-user-next-cursor 
			       :collector #'collect-it :skip skip :max max :test (lambda() nil)) (user-list-members :list-owner list-owner :list-id list-id :cursor -1)))
    ht))




;;(defvar *HL* *) (create-user-list "haffies")
;;(defvar *H* (do-user-search "haffmans" ))
;;(maphash (lambda (k v) (format t "k :~S:~S~%" k v)) *H*)
;;(maphash (lambda (k v) (add-user-list-members (list-type-id *HL*) k)) *H*)


;; Doesn't seem to be working properly.
;;(defun add-members-to-user-list (list-id screen-names &key (list-owner (twitter-user-screen-name *twitter-user*)) (user-ids nil) )
;;  (apply 'twitter-op :?user/?list_id/create_all :user list-owner :list-id list-id :screen-name screen-names  :user-id user-ids))

