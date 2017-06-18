(in-package :twitter)

;;
;; List resources
;;    :user/lists
;;    :user/lists/:id
;;    :user/lists
;;    :user/lists/:id
;;    :user/lists/:id
;;    :user/lists/:id/statuses
;;    :user/lists/memberships
;;    :user/lists/subscriptions
;;

(define-element list-type ( (user twitter-user))
  "a twitter list type"
  (id   "" nil)
  (name "" nil)
   (mode  "" nil) 
   (description "" nil) 
   (slug "" nil)
   (uri  "" nil) 
   (subscriber-count  "" nil) 
   (member-count  "" nil) 
   (id-str  "" nil)
   (created-at "" nil)
   (following "" nil)
   (full-name  "" nil)
   (user "" nil))

(defmethod print-object ((ref list-type) stream)
  (format stream "#<TWITTER-LIST-TYPE '~A:~A'>" (list-type-id ref) (list-type-full-name ref)))

(defun print-list-type (ref)
  (format t "~A:~A: ~A~%" 
	  (list-type-full-name ref)
	  (list-type-id ref)
	  (list-type-slug ref)))

;;---------------------------------------------------------------

(define-element cursor-user-lists ((lists (list-type)))
  "a twitter user type"
  (id   "" nil)
  (lists "" nil)
  (next-cursor-str     ""  nil)
  (previous-cursor-str "" nil)
  (next-cursor         ""  nil)
  (previous-cursor     ""  nil))


(defmethod print-object ((ref cursor-user-lists) stream)
  (format stream "#<TWITTER-CURSOR-USER-LISTS '~A:~A:~A'>" (cursor-user-lists-previous-cursor ref) (cursor-user-lists-next-cursor ref) (length (cursor-user-lists-lists ref) )))

(defun print-cursor-user-lists (ref)
  (format t "~A:~A: ~A~%" (cursor-user-lists-previous-cursor ref) (cursor-user-lists-next-cursor ref) (length (cursor-user-lists-lists ref) )))

;;-------------------complete revamp------------


(define-command lists/list (:get (:list-type))
    (twitter-app-uri "lists/list.json")
    "Returns all lists the authenticating or specified user subscribes to, including their own."
  :user_id    "optional : The ID of the user for whom to return results for."
  :screen_name "optional : The screen name of the user for whom to return results for. "
  :reverse "optional:Set this to true if you would like owned lists to be returned first. ")



(define-command lists/statuses (:get (tweet))
    (twitter-app-uri "lists/statuses.json")
    "Show tweet timeline for members of the specified list."
  :list_id     "Required: list id"
  :slug       "Required: You can identify a list by its slug instead of its numerical id.If you decide to do so, note that you’ll also have to specify the list owner using the owner_id or owner_screen_name parameters."
  :owner_screen_name "Optional: The screen name of the user who owns the list being requested by a slug."
  :owner_id "Optional: The user ID of the user who owns the list being requested by a slug."
  :since_id "Returns results with an ID greater than (that is, more recent than) the specified ID. "
  :max_id   "Returns results with an ID less than (that is, older than) or equal to the specified ID."
  :count   "Specifies the page of results to retrieve."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities "
  :include_rts "When set to either true, t or 1, each tweet will include a node called entities ")

(define-command lists/memberships (:get :cursor-user-lists)
    (twitter-app-uri "lists/memberships.json")
    "List the lists the specified user has been added to."
  :user_id     "Optional: The ID of the user for whom to return results for."
  :screen_name "Optional:The screen name of the user for whom to return results for."
  :count "Optional: The amount of results to return per page. default 20; max 1000" 
  :cursor  "Breaks the results into pages. A single page contains 20 lists. Provide a value of -1 to begin paging. "
  :filter_to_owned_lists "Optional: When set to true, t or 1, will return just lists the authenticating user owns, and the user represented by user_id or screen_name is a member of.")


(define-command lists/subscriptions (:get :cursor-user-lists) 
    (twitter-app-uri "lists/subscriptions.json")
    " List the lists the specified user follows."
  :user_id     "Optional: The ID of the user for whom to return results for."
  :screen_name "Optional:The screen name of the user for whom to return results for."
  :count "Optional: The amount of results to return per page. default 20; max 1000" 
  :cursor  "Breaks the results into pages. A single page contains 20 lists. Provide a value of -1 to begin paging. ")

(define-command lists/show (:get :list-type) 
    (twitter-app-uri "lists/show.json")
    "Returns the specified list. Private lists will only be shown if the authenticated user owns the specified list."
  :list_id "Required: The numerical id of the list."
  :slug "Required: You can identify a list by its slug instead of its numerical id. If you decide to do so, note that you’ll also have to specify the list owner using the owner_id or owner_screen_name parameters."
  :owner_screen_name "Optional: The screen name of the user who owns the list being requested by a slug."
  :owner_id "Optional: The user ID of the user who owns the list being requested by a slug.")


(define-command lists/create (:post :list-type)
    (twitter-app-uri "lists/create.json")
    "Creates a new list for the authenticated user. Accounts are limited to 20 lists."
  :name "Required : name of the list"
  :mode "Whether your list is public or private. Values can be public or private. If no mode is specified the list will be public."
  :description "The description to give the list.")

(define-command lists/update (:post :list-type)
    (twitter-app-uri "lists/update.json")
    "Updates a list for the authenticated user. Accounts are limited to 20 lists."
  :list_id "Required: The numerical id of the list."
  :slug "Required: You can identify a list by its slug instead of its numerical id. You’ll also have to specify the list owner using the owner_id or owner_screen_name parameters."
  :name "Optional : The name for the list."
  :mode "Optional: Whether your list is public or private. Values can be public or private. If no mode is specified the list will be public."
  :description "Optional: The description to give the list."
  :owner_screen_name "Optional: The screen name of the user who owns the list being requested by a slug."
  :owner_id "Optional: The user ID of the user who owns the list being requested by a slug.")

(define-command lists/delete (:post :list-type)
    (twitter-app-uri "lists/delete.json")
    "Updates a list for the authenticated user. Accounts are limited to 20 lists."
  :list_id "Required: The numerical id of the list."
  :slug "Required: You can identify a list by its slug instead of its numerical id. You’ll also have to specify the list owner using the owner_id or owner_screen_name parameters."
  :owner_screen_name "Optional: The screen name of the user who owns the list being requested by a slug."
  :owner_id "Optional: The user ID of the user who owns the list being requested by a slug.")

(define-command lists/ownerships (:get :cursor-user-lists)
    (twitter-app-uri "lists/ownerships.json")
    "Returns the lists owned by the specified Twitter user."
  :user_id "Optional: The ID of the user for whom to return results for."
  :screen_name "Optional: The screen name of the user for whom to return results for."
  :count "Optional: The amount of results to return per page. Defaults to 20. No more than 1000 results will ever be returned in a single page."
  :cursor "Optional Breaks the results into pages. Provide a value of -1 to begin paging. ")

(define-twitter-method lists-list ( () &key (user-id nil) (screen-name nil) (reverse nil)) :lists/list )
(define-twitter-method lists-statuses (() &key (list-id nil) (slug nil) (owner-screen-name nil) (owner-id nil) (since-id nil) (max-id nil) (count nil) (include-entities nil) (include-rts nil)) :lists/statuses )
(define-twitter-method lists-memberships (() &key (user-id nil) (screen-name nil) (count nil) (cursor nil) (filter-to-owned-lists nil)) :lists/memberships )
(define-twitter-method lists-subscriptions (() &key (user-id nil) (screen-name nil) (count nil) (cursor nil) ) :lists/subscriptions )
(define-twitter-method lists-show (() &key (list-id nil) (slug nil) (owner-screen-name nil) (owner-id nil)) :lists/show)
(define-twitter-method lists-create ((name) &key (mode nil) (description nil)) :lists/create :name)
(define-twitter-method lists-update (() &key (list-id nil) (slug nil) (name nil) (mode nil) (description nil) (owner-screen-name nil) (owner-id nil)) :lists/update)
(define-twitter-method lists-delete (() &key (list-id nil) (slug nil) (owner-screen-name nil) (owner-id nil)) :lists/delete)
(define-twitter-method lists-ownerships (() &key (user-id nil) (screen-name nil) (count nil) (cursor nil) ) :lists/ownerships )


;;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


(defun collect-memberships (screen-name &key (max -1) (skip 0))
  (let ((ht (make-hash-table  :test 'equal :size 100)))
    (labels ((collect-it (lst)
	       (dolist (item lst)
		 (setf (gethash (list-type-id item) ht) item))))
      (with-cursor (:extractor #'cursor-user-lists-lists :controller #'cursor-user-lists-next-cursor 
			       :collector #'collect-it :skip skip :max max :test (lambda() nil)) (lists-memberships :screen-name screen-name )))
    ht))

(defun collect-subscriptions (screen-name &key (max -1) (skip 0))
  (let ((ht (make-hash-table  :test 'equal :size 100)))
    (labels ((collect-it (lst)
	       (dolist (item lst)
		 (setf (gethash (list-type-id item) ht) item))))
      (with-cursor (:extractor #'cursor-user-lists-lists :controller #'cursor-user-lists-next-cursor 
			       :collector #'collect-it :skip skip :max max :test (lambda() nil)) (lists-subscriptions :screen-name screen-name )))
    ht))


(defun collect-tweets-2 (list-id &key (since-id nil) (max-id nil) (count 20) (include-entities nil) (include-rts nil))
  (collect-results nil (set-depth count)
                   #'lists-statuses
                   (arguments :list-id list-id
                              :count (set-count count)
                              :max-id max-id
                              :since-id since-id
                              :include-entities include-entities
                              :include-rts include-rts)))


;;---------------------------------------------------------------------------------------------------------------------------------------

      
(defun list-timeline (func user &key (max-per-list 20) )
  (labels ((user-list-data (ul);
             (list-type-id ul)))
    (let ((ulst (mapcar #'user-list-data (cursor-user-lists-lists (funcall func :screen-name user))))
	  (container nil))
      (dolist (item ulst)
        (handler-case
            (setf container (nconc (collect-tweets-2 item :count max-per-list) container))
          (error (c)
            (format t "error : ~S~%" c))))
      container)))


(defun list-memberships-timeline (user &key (max-per-list 20) )
  (list-timeline #'lists-memberships user :max-per-list max-per-list))


(defun list-subscriptions-timeline (user &key (max-per-list 20) )
  (list-timeline #'lists-subscriptions user :max-per-list max-per-list))

	
