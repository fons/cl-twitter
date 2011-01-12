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
  "a twitter user type"
  (id   "" nil)
   (mode  "" nil) 
   (description "" nil) 
   (slug "" nil)
   (uri  "" nil) 
   (member-count  "" nil) 
   (id-str  "" nil)
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



;;--------------------------------------------------------------------------------------------------------------------------------------------------------------------
;; Not sure what the point of the user field is. Testing seems to suggest that it matters littel as long as the user exists.
;; Lists are only created for the registered user.

(define-command ?user/lists (:post-user :list-type)
    (twitter-app-uri "<user>/lists.json")
    "Creates a new list for the authenticated user. Accounts are limited to 20 lists."
  :user "Required; authenticated user the list is created for"
  :name "Required : name of the list"
  :mode "Whether your list is public or private. Values can be public or private. If no mode is specified the list will be public."
  :description "The description to give the list.")

(define-command ?user/lists/?id (:post-user-id :list-type)
    (twitter-app-uri "<user>/lists/<id>.json")
    "Creates a new list for the authenticated user. Accounts are limited to 20 lists."
  :user "Required; authenticated user the list is created for"
  :id     "required : list id"
  :name "Required : name of the list"
  :mode "Whether your list is public or private. Values can be public or private. If no mode is specified the list will be public."
  :description "The description to give the list.")

(define-command ?user/lists/_get (:get-user :cursor-user-lists)
    (twitter-app-uri "<user>/lists.json")
    "List the lists of the specified user. Private lists will be included if the authenticated users is the same as the user who's lists are being returned."
  :user "Required; authenticated user the list is created for"
  :cursor     "Breaks the results into pages. A single page contains 20 lists. Provide a value of -1 to begin paging.")
  

(define-command ?user/lists/?id/_get (:get-user-id :list-type)
    (twitter-app-uri "<user>/lists/<id>.json")
   " Show the specified list. Private lists will only be shown if the authenticated user owns the specified list."
  :user "Required; authenticated user the list is created for"
  :id     "Required: list id")

(define-command ?user/lists/?id/_delete (:post-user-id :identity)
    (twitter-app-uri "<user>/lists/<id>.json")
   " Show the specified list. Private lists will only be shown if the authenticated user owns the specified list."
  :user "Required; authenticated user the list is created for"
  :id     "Required: list id"
  :_method "should be delete")
 
(define-command ?user/lists/?id/statuses (:get-user-id (tweet))
    (twitter-app-uri "<user>/lists/<id>/statuses.json")
    "Show tweet timeline for members of the specified list."
  :user "Required; authenticated user the list is created for"
  :id     "Required: list id"
  :since_id "Returns results with an ID greater than (that is, more recent than) the specified ID. "
  :max_id   "Returns results with an ID less than (that is, older than) or equal to the specified ID."
  :per_page "Specifies the page of results to retrieve."
  :page "Specifies the page of results to retrieve."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities ")

(define-command ?user/lists/memberships (:get-user :cursor-user-lists)
    (twitter-app-uri "<user>/lists/memberships.json")
    "List the lists the specified user has been added to."
  :user     "Required; authenticated user the list is created for"
  :cursor  "Breaks the results into pages. A single page contains 20 lists. Provide a value of -1 to begin paging. ")

(define-command ?user/lists/subscriptions (:get-user :cursor-user-lists) 
    (twitter-app-uri "<user>/lists/subscriptions.json")
    " List the lists the specified user follows."
  :user     "Required; authenticated user the list is created for"
  :cursor  "Breaks the results into pages. A single page contains 20 lists. Provide a value of -1 to begin paging. ")


;;---------------------------------------------------------------------------------------------------------------

(defun create-user-list (name &rest args &key (user (twitter-user-name *twitter-user*)) (mode "public") (description nil))
  (declare (ignore user mode description))
  (apply 'twitter-op :?user/lists  :name name args))
		    
(defun update-user-list (id name &key (list-owner (twitter-user-screen-name *twitter-user*))  (mode "public") (description nil))
  (apply 'twitter-op :?user/lists/?id  :id id :name name :user list-owner :mode mode :description description nil))
;;
(defun get-user-lists (&key (list-owner   (twitter-user-screen-name *twitter-user*))  (cursor -1))
  (apply 'twitter-op :?user/lists/_get   :user list-owner :cursor cursor nil))

(defun get-user-list (id &key (list-owner (twitter-user-screen-name *twitter-user*)))
  (apply 'twitter-op :?user/lists/?id/_get :id id :user list-owner  nil)) 

(defun delete-user-lists (id &key (list-owner (twitter-user-screen-name *twitter-user*)))
  (apply 'twitter-op :?user/lists/?id/_delete :id id :user list-owner  :_method "DELETE" nil)) 


(defun statuses-user-lists (&rest args &key (list-owner (twitter-user-screen-name *twitter-user*)) (id nil)  (page 1) (per-page 20)  (since-id nil) (max-id nil) (include-entities nil) )
  (declare (ignore id per-page  since-id max-id include-entities))
  (apply 'twitter-op :?user/lists/?id/statuses :user list-owner :page page (strip-keywords '(:cursor :list-owner) args) ))

(defun memberships-user-lists ( &key (screen-name (twitter-user-screen-name *twitter-user*))  (cursor -1))
  (apply 'twitter-op :?user/lists/memberships :user screen-name :cursor cursor nil))

(defun subscriptions-user-lists ( &key (screen-name (twitter-user-screen-name *twitter-user*))  (cursor -1))
  (apply 'twitter-op :?user/lists/subscriptions :user screen-name :cursor cursor nil))

;;--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

(defun collect-user-lists (list-owner &key (max -1) (skip 0))
  (let ((ht (make-hash-table  :test 'equal :size 100)))
    (labels ((collect-it (lst)
	       (dolist (item lst)
		 (setf (gethash (list-type-id item) ht) item))))
      (with-cursor (:extractor #'cursor-user-lists-lists :controller #'cursor-user-lists-next-cursor 
			       :collector #'collect-it :skip skip :max max :test (lambda() nil)) (get-user-lists :list-owner list-owner )))
    ht))

(defun collect-user-list-memberships (screen-name &key (max -1) (skip 0))
  (let ((ht (make-hash-table  :test 'equal :size 100)))
    (labels ((collect-it (lst)
	       (dolist (item lst)
		 (setf (gethash (list-type-id item) ht) item))))
      (with-cursor (:extractor #'cursor-user-lists-lists :controller #'cursor-user-lists-next-cursor 
			       :collector #'collect-it :skip skip :max max :test (lambda() nil)) (memberships-user-lists :screen-name screen-name )))
    ht))

(defun collect-user-list-subscriptions (screen-name &key (max -1) (skip 0))
  (let ((ht (make-hash-table  :test 'equal :size 100)))
    (labels ((collect-it (lst)
	       (dolist (item lst)
		 (setf (gethash (list-type-id item) ht) item))))
      (with-cursor (:extractor #'cursor-user-lists-lists :controller #'cursor-user-lists-next-cursor 
			       :collector #'collect-it :skip skip :max max :test (lambda() nil)) (subscriptions-user-lists :screen-name screen-name )))
    ht))
  
;;I use a hashtable because pages appear to return identical results.. 
;;stop when the hash table has no more new elements
(defun collect-user-list-statuses (list-owner list-id &key (max -1) (skip 0) (container (make-hash-table  :test 'equal :size 100)))
  (collect-tweets (:max max :skip skip :container container) statuses-user-lists :list-owner list-owner :id list-id :page 1))

;;---------------------------------------------------------------------------------------------------------------------------------------
(defun user-list-timeline (list-owner &key (max-per-list 1) )
  (let ((user-list-ids (mapcar #'list-type-id (cursor-user-lists-lists (get-user-lists :list-owner list-owner))))
	  (container (make-hash-table  :test 'equal :size 100)))
	(dolist (list-id user-list-ids)
	  (setf container (collect-user-list-statuses list-owner list-id :max max-per-list :container container)))
	container))
      
(defun member-list-timeline (user &key (max-per-list 1) )
  (labels ((user-list-data (ul)
	     (list (twitter-user-screen-name (list-type-user ul)) (list-type-slug ul)))) 
    (let ((ulst (mapcar #'user-list-data (cursor-user-lists-lists (memberships-user-lists :screen-name user))))
	  (container (make-hash-table  :test 'equal :size 100)))
      (dolist (item ulst)
	(setf container (collect-user-list-statuses (car item) (cadr item) :max max-per-list :container container)))
      container)))
	
(defun subscriber-list-timeline (user &key (max-per-list 1) )
  (labels ((user-list-data (ul)
	     (list (twitter-user-screen-name (list-type-user ul)) (list-type-slug ul)))) 
    (let ((ulst (mapcar #'user-list-data (cursor-user-lists-lists (subscriptions-user-lists :screen-name user))))
	  (container (make-hash-table  :test 'equal :size 100)))
      (dolist (item ulst)
	(setf container (collect-user-list-statuses (car item) (cadr item) :max max-per-list :container container)))
      container)))
	
