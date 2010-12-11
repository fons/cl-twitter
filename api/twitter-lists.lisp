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
;; This required both the replacement of :id and :user in the url.
;;
;;============>> TBD
#|
TWIT> (apply 'twitter-op :?user/lists/_get :user "fogus"  nil)
((:LISTS
  ((:MODE . "public") (:SLUG . "gliders") (:URI . "/fogus/gliders")
   (:MEMBER-COUNT . 9) (:ID-STR . "6504146") (:FULL-NAME . "@fogus/gliders")
   (:USER
    (:PROFILE-BACKGROUND-IMAGE-URL
     . "http://s.twimg.com/a/1291760612/images/themes/theme3/bg.gif")
    (:FAVOURITES-COUNT . 3) (:NOTIFICATIONS)
    (:PROFILE-SIDEBAR-BORDER-COLOR . "D3D2CF") (:CONTRIBUTORS-ENABLED)
    (:STATUSES-COUNT . 5291) (:PROFILE-BACKGROUND-TILE)
    (:PROFILE-IMAGE-URL
     . "http://a0.twimg.com/profile_images/1107443424/_videodrome_normal.jpeg")
    (:LOCATION . "DCA, IAD") (:LISTED-COUNT . 150) (:FOLLOWERS-COUNT . 1275)
    (:ID-STR . "14375110") (:LANG . "en")
    (:PROFILE-BACKGROUND-COLOR . "EDECE9") (:URL . "http://fogus.me")
    (:SCREEN-NAME . "fogus") (:VERIFIED) (:TIME-ZONE . "Quito")
    (:PROFILE-TEXT-COLOR . "634047") (:FOLLOWING) (:FRIENDS-COUNT . 168)
    (:PROTECTED) (:SHOW-ALL-INLINE-MEDIA) (:GEO-ENABLED)
    (:CREATED-AT . "Sun Apr 13 14:06:12 +0000 2008")
    (:PROFILE-LINK-COLOR . "088253")
    (:DESCRIPTION
     . "Programmer.  Co-author of The Joy of Clojure http://joyofclojure.com")
    (:NAME . "fogus") (:PROFILE-USE-BACKGROUND-IMAGE . T) (:ID . 14375110)
    (:FOLLOW-REQUEST-SENT) (:UTC-OFFSET . -18000)
    (:PROFILE-SIDEBAR-FILL-COLOR . "E3E2DE"))
   (:SUBSCRIBER-COUNT . 0) (:FOLLOWING) (:DESCRIPTION . "?ber-hackers")
   (:NAME . "gliders") (:ID . 6504146)))
 (:NEXT-CURSOR . 0) (:PREVIOUS-CURSOR . 0) (:NEXT-CURSOR-STR . "0")
 (:PREVIOUS-CURSOR-STR . "0"))
TWIT> 
((:MODE . "public") (:DESCRIPTION . "") (:SLUG . "test")
 (:URI . "/domore_eliza/test") (:MEMBER-COUNT . 0) (:ID-STR . "31367103")
 (:FULL-NAME . "@domore_eliza/test")

 (:USER (:CREATED-AT . "Fri Oct 22 10:05:49 +0000 2010")
  (:PROFILE-LINK-COLOR . "0084B4") (:DESCRIPTION . " a chatty katty")
  (:PROFILE-USE-BACKGROUND-IMAGE . T) (:FRIENDS-COUNT . 2)
  (:PROFILE-SIDEBAR-FILL-COLOR . "DDEEF6") (:STATUSES-COUNT . 30)
  (:PROFILE-BACKGROUND-IMAGE-URL
   . "http://s.twimg.com/a/1291318259/images/themes/theme1/bg.png")
  (:FAVOURITES-COUNT . 1) (:NOTIFICATIONS)
  (:PROFILE-SIDEBAR-BORDER-COLOR . "C0DEED") (:FOLLOW-REQUEST-SENT)
  (:PROFILE-BACKGROUND-TILE)
  (:PROFILE-IMAGE-URL
   . "http://s.twimg.com/a/1291318259/images/default_profile_1_normal.png")
  (:LOCATION) (:FOLLOWERS-COUNT . 1) (:ID-STR . "206126369") (:LANG . "en")
  (:VERIFIED) (:PROFILE-BACKGROUND-COLOR . "0099ff") (:PROTECTED) (:URL)
  (:SCREEN-NAME . "domore_eliza") (:CONTRIBUTORS-ENABLED)
  (:NAME . "domore eliza") (:SHOW-ALL-INLINE-MEDIA) (:LISTED-COUNT . 0)
  (:GEO-ENABLED) (:TIME-ZONE . "Eastern Time (US & Canada)")
  (:PROFILE-TEXT-COLOR . "333333") (:ID . 206126369) (:FOLLOWING)
  (:UTC-OFFSET . -18000))

 (:SUBSCRIBER-COUNT . 0) (:NAME . "test") (:ID . 31367103) (:FOLLOWING))
|#



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


(defun lookup-list-type (rec)
  (declare (ignore rec)))

(defun print-list-type (ref)
  (format t "~A:~A: ~A~%" 
	  (list-type-full-name ref)
	  (list-type-id ref)
	  (list-type-slug ref)))

(defmethod register-twitter-object ((ref list-type)))
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


(defun lookup-cursor-user-lists (rec)
  (declare (ignore rec)))

(defun print-list-type (ref)
  (format t "~A:~A: ~A~%" (cursor-user-lists-previous-cursor ref) (cursor-user-lists-next-cursor ref) (length (cursor-user-lists-lists ref) )))


(defmethod register-twitter-object ((ref cursor-user-lists)))

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
  

(define-command ?user/lists/?id/_get (:get-user-id :identity)
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

(define-command ?user/lists/subscriptions (:get-user :identity) 
    (twitter-app-uri "<user>/lists/subscriptions.json")
    " List the lists the specified user follows."
  :user     "Required; authenticated user the list is created for"
  :cursor  "Breaks the results into pages. A single page contains 20 lists. Provide a value of -1 to begin paging. ")


;;---------------------------------------------------------------------------------------------------------------

(defun create-user-list (name &rest args &key (user (twitter-user-name *twitter-user*)) (mode "public") (description nil))
  (declare (ignore user mode description))
  (apply 'twitter-op :?user/lists  :name name args))
		    
(defun update--user-list (id name &key (user (twitter-user-screen-name *twitter-user*))  (mode "public") (description nil))
  (apply 'twitter-op :?user/lists/?id  :id id :name name :user user :mode mode :description description nil))

(defun get-user-lists (&key (screen-name   (twitter-user-screen-name *twitter-user*))  (cursor -1))
  (apply 'twitter-op :?user/lists/_get   :user screen-name :cursor cursor nil))

(defun collect-user-lists (screen-name &key (max -1) (skip 0))
  (let ((ht (make-hash-table  :test 'equal :size 100)))
    (labels ((collect-it (lst)
	       (dolist (item lst)
		 (setf (gethash (list-type-id item) ht) item))))
      (with-cursor (:extractor #'cursor-user-lists-lists :controller #'cursor-user-lists-next-cursor 
			       :collector #'collect-it :skip skip :max max :test (lambda() nil)) (get-user-lists :screen-name screen-name )))
    ht))

(defun delete-user-lists (id &key (user (twitter-user-screen-name *twitter-user*)))
  (apply 'twitter-op :?user/lists/?id/_delete :id id :user user  :_method "DELETE" nil)) 

(defun membership-user-lists ( &key (screen-name (twitter-user-screen-name *twitter-user*))  (cursor -1))
  (apply 'twitter-op :?user/lists/memberships :user screen-name :cursor cursor nil))

(defun collect-user-list-memberships (screen-name &key (max -1) (skip 0))
  (let ((ht (make-hash-table  :test 'equal :size 100)))
    (labels ((collect-it (lst)
	       (dolist (item lst)
		 (setf (gethash (list-type-id item) ht) item))))
      (with-cursor (:extractor #'cursor-user-lists-lists :controller #'cursor-user-lists-next-cursor 
			       :collector #'collect-it :skip skip :max max :test (lambda() nil)) (membership-user-lists :screen-name screen-name )))
    ht))
  
;;page is called cursor so i can work with the with-cursor macro.
(defun statuses-user-lists (&key (screen-name (twitter-user-screen-name *twitter-user*)) (id nil)  (cursor 0) (per-page 20))
  (apply 'twitter-op :?user/lists/?id/statuses :user screen-name :id id :page cursor :per-page per-page nil))

;;(length (apply 'twitter-op :?user/lists/?id/statuses :user "fogus" :id 6504146 :page 1 nil))
(defun collect-user-list-statuses (screen-name id &key (max -1) (skip 0))
  (let ((lst ())
	(page 0))
    (labels ((collect-it (l)
	       (setf lst (nconc lst l)))
	     (next-page (item)
	       (declare (ignore item))
	       (incf page)))
      (with-cursor (:skip skip :max max :extractor #'identity :controller #'next-page :collector #'collect-it :test #'rate-limit-exceeded ) (statuses-user-lists :screen-name screen-name :id id)))
    lst))


