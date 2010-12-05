(in-package :twitter)


(define-element twitter-user ((status tweet)) 
  "This is the record for a twitter user; it stores both basic 
     and extended info."
  (id "A permanent unique id referencing an object, such as user or status" nil)
  (name "" nil)
  (screen-name "" nil)
  (password "" nil)
  (access-token nil nil)
  (description "" nil)
  (location "" nil)
  (profile-image-url "" nil)
  (url "" nil)
  (protected "" nil)
  (verified "" nil)
  (verified-profile "" nil)
  (contributors-enabled nil nil)
  (lang nil nil)
  ;; Embedded status
  (status "" nil)
  ;; Extended
  (geo "" nil)
  (geo-enabled "" nil)
  (created-at "" nil)
  (following "" nil)
  (followers-count "" nil)
  (statuses-count "" nil)
  (friends-count "" nil)
  (favourites-count "" nil)
  (notifications "" nil)
  (utc-offset "" nil)
  (time-zone "" nil)
  (profile-text-color "" nil)
  (profile-link-color "" nil)
  (profile-sidebar-color "" nil)
  (profile-sidebar-border-color "" nil)
  (profile-sidebar-fill-color "" nil)
  (profile-background-color "" nil)
  (profile-background-image-url "" nil)
  (profile-background-tile "" nil))

(defun get-user (ref)
  (when ref
    (if (twitter-user-p ref) ref
	(aif (gethash ref *twitter-users*) it
	     (show-user ref)))))


(defun lookup-twitter-user (rec)
  (let ((name (get-value :screen-name rec)))
    (gethash name *twitter-users*)))
  

(defmethod print-object ((user twitter-user) stream)
  (format stream "#<TWITTER-USER '~A'>" (twitter-user-screen-name user)))


(defmethod register-twitter-object ((user twitter-user))
  (setf (gethash (twitter-user-screen-name user) *twitter-users*) user))

(defmethod describe-object ((user twitter-user) stream)
  (format stream "Name: ~A ('~A') id:~A~%" 
	  (twitter-user-name user)
	  (twitter-user-screen-name user)
	  (twitter-user-id user))
  (format stream "Created at: ~A~%" (twitter-user-created-at user))
  (format stream "Description: ~A~%" (twitter-user-description user))
  (format stream "Counts: friends ~A, followers ~A, statuses ~A~%" 
	  (twitter-user-friends-count user)
	  (twitter-user-followers-count user)
	  (twitter-user-statuses-count user))
  (format stream "Location: ~A~%" (twitter-user-location user))
  (format stream "Time Zone: ~A~%" (twitter-user-time-zone user)))

(define-element cursor-user ((users (twitter-user)))
  "a cursor element "
  (id                  "" nil)
  (next-cursor-str     ""  nil)
  (previous-cursor-str "" nil)
  (next-cursor         ""  nil)
  (users               ""  nil)
  (previous-cursor     ""  nil))

(defmethod print-object ((ref cursor-user) stream)
  (format stream "#<TWITTER-CURSOR-USER '~A:~A'>" (cursor-user-next-cursor ref) (length (cursor-user-users ref)) ))

(defun lookup-cursor-user (rec)
  (declare (ignore rec)))

(defun print-cursor-user (ref)
  (format t "~A: ~A ~A~%" (cursor-user-previous-cursor ref) (cursor-user-next-cursor ref) (length (cursor-user-ids ref))))

(defmethod register-twitter-object ((ref cursor-user)))

;;
;; User resources
;;   users/show
;;   users/lookup
;;   users/search
;;   users/suggestions
;;   users/suggestions/twitter
;;   users/profile_image/twitter X
;;   statuses/friends
;;   statuses/followers

;; Is this a deprecated api call ? Don't see it documented..
;;(define-command user-show (:get-id :twitter-user)
;;    (twitter-users-uri "<id>.json")
;;    "Returns a single-status specified by the id parameter"
;;  :id "Required. The ID or screen name of a user."
;;  :email "Optional.  May be used in place of :id.")


(define-command users/show (:get :twitter-user)
    (twitter-app-uri "users/show.json")
    "Returns a single-status specified by the id parameter"
  :user_id "Required. The ID or screen name of a user."
  :screen_name "Optional.  May be used in place of :id."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities. ")

(define-command users/lookup (:get (:twitter-user) )
    (twitter-app-uri "users/lookup.json")
    "Return up to 100 users worth of extended information, specified by either ID, screen name, or combination of the two."
  :user_id "A comma separated list of user IDs, up to 100 are allowed in a single request."
  :screen_name "A comma separated list of screen names, up to 100 are allowed in a single request."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities. ")


(define-command users/search (:get (:twitter-user) )
    (twitter-app-uri "users/search.json")
    "Runs a search for users similar to Find People button on Twitter.com."
  :q "Required; The search query to run against people search."
  :per_page "The number of people to retrieve. Maxiumum of 20 allowed per page."
  :page "Specifies the page of results to retrieve."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities")

;;TODO Need suggestions element
(define-command users/suggestions (:get (:identity))
    (twitter-app-uri "users/suggestions.json")
    "Access to Twitter's suggested user list. This returns the list of suggested user categories. ")

;;TODO unable to parse returns which contains user elements and slugs (?); 
;;should be able to use slug as substitution

(define-command users/suggestions/?slug (:get-id (:identity) )
    (twitter-app-uri "users/suggestions/<id>.json")
    "Access the users in a given category of the Twitter suggested user list."
  :id "The short name of list or a category" )

;;
;;This resource does not return JSON or XML, but instead returns a 302 redirect to the actual image resource.
;;This method should only be used by application developers to lookup or check the profile image URL for a user. 
;;This method must not be used as the image source URL presented to users of your application.
;; Doesn't work with twitter-op because it doesn't return json.

(define-command users/profile-image/?screen-name (:get-id (:identity) )
    (twitter-app-uri "users/profile_image/<id>.json")
    "Access the profile image in various sizes for the user with the indicated screen_name. "
  :id   "Required ; screen name of the user" 
  :size "Optional; Specifies the size of image to fetch.")



;; TODO : cursor doesn't work : this does not return a "twitter-user" list.
;; Probably better to create a seperate 'cursor' command ?
;;

(define-command statuses/friends (:get :cursor-user )
    (twitter-app-uri "statuses/friends.json")
    "Returns the authenticating user's friends, each with current status inline. They are ordered by the order in which they were added as friends. 
     It's also possible to request another user's recent friends list via the id parameter below."
  :user_id "The ID of the user for whom to return results for." 
  :screen_name "The screen name of the user for whom to return results for."
  :cursor "Breaks the results into pages. This is recommended for users who are following many users. Provide a value of -1 to begin paging. 
          Provide values as returned in the response body's next_cursor and previous_cursor attributes to page back and forth in the list."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities." )

;;(define-command statuses/followers (:get :identity )

(define-command statuses/followers (:get :cursor-user )
    (twitter-app-uri "statuses/followers.json")
    "Returns the authenticating user's followers, each with current status inline. "
  :user_id "The ID of the user for whom to return results for." 
  :screen_name "The screen name of the user for whom to return results for."
  :cursor "Breaks the results into pages. This is recommended for users who are following many users. Provide a value of -1 to begin paging. 
          Provide values as returned in the response body's next_cursor and previous_cursor attributes to page back and forth in the list."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities." )

;; ---level 1 api --------------------------------

(defun show-user (screen-name &rest args &key (user-id nil) (include-entities nil) )
  (declare (ignore user-id include-entities))
  (apply 'twitter-op :users/show :screen_name screen-name  args))

(defun show-user-by-id (user-id &rest args &key (screen-name nil) (include-entities nil) )
  (declare (ignore screen-name include-entities))
  (apply 'twitter-op :users/show :user_id user-id  args))

;; probably should built in some resiliency in that the user can pass in a list ??
(defun lookup-users (screen-names &rest args &key (user-id nil) (include-entities nil) )
  (declare (ignore user-id include-entities))
  (apply 'twitter-op :users/lookup :screen-name screen-names args))

;; does the query need to be url encoded ????
;; used url-rewrite for encoding; gets same result set as twitter for simple name queries..
;;
(defun search-users (query &rest args &key (per-page nil) (page nil) (include-entities nil) )
  (declare (ignore per-page page include-entities))
  (apply 'twitter-op :users/search :q query  args))


(defun friends-statuses (&rest args &key (user-id nil) (screen-name nil) (include-entities nil) (cursor -1) )
  (declare (ignore  user-id screen-name include-entities cursor))
  (apply 'twitter-op :statuses/friends  args))

;;cursor is required to get results as a twitter-cursor-user..
(defun followers-statuses (&rest args &key (user-id nil) (screen-name nil) (include-entities nil) (cursor -1) )
  (declare (ignore user-id screen-name include-entities cursor))
  (apply 'twitter-op :statuses/followers  args))

;;--------------------------------------------------------------


(defun collect-follower-statuses (screen-name &key (max -1) (skip 0))
  (let ((ht (make-hash-table  :test 'equal :size 100)))
    (labels ((collect-it (lst)
	       (dolist (item lst)
		 (setf (gethash (twitter-user-id item) ht) item))))
      (with-cursor (:extractor #'cursor-user-users :controller #'cursor-user-next-cursor 
			       :collector #'collect-it :skip skip :max max :test (lambda() nil)) (followers-statuses :screen-name screen-name )))
    ht))


(defun collect-friend-statuses (screen-name &key (max -1) (skip 0))
  (let ((ht (make-hash-table  :test 'equal :size 100)))
    (labels ((collect-it (lst)
	       (dolist (item lst)
		 (setf (gethash (twitter-user-id item) ht) item))))
      (with-cursor (:extractor #'cursor-user-users :controller #'cursor-user-next-cursor 
			       :collector #'collect-it :skip skip :max max :test (lambda() nil)) (friends-statuses :screen-name screen-name )))
    ht))
