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

(defmethod print-object ((user twitter-user) stream)
  (format stream "#<TWITTER-USER '~A:~A'>" (twitter-user-id user) (twitter-user-screen-name user)))

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


(defun print-cursor-user (ref)
  (format t "~A: ~A ~A~%" (cursor-user-previous-cursor ref) (cursor-user-next-cursor ref) (length (cursor-user-id ref))))

;;. "staff-picks") (:NAME . "Staff Picks"))
;;((:SIZE . 81) (:SLUG . "mlb") (:NAME . "MLB"))
;;((:SIZE . 98) (:SLUG . "nascar") (:NAME . "NASCAR"))
;;((:SIZE . 62) (:SLUG . "nhl") (:NAME . "NHL"))
;;((:SIZE . 122) (:SLUG . "pga") (:NAME . "PGA"))
;;((:SIZE . 9) (:SLUG . "march-madness") (:NAME . "March Madness"))

(define-element suggestion ()
  "a suggestion element "
  (size                  "" nil)
  (slug           ""  nil)
  (name          "" nil))

(defmethod print-object ((ref suggestion) stream)
  (format stream "#<TWITTER-SUGGESTION '~A:~A:~A'>" (suggestion-name ref) (suggestion-slug ref) (suggestion-size ref)))


(defun print-suggestion (ref)
  (format t "~A ~A~%" (suggestion-name ref) (suggestion-slug ref)))


;;((:USERS
;;  ((:ID . 19923144) (:ID-STR . "19923144") (:NAME . "NBA")
;;   (:SCREEN-NAME . "NBA") (:LOCATION . "") (:PROFILE-LOCATION)
;;   (:DESCRIPTION
;;    . "News and notes directly from the National Basketball Association.")
;;   (:URL . "http://t.co/sT8h1uaARA")
;;   (:ENTITIES

(define-element user-list ((users (twitter-user)))
  "a suggestion element "
  (users     "" nil)
  (size      "" nil)
  (slug      ""  nil)
  (name      "" nil))

(defmethod print-object ((ref user-list) stream)
  (format stream "#<TWITTER-USER-LIST '~A:~A:~A:~A'>" (user-list-name ref) (user-list-slug ref) (user-list-size ref) (user-list-users ref)))


(defun print-user-list (ref)
  (format t "~A ~A~%" (user-list-name ref) (user-list-slug ref)))
  
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
  :screen_name "Required.  May be used in place of :user_id."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities. ")


(define-command users/lookup (:get (:twitter-user) )
    (twitter-app-uri "users/lookup.json")
    "Return up to 100 users worth of extended information, specified by either ID, screen name, or combination of the two."
  :user_id     "A comma separated list of user IDs, up to 100 are allowed in a single request."
  :screen_name "A comma separated list of screen names, up to 100 are allowed in a single request."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities. ")

(define-command users/search (:get (:twitter-user) )
    (twitter-app-uri "users/search.json")
    "Runs a search for users similar to Find People button on Twitter.com."
  :q "Required; The search query to run against people search."
  :count "The number of people to retrieve. Maxiumum of 20 allowed per page."
  :page "Specifies the page of results to retrieve."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities")

(define-command users/profile-banner (:get (:identity))
    (twitter-app-uri "users/profile_banner.json")
    "Returns a map of the available size variations of the specified user’s profile banner"
  :user_id "Optional. The ID or screen name of a user."
  :screen_name "Optional.  May be used in place of :user_id."
  )

(define-command users/suggestions (:get (:suggestion))
    (twitter-app-uri "users/suggestions.json")
    "Access to Twitter's suggested user list. This returns the list of suggested user categories. "
  :lang "optional. Restricts the suggested categories to the requested language"
  )

(define-command users/suggestions/?id (:get-id :user-list)
    (twitter-app-uri "users/suggestions/<id>.json")
    "Access the users in a given category of the Twitter suggested user list."
  :id "The short name of list or a category"
  :lang "optional. Restricts the suggested categories to the requested language"
  )


(define-command users/suggestions/?id/members (:get-id (:tweet))
    (twitter-app-uri "users/suggestions/<id>/members.json")
    "Access the users in a given category of the Twitter suggested user list and return their most recent status if they are not a protected user"
    :id     "Required: slug Access the users in a given category of the Twitter suggested user list and return their most recent status if they are not a protected user."
    )

;;
;;Spam Reporting resources
;;   report_spam
(define-command users/report-spam (:post :twitter-user)
    (twitter-app-uri "users/report_spam.json")
    "The user specified in the id is blocked by the authenticated user and reported as a spammer."
  :user_id "Optional :The ID of the user for whom to return results for. Helpful for disambiguating when a valid user ID is also a valid screen name."
  :screen_name "Optional : The screen name of the user for whom to return results for. Helpful for disambiguating when a valid screen name is also a user ID.")



;; ---level 1 api --------------------------------

(define-twitter-method users-show (() &key (screen-name nil) (user-id nil)  (include-entities t)) :users/show)
(define-twitter-method users-lookup (()  &key (screen-name nil) (user-id nil) (include-entities t)) :users/lookup )
(define-twitter-method users-search ((query) &key (per-page nil) (page nil) (include-entities t) ) :users/search :q )
(define-twitter-method users-profile-banner (() &key (screen-name nil) (user-id nil)) :users/profile-banner)
;;combines both methods
(defmethod users-suggestions ( &key (slug nil) (lang nil))
  (if slug
      (apply 'twitter-op :users/suggestions/?id  :id slug :lang lang nil )
      (apply 'twitter-op :users/suggestions :lang lang nil )))

(define-twitter-method users-suggestions-slug-members ((slug))  :users/suggestions/?id/members :id)
(define-twitter-method users-report-spam (() &key (screen-name nil) (user-id nil) ) :users/report-spam )

;;;=============================



;;(define-twitter-method users-lookup-by-id ((user-ids-str) &key (include-entities  t)) :users/lookup :user-id)

;; does the query need to be url encoded ????
;; used url-rewrite for encoding; gets same result set as twitter for simple name queries..



(defun report-spam (screen-name &key (user-id nil))
  (apply 'twitter-op :report-spam :screen-name screen-name :user-id user-id))

;;--------------------------------------------------------------

(defun show-user (screen-name &key (include-entities t))
  (users-show :screen-name screen-name :include-entities include-entities))

(defun show-user-by-id (user-id &key (include-entities t))
  (users-show :user-id user-id :include-entities include-entities))


(defgeneric lookup-user (obj))

(defmethod lookup-user ((name string))
  (show-user name))

(defmethod show ( (obj t) &optional (s *standard-output*))
  (format s "~&~1t~S~%" obj))


(defun collect-users-by-id (&rest ids)
  (users-lookup :user-id (format nil "~{~a,~}" ids)))

(defun do-user-search (q &key (max 2) (skip 0) (container (make-hash-table  :test 'equal :size 100)))
   (let ((ht container)
	(ht-size 0)
	(page 1))
    (labels ((collect-it (lst)
	       (dolist (item lst)
		 (setf (gethash (twitter-user-id item) ht) item)))
	     (stop-it ()
	       (prog1 
		   (or (rate-limit-exceeded) (and (< 0 ht-size) (= ht-size (hash-table-count ht))))
		 (setf ht-size (hash-table-count ht))))
	     (next-page (item)
	       (declare (ignore item))
	       (incf page)))
      (with-cursor (:skip skip :max max :extractor #'identity :controller #'next-page :collector #'collect-it :test #'stop-it :cursor :page) (users-search  q :page 1 :per-page 100)))
    ht))

(defun get-user (ref)
  (when ref
    (if (twitter-user-p ref) ref
	(aif (lookup-twitter-object ref nil) it
	     (show-user ref)))))
