(in-package :cl-twitter)

;;((:RELATIONSHIP
;;  (:TARGET (:FOLLOWING) (:ID-STR . "65493023") (:ID . 65493023)
;;   (:FOLLOWED-BY . T) (:SCREEN-NAME . "SarahPalinUSA"))
;;  (:SOURCE (:ALL-REPLIES) (:WANT-RETWEETS . T) (:BLOCKING) (:MARKED-SPAM)
;;   (:FOLLOWING . T) (:ID-STR . "206126369") (:NOTIFICATIONS-ENABLED)
;;   (:ID . 206126369) (:FOLLOWED-BY) (:SCREEN-NAME . "domore_eliza"))))

;;
;;((:RELATIONSHIP
;;  (:TARGET (:FOLLOWED-BY) (:FOLLOWING . T) (:ID-STR . "206126369") (:ID . 206126369) (:SCREEN-NAME . "domore_eliza"))
;;   
;;  (:SOURCE (:MARKED-SPAM) (:FOLLOWED-BY . T) (:ALL-REPLIES) (:BLOCKING)
;;  (:NOTIFICATIONS-ENABLED) (:FOLLOWING) (:ID-STR . "65493023")
;;   (:WANT-RETWEETS) (:ID . 65493023) (:SCREEN-NAME . "SarahPalinUSA"))))

;;(((:NAME . "mao zedong") (:SCREEN-NAME . "darealmaozedong") (:ID . 395911020)
;;  (:ID-STR . "395911020") (:CONNECTIONS "following" "followed_by"))

(define-element connection  ((connections (identity)))
  "a connection"
  (id              "" nil)
  (id-str          "" nil)
  (screen-name     "" nil)
  (name            "" nil)
  (connections     "" nil)
  )

(defmethod print-object ((ref connection) stream)
  (format stream "#<TWITTER-CONNECTION '~A:~A'>" (connection-screen-name ref) (connection-connections ref)))

(define-element follow-target ()
  "a follower "
  (id              "" nil)
  (id-str          "" nil)
  (following       "" nil)
  (followed-by     "" nil)
  (events          "" nil)
  (screen-name     "" nil))

(defmethod print-object ((ref follow-target) stream)
  (format stream "#<TWITTER-FOLLOW-TARGET '~A'>" (follow-target-screen-name ref)))

(defun print-follow-target (ref)
  (format t "~A: ~A ~A~%" (follow-target-screen-name ref) (follow-target-id ref)  (follow-target-following  ref)))


(define-element follow-source ()
  "a follower "
  (id              "" nil)
  (id-str          "" nil)
  (marked-spam     "" nil)
  (all-replies     "" nil)
  (blocking        "" nil)
  (notifications-enabled "" nil)
  (want-retweets   ""  nil)
  (following       "" nil)
  (followed-by     "" nil)
  (events          "" nil)
  (screen-name     "" nil))

(defmethod print-object ((ref follow-source) stream)
  (format stream "#<TWITTER-FOLLOW-SOURCE '~A'>" (follow-source-screen-name ref) ))

(defun print-follow-source (ref)
  (format t "print-follow-source ~%"))

(define-element relationship ((source follow-source) (target follow-target))
  "a follower "
  (id              "" nil)
  (target          "" nil)
  (source          "" nil))

(defmethod print-object ((ref relationship) stream)
  (if (follow-source-following (relationship-source ref))
      (format stream "#<TWITTER-RELATIONSHIP '~A following ~A'>" (relationship-source ref) (relationship-target ref))
      (format stream "#<TWITTER-RELATIONSHIP '~A is followed by ~A'>" (relationship-source ref) (relationship-target ref))))

(defun print-relationship (ref)
  (format t "~A: ~A ~%" (relationship-source ref) (relationship-target  ref)))


(define-element relationships ((relationship relationship) )
  "a follower "
  (id              "" nil)
  (relationship    "" nil))


(defmethod print-object ((ref relationships) stream)
  (format stream "#<TWITTER-RELATIONSHIPS '~A'>" (relationships-relationship ref) ))

(defun print-relationships (ref)
  (format t "~A~%" (relationships-relationship ref)))


;;((:IDS) (:NEXT-CURSOR . 0) (:NEXT-CURSOR-STR . "0") (:PREVIOUS-CURSOR . 0)
;; (:PREVIOUS-CURSOR-STR . "0"))

(define-element cursor-friend-id ((ids (identity)))
  "a cursor element "
  (next-cursor-str     ""  nil)
  (previous-cursor-str "" nil)
  (next-cursor         ""  nil)
  (ids                 ""  nil)
  (previous-cursor     ""  nil))

(defmethod print-object ((ref cursor-friend-id) stream)
  (format stream "#<CURSOR-friend-ID '~A:~A'>" (cursor-friend-id-ids ref) (length (cursor-friend-id-ids ref)) ))


;;
;; Friendship Methods
;;
;; Friendship resources
;;   friendships/create
;;   friendships/destroy
;;   friendships/exists
;;   friendships/show
;;   friendships/incoming
;;   friendships/outgoing


(define-command friendships/incoming (:get :cursor-friend-id)
    (twitter-app-uri "friendships/incoming.json")
    "Returns an array of numeric IDs for every user who has a pending request to follow the authenticating user."
  :cursor "Breaks the results into pages. This is recommended for users who are following many users. Provide a value of -1 to begin paging."
  :stringify_ids "Many programming environments will not consume our Tweet ids due to their size")

(define-command friendships/outgoing (:get :cursor-friend-id)
    (twitter-app-uri "friendships/outgoing.json")
    "Returns an array of numeric IDs for every protected user for whom the authenticating user has a pending follow request."
  :cursor "Breaks the results into pages. This is recommended for users who are following many users. Provide a value of -1 to begin paging."
  :stringify_ids "Many programming environments will not consume our Tweet ids due to their size")

(define-command friendships/create (:post :twitter-user)
    (twitter-app-uri "friendships/create.json")
    "Allows the authenticating users to follow the user specified in the ID parameter."
  :user_id "The ID of the user for whom to return results for. Helpful for disambiguating when a valid user ID is also a valid screen name."
  :screen_name "The screen name of the user for whom to return results for. Helpful for disambiguating when a valid screen name is also a user ID."
  :follow "Enable notifications for the target user.")

(define-command friendships/destroy (:post :twitter-user)
    (twitter-app-uri "friendships/destroy.json")
    "Allows the authenticating users to unfollow the user specified in the ID parameter."
  :user_id "The ID of the user for whom to return results for. Helpful for disambiguating when a valid user ID is also a valid screen name."
  :screen_name "The screen name of the user for whom to return results for. Helpful for disambiguating when a valid screen name is also a user ID.")


(define-command friendships/update (:post :relationships )
    (twitter-app-uri "friendships/update.json")
    "Allows one to enable or disable retweets and device notifications from the specified user."
  :user_id       "The ID of the user for whom to return results for. Helpful for disambiguating when a valid user ID is also a valid screen name."
  :screen_name  "The screen name of the user for whom to return results for. Helpful for disambiguating when a valid screen name is also a user ID."
  :device       "Enable/disable device notifications from the target user."
  :retweets     "Enable/disable retweets from the target user.")

(define-command friendships/show (:get :relationships )
    (twitter-app-uri "friendships/show.json")
    "Returns detailed information about the relationship between two users."
  :source_id "The user_id of the subject user."
  :source_screen_name "The screen_name of the subject user."
  :target_id "The user_id of the target user."
  :target_screen_name "The screen_name of the target user.")

(define-command friendships/no_retweets/ids (:get :identity)
    (twitter-app-uri "friendships/no_retweets/ids.json")
    "Returns a collection of user_ids that the currently authenticated user does not want to receive retweets from."
  :stringify_ids  "Required.  The ID or screen_name of the first user to test friendship for.")

(define-command friendships/lookup (:get (:connection))
    (twitter-app-uri "friendships/lookup.json")
    "Returns the relationships of the authenticating user to the comma-separated list of up to 100 screen_names or user_ids provided."
  :user_id       "A comma separated list of user IDs, up to 100 are allowed in a single request"
  :screen_name  "optional; A comma separated list of screen names, up to 100 are allowed in a single request.")
  
;;==========


;;;deprecated ??
;;(define-command friendships/exists (:get :identity)
;;    (twitter-app-uri "friendships/exists.json")
;;;    "Test for the existence of friendship between two users. Will return true if user_a follows user_b, otherwise will return false."
;;  :user_a "Required.  The ID or screen_name of the first user to test friendship for."
;;  :user_b "Required.  The ID or screen_name of the second user to test friendship for.")





;;--------------------------- end of frienship methods ----------------------------------------------------------------------------
(define-twitter-method friendships-incoming ( () &key (cursor nil) (stringify-ids nil)) :friendships/incoming )
(define-twitter-method friendships-outgoing ( () &key (cursor nil) (stringify-ids nil)) :friendships/outgoing )
(define-twitter-method friendships-create ((screen-name)  &key (user-id nil) (follow nil)) :friendships/create :screen-name )
(define-twitter-method friendships-destroy ((screen-name)  &key (user-id nil)) :friendships/destroy :screen-name )
(define-twitter-method friendships-update ((screen-name)  &key (user-id nil) (device nil) (retweets nil)) :friendships/update :screen-name )
(defun friendships-show (source-screen-name target-screen-name &rest args &key (source-id nil) (target-id nil))
  (declare (ignore source-id target-id))
  (apply 'twitter-op :friendships/show :source-screen-name source-screen-name :target-screen-name target-screen-name args))
(define-twitter-method friendships-no-retweets-ids ( () &key (stringify-ids nil))  :friendships/no_retweets/ids)
(define-twitter-method friendships-lookup ( () &key (screen-name nil) (user-id nil)) :friendships/lookup)

;;=====


  
(defun follow (screen-name)
  (friendships-create screen-name))
   
(defun unfollow (screen-name)
  (friendships-destroy screen-name))

