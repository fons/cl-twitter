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


(defun lookup-follow-target (rec)
  (declare (ignore rec)))

(defun print-follow-target (ref)
  (format t "~A: ~A ~A~%" (follow-target-screen-name ref) (follow-target-id ref)  (follow-target-following  ref)))


(defmethod register-twitter-object ((ref follow-target)))

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


(defun lookup-follow-source (rec)
  (declare (ignore rec)))

(defun print-follow-source (ref)
  (format t "print-follow-source ~%"))

(defmethod register-twitter-object ((ref follow-source)))

(define-element relationship ((source follow-source) (target follow-target))
  "a follower "
  (id              "" nil)
  (target          "" nil)
  (source          "" nil))

(defmethod print-object ((ref relationship) stream)
  (if (follow-source-following (relationship-source ref))
      (format stream "#<TWITTER-RELATIONSHIP '~A following ~A'>" (relationship-source ref) (relationship-target ref))
      (format stream "#<TWITTER-RELATIONSHIP '~A is followed by ~A'>" (relationship-source ref) (relationship-target ref))))

(defun lookup-relationship (rec)
  (declare (ignore rec)))

(defun print-relationship (ref)
  (format t "~A: ~A ~%" (relationship-source ref) (relationship-target  ref)))

(defmethod register-twitter-object ((ref relationship)))

(define-element relationships ((relationship relationship) )
  "a follower "
  (id              "" nil)
  (relationship    "" nil))


(defmethod print-object ((ref relationships) stream)
  (format stream "#<TWITTER-RELATIONSHIPS '~A'>" (relationships-relationship ref) ))

(defun lookup-relationships (rec)
  (declare (ignore rec)))

(defun print-relationships (ref)
  (format t "~A~%" (relationships-relationship ref)))
	  


(defmethod register-twitter-object ((ref relationships)))

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



(define-command friendships/create (:post :twitter-user)
    (twitter-app-uri "friendships/create.json")
    "Allows the authenticating users to follow the user specified in the ID parameter."
  :user_id "The ID of the user for whom to return results for. Helpful for disambiguating when a valid user ID is also a valid screen name."
  :screen_name "The screen name of the user for whom to return results for. Helpful for disambiguating when a valid screen name is also a user ID."
  :follow "Enable notifications for the target user."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities")

 
(define-command friendships/destroy (:post :twitter-user)
    (twitter-app-uri "friendships/destroy.json")
    "Allows the authenticating users to unfollow the user specified in the ID parameter."
  :user_id "The ID of the user for whom to return results for. Helpful for disambiguating when a valid user ID is also a valid screen name."
  :screen_name "The screen name of the user for whom to return results for. Helpful for disambiguating when a valid screen name is also a user ID."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities")

(define-command friendships/exists (:get :identity)
    (twitter-app-uri "friendships/exists.json")
    "Test for the existence of friendship between two users. Will return true if user_a follows user_b, otherwise will return false."
  :user_a "Required.  The ID or screen_name of the first user to test friendship for."
  :user_b "Required.  The ID or screen_name of the second user to test friendship for.")



(define-command friendships/show (:get :relationships )
    (twitter-app-uri "friendships/show.json")
    "Returns detailed information about the relationship between two users."
  :source_id "The user_id of the subject user."
  :source_screen_name "The screen_name of the subject user."
  :target_id "The user_id of the target user."
  :target_screen_name "The screen_name of the target user.")

;; TODO handle cursors....
(define-command friendships/incoming (:get :identity)
    (twitter-app-uri "friendships/incoming.json")
    "Returns an array of numeric IDs for every user who has a pending request to follow the authenticating user."
  :cursor "Breaks the results into pages. This is recommended for users who are following many users. Provide a value of -1 to begin paging.")
;; Provide values as returned in the response body's next_cursor and previous_cursor attributes to page back and forth in the list.

;; TODO handle cursors....
(define-command friendships/outgoing (:get :identity)
    (twitter-app-uri "friendships/outgoing.json")
    "Returns an array of numeric IDs for every protected user for whom the authenticating user has a pending follow request."
    :cursor "Breaks the results into pages. This is recommended for users who are following many users. Provide a value of -1 to begin paging.")
;; Provide values as returned in the response body's next_cursor and previous_cursor attributes to page back and forth in the list.


;;--------------------------- end of frienship methods ----------------------------------------------------------------------------

(defun follow (screen-name &rest args &key (user-id nil) (follow nil) (include-entities nil))
  (declare (ignore user-id follow include-entities))
  (apply 'twitter-op :friendships/create :screen-name screen-name args))

(defun unfollow (screen-name &rest args &key (user-id nil) (include-entities nil))
  (declare (ignore user-id include-entities))
  (apply 'twitter-op :friendships/destroy :screen-name screen-name args))
  
(defun user-a-following-user-b? (user-a user-b)
  (apply 'twitter-op :friendships/exists :user-a user-a :user-b user-b nil))

(defun follower-relationship (source-screen-name target-screen-name &rest args &key (source-id nil) (target-id nil))
  (declare (ignore source-id target-id))
  (apply 'twitter-op :friendships/show :source-screen-name source-screen-name :target-screen-name target-screen-name args))

(defun incoming-follow-requests (&rest args &key (cursor nil))
  (declare (ignore cursor))
  (apply 'twitter-op :friendships/incoming args))

(defun outgoing-follow-requests (&rest args &key (cursor nil))
  (declare (ignore cursor))
  (apply 'twitter-op :friendships/outgoing args))

