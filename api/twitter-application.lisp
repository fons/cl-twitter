(in-package :twitter)
#|
((:RATE-LIMIT-CONTEXT
  (:ACCESS-TOKEN . "206126369-NCSZgaSa6kOmeqqbeVIiJJE2AgeYeSr9IWA8TAbL"))
 (:RESOURCES
  (:LISTS (:/LISTS/LIST (:LIMIT . 15) (:REMAINING . 15) (:RESET . 1431039662))
   (:/LISTS/MEMBERSHIPS (:LIMIT . 15) (:REMAINING . 15) (:RESET . 1431039662))
   (:/LISTS/SUBSCRIBERS/SHOW (:LIMIT . 15) (:REMAINING . 15)
    (:RESET . 1431039662))
   (:/LISTS/MEMBERS (:LIMIT . 180) (:REMAINING . 180) (:RESET . 1431039662))
   (:/LISTS/SUBSCRIPTIONS (:LIMIT . 15) (:REMAINING . 15)
    (:RESET . 1431039662))
   (:/LISTS/SHOW (:LIMIT . 15) (:REMAINING . 15) (:RESET . 1431039662))
   (:/LISTS/OWNERSHIPS (:LIMIT . 15) (:REMAINING . 15) (:RESET . 1431039662))
   (:/LISTS/SUBSCRIBERS (:LIMIT . 180) (:REMAINING . 180)
    (:RESET . 1431039662))
   (:/LISTS/MEMBERS/SHOW (:LIMIT . 15) (:REMAINING . 15) (:RESET . 1431039662))
   (:/LISTS/STATUSES (:LIMIT . 180) (:REMAINING . 180) (:RESET . 1431039662)))

  (:APPLICATION
   (:/APPLICATION/RATE-LIMIT-STATUS (:LIMIT . 180) (:REMAINING . 175) (:RESET . 1431038955)))

  (:MUTES
   (:/MUTES/USERS/LIST (:LIMIT . 15) (:REMAINING . 15) (:RESET . 1431039662))
   (:/MUTES/USERS/IDS (:LIMIT . 15) (:REMAINING . 15) (:RESET . 1431039662)))

  (:FRIENDSHIPS
   (:/FRIENDSHIPS/OUTGOING (:LIMIT . 15) (:REMAINING . 15)
    (:RESET . 1431039662))
   (:/FRIENDSHIPS/NO-RETWEETS/IDS (:LIMIT . 15) (:REMAINING . 15)
    (:RESET . 1431039662))
   (:/FRIENDSHIPS/LOOKUP (:LIMIT . 15) (:REMAINING . 15) (:RESET . 1431039662))
   (:/FRIENDSHIPS/INCOMING (:LIMIT . 15) (:REMAINING . 15)
    (:RESET . 1431039662))
   (:/FRIENDSHIPS/SHOW (:LIMIT . 180) (:REMAINING . 180)
    (:RESET . 1431039662)))

  (:BLOCKS
   (:/BLOCKS/LIST (:LIMIT . 15) (:REMAINING . 15) (:RESET . 1431039662))
   (:/BLOCKS/IDS (:LIMIT . 15) (:REMAINING . 15) (:RESET . 1431039662)))

  (:GEO
   (:/GEO/SIMILAR-PLACES (:LIMIT . 15) (:REMAINING . 15) (:RESET . 1431039662))
   (:|/GEO/ID/:PLACE-ID| (:LIMIT . 15) (:REMAINING . 15) (:RESET . 1431039662))
   (:/GEO/REVERSE-GEOCODE (:LIMIT . 15) (:REMAINING . 15)
    (:RESET . 1431039662))
   (:/GEO/SEARCH (:LIMIT . 15) (:REMAINING . 15) (:RESET . 1431039662)))

  (:USERS
   (:/USERS/REPORT-SPAM (:LIMIT . 15) (:REMAINING . 15) (:RESET . 1431039662))
   (:|/USERS/SHOW/:ID| (:LIMIT . 180) (:REMAINING . 180) (:RESET . 1431039662))
   (:/USERS/SEARCH (:LIMIT . 180) (:REMAINING . 180) (:RESET . 1431039662))
   (:|/USERS/SUGGESTIONS/:SLUG| (:LIMIT . 15) (:REMAINING . 15)
    (:RESET . 1431039662))
   (:/USERS/DERIVED-INFO (:LIMIT . 15) (:REMAINING . 15) (:RESET . 1431039662))
   (:/USERS/PROFILE-BANNER (:LIMIT . 180) (:REMAINING . 180)
    (:RESET . 1431039662))
   (:|/USERS/SUGGESTIONS/:SLUG/MEMBERS| (:LIMIT . 15) (:REMAINING . 15)
    (:RESET . 1431039662))
   (:/USERS/LOOKUP (:LIMIT . 180) (:REMAINING . 180) (:RESET . 1431039662))
   (:/USERS/SUGGESTIONS (:LIMIT . 15) (:REMAINING . 15) (:RESET . 1431039662)))

  (:FOLLOWERS
   (:/FOLLOWERS/IDS (:LIMIT . 15) (:REMAINING . 15) (:RESET . 1431039662))
   (:/FOLLOWERS/LIST (:LIMIT . 15) (:REMAINING . 15) (:RESET . 1431039662)))

;;  (:STATUSES
;;   (:/STATUSES/RETWEETERS/IDS (:LIMIT . 15) (:REMAINING . 15)
;;                              (:RESET . 1431039662))
;;   (:/STATUSES/RETWEETS-OF-ME (:LIMIT . 15) (:REMAINING . 15)
;;                              (:RESET . 1431039662))
;;   (:/STATUSES/HOME-TIMELINE (:LIMIT . 15) (:REMAINING . 15)
;;                             (:RESET . 1431039662))
;;   (:|/STATUSES/SHOW/:ID| (:LIMIT . 180) (:REMAINING . 180)
;;     (:RESET . 1431039662))
;;   (:/STATUSES/USER-TIMELINE (:LIMIT . 180) (:REMAINING . 180)
;;                             (:RESET . 1431039662))
;;   (:/STATUSES/FRIENDS (:LIMIT . 15) (:REMAINING . 15) (:RESET . 1431039662))
;;   (:|/STATUSES/RETWEETS/:ID| (:LIMIT . 60) (:REMAINING . 60)
;;     (:RESET . 1431039662))
;;   (:/STATUSES/MENTIONS-TIMELINE (:LIMIT . 15) (:REMAINING . 15)
;;                                 (:RESET . 1431039662))
;;   (:/STATUSES/OEMBED (:LIMIT . 180) (:REMAINING . 180) (:RESET . 1431039662))
;;  (:/STATUSES/LOOKUP (:LIMIT . 180) (:REMAINING . 180) (:RESET . 1431039662)))

  (:CONTACTS
   (:/CONTACTS/UPLOADED-BY (:LIMIT . 300) (:REMAINING . 300)
    (:RESET . 1431039662))
   (:/CONTACTS/USERS (:LIMIT . 300) (:REMAINING . 300) (:RESET . 1431039662))
   (:/CONTACTS/ADDRESSBOOK (:LIMIT . 300) (:REMAINING . 300)
    (:RESET . 1431039662))
   (:/CONTACTS/USERS-AND-UPLOADED-BY (:LIMIT . 300) (:REMAINING . 300)
    (:RESET . 1431039662))
   (:/CONTACTS/DELETE/STATUS (:LIMIT . 300) (:REMAINING . 300)
    (:RESET . 1431039662)))

  (:HELP (:/HELP/TOS (:LIMIT . 15) (:REMAINING . 15) (:RESET . 1431039662))
   (:/HELP/CONFIGURATION (:LIMIT . 15) (:REMAINING . 15) (:RESET . 1431039662))
   (:/HELP/SETTINGS (:LIMIT . 15) (:REMAINING . 15) (:RESET . 1431039662))
   (:/HELP/PRIVACY (:LIMIT . 15) (:REMAINING . 15) (:RESET . 1431039662))
   (:/HELP/LANGUAGES (:LIMIT . 15) (:REMAINING . 15) (:RESET . 1431039662)))

  (:FRIENDS
   (:/FRIENDS/FOLLOWING/IDS (:LIMIT . 15) (:REMAINING . 15)
    (:RESET . 1431039662))
   (:/FRIENDS/FOLLOWING/LIST (:LIMIT . 15) (:REMAINING . 15)
    (:RESET . 1431039662))
   (:/FRIENDS/LIST (:LIMIT . 15) (:REMAINING . 15) (:RESET . 1431039662))
   (:/FRIENDS/IDS (:LIMIT . 15) (:REMAINING . 15) (:RESET . 1431039662)))

  (:DIRECT-MESSAGES
   (:/DIRECT-MESSAGES/SENT (:LIMIT . 15) (:REMAINING . 15)
    (:RESET . 1431039662))
   (:/DIRECT-MESSAGES (:LIMIT . 15) (:REMAINING . 15) (:RESET . 1431039662))
   (:/DIRECT-MESSAGES/SENT-AND-RECEIVED (:LIMIT . 15) (:REMAINING . 15)
    (:RESET . 1431039662))
   (:/DIRECT-MESSAGES/SHOW (:LIMIT . 15) (:REMAINING . 15)
    (:RESET . 1431039662)))

  (:ACCOUNT
   (:/ACCOUNT/LOGIN-VERIFICATION-ENROLLMENT (:LIMIT . 15) (:REMAINING . 15)
    (:RESET . 1431039662))
   (:/ACCOUNT/UPDATE-PROFILE (:LIMIT . 15) (:REMAINING . 15)
    (:RESET . 1431039662))
   (:/ACCOUNT/VERIFY-CREDENTIALS (:LIMIT . 15) (:REMAINING . 15)
    (:RESET . 1431039662))
   (:/ACCOUNT/SETTINGS (:LIMIT . 15) (:REMAINING . 15) (:RESET . 1431039662)))

  (:FAVORITES
   (:/FAVORITES/LIST (:LIMIT . 15) (:REMAINING . 15) (:RESET . 1431039662)))

  (:DEVICE
   (:/DEVICE/TOKEN (:LIMIT . 15) (:REMAINING . 15) (:RESET . 1431039662)))

  (:SAVED-SEARCHES
   (:|/SAVED-SEARCHES/DESTROY/:ID| (:LIMIT . 15) (:REMAINING . 15)
    (:RESET . 1431039662))
   (:|/SAVED-SEARCHES/SHOW/:ID| (:LIMIT . 15) (:REMAINING . 15)
    (:RESET . 1431039662))
   (:/SAVED-SEARCHES/LIST (:LIMIT . 15) (:REMAINING . 15)
    (:RESET . 1431039662)))

  (:SEARCH
   (:/SEARCH/TWEETS (:LIMIT . 180) (:REMAINING . 180) (:RESET . 1431039662)))

  (:TRENDS
   (:/TRENDS/CLOSEST (:LIMIT . 15) (:REMAINING . 15) (:RESET . 1431039662))
   (:/TRENDS/AVAILABLE (:LIMIT . 15) (:REMAINING . 15) (:RESET . 1431039662))
   (:/TRENDS/PLACE (:LIMIT . 15) (:REMAINING . 15) (:RESET . 1431039662)))))
|#


(define-element limits ()
  "search/tweets resource"
  (limit "" nil)
  (remaining "" nil)
  (reset     "" nil))

(defmethod print-object ((ref limits) stream)
  (format stream "#<LIMITS '~A:~A:~A'>" (limits-limit ref) (limits-remaining ref) (limits-reset ref)))

;;----------------=====================================================
(define-element search2 ((/search/tweets limits))
  "search/tweets resource"
  (/search/tweets "" nil))

(defmethod print-object ((ref search2) stream)
  (format stream "#<SEARCH '~A'>" (search2-/search/tweets ref)))

;;========================================================================================
(define-element statuses ((/statuses/retweeters/ids limits)
                          (/statuses/retweets-of-me limits)
                          (/statuses/home-timeline limits)
                          (|/statuses/show/:id| limits)
                          (/statuses/user-timeline  limits)
                          (/statuses/friends  limits)
                          (|/statuses/retweets/:id| limits)
                          (/statuses/mentions-timeline  limits)
                          (/statuses/oembed limits)
                          (/statuses/lookup limits))
  "status rate limit"
  (/statuses/retweeters/ids  "" nil) 
  (/statuses/retweets-of-me   "" nil) 
  (/statuses/home-timeline   "" nil) 
  (|/statuses/show/:id|  "" nil) 
  (/statuses/user-timeline   "" nil) 
  (/statuses/friends  "" nil)  
  (|/statuses/retweets/:id|  "" nil) 
  (/statuses/mentions-timeline   "" nil) 
  (/statuses/oembed  "" nil) 
  (/statuses/lookup  "" nil) )

(defmethod print-object ((ref statuses) stream)
  (format stream "#<STATUSES ~A:~A:~A:~A:~A:~A:~A:~A:~A:~A>" 
          (statuses-/statuses/retweeters/ids  ref) 
          (statuses-/statuses/retweets-of-me   ref) 
          (statuses-/statuses/home-timeline   ref)
          (statuses-|/statuses/show/:id|  ref) 
          (statuses-/statuses/user-timeline   ref) 
          (statuses-/statuses/friends  ref)  
          (statuses-|/statuses/retweets/:id|  ref) 
          (statuses-/statuses/mentions-timeline   ref) 
          (statuses-/statuses/oembed  ref) 
          (statuses-/statuses/lookup  ref) ))


;;----------------=====================================================
(define-element lists ((/lists/list limits ) (/lists/memberships limits ) (/lists/subscribers/show limits )
                       (/lists/members limits )(/lists/subscriptions limits )(/lists/show limits )
                       (/lists/ownerships limits )(/lists/subscribers limits )(/lists/members/show limits )(/lists/statuses limits ))
  "a resource lists element"
  (/lists/list "" nil)
  (/lists/memberships "" nil)
  (/lists/subscribers/show "" nil)
  (/lists/members "" nil)
  (/lists/subscriptions "" nil)
  (/lists/show "" nil)
  (/lists/ownerships "" nil)
  (/lists/subscribers "" nil)
  (/lists/members/show "" nil)
  (/lists/statuses "" nil))

(defmethod print-object ((ref lists) stream)
  (format stream "#<LISTS ~A:~A:~A:~A:~A:~A:~A:~A:~A:~A>" (lists-/lists/list ref ) (lists-/lists/memberships ref ) (lists-/lists/subscribers/show ref )
          (lists-/lists/members ref )(lists-/lists/subscriptions ref )(lists-/lists/show ref )
          (lists-/lists/ownerships ref )(lists-/lists/subscribers ref )(lists-/lists/members/show ref )(lists-/lists/statuses ref )))

;;----------------=====================================================
(define-element resources ((search search2) (lists lists) (statuses statuses))
  "resource element"
  (search "" nil)
  (statuses "" nil)
  (lists ""  nil))

(defmethod print-object ((ref resources) stream)
  (format stream "#<RESOURCES '~A:~A:~A'>" (resources-search ref) (resources-lists ref) (resources-statuses ref)))

;;==========================================================
(define-element rate-limit-context ()
  "rate limit context"
  (access-token "" nil))

(defmethod print-object ((ref rate-limit-context) stream)
  (format stream "#<RATE-LIMIT-CONTEXT '~A'>" (rate-limit-context-access-token ref)))

;;==========================================================
(define-element twitter-rate-limit ((rate-limit-context rate-limit-context) (resources resources))
  "rate limit"
  (rate-limit-context "" nil)
  (resources    "" nil))

(defmethod print-object ((ref twitter-rate-limit) stream)
  (format stream "#<TWITTER-RATE-LIMIT '~A:~A'>" (twitter-rate-limit-rate-limit-context ref) (twitter-rate-limit-resources ref) ))


;;(defun print-rate-limit (ref)
;;  (format t "~A: ~A~%" (rate-limit-context-access-token ref) (rate-limit-context-resources ref)))




(defun rate-limit-exceeded () (not t))
#+nil  (let ((rls (rate-limit-status)))
         (zerop (rate-limit-remaining-hits rls)))

;;Calls to rate_limit_status do not count against the rate limit.  
;;If authentication credentials are provided, the rate limit status for the authenticating user is returned.  
;;Otherwise, the rate limit status for the requester's IP address is returned.")

(define-command application/rate-limit-status (:get :twitter-rate-limit)
    (twitter-app-uri "application/rate_limit_status.json")
    "Returns the remaining number of API requests available to the requesting user before the API limit is reached for the current hour. "
  :resources "A comma-separated list of resource families you want to know the current rate limit disposition for. "
  )

(define-twitter-method application-rate-limit-status (() &key (resources nil)) :application/rate-limit-status)
