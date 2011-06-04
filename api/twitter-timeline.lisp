(in-package :twitter)

;;---------> for elements see twitter-tweet-status

;;
;; Time line resources as per 
;;                      statuses/public_timeline
;;                      statuses/home_timeline
;;                      statuses/friends_timeline
;;                      statuses/user_timeline
;;                      statuses/mentions
;;                      statuses/retweeted_by_me
;;                      statuses/retweeted_to_me
;;                      statuses/retweets_of_me
;;
;;

(define-command statuses/public-timeline (:get (:tweet))
    (twitter-app-uri "statuses/public_timeline.json")
  "Returns 20 most recent statuses from non-protected users"
  :trim_user "When set to either true, t or 1, each tweet returned in a timeline will include a user object including only the status authors numerical ID. 
             Omit this parameter to receive the complete user object."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities. ")

(define-command statuses/home-timeline (:get (:tweet))
    (twitter-app-uri "statuses/home_timeline.json")
    "Returns the 20 most recent statuses, including retweets if they exist, posted by the authenticating user and the user's they follow. 
     This is the same timeline seen by a user when they login to twitter.com."
  :since_id "Optional. Returns statuses with an ID more recent than this one"
  :max_id  "Returns results with an ID less than (that is, older than) or equal to the specified ID."
  :count "Optional. Returns the number of statuses to receive"
  :page "Optional. Returns the statuses at page * count offset"
  :trim_user "When set to either true, t or 1, each tweet returned in a timeline will include a user object including only the status authors numerical ID."
  :include_rts "When set to either true, t or 1,the timeline will contain native retweets (if they exist) in addition to the standard stream of tweets. "
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities. ")


(define-command statuses/friends-timeline (:get (:tweet))
    (twitter-app-uri "statuses/friends_timeline.json")
  "Returns 20 most recent statuss from provided users friends"
  :since_id "Optional. Returns statuses with an ID more recent than this one"
  :max_id  "Returns results with an ID less than (that is, older than) or equal to the specified ID."
  :count "Optional. Returns the number of statuses to receive"
  :page "Optional. Returns the statuses at page * count offset"
  :trim_user "When set to either true, t or 1, each tweet returned in a timeline will include a user object including only the status authors numerical ID."
  :include_rts "When set to either true, t or 1,the timeline will contain native retweets (if they exist) in addition to the standard stream of tweets. "
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities. ")


(define-command statuses/user-timeline (:get (:tweet))
    (twitter-app-uri "statuses/user_timeline.json")
  "Returns 20 most recent statuses from provided users friends"
  :user_id "Optional. Specifies the ID or screen name of the user for whom to return the friends_timeline"
  :screen_name "The screen name of the user for whom to return results for. Helpful for disambiguating when a valid screen name is also a user ID."
  :since_id "Optional. Returns statuses with an ID more recent than this one"
  :count "Optional. Returns the number of statuses to receive"
  :page "Optional. Returns the statuses at page * count offset"
  :trim_user "When set to either true, t or 1, each tweet returned in a timeline will include a user object including only the status authors numerical ID."
  :include_rts "When set to either true, t or 1,the timeline will contain native retweets (if they exist) in addition to the standard stream of tweets. "
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities. ")


(define-command statuses/mentions (:get (:tweet))
    (twitter-app-uri "statuses/mentions.json")
  "Returns the 20 most recent mentions (status containing @username) for the authenticating user."
  :since_id "Optional. Returns statuses with an ID more recent than this one"
  :max_id "Returns results with an ID less than (that is, older than) or equal to the specified ID."
  :count "Optional. Returns the number of statuses to receive"
  :page "Optional. Returns the statuses at page * count offset"
  :trim_user "When set to either true, t or 1, each tweet returned in a timeline will include a user object including only the status authors numerical ID."
  :include_rts "When set to either true, t or 1,the timeline will contain native retweets (if they exist) in addition to the standard stream of tweets. "
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities. ")

(define-command statuses/retweeted-by-me (:get (:tweet))
    (twitter-app-uri "statuses/retweeted_by_me.json")
    "Returns the 20 most recent retweets posted by the authenticating user."
  :since_id "Optional. Returns statuses with an ID more recent than this one"
  :max_id "Returns results with an ID less than (that is, older than) or equal to the specified ID."
  :count "Optional. Returns the number of statuses to receive"
  :page "Optional. Returns the statuses at page * count offset"
  :trim_user "When set to either true, t or 1, each tweet returned in a timeline will include a user object including only the status authors numerical ID."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities. ")

(define-command statuses/retweeted-to-me (:get (:tweet))
    (twitter-app-uri "statuses/retweeted_to_me.json")
    "Returns the 20 most recent retweets posted by users the authenticating user follow."
  :since_id "Optional. Returns statuses with an ID more recent than this one"
  :max_id "Returns results with an ID less than (that is, older than) or equal to the specified ID."
  :count "Optional. Returns the number of statuses to receive"
  :page "Optional. Returns the statuses at page * count offset"
  :trim_user "When set to either true, t or 1, each tweet returned in a timeline will include a user object including only the status authors numerical ID."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities. ")

(define-command statuses/retweets-of-me (:get (:tweet))
    (twitter-app-uri "statuses/retweets_of_me.json")
    "Returns the 20 most recent tweets of the authenticated user that have been retweeted by others."
  :since_id "Optional. Returns statuses with an ID more recent than this one"
  :max_id "Returns results with an ID less than (that is, older than) or equal to the specified ID."
  :count "Optional. Returns the number of statuses to receive"
  :page "Optional. Returns the statuses at page * count offset"
  :trim_user "When set to either true, t or 1, each tweet returned in a timeline will include a user object including only the status authors numerical ID."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities. ")

;;---------------------end of timeline resources ----------------------------------------------------------------------------


(define-twitter-method public-timeline (() &key (trim-user nil) (include-entities t)) :statuses/public-timeline )

(define-twitter-method home-timeline (() &key (since-id nil) (max-id nil) (count nil) (page nil) 
					   (trim-user nil) (include_rts nil) (include-entities t)) :statuses/home-timeline )

(define-twitter-method friends-timeline (() &key (since-id nil) (max-id nil) (count nil) (page nil) 
					      (trim-user nil) (include_rts nil) (include-entities t)) :statuses/friends-timeline )

(define-twitter-method user-timeline (() &key (since-id nil) (max-id nil) (count nil) (page nil) 
					   (trim-user nil) (include_rts nil) (include-entities t)) :statuses/user-timeline )

(define-twitter-method mentions (() &key (since-id nil) (max-id nil) (count nil) (page nil) (trim-user nil) (include_rts nil) (include-entities t)) :statuses/mentions)

(define-twitter-method retweeted-by-me (() &key (since-id nil) (max-id nil) (count nil) 
					     (page nil) (trim-user nil) (include_rts nil) (include-entities t)) :statuses/retweeted-by-me)

(define-twitter-method retweeted-to-me (() &key (since-id nil) (max-id nil) (count nil) 
					     (page nil) (trim-user nil) (include_rts nil) (include-entities t)) :statuses/retweeted-to-me)

(define-twitter-method retweets-of-me (() &key (since-id nil) (max-id nil) (count nil) 
					    (page nil) (trim-user nil) (include_rts nil) (include-entities t)) :statuses/retweets-of-me)


;;----------------------------------------------------------------------------------------------------------------------------------------------------

(defun collect-home-timeline (&key (max 3) (skip 0) (container (make-hash-table  :test 'equal :size 100)))
  (collect-tweets (:max max :skip skip :container container) home-timeline))

(defun collect-friends-timeline (&key (max 3) (skip 0) (container (make-hash-table  :test 'equal :size 100)))
  (collect-tweets (:max max :skip skip :container container) friends-timeline))

(defun collect-user-timeline (&key (max 3) (skip 0) (container (make-hash-table  :test 'equal :size 100)))
  (collect-tweets (:max max :skip skip :container container) user-timeline))
