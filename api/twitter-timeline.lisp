(in-package :twitter)

;;---------> for elements see twitter-tweet-status

;;
;; Time line resources as per 
;;                      statuses/home_timeline
;;                      statuses/user_timeline
;;                      statuses/mentions
;;                      statuses/retweets_of_me
;;
;;



(define-command statuses/home-timeline (:get (:tweet))
    (twitter-app-uri "statuses/home_timeline.json")
    "Returns the 20 most recent statuses, including retweets if they exist, posted by the authenticating user and the user's they follow. 
     This is the same timeline seen by a user when they login to twitter.com."
  :count "Optional. Returns the number of statuses to receive"
  :since_id "Optional. Returns statuses with an ID more recent than this one"
  :max_id  "Returns results with an ID less than (that is, older than) or equal to the specified ID."
  :trim_user "When set to either true, t or 1, each tweet returned in a timeline will include a user object including only the status authors numerical ID."
  :exclude_replies "This parameter will prevent replies from appearing in the returned timeline."
  :contributer_details "This parameter enhances the contributors element of the status response to include the screen_name of the contributor."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities. ")


(define-command statuses/user-timeline (:get (:tweet))
    (twitter-app-uri "statuses/user_timeline.json")
  "Returns 3200 most recent statuses from provided users friends"
  :user_id "Optional. Specifies the ID or screen name of the user for whom to return the friends_timeline"
  :screen_name "The screen name of the user for whom to return results for. Helpful for disambiguating when a valid screen name is also a user ID."
  :since_id "Optional. Returns statuses with an ID more recent than this one"
  :count "Optional. Returns the number of statuses to receive"
  :max_id "Returns results with an ID less than (that is, older than) or equal to the specified ID" 
  :trim_user "When set to either true, t or 1, each tweet returned in a timeline will include a user object including only the status authors numerical ID."
  :exclude_replies "This parameter will prevent replies from appearing in the returned timeline."
  :contributer_details "This parameter enhances the contributors element of the status response to include the screen_name of the contributor."
  :include_rts "When set to either true, t or 1,the timeline will contain native retweets (if they exist) in addition to the standard stream of tweets. ")


(define-command statuses/mentions-timeline (:get (:tweet))
    (twitter-app-uri "statuses/mentions_timeline.json")
  "Returns the 20 most recent mentions (status containing @username) for the authenticating user."
  :since_id "Optional. Returns statuses with an ID more recent than this one"
  :max_id "Returns results with an ID less than (that is, older than) or equal to the specified ID."
  :count "Optional. Returns the number of statuses to receive"
  :trim_user "When set to either true, t or 1, each tweet returned in a timeline will include a user object including only the status authors numerical ID."
  :contributer_details "This parameter enhances the contributors element of the status response to include the screen_name of the contributor."  
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities. ")

(define-command statuses/retweets-of-me (:get (:tweet))
    (twitter-app-uri "statuses/retweets_of_me.json")
    "Returns the 20 most recent tweets of the authenticated user that have been retweeted by others."
  :since_id "Optional. Returns statuses with an ID more recent than this one"
  :max_id "Returns results with an ID less than (that is, older than) or equal to the specified ID."
  :count "Optional. Returns the number of statuses to receive"
  :trim_user "When set to either true, t or 1, each tweet returned in a timeline will include a user object including only the status authors numerical ID."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities. "
  :include_user_entities "When set to either true, t or 1, each tweet will include a node called user entities. ")

;;---------------------end of timeline resources ----------------------------------------------------------------------------

(define-twitter-method statuses-home-timeline (() &key (since-id nil) (max-id nil) (count nil) 
                                               (trim-user nil) (exclude-replies nil) (contributer-details nil) (include-entities t)) :statuses/home-timeline )

(define-twitter-method statuses-user-timeline (() &key (since-id nil) (max-id nil) (count nil) (user-id nil) (screen-name nil)
                                               (trim-user nil) (include-rts nil) (exclude-replies nil) (contributer-details nil)) :statuses/user-timeline )

(define-twitter-method statuses-mentions-timeline (() &key (since-id nil) (max-id nil) (count nil)  (trim-user nil) (contributer-details nil) (include-entities t)) :statuses/mentions-timeline)

(define-twitter-method statuses-retweets-of-me (() &key (since-id nil) (max-id nil) (count nil) 
                                                (trim-user nil) (include-entities t) (include-user-entities t)) :statuses/retweets-of-me)


;;----------------------------------------------------------------------------------------------------------------------------------------------------



(defun home-timeline (&key (since-id nil) (max-id nil) (count 20) (trim-user nil) (exclude-replies nil) (contributer-details nil) (include-entities t))
  (collect-results nil (set-depth count)
                    #'statuses-home-timeline
                    (arguments :count (set-count count)
                               :max-id max-id
                               :since-id since-id
                               :trim-user trim-user
                               :exclude-replies exclude-replies
                               :contributer-details contributer-details
                               :include-entities include-entities)))



(defun user-timeline (screen-name &key (since-id nil) (max-id nil) (count 20) (trim-user nil) (exclude-replies nil) (contributer-details nil) (include-rts t))
  (collect-results nil (set-depth count)
                    #'statuses-user-timeline
                    (arguments :count (set-count count)
                               :max-id max-id
                               :since-id since-id
                               :screen-name screen-name
                               :trim-user trim-user
                               :exclude-replies exclude-replies
                               :contributer-details contributer-details
                               :include-rts include-rts)))





(defun mentions-timeline (&key (since-id nil) (max-id nil) (count 20) (trim-user nil) (contributer-details nil)  (include-entities t))
  (collect-results nil (set-depth count)
                    #'statuses-mentions-timeline
                    (arguments :count (set-count count)
                               :max-id max-id
                               :since-id since-id
                               :trim-user trim-user                               
                               :contributer-details contributer-details
                               :include-entities include-entities)))



(defun retweets-of-me (&key (since-id nil) (max-id nil) (count 20) (trim-user nil) (include-entities t) (include-user-entities t) )
  (collect-results nil (set-depth count)
                    #'statuses-retweets-of-me
                    (arguments :count (set-count count)
                               :max-id max-id
                               :since-id since-id
                               :trim-user trim-user                               
                               :include-entities include-entities
                               :include-user-entities include-user-entities)))


