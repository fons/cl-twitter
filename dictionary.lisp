(in-package :twitter)

(defvar *twitter-app-uri*       "http://api.twitter.com/1/")
(defvar *twitter-search-uri*    "http://search.twitter.com/")

(defun twitter-app-uri (method)
  (concatenate 'string *twitter-app-uri* method))

(defun twitter-search-uri (method)
  (concatenate 'string *twitter-search-uri* method))

;;
;; TODO NEED URL ENCODING !!!!! hunchentoot:url-encode ?? ==> use url-rewrite !!!!
;;

;;
;; STATUS METHODS
;;
;;   "http://twitter.com/statuses/public_timeline.json"

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
  :trim_user "When set to either true, t or 1, each tweet returned in a timeline will include a user object including only the status authors numerical ID. Omit this parameter to receive the complete user object."
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
;;
;;  Tweets resources
;;           statuses/show/:id
;;           statuses/update
;;           statuses/destroy/:id
;;           statuses/retweet/:id
;;           statuses/retweets/:id
;;           statuses/:id/retweeted_by
;;           statuses/:id/retweeted_by/ids
;;

(define-command statuses/show/?id (:get-id :tweet)
    (twitter-app-uri "statuses/show/<id>.json")
    "Returns a single-status specified by the id parameter"
  :id "Required. The numberical ID of the status you want to retrieve"
  :trim_user "When set to either true, t or 1, each tweet returned in a timeline will include a user object including only the status authors numerical ID."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities. ")

(define-command statuses/update (:post :tweet)
    (twitter-app-uri "statuses/update.json")
    "Updates the authenticating user's status.  Requires status parameter"
  :status "Required.  The text of your status update.  Must be less than 140 characters"
  :in_reply_to_status_id "Optional.  The id of an existing status that this post is in reply to.  This sets the appropriate attribute of the result status."
  :lat "The latitude of the location this tweet refers to."
  :long "The longitude of the location this tweet refers to."
  :place_id "A place in the world. These IDs can be retrieved from geo/reverse_geocode."
  :display_coordinates "Whether or not to put a pin on the exact coordinates a tweet has been sent from"
  :trim_user "When set to either true, t or 1, each tweet returned in a timeline will include a user object including only the status authors numerical ID."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities. ")


(define-command statuses/destroy/?id (:post-id :tweet)
    (twitter-app-uri "statuses/destroy/<id>.json")
    "Destroys the status specified by the required ID parameter."
  :id "Required. The numberical ID of the status you want to retrieve"
  :trim_user "When set to either true, t or 1, each tweet returned in a timeline will include a user object including only the status authors numerical ID."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities. ")

(define-command statuses/retweet/?id (:post-id :tweet)
    (twitter-app-uri "statuses/retweet/<id>.json")
    "Retweets a tweet. Returns the original tweet with retweet details embedded."
  :id "Required. The numberical ID of the status you want to retrieve"
  :trim_user "When set to either true, t or 1, each tweet returned in a timeline will include a user object including only the status authors numerical ID."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities. ")

(define-command statuses/retweets/?id (:get-id (:tweet))
    (twitter-app-uri "statuses/retweets/<id>.json")
    "Returns up to 100 of the first retweets of a given tweet."
  :id "Required. The numberical ID of the status you want to retrieve"
  :count "Specifies the number of records to retrieve. Must be less than or equal to 100."
  :trim_user "When set to either true, t or 1, each tweet returned in a timeline will include a user object including only the status authors numerical ID."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities. ")


(define-command statuses/?id/retweeted-by (:get-id (:twitter-user))
    (twitter-app-uri "statuses/<id>/retweeted_by.json")
    "Show user objects of up to 100 members who retweeted the status"
  :id        "Required. The numberical ID of the status you want to retrieve"
  :count     "Specifies the number of records to retrieve. Must be less than or equal to 100."
  :page      "Specifies the page of results to retrieve."
  :trim_user "When set to either true, t or 1, each tweet returned in a timeline will include a user object including only the status authors numerical ID."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities. ")

(define-command statuses/?id/retweeted-by/ids (:get-id (:indentity ))
    (twitter-app-uri "statuses/<id>/retweeted_by/ids.json")
    "Show user ids of up to 100 users who retweeted the status."
  :id        "Required. The numberical ID of the status you want to retrieve"
  :count     "Specifies the number of records to retrieve. Must be less than or equal to 100."
  :page      "Specifies the page of results to retrieve."
  :trim_user "When set to either true, t or 1, each tweet returned in a timeline will include a user object including only the status authors numerical ID."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities. ")

;;
;; User resources
;;   users/show
;;   users/lookup
;;   users/search
;;   users/suggestions
;;   users/suggestions/twitter
;;   users/profile_image/twitter
;;   statuses/friends
;;   statuses/followers

;; Is this a deprecated api call ? Don't see it documented..
;;(define-command user-show (:get-id :twitter-user)
;;    (twitter-users-uri "<id>.json")
 ;;   "Returns a single-status specified by the id parameter"
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

(define-command users/profile-image/?screen_name (:get-id (:identity) )
    (twitter-app-uri "users/profile_image/<id>.json")
    "Access the profile image in various sizes for the user with the indicated screen_name. "
  :id   "Required ; screen name of the user" 
  :size "Optional; Specifies the size of image to fetch.")

;; TODO : cursor doesn't work : this does not return a "twitter-user" list.
;; Probably better to create a seperate 'cursor' command ?
(define-command statuses/friends (:get (:twitter-user) )
    (twitter-app-uri "statuses/friends.json")
    "Returns the authenticating user's friends, each with current status inline. They are ordered by the order in which they were added as friends. It's also possible to request another user's recent friends list via the id parameter below."
  :user_id "The ID of the user for whom to return results for." 
  :screen_name "The screen name of the user for whom to return results for."
  :cursor "Breaks the results into pages. This is recommended for users who are following many users. Provide a value of -1 to begin paging. Provide values as returned in the response body's next_cursor and previous_cursor attributes to page back and forth in the list."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities." )


(define-command statuses/followers (:get (:twitter-user) )
    (twitter-app-uri "statuses/followers.json")
    "Returns the authenticating user's followers, each with current status inline. "
  :user_id "The ID of the user for whom to return results for." 
  :screen_name "The screen name of the user for whom to return results for."
  :cursor "Breaks the results into pages. This is recommended for users who are following many users. Provide a value of -1 to begin paging. Provide values as returned in the response body's next_cursor and previous_cursor attributes to page back and forth in the list."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities." )

;;
;; Trends resources
;;     trends
;;     trends/current
;;     trends/daily
;;     trends/weekly


(defmethod parse-record (response (prim-type (eql :trends)))
  "Unparse and unpack trend type records"
  (cons (get-value :as-of response)
	(mapcar (lambda (trend)
		  (list (cdr (first trend))
			(cdr (second trend))))
		(get-value :trends response))))

(defmethod parse-record (response (prim-type (eql :new-trends)))
  "Unparse and unpack trend type records"
  (cons (get-value :as-of response)
	(mapcar (lambda (trend-set)
		  (list (first trend-set)
			(mapcar (lambda (trend)
				  (cons (cdr (first trend))
					(cdr (second trend))))
				(rest trend-set))))
		(get-value :trends response))))


(define-command trends (:get :trends)
    (twitter-app-uri "trends.json")
    "Returns the top ten topics that are currently trending on Twitter. The response includes the time of the request, the name of each trend, and the url to the Twitter Search results page for that topic.")

;;TODO : needs beter return list when :exclude option is used; change lambda in parse-record..
(define-command trends/current (:get :new-trends)
    (twitter-app-uri "trends/current.json")
    "Returns the top ten queries that are currently trending on Twitter.  The response includes the time of the request, the name of each trending topic, and the url to the Twitter Search results page for that topic. "
  :exclude "Setting this equal to hashtags will remove all hashtags from the trends list.")

(define-command trends/daily (:get :new-trends)
    (twitter-app-uri "trends/daily.json")
    "Returns the top 20 trending topics for each hour in a given day."
  :date "The start date for the report. The date should be formatted YYYY-MM-DD. A 404 error will be thrown if the date is older than the available search index (7-10 days). "
:exclude "Setting this equal to hashtags will remove all hashtags from the trends list")

;;TODO better format for return list
(define-command trends/weekly (:get :new-trends)
    (twitter-app-uri "trends/weekly.json")
    "Returns the top 30 trending topics for each day in a given week."
  :date "The start date for the report. The date should be formatted YYYY-MM-DD. A 404 error will be thrown if the date is older than the available search index (7-10 days). "
  :exclude "Setting this equal to hashtags will remove all hashtags from the trends list")


;;--------------end of trends-------------------------------------
;;
;; Local Trends resources
;;     trends/available
;;     trends/1
;;
;; The response is an array of "locations" that encode the location's WOEID and some other human-readable information such as 
;; a canonical name and country the location belongs in.
;; A WOEID is a Yahoo! Where On Earth ID.
;; TODO : develop a return type and tie to WOEID 


(define-command trends/available (:get :identity)
    (twitter-app-uri "trends/available.json")
    "Returns the locations that Twitter has trending topic information for."
  :lat "If provided with a long parameter the available trend locations will be sorted by distance, nearest to furthest, to the co-ordinate pair."
  :long "If provided with a lat parameter the available trend locations will be sorted by distance, nearest to furthest, to the co-ordinate pair.")


(define-command trends/1 (:get-id :identity)
    (twitter-app-uri "trends/<id>.json")
    "Returns the locations that Twitter has trending topic information for."
  :id "Required woeid; The Yahoo! Where On Earth ID of the location to return trending information for. Global information is available by using 1 as the WOEID.")

;;---------------------- end of local trends resources ----------------------------------------------------
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

;;List Members resources
;;   :user/:list_id/members
;;   :user/:list_id/members
;;   :user/:list_id/create_all
;;   :user/:list_id/members
;;   :user/:list_id/members/:id
;; similarly requires the rpalcement of two ids in the url
;; =======>>>>>>>TBD

;; List Subscribers resources
;;     :user/:list_id/subscribers
;;     :user/:list_id/subscribers
;;     :user/:list_id/subscribers
;;     :user/:list_id/subscribers/:id
;; similarly requires the rpalcement of two ids in the url
;; =======>>>>>>>TBD

;;
;; DIRECT MESSAGES
;;
;;
;;  Direct Messages resources
;;      direct_messages
;;      direct_messages/sent
;;      direct_messages/new
;;      direct_messages/destroy/:id
;;

(define-command direct-messages (:get (:twitter-message))
    (twitter-app-uri "direct_messages.json")
    "Returns a list of the 20 most recent direct messages sent to the authenticating user.  The XML and JSON versions include detailed information about the sending and recipient users."
  :since  "Optional.  Narrows the resulting list of direct messages to just those sent after the specified HTTP-formatted date, up to 24 hours old.  The same behavior is available by setting the If-Modified-Since parameter in your HTTP request."
  :max_id "Returns results with an ID less than (that is, older than) or equal to the specified ID."
  :count "Specifies the number of records to retrieve. Must be less than or equal to 200."
  :page "Specifies the page of results to retrieve."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities")


(define-command direct-messages/sent (:get (:twitter-message))
    (twitter-app-uri "direct_messages/sent.json")
    "Returns a list of the 20 most recent direct messages sent by the authenticating user."  
  :since_id  "Optional.  Returns only sent direct messages with an ID greater than 
              (that is, more recent than) the specified ID."
  :max_id "Returns results with an ID less than (that is, older than) or equal to the specified ID."
  :count "Specifies the number of records to retrieve. Must be less than or equal to 200."
  :page "Specifies the page of results to retrieve."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities")


;;messages can only be sent to users that are following you....
(define-command direct-messages/new (:post :twitter-message)
    (twitter-app-uri "direct_messages/new.json")
    "Sends a new direct message to the specified user from the authenticating user."  
  :screen_name "Required. The screen name of the user who should receive the direct message."
  :user_id "Required.  The ID or screen name of the recipient user."
  :text "Required.  The text of your direct message.  Be sure to URL encode as necessary, and keep it under 140 characters."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities")


(define-command direct-messages/destroy/?id (:post-id :twitter-message)
    (twitter-app-uri "direct_messages/destroy/<id>.json")
    "Destroys the direct message specified in the required ID parameter.  The authenticating user must be the recipient of the specified direct message."
  :id "Required.  The ID of the direct message to destroy."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities")

;;----------------------end of direct messages------------------------------------------------------------------------

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


;;; TODO : this returns a relationship for which an element needs to be created...
(define-command friendships/show (:get :identity)
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
    :cursor "Breaks the results into pages. This is recommended for users who are following many users. Provide a value of -1 to begin paging.")
;; Provide values as returned in the response body's next_cursor and previous_cursor attributes to page back and forth in the list.


;;--------------------------- end of frienship methods ----------------------------------------------------------------------------

;; Social Graph Methods
;;
;;  Friends and Followers resources :
;;     friends/ids
;;     followers/ids
;;
;; To begin paging provide a value of -1 as the cursor. 
;; The response from the API will include a previous_cursor and next_cursor to allow paging back and forth. 
;; If the cursor is not provided the API will attempt to return all IDs. For users with many connections this will probably fail. 
;; Querying without the cursor parameter is deprecated and should be avoided. The API is being updated to force the cursor to be -1 if it isn't supplied.
;; TODO : handle the cursor...

(define-command friends/ids (:get (:identity))
    (twitter-app-uri "friends/ids.json")
    "Returns an array of numeric IDs for every user the specified user is following."
  :user_id "Optional.  The ID or screen name of (the user for whom to request a list of friends."
  :screen_name "Optional.  The ID or screen name of (the user for whom to request a list of friends."
  :cursor "Causes the list of connections to be broken into pages of no more than 5000 IDs at a time. ")


(define-command followers/ids (:get (:identity))
    (twitter-app-uri "followers/ids.json")
    "Returns an array of numeric IDs for every user following the specified user."
  :user_id "Optional.  The ID or screen name of (the user for whom to request a list of friends."
  :screen_name "Optional.  The ID or screen name of (the user for whom to request a list of friends."
  :cursor "Causes the list of connections to be broken into pages of no more than 5000 IDs at a time." )

;;----------------------end of friends and followers resources/ social graph methods----------------------------------

;;
;; ACCOUNT METHODS
;;
;;   Account resources
;;        account/verify_credentials
;;        account/rate_limit_status
;;        account/end_session
;;        account/update_delivery_device
;;        account/update_profile_colors
;;        account/update_profile_image
;;        account/update_profile_background_image
;;        account/update_profile
;;


(define-command account/verify-credentials (:get :twitter-user)
    (twitter-app-uri "account/verify_credentials.json")
    :include_entities "When set to either true, t or 1, each tweet will include a node called entities")

;;Calls to rate_limit_status do not count against the rate limit.  
;;If authentication credentials are provided, the rate limit status for the authenticating user is returned.  
;;Otherwise, the rate limit status for the requester's IP address is returned.")
;;TODO need a sensible return structure

(define-command account/rate-limit-status (:get :identity)
    (twitter-app-uri "account/rate_limit_status.json")
    "Returns the remaining number of API requests available to the requesting user before the API limit is reached for the current hour. ")

;;TODO need a sensible return structure
(define-command account/end-session (:post :identity)
    (twitter-app-uri "account/end_session.json")
    "Ends the session of the authenticating user, returning a null cookie.")

;;TODO : doesn't work; parsing error
(define-command account/update-delivery-device (:post :identity)
    (twitter-app-uri "account/update_delivery_device.json")
    "Sets which device Twitter delivers updates to for the authenticating user.  Sending none as the device parameter will disable IM or SMS updates."
  :device "Required.  Must be one of: sms, im, none.")

 
(define-command account/update-profile-colors (:post :twitter-user)
    (twitter-app-uri "account/update_profile_colors.json")
    "Sets one or more hex values that control the color scheme of the authenticating user's
     profile page on twitter.com.  These values are also returned in the /users/show 
     API method."
  :profile-background-color "Optional. "
  :profile-text-color "Optional"
  :profile-link-color "Optional"
  :profile-sidebar-fill-color "Optional"
  :profile-sidebar-border-color "Optional"
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities")

;; not tested 
(define-command account/update-profile-image (:post :twitter-user)
    (twitter-app-uri "account/update_profile_image.json")
    "Updates the authenticating user's profile image.  Expects raw multipart data, not a URL to an image."
  :image "Required.  Must be a valid GIF, JPG, or PNG image of less than 700 kilobytes in size.  Images with width larger than 500 pixels will be scaled down."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities")


;;not tested
(define-command account/update-profile-background-image (:post :twitter-user)
    (twitter-app-uri "account/update_profile_background_image.json")
    "Updates the authenticating user's profile background image.  Expects raw multipart data, not a URL to an image."
  :image "Required.  Must be a valid GIF, JPG, or PNG image of less than 800 kilobytes in size.  Images with width larger than 2048 pixels will be scaled down."
  :tile "Whether or not to tile the background image. If set to true the background image will be displayed tiled. The image will not be tiled otherwise."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities")


;;Only the parameters specified will be updated; to only update the 'name' attribute, for example, only include that parameter in your request."
(define-command account/update-profile (:post :twitter-user)
    (twitter-app-uri "account/update_profile.json")
    "Sets values that users are able to set under the 'Account' tab of their settings page."
  :name "Optional. Maximum of 20 characters. One or more of all these parameters must be present."
  :url "Optional. Maximum of 100 characters. Will be prepended with 'http://' if not present."
  :location "Optional. Maximum of 30 characters. The contents are not normalized or geocoded in any way."
  :description "Optional. Maximum of 160 characters."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities")

;;----------------------- end of account methods -----------------------------------------------------------------------------
 
;;
;; Favorite Methods
;; 
;;  Favorites resources
;;      favorites
;;      favorites/create/:id
;;      favorites/destroy/:id


(define-command favorites (:get (:tweet))
    (twitter-app-uri "favorites.json")
    "Returns the 20 most recent favorite statuses for the authenticating user or user specified by the ID parameter in the requested format."
  :id "Optional. The ID or screen name of the user for whom to request a list of favorite statuses.  "
  :page "Optional. Retrieves the 20 next most recent favorite statuses."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities")


(define-command favorites/create/?id (:post-id :tweet)
    (twitter-app-uri "favorites/create/<id>.json")
    "Favorites the status specified in the ID parameter as the authenticating user.  Returns the favorite status when successful."
  :id "Required.  The ID of the status to favorite."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities")

(define-command favorites/destroy/?id (:post-id :tweet)
    (twitter-app-uri "favorites/destroy/<id>.json")
    "Un-favorites the status specified in the ID parameter as the authenticating user.  Returns the un-favorited status in the requested format when successful."
  :id "Required.  The ID of the status to un-favorite."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities")

;;----------------------- end of favorites -------------------------------------------------------------------------------

;;
;; Notification Methods
;; 
;; Notification resources
;;      notifications/follow
;;      notifications/leave
;; NOTE: The Notification Methods require the authenticated user to already be friends with the specified user otherwise 
;; the error "there was a problem following the specified user" will be returned. You create and manage friendships with these services.

(define-command notifications/follow (:post-id :twitter-user)
    (twitter-app-uri "notifications/follow.json")
    "Enables device notifications for updates from the specified user. Returns the specified user when successful."
    :user_id "The ID of the user for whom to return results for. Helpful for disambiguating when a valid user ID is also a valid screen name."
    :screen_name "The screen name of the user for whom to return results for. Helpful for disambiguating when a valid screen name is also a user ID."
    :include_entities "When set to either true, t or 1, each tweet will include a node called entities")
 
(define-command notifications/leave (:post-id :twitter-user)
    (twitter-app-uri "notifications/leave.json")
    "Disables notifications for updates from the specified user to the authenticating user.  Returns the specified user when successful."
    :user_id "The ID of the user for whom to return results for. Helpful for disambiguating when a valid user ID is also a valid screen name."
    :screen_name "The screen name of the user for whom to return results for. Helpful for disambiguating when a valid screen name is also a user ID."
    :include_entities "When set to either true, t or 1, each tweet will include a node called entities")
 
 
;;
;; Block Methods
;;
;; Block resources
;;    blocks/create
;;    blocks/destroy
;;    blocks/exists
;;    blocks/blocking
;;    blocks/blocking/ids


;;Destroys a friendship to the blocked user if it exists. Returns the blocked user in the requested format when successful.
(define-command blocks/create (:post :twitter-user)
    (twitter-app-uri "blocks/create.json")
    "Blocks the user specified in the ID parameter as the authenticating user. "
    :user_id "The ID of the user for whom to return results for. Helpful for disambiguating when a valid user ID is also a valid screen name."
    :screen_name "The screen name of the user for whom to return results for. Helpful for disambiguating when a valid screen name is also a user ID."
    :include_entities "When set to either true, t or 1, each tweet will include a node called entities")
 
(define-command blocks/destroy (:post :twitter-user)
    (twitter-app-uri "blocks/destroy.json")
    "Un-blocks the user specified in the ID parameter as the authenticating user.  Returns the un-blocked user in the requested format when successful."
    :user_id "The ID of the user for whom to return results for. Helpful for disambiguating when a valid user ID is also a valid screen name."
    :screen_name "The screen name of the user for whom to return results for. Helpful for disambiguating when a valid screen name is also a user ID."
    :include_entities "When set to either true, t or 1, each tweet will include a node called entities")

(define-command blocks/exists (:get :twitter-user)
    (twitter-app-uri "blocks/exists.json")
    "Returns if the authenticating user is blocking a target user."
    :user_id "The ID of the user for whom to return results for. Helpful for disambiguating when a valid user ID is also a valid screen name."
    :screen_name "The screen name of the user for whom to return results for. Helpful for disambiguating when a valid screen name is also a user ID."
    :include_entities "When set to either true, t or 1, each tweet will include a node called entities")

(define-command blocks/blocking (:get (:twitter-user))
    (twitter-app-uri "blocks/blocking.json")
    "Returns an array of user objects that the authenticating user is blocking."
  :page "Specifies the page of results to retrieve."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities")

(define-command blocks/blocking/ids (:get (:identity))
    (twitter-app-uri "blocks/blocking/ids.json")
    "Returns an array of numeric user ids the authenticating user is blocking.")

 
;;--------------------- end of blocks resources -----------------------------------------------


;;
;;Spam Reporting resources
;;   report_spam
(define-command report_spam (:post :twitter-user)
    (twitter-app-uri "report_spam.json")
    "The user specified in the id is blocked by the authenticated user and reported as a spammer."
  :user_id "The ID of the user for whom to return results for. Helpful for disambiguating when a valid user ID is also a valid screen name."
  :screen_name "The screen name of the user for whom to return results for. Helpful for disambiguating when a valid screen name is also a user ID."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities")



;;--------------- end of spam reporting resources ------------------------------------------

;;
;; Saved Searches resources
;;          saved_searches
;;          saved_searches/show/:id
;;          saved_searches/create
;;          saved_searches/destroy/:id
;; -> in general for all the save searches : ;; TODO : sensible return structure

;; TODO : sensible return structure
(define-command saved-searches (:get :identity)
    (twitter-app-uri "saved_searches.json")
    "Returns the authenticated user's saved search queries.")

;; TODO : sensible return structure
(define-command saved-searches/show/?id (:get-id :identity)
    (twitter-app-uri "saved_searches/show/<id>.json")
    "Retrieve the data for a saved search owned by the authenticating user specified by the given id."
  :id "The ID of the saved search.")

;; TODO : sensible return structure
(define-command saved-searches/create (:post :identity)
    (twitter-app-uri "saved_searches/create.json")
    "Creates a saved search for the authenticated user."
  :query "The query of the search the user would like to save.")

;; TODO : sensible return structure
(define-command saved-searches/destroy/?id (:post-id :identity)
    (twitter-app-uri "saved_searches/destroy/<id>.json")
    "Destroys a saved search for the authenticated user. The search specified by id must be owned by the authenticating user."
  :id "The ID of the saved search.")

;;------------------------- end of saved searches ----------------------------------------------------------

;;
;; OAuth Methods
;;
;; OAuth resources
;;        oauth/request_token
;;        oauth/authorize
;;        oauth/authenticate
;;        oauth/access_token
;;
;; cl-oauth is used in this library.
;; These methods have not been tested and aren't used ...

(define-command oauth/request-token (:get :string)
    (twitter-app-uri "oauth/request_token")
    "Allows a Consumer application to obtain an OAuth Request Token to request user authorization"
  :force_login "Forces the user to enter their credentials to ensure the correct users account is authorized.")

(define-command oauth/authorize (:get :string)
    (twitter-app-uri "oauth/authorize")
    "Allows a Consumer application to use an OAuth Request Token to request user authorization. ")

(define-command oauth/authenticate (:get :string)
    (twitter-app-uri "oauth/authorize")
    "Allows a Consumer application to use an OAuth request_token to request user authorization. "
  :force_login "Forces the user to enter their credentials to ensure the correct users account is authorized.")

(define-command oauth/access-token (:get :string)
    (twitter-app-uri "oauth/access_token")
    "Allows a Consumer application to exchange the OAuth Request Token for an OAuth Access Token. "
  :x_auth_username "The username of the user to obtain a token for. (required when using xAuth)"
  :x_auth_password "The password of the user for which to obtain a token for. (required when using xAuth)"
  :x_auth_mode "Set this value to "client_auth", without the quotes. (required when using xAuth)")

;;------------------ end of oauth methods -------------------------------------------

;;
;; Geo resources
;;         geo/nearby_places ====> deprecated
;;    geo/search
;;    geo/similar_places
;;    geo/reverse_geocode
;;    geo/id/247f43d441defc03
;;    geo/place
;; Test case : (apply 'twitter-op :geo/search :lat 37.7821120598956 :long -122.400612831116  ())
;; return san fran..
;;  (apply 'twitter-op :geo/search :lat 40.88 :long -73.41  ()) --> huntington

;; TODO --> reasonable return structure
;; TODO handle this :   :attribute:street_address "This parameter searches for places which have this given street address. "

(define-command geo/search (:get :identity)
    (twitter-app-uri "geo/search.json")
    "Search for places that can be attached to a statuses/update. Given a latitude and a longitude pair, an IP address, or a name, this request will return a list of all the valid places that can be used as the place_id when updating a status."
    :lat "The latitude to search around. Ignored unless between [-90.0 +90.0] (North is positive) and :long is specified"
    :long "The longitude to search around. Ignored unless between [-180.0 to +180.0] (East is positive) inclusive. must be geo enabled and must have a :lat spec"
    :query "Free-form text to match against while executing a geo-based query, best suited for finding nearby locations by name."
    :ip "An IP address. Used when attempting to fix geolocation based off of the user's IP address."
    :granularity "This is the minimal granularity of place types to return and must be one of: poi, neighborhood (default), city, admin or country."
    :accuracy "A hint on the 'region' in which to search."
    :max_results "A hint as to the number of results to return. "
    :contained_within "This is the place_id which you would like to restrict the search results to. "
    :callback "If supplied, the response will use the JSONP format with a callback of the given name.")



;; Consult the api pages for more information

;; on accruarcy : If a number, then this is a radius in meters, but it can also take a string that is suffixed with ft to specify feet. 
;;    If this is not passed in, then it is assumed to be 0m. If coming from a device, in practice, this value is whatever accuracy the device has 
;;    measuring its location (whether it be coming from a GPS, WiFi triangulation, etc.).

;; TODO --> reasonable return structure
;; TODO handle this :   :attribute:street_address "This parameter searches for places which have this given street address. "

(define-command geo/similar_places (:get :identity)
    (twitter-app-uri "geo/similar_places.json")
    "Locates places near the given coordinates which are similar in name."
    :lat "Required  : The latitude to search around. Ignored unless between [-90.0 +90.0] (North is positive) and :long is specified"
    :long "Required : The longitude to search around. Ignored unless between [-180.0 to +180.0] (East is positive) inclusive." 
    :name "Required : The name a place is known as."
    :contained_within "This is the place_id which you would like to restrict the search results to. "
    :callback "If supplied, the response will use the JSONP format with a callback of the given name.")

;; TODO : reasonable return type

(define-command geo/reverse_geocode (:get :identity)
    (twitter-app-uri "geo/reverse_geocode.json")
    "Given a latitude and a longitude, searches for up to 20 places that can be used as a place_id when updating a status."
    :lat "Required  : The latitude to search around. Ignored unless between [-90.0 +90.0] (North is positive) and :long is specified"
    :long "Required : The longitude to search around. Ignored unless between [-180.0 to +180.0] (East is positive) inclusive." 
    :accuracy "A hint on the 'region' in which to search."
    :granularity "This is the minimal granularity of place types to return and must be one of: poi, neighborhood (default), city, admin or country."
    :max_results "A hint as to the number of results to return. "
    :callback "If supplied, the response will use the JSONP format with a callback of the given name.")

;; TODO : reasonable return type
;; test case   (apply 'twitter-op :geo/id/place_id :id "94965b2c45386f87" nil)

(define-command geo/id/?place_id (:get-id :identity)
    (twitter-app-uri "geo/id/<id>.json")
    "Given a latitude and a longitude, searches for up to 20 places that can be used as a place_id when updating a status."
  :id "A place in the world. These IDs can be retrieved from geo/reverse_geocode.")

;; TODO --> reasonable return structure
;; TODO handle this :   :attribute:street_address "This parameter searches for places which have this given street address. "
;; TODO not tested
(define-command geo/place (:post :identity)
    (twitter-app-uri "geo/place.json")
    "Creates a new place at the given latitude and longitude."
  :name "Required :The name a place is known as."
  :contained_within "Required :The place_id within which the new place can be found. Try and be as close as possible with the containing place. "
  :token "Required: The token found in the response from geo/similar_places."
  :lat "Required :The latitude the place is located at. This parameter will be ignored unless it is inside the range -90.0 to +90.0 (North is positive) inclusive."
  :long "Required :The longitude the place is located at. The valid ranges for longitude is -180.0 to +180.0 (East is positive) inclusive. "
  :callback "If supplied, the response will use the JSONP format with a callback of the given name.")  


;;----------------------------------end of geo resources --------------------------

;; Legal resources
;;    legal/tos
;;    legal/privacy

(define-command legal/tos (:get :string)
    (twitter-app-uri "legal/tos.json")
    "Returns Twitter's' Terms of Service in the requested format. These are not the same as the Developer Terms of Service.")

(define-command legal/privacy (:get :string)
    (twitter-app-uri "legal/privacy.json")
    "Returns Twitter's' Terms of Service in the requested format. These are not the same as the Developer Terms of Service.")

;;
;; Help Methods
;;
;;  Help resources
;;     help/test
;;

(define-command test (:get :string)
    "http://twitter.com/help/test.json"
    "Returns the string 'ok' in the requested format with a 200 OK HTTP status code.")

;; Streamed Tweets resources
;;     statuses/filter
;;     statuses/firehose
;;     statuses/retweet
;;     statuses/sample
;;
;;      TBD 
;;  This defines twitter's streaming api which requires end points to be up.
;;  The Twitter Streaming API allows high-throughput near-realtime access to various subsets of public and protected Twitter data. 
;;  Developers are strongly encouraged to read all of the documentation linked to by this document thoroughly.

;; 
;; SEARCH API
;;
;;
;;  Search resources
;;         search

(define-command search (:get :search-result)
    (twitter-search-uri "search.json")
    "Returns tweets that match a specified query."
  :q "Required. The search string"
  :callback "Only available for JSON format. If supplied, the response will use the JSONP format with a callback of the given name."
  :lang "Restricts tweets to a particular language given by an ISO 639-1 code."
  :locale "Specify the language of the query you are sending (only ja is currently effective). "
  :rpp "The number of tweets to return per page, up to 100."
  :page "The page number."
  :since_id "Returns tweets with status ids greater than the given id"
  :geocode "Returns tweets by users located within a given radius of the given
            latitude/longitude, where the user's location is taken from their 
            Twitter profile.  The parameter value is specified by
            'latitude, longitude, radius' where radius units must be
            specified as either miles or kilometers"
  :show-user "When 'true' adds '<user>:' to the beginning of the tweet.  This is
              useful for readers that do not display Atom's author field.  The
              default is 'false'"
  :result_type "Optional. Specifies what type of search results you would prefer to receive. The current default is 'mixed.' 
                Valid values include:
                             mixed: Include both popular and real time results in the response.
                             recent: return only the most recent results in the response
                             popular: return only the most popular results in the response.")

