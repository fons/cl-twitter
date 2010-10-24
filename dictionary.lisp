(in-package :twitter)

;;
;; STATUS METHODS
;;

(define-command public-timeline (:get (:tweet))
    "http://twitter.com/statuses/public_timeline.json"
  "Returns 20 most recent statuses from non-protected users")

(define-command friends-timeline (:get (:tweet))
    "http://twitter.com/statuses/friends_timeline.json"
  "Returns 20 most recent statuss from provided users friends"
  :since "Optional.  Narrows returned results to those created after
          the HTTP-formatted date."
  :since-id "Optional. Returns statuses with an ID more recent than this one"
  :count "Optional. Returns the number of statuses to receive"
  :page "Optional. Returns the statuses at page * count offset")

(define-command user-timeline (:get (:tweet))
    "http://twitter.com/statuses/user_timeline.json"
  "Returns 20 most recent statuses from provided users friends"
  :id "Optional. Specifies the ID or screen name of the user for whom to return the friends_timeline"
  :since "Optional.  Narrows returned results to those created after
          the HTTP-formatted date."
  :since-id "Optional. Returns statuses with an ID more recent than this one"
  :count "Optional. Returns the number of statuses to receive"
  :page "Optional. Returns the statuses at page * count offset")

(define-command tweet-show (:get-id :tweet)
    "http://twitter.com/statuses/show/<id>.json"
  "Returns a single-status specified by the id parameter"
  :id "Required. The numberical ID of the status you want to retrieve")

(define-command tweet-delete (:post-id :tweet)
    "http://twitter.com/statuses/destroy/<id>.json"
    "Returns a single-status specified by the id parameter"
  :id "Required. The numberical ID of the status you want to retrieve")

(define-command tweet-update (:post :tweet)
    "http://twitter.com/statuses/update.json"
    "Updates the authenticating user's status.  Requires status parameter"
  :status "Required.  The text of your status update.  Must be less than
           140 characters"
  :in-reply-to-status-id "Optional.  The id of an existing status 
           that this post is in reply to.  This sets the appropriate
           attribute of the result status.")

(define-command tweet-replies (:post (:tweet))
    "http://twitter.com/statuses/update.json"
    "Updates the authenticating user's status.  Requires status parameter"
  :page "Optional. Returns the statuses at page * 20"
  :since "Optional.  Narrows returned results to those created after
          the HTTP-formatted date."
  :since-id "Optional. Returns statuses with an ID more recent than this one")


;;
;; USER METHODS
;;


(define-command user-friends (:get (:twitter-user))
    "http://twitter.com/statuses/friends.json"
    "Returns the authenticating user's friends, each with current status inline. They are ordered by the order in which they were added as friends. It's also possible to request another user's recent friends list via the id parameter below."
  :id "Optional.  The ID or screen name of (the user for whom to request a list of friends."
  :page "Optional. Retrieves the next 100 friends.")

(define-command user-followers (:get (:twitter-user))
    "http://twitter.com/statuses/followers.json"
    "Returns the authenticating user's followers, each with current status inline.  They are ordered by the order in which they joined Twitter (this is going to be changed)."
  :id "Optional.  The ID or screen name of (the user for whom to request a list of followers."
  :page "Optional. Retrieves the next 100 followers.")

(define-command user-show (:get-id :twitter-user)
    "http://twitter.com/users/show/<id>.json"
  "Returns a single-status specified by the id parameter"
  :id "Required. The ID or screen name of a user."
  :email "Optional.  May be used in place of :id.")

(define-command user-followers/ids (:get (:identity))
    "http://twitter.com/followers/ids.json"
    "Returns the authenticating user's friends, each with current status inline. They are ordered by the order in which they were added as friends. It's also possible to request another user's recent friends list via the id parameter below."
  :id "Optional.  The ID or screen name of (the user for whom to request a list of friends."
  :user_id "Optional.  The ID or screen name of (the user for whom to request a list of friends."
  :screen_name "Optional.  The ID or screen name of (the user for whom to request a list of friends."
  :page "Optional. Retrieves the next 5000 ids.")

(define-command user-friends/ids (:get (:identity))
    "http://twitter.com/friends/ids.json"
    "Returns the authenticating user's friends, each with current status inline. They are ordered by the order in which they were added as friends. It's also possible to request another user's recent friends list via the id parameter below."
  :id "Optional.  The ID or screen name of (the user for whom to request a list of friends."
  :user_id "Optional.  The ID or screen name of (the user for whom to request a list of friends."
  :screen_name "Optional.  The ID or screen name of (the user for whom to request a list of friends."
  :page "Optional. Retrieves the next 5000 ids.")


;;
;; DIRECT MESSAGES
;;

(define-command messages-received (:get (:twitter-message))
    "http://twitter.com/direct_messages.json"
    "Returns a list of the 20 most recent direct messages sent to the authenticating user.  The XML and JSON versions include detailed information about the sending and recipient users."
  :since  "Optional.  Narrows the resulting list of direct messages to just those sent after the specified HTTP-formatted date, up to 24 hours old.  The same behavior is available by setting the If-Modified-Since parameter in your HTTP request."
  :since_id "Optional.  Returns only direct messages with an ID greater than (that is, more recent than) the specified ID."
  :page "Optional. Retrieves the 20 next most recent direct messages.")

(define-command messages-sent (:get (:twitter-message))
    "http://twitter.com/direct_messages/sent.json"
    "Returns a list of the 20 most recent direct messages sent by the authenticating user.  The XML and JSON versions include detailed information about the sending and recipient users. "
  :since "Optional.  Narrows the resulting list of direct messages to just those sent 
          after the specified HTTP-formatted date, up to 24 hours old.  The same 
          behavior is available by setting the If-Modified-Since parameter in your 
          HTTP request."
  :since-id  "Optional.  Returns only sent direct messages with an ID greater than 
              (that is, more recent than) the specified ID."
  :page "Optional. Retrieves the 20 next most recent direct messages sent.")
 
(define-command message-new (:post :twitter-message)
    "http://twitter.com/direct_messages/new.json"
    "Sends a new direct message to the specified user from the authenticating user.  Requires both the user and text parameters below.  Request must be a POST.  Returns the sent message in the requested format when successful."
  :user "Required.  The ID or screen name of the recipient user."
  :text "Required.  The text of your direct message.  Be sure to URL encode as necessary, and keep it under 140 characters.")
 
(define-command message-delete (:post-id (:twitter-message))
    "http://twitter.com/direct_messages/destroy/<id>.json"
    "Destroys the direct message specified in the required ID parameter.  The authenticating user must be the recipient of the specified direct message."
  :id "Required.  The ID of the direct message to destroy.")

;;
;; Friendship Methods
;;

(define-command friend-create (:post-id :twitter-user)
    "http://twitter.com/friendships/create/<id>.json"
    "Befriends the user specified in the ID parameter as the authenticating user.  Returns the befriended user in the requested format when successful.  Returns a string describing the failure condition when unsuccessful."
  :id  "Required.  The ID or screen name of the user to befriend.  "
  :follow  "Optional.  Enable notifications for the target user in addition to becoming friends.  ")

 
(define-command friend-delete (:post-id :twitter-user)
    "http://twitter.com/friendships/destroy/<id>.json"
    "Discontinues friendship with the user specified in the ID parameter as the authenticating user.  Returns the un-friended user in the requested format when successful.  Returns a string describing the failure condition when unsuccessful.  "
  :id  "Required.  The ID or screen name of the user with whom to discontinue friendship.")
 
(define-command friend-exists (:post :twitter-user)
    "http://twitter.com/friendships/exists.json"
    "Tests if a friendship exists between two users."
  :user-a "Required.  The ID or screen_name of the first user to test friendship for."
  :user-b "Required.  The ID or screen_name of the second user to test friendship for.")

;;
;; Social Graph Methods

(define-command friend-ids (:get :identity)
  "http://twitter.com/friends/ids.json"
  "Returns an array of numeric IDs jsonery user the specified user is following."
  :id "Optional.  The ID or screen_name of the user to retrieve the friends ID list for.")
 
(define-command follower-ids (:get :identity)
    "http://twitter.com/followers/ids.json"
    "Returns an array of numeric IDs for every user the specified user is followed by."
  :id "Optional.  The ID or screen_name of the user to retrieve the friends ID list for.")

 
;;
;; ACCOUNT METHODS
;;

(define-command verify-credentials (:get :identity)
    "http://twitter.com/account/verify_credentials.json"
    "Returns an HTTP 200 OK response code and a representation of the requesting user if authentication was successful; returns a 401 status code and an error message if not.  Use this method to test if supplied user credentials are valid.")

(define-command end-session (:post :identity)
    "http://twitter.com/account/end_session.json"
    "Ends the session of the authenticating user, returning a null cookie.  Use this method to sign users out of client-facing applications like widgets.")
 
(define-command update-delivery-device (:post :twitter-user)
    "http://twitter.com/account/update_delivery_device.json"
    "Sets which device Twitter delivers updates to for the authenticating user.  Sending none as the device parameter will disable IM or SMS updates."
  :device "Required.  Must be one of: sms, im, none.")
 
(define-command update-profile-colors (:post :twitter-user)
    "http://twitter.com/account/update_profile_colors.json"
    "Sets one or more hex values that control the color scheme of the authenticating user's
     profile page on twitter.com.  These values are also returned in the /users/show 
     API method."
  :profile-background-color "Optional.  one or more of the following parameters must be present.  Each parameter's value must be a valid hexidecimal value, and may be either three or six characters (ex: #fff or #ffffff)"
  :profile-text-color "Optional"
  :profile-link-color "Optional"
  :profile-sidebar-fill-color "Optional"
  :profile-sidebar-border-color "Optional")
 
(define-command update-profile-image (:post :twitter-user)
    "http://twitter.com/account/update_profile_image.json"
    "Updates the authenticating user's profile image.  Expects raw multipart data, not a URL to an image."
  :image "Required.  Must be a valid GIF, JPG, or PNG image of less than 700 kilobytes in size.  Images with width larger than 500 pixels will be scaled down.")
 
(define-command update-profile-background-image (:post :twitter-user)
    "http://twitter.com/account/update_profile_background_image.json"
    "Updates the authenticating user's profile background image.  Expects raw multipart data, not a URL to an image."
  :image "Required.  Must be a valid GIF, JPG, or PNG image of less than 800 kilobytes in size.  Images with width larger than 2048 pixels will be scaled down.")
 
(define-command rate-limit-status (:get :identity)
    "http://twitter.com/account/rate_limit_status.json"
    "Returns the remaining number of API requests available to the requesting user before the API limit is reached for the current hour. Calls to rate_limit_status do not count against the rate limit.  If authentication credentials are provided, the rate limit status for the authenticating user is returned.  Otherwise, the rate limit status for the requester's IP address is returned.")
 
(define-command update-profile (:post :twitter-user)
    "http://twitter.com/account/update_profile.json"
    "Sets values that users are able to set under the 'Account' tab of their settings page. Only the parameters specified will be updated; to only update the 'name' attribute, for example, only include that parameter in your request."
  :name "Optional. Maximum of 20 characters. One or more of all these parameters must be present."
  :email "Optional. Maximum of 40 characters. Must be a valid email address. "
  :url "Optional. Maximum of 100 characters. Will be prepended with 'http://' if not present."
  :location "Optional. Maximum of 30 characters. The contents are not normalized or geocoded in any way."
  :description "Optional. Maximum of 160 characters.")
 
;;
;; Favorite Methods
;; 

(define-command favorites (:get (:tweet))
    "http://twitter.com/favorites.json"
    "Returns the 20 most recent favorite statuses for the authenticating user or user specified by the ID parameter in the requested format."
  :id "Optional. The ID or screen name of the user for whom to request a list of favorite statuses.  "
  :page "Optional. Retrieves the 20 next most recent favorite statuses.")
 
(define-command favorite-create (:post-id :tweet)
    "http://twitter.com/favorites/create/<id>.json"
    "Favorites the status specified in the ID parameter as the authenticating user.  Returns the favorite status when successful."
  :id "Required.  The ID of the status to favorite.")
 
(define-command favorite-delete (:post-id :tweet)
    "http://twitter.com/favorites/destroy/<id>.json"
    "Un-favorites the status specified in the ID parameter as the authenticating user.  Returns the un-favorited status in the requested format when successful."
  :id "Required.  The ID of the status to un-favorite.")

;;
;; Notification Methods
;; 

(define-command follow (:post-id :twitter-user)
    "http://twitter.com/notifications/follow/<id>.json"
    "Enables notifications for updates from the specified user to the authenticating user.  Returns the specified user when successful.  Must be friends with the user in order to follow.  Can also use :follow argument to :friend-create."
  :id "Required.  The ID or screen name of the user to follow.")
 
(define-command leave (:post-id :twitter-user)
    "http://twitter.com/notifications/leave/<id>.json"
    "Disables notifications for updates from the specified user to the authenticating user.  Returns the specified user when successful."
  :id "Required.  The ID or screen name of the user to leave.")
 
;; NOTE: The Notification Methods require the authenticated user to already be friends with the specified user otherwise the error "there was a problem following the specified user" will be returned. You create and manage friendships with these services.
 
;;
;; Block Methods
;;

(define-command create-block (:post-id :twitter-user)
    "http://twitter.com/blocks/create/<id>.json"
    "Blocks the user specified in the ID parameter as the authenticating user.  Returns the blocked user in the requested format when successful.  You can find out more about blocking in the Twitter Support Knowledge Base."
  :id "Required.  The ID or screen_name of the user to block.")
 
(define-command delete-block (:post-id :twitter-user)
    "http://twitter.com/blocks/destroy/<id>.json"
    "Un-blocks the user specified in the ID parameter as the authenticating user.  Returns the un-blocked user in the requested format when successful."
  :id "Required.  The ID or screen_name of the user to un-block.")
 

;;
;; OAuth Methods
;;

(define-command oauth-request (:get :string)
    "http://www.twitter.com/oauth/request_token"
    "Requests that the user authorize the client to act on their behalf")

(define-command oauth-access (:get :string)
    "http://www.twitter.com/oauth/request_token"
    "Requests a user token for subsequent commands")

(define-command oauth-authorize (:get :string)
    "http://www.twitter.com/oauth/authorize"
    "foo")

(define-command oauth-authenticate (:get :string)
    "http://www.twitter.com/oauth/authenticate"
    "foo")

;;
;; Help Methods
;;

(define-command test (:get :string)
    "http://twitter.com/help/test.json"
    "Returns the string 'ok' in the requested format with a 200 OK HTTP status code.")

;; 
;; SEARCH API
;;

(define-command search (:get :search-result)
    "http://search.twitter.com/search.json"
    "Returns an HTTP 200 OK response code and a representation of the requesting user if authentication was successful; returns a 401 status code and an error message if not.  Use this method to test if supplied user credentials are valid."
  :q "Required. The search string"
  :lang "Restricts tweets to a particular language"
  :rpp "The number of tweets to return per page, up to 100."
  :max-id "The maximum tweet id for the search results (for pagination mostly)."
  :page "The page number."
  :since-id "Returns tweets with status ids greater than the given id"
  :max-id "The maximum tweet id for the search results (for pagination mostly)."
  :geocode "Returns tweets by users located within a given radius of the given
            latitude/longitude, where the user's location is taken from their 
            Twitter profile.  The parameter value is specified by
            'latitude, longitude, radius' where radius units must be
            specified as either miles or kilometers"
  :show-user "When 'true' adds '<user>:' to the beginning of the tweet.  This is
              useful for readers that do not display Atom's author field.  The
              default is 'false'")

(define-command trends (:get :trends)
    "http://search.twitter.com/trends.json"
    "Returns the top ten queries that are currently trending on Twitter.  The response includes the time of the request, the name of each trending topic, and the url to the Twitter Search results page for that topic.  Currently, the only supported format for this method is JSON.  The callback parameter is supported, however.")

(defmethod parse-record (response (prim-type (eql :trends)))
  "Unparse and unpack trend type records"
  (cons (get-value :as-of response)
	(mapcar (lambda (trend)
		  (list (cdr (first trend))
			(cdr (second trend))))
		(get-value :trends response))))

(define-command current-trends (:get :new-trends)
    "http://search.twitter.com/trends/current.json"
    "Returns the top ten queries that are currently trending on Twitter.  The response includes the time of the request, the name of each trending topic, and the url to the Twitter Search results page for that topic.  Currently, the only supported format for this method is JSON.  The callback parameter is supported, however.")

(define-command daily-trends (:get :new-trends)
    "http://search.twitter.com/trends/daily.json"
    "Returns the top 20 trending topics for each hour in a given day.")

(define-command weekly-trends (:get :new-trends)
    "http://search.twitter.com/trends/weekly.json"
    "Returns the top 20 trending topics for each day in a given day.")

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
