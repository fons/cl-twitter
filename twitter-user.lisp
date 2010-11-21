(in-package :twitter)

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

(define-command users/profile-image/?screen_name (:get-id (:identity) )
    (twitter-app-uri "users/profile_image/<id>.json")
    "Access the profile image in various sizes for the user with the indicated screen_name. "
  :id   "Required ; screen name of the user" 
  :size "Optional; Specifies the size of image to fetch.")



;; TODO : cursor doesn't work : this does not return a "twitter-user" list.
;; Probably better to create a seperate 'cursor' command ?
(define-command statuses/friends (:get (:twitter-user) )
    (twitter-app-uri "statuses/friends.json")
    "Returns the authenticating user's friends, each with current status inline. They are ordered by the order in which they were added as friends. 
     It's also possible to request another user's recent friends list via the id parameter below."
  :user_id "The ID of the user for whom to return results for." 
  :screen_name "The screen name of the user for whom to return results for."
  :cursor "Breaks the results into pages. This is recommended for users who are following many users. Provide a value of -1 to begin paging. 
          Provide values as returned in the response body's next_cursor and previous_cursor attributes to page back and forth in the list."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities." )


(define-command statuses/followers (:get (:twitter-user) )
    (twitter-app-uri "statuses/followers.json")
    "Returns the authenticating user's followers, each with current status inline. "
  :user_id "The ID of the user for whom to return results for." 
  :screen_name "The screen name of the user for whom to return results for."
  :cursor "Breaks the results into pages. This is recommended for users who are following many users. Provide a value of -1 to begin paging. 
          Provide values as returned in the response body's next_cursor and previous_cursor attributes to page back and forth in the list."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities." )

;; ---level 1 api --------------------------------

(defun show-user (screen-name &rest args)
  (apply 'twitter-op :users/show :screen_name screen-name  args))

(defun show-user-by-id (user-id &rest args)
  (apply 'twitter-op :users/show :user_id user-id  args))

;; probably should built in some resiliency in that the user can pass in a list ??
(defun lookup-users (screen-names user-ids)
  (apply 'twitter-op :users/lookup :user_id user-ids :screen_name screen-names  args))

;; does the query need to be url encoded ????
(defun search-users (query)
  (apply 'twitter-op :users/search :q query  args))

(defun friends-statuses (&rest args)
  (apply 'twitter-op :statuses/friends  args))

(defun follower-statuses (&rest args)
  (apply 'twitter-op :statuses/followers  args))

;;--------------------------------------------------------------

