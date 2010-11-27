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
;;Spam Reporting resources
;;   report_spam
(define-command report_spam (:post :twitter-user)
    (twitter-app-uri "report_spam.json")
    "The user specified in the id is blocked by the authenticated user and reported as a spammer."
  :user_id "The ID of the user for whom to return results for. Helpful for disambiguating when a valid user ID is also a valid screen name."
  :screen_name "The screen name of the user for whom to return results for. Helpful for disambiguating when a valid screen name is also a user ID."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities")



;;--------------- end of spam reporting resources ------------------------------------------


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

