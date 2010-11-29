(in-package :cl-user)

(defpackage :twitter
  (:use :cl :drakma :json :anaphora); :trivial-http)
  (:nicknames :cl-twitter :twit)
  (:export 
   ;;------revised api -------
   ;;----lisp rest api ------

   ;; Time line resources as per 
   #:public-timeline
   #:home-timeline
   #:friends-timeline
   #:user-timeline
   #:mentions
   #:retweeted-by-me
   #:retweeted-to-me
   #:retweets-of-me

   ;;  Tweets resources
   #:get-status
   #:update-status
   #:delete-status
   #:retweet-status
   #:status-retweets
   #:status-retweeted-by
   #:status-retweeted-by-ids
   ;;  Tweets resources: app api
   #:tweet?
   #:send-tweet
   #:reply-to
   #:@reply-to
   #:delete-tweet
   #:retweet
   #:retweets
   #:retweeted-by

   ;; Trends resources
   #:trends
   #:current-trends
   #:daily-trends
   #:weekly-trends
   #:location-trends
   #:trends@location
   ;; Trends resources ap api
   #:trending-locations

   ;;  Direct Messages resources
   :direct-messages-received
   :direct-messages-sent
   :send-direct-message
   :delete-direct-message
   ;;  Direct Messages resources api
   #:send-message
   #:delete-message
   #:messages
   #:sent-messages

   ;;  Favorites resources
   #:favorites
   #:create-favorite
   #:delete-favorite
   ;;  Favorites resources api
   #:favor-tweet
   #:unfavor-tweet

   ;; Block resources
   #:create-block
   #:remove-block
   #:blocks
   #:blocked-user-ids
   ;; Block resources api
   #:is-blocked?

   ;;   Account resources : partially supported
   #:verify-credentials
   #:rate-limit-status
   #:end-session
   #:update-profile-colors
   #:update-profile


   ;; User resources
   #:show-user
   #:show-user-by-id
   #:lookup-users
   #:search-users
   ;;cursors not supported
   #:friends-statuses
   ;; cursors not supported
   #:followers-statuses

   ;; Friendship Methods
   #:follow
   #:unfollow
   #:user-a-following-user-b?
   #:follower-relationship
   ;; cursors not supported
   #:incoming-follow-requests
   ;; cursors not supported
   #:outgoing-follow-requests 

   ;; Saved Searches resources
   
   #:saved-searches
   #:saved-search
   #:save-search
   #:delete-search
   ;; Saved Searches resources api
   #:show-search
   #:rm-search

   ;; Social Graph Methods
   ;;  Friends and Followers resources :
   #:friend-ids
   #:follower-ids

   ;;  Friends and Followers resources api:
   #:with-cursor
   #:collect-followers
   #:collect-friends

   ;;  Search resources
   #:search-twitter

   ;;  Search resources api
   #:with-paging
   #:do-search 
   ;;--------------------


   #:*access-file*
   #:*http-request-function*
   ;; Interactive API
   #:authenticate-user
   #:authenticated-user
   #:*twitter-user*
   ;; OAuth
   #:oauth-make-twitter-authorization-uri
   #:oauth-authenticate-user
   #:repl-authenticate-user
   #:get-authenticated-user
   ;; Updates
   #:do-search
   ;; Actions

   ;; I/O
   #:print-tweets
   #:get-tinyurl
   ;; Twitter command operations
   #:twitter-op
   #:twitter-api-condition
;;   #:return-code
;;   #:short-message
;;   #:long-message
;;   #:request-uri
;;   #:request-message
   ;; Documentation
   #:list-commands
   #:command-help
   #:element-help

   ;; User Element
   #:twitter-user
   #:twitter-user-id
   #:twitter-user-name
   #:twitter-user-screen-name
   #:twitter-user-password
   #:twitter-user-location
   #:twitter-user-description
   #:twitter-user-profile-image-url
   #:twitter-user-url
   #:twitter-user-protected
   #:twitter-user-followers-count
   #:twitter-user-status
   #:twitter-user-profile-background-color
   #:twitter-user-profile-text-color
   #:twitter-user-profile-link-color
   #:twitter-user-profile-sidebar-color
   #:twitter-user-profile-sidebar-border-color
   #:twitter-user-profile-sidebar-fill-color
   #:twitter-user-friends-count
   #:twitter-user-created-at
   #:twitter-user-favourites-count
   #:twitter-user-utc-offset
   #:twitter-user-time-zone
   #:twitter-user-profile-background-image-url
   #:twitter-user-profile-background-tile
   #:twitter-user-following
   #:twitter-user-notifications
   #:twitter-user-statuses-count
   ;; Tweet Element
   #:tweet
   #:tweet-id
   #:tweet-created-at 
   #:tweet-text 
   #:tweet-source 
   #:tweet-truncated 
   #:tweet-favorited 
   #:tweet-in-reply-to-status-id 
   #:tweet-in-reply-to-user-id 
   #:tweet-in-reply-to-screen-name 
   #:tweet-user 
   ;; Message element
   #:twitter-message
   #:twitter-message-id
   #:twitter-message-created-at
   #:twitter-message-text
   #:twitter-message-sender-id
   #:twitter-message-sender-screen-name
   #:twitter-message-recipient-id
   #:twitter-message-recipient-screen-name
   #:twitter-message-sender
   #:twitter-message-recipient
   ;; Search Result Wrapper
   #:search-result
   #:search-result-id
   #:search-result-results
   #:search-result-since-id
   #:search-result-max-id
   #:search-result-warning
   #:search-result-refresh-url
   #:search-result-page
   #:search-result-results-per-page
   #:search-result-next-page
   #:search-result-completed-in
   #:search-result-query
   ;; Search Reference
   #:search-ref-id
   #:search-ref-text
   #:search-ref-to-user
   #:search-ref-to-user-id
   #:search-ref-from-user
   #:search-ref-from-user-id
   #:search-ref-created-at
   #:search-ref-iso-language-code
   #:search-ref-profile-image-url
   ;; Search Meta Data
   #:search-ref-metadata-result-type
   #:search-ref-metadata-recent-retweets

   ;;Rate Limit
   #:rate-limit-reset-time
   #:rate-limit-reset-time-in-seconds
   #:rate-limit-remaining-hits
   #:rate-limit-hourly-limit

   ;;End Session
   #:end-session-request
   #:end-session-error

   ;;place type
   #:place-type-code
   #:place-type-name

   ;;place 
   #:place-ural
   #:place-placetype
   #:place-country
   #:place-woeid
   #:place-name

   ;;trend
   #:trend-url
   #:trend-query
   #:trend-promoted-content
   #:trend-events
   #:trend-name

   ;;trend list
   #:trend-list-created-at
   #:trend-list-locations
   #:trend-list-trends
   #:trend-list-as-of

   ;;saved search
   #:saved-search-id
   #:saved-search-query
   #:saved-search-created-at
   #:saved-search-id-str
   #:saved-search-name
   #:saved-search-position

   ;; social graph ;Friends and Followers resources :
   #:cursor-id-id
   #:cursor-id-next-cursor-str
   #:cursor-id-next-cursor
   #:cursor-id-previous-cursor-str
   #:cursor-id-previous-cursor
   #:cursor-id-ids

   ;; conditions
   #:return-code))

   

