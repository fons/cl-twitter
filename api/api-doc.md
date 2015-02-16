# cl-twitter api overview

## Helper Functions

#### twitter-oauth-uri

#### [show](https://github.com/fons/cl-twitter/blob/master/api/twitter-show.lisp)

The *show*  is used to pretty print the results returned by the twitter rest api functions.

<pre><code>TWIT-REPL> (home-timeline)
(#&ltTWEET 'Franzinc' id:565564990447702016>
 #&ltTWEET 'Franzinc' id:565534775893835777>
 [...])
</code></pre>

<pre><code>TWIT-REPL> (show(home-timeline))
Franzinc                                                                                                                Wed Feb 11 17:36:16 +0000 2015
RT @HaystaxTech: Haystax CTO Bryan Ware will be presenting Enterprise Threat Management: A Comprehensive Risk-Based Approach at CyberOSINT …
 
Franzinc                                                                                                                Wed Feb 11 15:36:12 +0000 2015
Check out the new #AllegroGraph 5.0 release - Free Download  http://t.co/lOiVl4iIYk  #nosql #graphdatabase
[...]         
</code></pre>

## [Twitter Rest API](https://dev.twitter.com/rest/public)

### Time line resources 

#### [user-timeline](https://dev.twitter.com/rest/reference/get/statuses/user_timeline) 

#### home-timeline
#### friends-timeline

#### mentions
#### retweeted-by-me
#### retweeted-to-me
#### retweets-of-me

### Time Line Resources api
####collect-home-timeline
####collect-friends-timeline
####collect-user-timeline

###  Tweets resources

####get-status
####update-status
####delete-status
####retweet-status
####status-retweets
####status-retweeted-by
####status-retweeted-by-ids

###  Tweets resources: app api

####tweet?
####tweet
####reply-to
####@reply-to
####delete-tweet
####retweet
####retweets
####retweeted-by
####@mention

### Trends resources

####trends
####current-trends
####daily-trends
####weekly-trends
####location-trends
####trends@location


### Trends resources ap api

####trending-locations

###  Direct Messages resources
   :direct-messages-received
   :direct-messages-sent
   :send-direct-message
   :delete-direct-message
###  Direct Messages resources api

####send-message
####delete-message
####messages
####sent-messages

###  Favorites resources

####favorites
####create-favorite
####delete-favorite

###  Favorites resources api

####favor-tweet
####unfavor-tweet

### Block resources
####create-block
####remove-block
####blocks
####blocked-user-ids
### Block resources api
####is-blocked?

###   Account resources : partially supported
####verify-credentials
####rate-limit-status
####end-session
####update-profile-colors
####update-profile


### User resources
####show-user
####show-user-by-id
####lookup-users
####search-users
####friends-statuses
####followers-statuses

### User Resources api
####collect-friend-statuses
####collect-follower-statuses
####do-user-search

### Friendship Methods
####follow
####unfollow
####user-a-following-user-b?
####follower-relationship
### cursors not supported
####incoming-follow-requests
### cursors not supported
####outgoing-follow-requests

### Saved Searches resources

####saved-searches
####saved-search
####save-search
####delete-search
### Saved Searches resources api
####show-search
####rm-search

### Social Graph Methods
###  Friends and Followers resources :
####friend-ids
####follower-ids

###  Friends and Followers resources api:
####with-cursor
####collect-follower-ids
####collect-friend-ids

###  Search resources
####search-twitter

###  Search resources api
####with-paging
####do-search

### List resources

####create-user-list
####update-user-list
####get-user-list
####get-user-lists
####delete-user-lists
####statuses-user-lists
####memberships-user-lists
####subscriptions-user-lists

####collect-user-lists
####collect-user-list-memberships
####collect-user-list-subscriptions
####collect-user-list-statuses

####user-list-timeline
####member-list-timeline
####subscriber-list-timeline

###List Members resources
####user-list-members
####collect-user-list-members
####add-user-list-members
####delete-user-list-members
####user-list-members-p

### List Subscribers resources

####user-list-subscribers
####collect-user-list-subscribers
####add-user-list-subscribers
####delete-user-list-subscribers
####user-list-subscribers-p

### Notification resources
####follow-notification
####leave-notification

### miscellaneous resources
####twitter-test
####twitter-terms-of-service
####twitter-privacy
####report-spam

### Notification resources
####follow-notification
####leave-notification

### Geo resources

####geo-search
####geo-similar-places
####geo-reverse-geocode
####geo-place-by-id
####geo-register-place

### Geo resources api :
####geo-print-places
####geo-token

###--------------------

#### \*access-file*
#### \*http-request-function*

### Interactive API
####authenticate-user
####authenticated-user
####\*twitter-user*

### OAuth
####oauth-make-twitter-authorization-uri
####oauth-authenticate-user
####repl-authenticate-user
####get-authenticated-user

### Updates

### I/O
####print-tweets
####get-tinyurl
### Twitter command operations
####twitter-op
####twitter-api-condition
#####return-code
#####short-message
#####long-message
#####request-uri
#####request-message

### Documentation

####list-commands
####command-help
####element-help

### User Element

####twitter-user
####twitter-user-id
####twitter-user-name
####twitter-user-access-token
####twitter-user-screen-name
####twitter-user-password
####twitter-user-location
####twitter-user-description
####twitter-user-profile-image-url
####twitter-user-url
####twitter-user-protected
####twitter-user-followers-count
####twitter-user-status
####twitter-user-profile-background-color
####twitter-user-profile-text-color
####twitter-user-profile-link-color
####twitter-user-profile-sidebar-color
####twitter-user-profile-sidebar-border-color
####twitter-user-profile-sidebar-fill-color
####twitter-user-friends-count
####twitter-user-created-at
####twitter-user-favourites-count
####twitter-user-utc-offset
####twitter-user-time-zone
####twitter-user-profile-background-image-url
####twitter-user-profile-background-tile
####twitter-user-following
####twitter-user-notifications
####twitter-user-statuses-count

### Tweet Element
####tweet
####tweet-id
####tweet-created-at
####tweet-text
####tweet-source
####tweet-truncated
####tweet-favorited
####tweet-in-reply-to-status-id
####tweet-in-reply-to-user-id
####tweet-in-reply-to-screen-name
####tweet-user
####tweet-entities

### Message element
####twitter-message
####twitter-message-id
####twitter-message-created-at
####twitter-message-text
####twitter-message-sender-id
####twitter-message-sender-screen-name
####twitter-message-recipient-id
####twitter-message-recipient-screen-name
####twitter-message-sender
####twitter-message-recipient
### Search Result Wrapper
####search-result
####search-result-statuses
####search-result-search-metadata
### Search Meta Data
####search-metadata
####search-metadata-completed-in
####search-metadata-max-id
####search-metadata-max-id_str
####search-metadata-next-results
####search-metadata-query
####search-metadata-refresh-url
####search-metadata-count
####search-metadata-since-id
####search-metadata-since-id-str


###Rate Limit
####rate-limit-reset-time
####rate-limit-reset-time-in-seconds
####rate-limit-remaining-hits
####rate-limit-hourly-limit

###End Session
####end-session-request
####end-session-error

###place type
####place-type-code
####place-type-name

###place
####place-ural
####place-placetype
####place-country
####place-woeid
####place-name

###trend
####trend
####trend-id

####trend-url
####trend-query
####trend-promoted-content
####trend-events
####trend-name

###trend list
####trend-list
####trend-list-id
####trend-list-created-at
####trend-list-locations
####trend-list-trends
####trend-list-as-of

###saved search
####saved-search-id
####saved-search-query
####saved-search-created-at
####saved-search-id-str
####saved-search-name
####saved-search-position

### social graph ;Friends and Followers resources :
####social-graph-cursor-id
####social-graph-cursor-id-id
####social-graph-cursor-id-screen-name
####social-graph-cursor-id-next-cursor-str
####social-graph-cursor-id-next-cursor
####social-graph-cursor-id-previous-cursor-str
####social-graph-cursor-id-previous-cursor
####social-graph-cursor-id-ids
###list resources
####cursor-user-lists-id
####cursor-user-lists-next-cursor-str
####cursor-user-lists-next-cursor
####cursor-user-lists-previous-cursor-str
####cursor-user-lists-previous-cursor
####cursor-user-lists-lists

####list-type-id
####list-type-mode
####list-type-description
####list-type-slug
####list-type-uri
####list-type-member-count
####list-type-id-str
####list-type-full-name
####list-type-user

###geo resources

####geo-coordinate-id
####geo-coordinate-long
####geo-coordinate-lat

####geo-bounding-box-id
####geo-bounding-box-coordinates
####geo-bounding-box-type

####geo-query-id
####geo-query-type
####geo-query-params
####geo-query-url

####geo-result-id
####geo-result-result
####geo-result-query

####geo-places-id
####geo-places-token
####geo-places-places

####geo-place-id
####geo-place-code
####geo-place-name
####geo-place-attributes
####geo-place-url
####geo-place-place-type
####geo-place-full-name
####geo-place-country
####geo-place-bounding-box
####geo-place-geometry
####geo-place-country-code
####geo-place-polylines
####geo-place-contained-within
####print-geo-place

####geo-token
####geo-print-places

###twitter-entity exports
####twitter-entities-id
####twitter-entities-hashtags
####twitter-entities-user-mentions
####twitter-entities-media
####twitter-entities-urls

####media-entity-id
####media-entity-id-str
####media-entity-media-url
####media-entity-media-url-https
####media-entity-url
####media-entity-display-url
####media-entity-expanded-url
####media-entity-type
####media-entity-sizes
####media-entity-indices

####user-mention-id
####user-mention-id-str
####user-mention-screen-name
####user-mention-name
####user-mention-screen-indices

####hashtag-id
####hashtag-text
####hashtag-indices

####url-entity-id
####url-entity-url
####url-entity-display-url
####url-entity-expanded-url
####url-entity-indices

####media-size-id
####media-size-w
####media-size-h
####media-size-resize

####entity-hashtags
####entity-user-mentions
####entity-urls
####entity-media

###persistence/lookup hooks
####register-twitter-object
####lookup-twitter-object

### conditions

####return-code
####default-file-path
####get-user

### db/cache
####use-cache
####unique-id
####caches
####drop-cache
####read-cache-spec
####write-cache-spec
####cache-map-reduce
####map-reduce-cache

####db-initialize
####db-shutdown
####db-store-object
####db-map-reduce
####db-status
####use-db

