(in-package :twitter)

(defvar *twitter-app-uri*       "http://api.twitter.com/1/")
(defvar *twitter-search-uri*    "http://search.twitter.com/")

(defun twitter-app-uri (method)
  (concatenate 'string *twitter-app-uri* method))

(defun twitter-search-uri (method)
  (concatenate 'string *twitter-search-uri* method))



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

