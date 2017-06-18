(in-package :cl-twitter)


;;
;; Status/Tweet Element --> depends on twitter user..
;;

(define-element tweet ((user twitter-user) (entities twitter-entities))
  "A status element consisting of information on a status and a nested
   user element"
  (id "" nil)
  (contributors nil nil)
  (created-at "" nil)
  (text "" nil)
  (source "" nil)
  (truncated "" nil)
  (favorited "" nil)
  (in-reply-to-status-id "" nil)
  (in-reply-to-user-id "" nil)
  (in-reply-to-screen-name "" nil)
  (geo "" nil)
  (geo-enabled "" nil)
  ;; embedded twitter entities
  (entities "" nil)
  ;; embedded user
  (user "" nil))

(defmethod print-object ((tweet tweet) stream)
  (format stream "#<TWEET '~A' id:~A>"
	  (if (tweet-user tweet)
	      (twitter-user-screen-name (tweet-user tweet))
	      "none")
	  (tweet-id tweet)))

(defmethod describe-object ((tweet tweet) stream)
  (format stream "status: \"~A\"~%by: ~A (~A) on: ~A"
	  (tweet-text tweet)
	  (if (tweet-user tweet)
	      (twitter-user-name (tweet-user tweet))
	      "")
	  (if (tweet-user tweet)
	      (twitter-user-screen-name (tweet-user tweet))
	      "none")
	  (tweet-created-at tweet)))

(defmethod print-tweet ((tweet (eql nil)))
  ())

(defmethod print-tweet (tweet)
  (format t "~A~%by ~A at ~A~%~%"
	  (tweet-text tweet)
	  (twitter-user-screen-name (tweet-user tweet))
	  (tweet-created-at tweet)))

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
(define-element cursor-tweet ((user-ids (identity)))
  "a cursor element "
  (next-cursor-str     ""  nil)
  (previous-cursor-str "" nil)
  (next-cursor         ""  nil)
  (user-ids            ""  nil)
  (previous-cursor     ""  nil))

(defmethod print-object ((ref cursor-tweet) stream)
  (format stream "#<CURSOR-TWEET '~A:~A'>" (cursor-tweet-user-ids ref) (length (cursor-tweet-user-ids ref)) ))


(define-command statuses/retweets/?id (:get-id (:tweet))
    (twitter-app-uri "statuses/retweets/<id>.json")
    "Returns up to 100 of the first retweets of a given tweet."
  :id "Required. The numberical ID of the status you want to retrieve"
  :count "Specifies the number of records to retrieve. Must be less than or equal to 100."
  :trim_user "When set to either true, t or 1, each tweet returned in a timeline will include a user object including only the status authors numerical ID.")


(define-command statuses/show/?id (:get-id :tweet)
    (twitter-app-uri "statuses/show/<id>.json")
    "Returns a single-status specified by the id parameter"
  :id "Required. The numberical ID of the status you want to retrieve"
  :trim_user "When set to either true, t or 1, each tweet returned in a timeline will include a user object including only the status authors numerical ID."
  :include_my_retweet "When set to either true, t or 1, any Tweets returned that have been retweeted by the authenticating user will include an additional current_user_retweet node, containing the ID of the source status for the retweet"
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities. ")

(define-command statuses/destroy/?id (:post-id :tweet)
    (twitter-app-uri "statuses/destroy/<id>.json")
    "Destroys the status specified by the required ID parameter."
  :id "Required. The numberical ID of the status you want to retrieve"
  :trim_user "When set to either true, t or 1, each tweet returned in a timeline will include a user object including only the status authors numerical ID.")

(define-command statuses/update (:post :tweet)
    (twitter-app-uri "statuses/update.json")
    "Updates the authenticating user's status.  Requires status parameter"
  :status "Required.  The text of your status update.  Must be less than 140 characters"
  :in_reply_to_status_id "Optional.  The id of an existing status that this post is in reply to.  This sets the appropriate attribute of the result status."
  :possibly_sensitive "If you upload Tweet media that might be considered sensitive content you should set this value to true. Defaults to false."
  :lat "The latitude of the location this tweet refers to."
  :long "The longitude of the location this tweet refers to."
  :place_id "A place in the world. These IDs can be retrieved from geo/reverse_geocode."
  :display_coordinates "Whether or not to put a pin on the exact coordinates a tweet has been sent from"
  :trim_user "When set to either true, t or 1, each tweet returned in a timeline will include a user object including only the status authors numerical ID."
)
;;  :media_ids "A list of media ids to associate with the Tweet. You may associated up to 4 media to a Tweet. See Uploading Media for further details on uploading media."

(define-command statuses/retweet/?id (:post-id :tweet)
    (twitter-app-uri "statuses/retweet/<id>.json")
    "Retweets a tweet. Returns the original tweet with retweet details embedded."
  :id "Required. The numberical ID of the status you want to retrieve"
  :trim_user "When set to either true, t or 1, each tweet returned in a timeline will include a user object including only the status authors numerical ID."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities. ")


(define-command statuses/retweeters/ids (:get :cursor-tweet)
    (twitter-app-uri "statuses/retweeters/ids.json")
    "Show user ids of up to 100 users who retweeted the status."
  :id            "Required. The numberical ID of the status you want to retrieve"
  :cursor       "Causes the list of IDs to be broken into pages of no more than 100 IDs at a time"
  :stringify_ids "Provide this option to have ids returned as strings instead.")


(define-command statuses/lookup (:get (:tweet))
    (twitter-app-uri "statuses/lookup.json")
   "Returns fully-hydrated tweet objects for up to 100 tweets per request, as specified by comma-separated values passed to the id parameter."
  :id            "A comma separated list of tweet IDs, up to 100 are allowed in a single request."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities. "
  :trim_user "When set to either true, t or 1, each tweet returned in a timeline will include a user object including only the status authors numerical ID."
  :map      "When using the map parameter, tweets that do not exist or cannot be viewed by the current user will still have their key represented but with an explicitly null value paired with it")

;;;---------------------------- end of command definitions --------------------------
(define-twitter-method statuses-show( (tweet-id) &key (trim-user nil)(include-my-retweet nil) (include-entities t)) :statuses/show/?id :id )
(define-twitter-method statuses-retweets ( (tweet-id) &key (count nil) (trim-user nil) )            :statuses/retweets/?id          :id )

(define-twitter-method statuses-destroy( (tweet-id) &key (trim-user nil) )    :statuses/destroy/?id :id )
(define-twitter-method statuses-update( (status) &key (in-reply-to-status-id nil) (possibly-sensitive nil) (lat nil) (long nil)
					 (place-id nil) (display-coordinates nil) (trim-user nil) ) :statuses/update :status )
(define-twitter-method statuses-retweet( (tweet-id) &key (trim-user nil) (include-entities t)) :statuses/retweet/?id :id )
(define-twitter-method statuses-retweeters( (tweet-id) &key (cursor nil) (stringify-ids nil) )   :statuses/retweeters/ids :id )
(define-twitter-method statuses-lookup( (tweet-id-str) &key (trim-user nil) (include-entities nil) (_map t) ) :statuses/lookup :id)

;;---------------------------------------------------------------------------
;;

(defmethod tweet? ((tweet tweet) &key (trim-user nil) (include-entities nil) (include-my-retweet nil))
  (statuses-show (tweet-id tweet) :trim-user trim-user :include-entities include-entities :include-my-retweet include-my-retweet ))

(defmethod retweets ((tweet tweet) &key (trim-user nil) ) (statuses-retweets (tweet-id tweet) :trim-user trim-user))

(defmethod delete-tweet ((tweet-id integer) &key (trim-user nil) )
    (statuses-destroy tweet-id  :trim-user trim-user))

(defmethod delete-tweet ((tweet tweet) &key (trim-user nil) )
  (delete-tweet (tweet-id tweet) :trim-user trim-user))

(defmethod tweet ((text string) &key (tiny-url-p t) (in-reply-to-status-id nil) (lat nil) (long nil) (place-id nil) (display-coordinates nil) (trim-user nil) )
  (let ((newtext (if tiny-url-p (convert-to-tinyurl text) text)))
    (if (<= (length newtext) 140)
	(statuses-update newtext
                       :in-reply-to-status-id in-reply-to-status-id
                       :place-id place-id
                       :lat lat
                       :long long
                       :display-coordinates display-coordinates
                       :trim-user trim-user )
	(error "Tweet updates must be less than 140 characters.  Length is ~A" (length newtext)))))

(defmethod reply-to ((tweet tweet) (text string) &key (tiny-url-p t) (lat nil) (long nil) (place-id nil) (display-coordinates nil) (trim-user nil) )
  (let ((fmt (format nil "@~A ~A" (twitter-user-screen-name (tweet-user tweet)) text)))
    (tweet fmt :in-reply-to-status-id (tweet-id tweet)  :tiny-url-p tiny-url-p :place-id place-id :lat lat :long long :display-coordinates display-coordinates
	   :trim-user trim-user)))

(defmethod reply-to ((name string) (text string) &key (tiny-url-p t) (lat nil) (long nil) (place-id nil) (display-coordinates nil) (trim-user nil))
  (let ((fmt (format nil "@~A ~A" (twitter-user-screen-name (show-user name)) text)))
    (tweet fmt :tiny-url-p tiny-url-p :place-id place-id :lat lat :long long :display-coordinates display-coordinates :trim-user trim-user )))


(defmethod retweet ((tweet-id integer) &key (trim-user nil) (include-entities t))
  (statuses-retweet tweet-id :trim-user trim-user :include-entities include-entities))

(defmethod retweet ((tweet tweet) &key (trim-user nil) (include-entities t))
  (retweet (tweet-id tweet) :trim-user trim-user :include-entities include-entities))


(defmethod retweeted-by ((tweet-id integer) &key (max -1) (skip 0))
  (let ((lst))
    (labels ((collect-it (l)
	       (setf lst (nconc lst l))))
      (with-cursor (:skip skip :max max :extractor #'cursor-tweet-user-ids :controller #'cursor-tweet-next-cursor :collector #'collect-it :test #'rate-limit-exceeded )
        (statuses-retweeters tweet-id )))
    (lookup :users lst)))

(defmethod retweeted-by ((tweet tweet) &key (max -1) (skip 0))
  (retweeted-by (tweet-id tweet) :max max :skip skip))




