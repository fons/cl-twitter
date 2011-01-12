(in-package :cl-twitter)


;;
;; Status/Tweet Element --> depends on twitter user..
;;

(define-element tweet ((user twitter-user))
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

(define-command statuses/?id/retweeted-by/ids (:get-id (:identity))
    (twitter-app-uri "statuses/<id>/retweeted_by/ids.json")
    "Show user ids of up to 100 users who retweeted the status."
  :id        "Required. The numberical ID of the status you want to retrieve"
  :count     "Specifies the number of records to retrieve. Must be less than or equal to 100."
  :page      "Specifies the page of results to retrieve."
  :trim_user "When set to either true, t or 1, each tweet returned in a timeline will include a user object including only the status authors numerical ID."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities. ")

;;;---------------------------- end of command definitions --------------------------

(defun get-status (tweet-id &rest args &key (trim-user nil) (include-entities nil))
  (declare (ignore trim-user include-entities))
  (apply 'twitter-op :statuses/show/?id :id tweet-id  args))

(defun update-status (status &rest args &key (in-reply-to-status-id nil) (lat nil) (long nil) (place-id nil) (display-coordinates nil) (trim-user nil) (include-entities nil))
  (declare (ignore in-reply-to-status-id lat long place-id display-coordinates trim-user include-entities))
  (apply 'twitter-op :statuses/update :status status  (rem-nil-keywords args '(:long :lat))))

(defun delete-status (tweet-id &rest args &key (trim-user nil) (include-entities nil))
  (declare (ignore trim-user include-entities))
  (apply 'twitter-op :statuses/destroy/?id :id tweet-id  args))

(defun retweet-status (tweet-id &rest args &key (trim-user nil) (include-entities nil))
  (declare (ignore trim-user include-entities))
  (apply 'twitter-op :statuses/retweet/?id :id tweet-id  args))

(defun status-retweets (tweet-id &rest args &key (count nil) (trim-user nil) (include-entities nil))
  (declare (ignore count trim-user include-entities))
  (apply 'twitter-op :statuses/retweets/?id :id tweet-id  args))

(defun status-retweeted-by (tweet-id &rest args &key (count nil) (trim-user nil) (page nil) (include-entities nil))
  (declare (ignore count page trim-user include-entities))
  (apply 'twitter-op :statuses/?id/retweeted-by :id tweet-id  args))

(defun status-retweeted-by-ids (tweet-id &rest args &key (count nil) (trim-user nil) (page nil) (include-entities nil))
  (declare (ignore count page trim-user include-entities))
  (apply 'twitter-op :statuses/?id/retweeted-by/ids :id tweet-id  args))

;;---------------------------------------------------------------------------
;;

(defun tweet? (id &key (trim-user nil) (include-entities nil))
  (get-status id :trim-user trim-user :include-entities include-entities))

(defun tweet (text &key (tiny-url-p t) (in-reply-to-status-id nil) (lat nil) (long nil) (place-id nil) (display-coordinates nil) (trim-user nil) (include-entities nil))
  (let ((newtext (if tiny-url-p (convert-to-tinyurl text) text)))
    (if (<= (length newtext) 140)
	(update-status newtext :in-reply-to-status-id in-reply-to-status-id :place-id place-id :lat lat :long long :display-coordinates display-coordinates :trim-user trim-user :include-entities include-entities)
	(error "Tweet updates must be less than 140 characters.  Length is ~A" (length newtext)))))

(defun reply-to (tweet text &key (tiny-url-p t) (lat nil) (long nil) (place-id nil) (display-coordinates nil) (trim-user nil) (include-entities nil))
  (tweet text :in-reply-to-status-id (tweet-id tweet) :tiny-url-p tiny-url-p :place-id place-id :lat lat :long long :display-coordinates display-coordinates 
	 :trim-user trim-user :include-entities include-entities))

(defun @reply-to (tweet text &key (tiny-url-p t) (lat nil) (long nil) (place-id nil) (display-coordinates nil) (trim-user nil) (include-entities nil))
  (let ((fmt (format nil "@~A ~A" (twitter-user-screen-name (tweet-user tweet)) text)))
    (tweet fmt :in-reply-to-status-id (tweet-id tweet) :tiny-url-p tiny-url-p :place-id place-id :lat lat :long long :display-coordinates display-coordinates 
	   :trim-user trim-user :include-entities include-entities)))

(defun @mention (name text &key (tiny-url-p t) (lat nil) (long nil) (place-id nil) (display-coordinates nil) (trim-user nil) (include-entities nil))
  (let ((fmt (format nil "@~A ~A" (twitter-user-screen-name (show-user name)) text)))
    (tweet fmt :tiny-url-p tiny-url-p :place-id place-id :lat lat :long long :display-coordinates display-coordinates :trim-user trim-user :include-entities include-entities)))
	   
  
(defun delete-tweet (tweet &key (trim-user nil) (include-entities nil))
  (delete-status (tweet-id tweet) :trim-user trim-user :include-entities include-entities))

(defun retweet (tweet &key (trim-user nil) (include-entities nil))
  (retweet-status (tweet-id tweet) :trim-user trim-user :include-entities include-entities))

(defun retweets (tweet &key (trim-user nil) (include-entities nil))
  (status-retweets (tweet-id tweet) :trim-user trim-user :include-entities include-entities))

(defun retweeted-by (tweet &key (trim-user nil) (include-entities nil))
  (status-retweeted-by (tweet-id tweet) :trim-user trim-user :include-entities include-entities))


