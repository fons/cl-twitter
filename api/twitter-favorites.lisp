(in-package :twitter)
;;
;; Favorite Methods
;; 
;;  Favorites resources
;;      favorites
;;      favorites/create/:id
;;      favorites/destroy/:id


(define-command favorites/list (:get (:tweet))
    (twitter-app-uri "favorites/list.json")
    "Returns the 20 most recent favorite statuses for the authenticating user or user specified by the ID parameter in the requested format."
  :user_id "Optional:The ID of the user for whom to return results for."
  :screen_name "Optional:The screen name of the user for whom to return results for."
  :count "Optional: Specifies the number of records to retrieve. Must be less than or equal to 200; defaults to 20."
  :since_id "Optional: Returns results with an ID greater than (that is, more recent than) the specified ID. "
  :max_id "Optional: Returns results with an ID less than (that is, older than) or equal to the specified ID."
  :include_entities "Optional: The entities node will be omitted when set to false.")

(define-command favorites/create (:post :tweet)
    (twitter-app-uri "favorites/create.json")
    "Favorites the status specified in the ID parameter as the authenticating user.  Returns the favorite status when successful."
  :id "Required.  The ID of the status to favorite."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities")

(define-command favorites/destroy (:post :tweet)
    (twitter-app-uri "favorites/destroy.json")
    "Un-favorites the status specified in the ID parameter as the authenticating user.  Returns the un-favorited status in the requested format when successful."
  :id "Required.  The ID of the status to un-favorite."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities")

(define-twitter-method favorites-list (() &key (user-id nil) (screen-name nil) (count nil) (since-id nil) (max-id nil) (include-entities t)) :favorites/list )
(define-twitter-method favorites-create ((id) &key (include-entities t) ) :favorites/create :id )
(define-twitter-method favorites-destroy ((id)   &key (include-entities t) ) :favorites/destroy :id )

;;-----------------------------------------------------------------------------------------------------------------------

(defun favor-tweet (tweet)
  (favorites-create (tweet-id tweet) ))

(defun unfavor-tweet (tweet)
  (favorites-destroy (tweet-id tweet) ))
