(in-package :twitter)
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


(define-twitter-method favorites (() &key (id nil) (page nil) (include-entities t)) :favorites )

(define-twitter-method create-favorite ((id)   &key (include-entities t) ) :favorites/create/?id :id )

(define-twitter-method delete-favorite ((id)   &key (include-entities t) ) :favorites/destroy/?id :id )

;;-----------------------------------------------------------------------------------------------------------------------

(defun favor-tweet (tweet)
  (create-favorite (tweet-id tweet) ))

(defun unfavor-tweet (tweet)
  (delete-favorite (tweet-id tweet) ))
