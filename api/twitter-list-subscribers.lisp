(in-package :twitter)

;; List Subscribers resources
;;     :user/:list_id/subscribers
;;     :user/:list_id/subscribers
;;     :user/:list_id/subscribers
;;     :user/:list_id/subscribers/:id
;; similarly requires the rpalcement of two ids in the url
;; =======>>>>>>>TBD

(define-command ?user/?list_id/subscribers/_get (:get-user-list-id :cursor-user)
    (twitter-app-uri "<user>/<list-id>/subscribers.json")
    "Returns the subscribers of the specified list."
  :user "user"
  :list_id "The id or slug of the list."
  :cursor "Breaks the results into pages. This is recommended for users who are following many users. Provide a value of -1 to begin paging. "
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities.")

(define-command ?user/?list_id/subscribers (:post-user-list-id :list-type)
    (twitter-app-uri "<user>/<list-id>/subscribers.json")
    "Make the authenticated user follow the specified list."
  :user "user"
  :list_id "The id or slug of the list.")

(define-command ?user/?list_id/subscribers/_delete (:post-user-list-id :list-type)
    (twitter-app-uri "<user>/<list-id>/subscribers.json")
    "Make the authenticated user follow the specified list."
  :user "user; list owner"
  :list_id "The id or slug of the list."
  :_method "should be delete")

(define-command ?user/?list_id/subscribers/?id (:get-user-list-id-id :twitter-user)
    (twitter-app-uri "<user>/<list-id>/subscribers/<id>.json")
    "Check if a user is a subscriber of the specified list."
  :user "Required : user; owner of the list"
  :list_id "Required : The id or slug of the list."
  :id        "Required : id of the user whose membership you're checking"
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities.")

;;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(defun user-list-subscribers (&key  (list-owner (twitter-user-screen-name *twitter-user*)) (list-id nil) (cursor -1) (include-entities t))
  (apply 'twitter-op :?user/?list_id/subscribers/_get :user list-owner :list_id list-id :cursor cursor :include-entities include-entities nil))

(defun collect-user-list-subscribers (list-owner list-id &key (max -1) (skip 0))
  (let ((ht (make-hash-table  :test 'equal :size 100)))
    (labels ((collect-it (lst)
	       (dolist (item lst)
		 (setf (gethash (twitter-user-id item) ht) item))))
      (with-cursor (:extractor #'cursor-user-users :controller #'cursor-user-next-cursor 
			       :collector #'collect-it :skip skip :max max :test (lambda() nil)) (user-list-subscribers :list-owner list-owner :list-id list-id :cursor -1)))
    ht))

(defun add-user-list-subscribers (list-id  &key (list-owner (twitter-user-screen-name *twitter-user*)))
  (apply 'twitter-op :?user/?list_id/subscribers :user list-owner :list_id list-id  nil))

(defun delete-user-list-subscribers (list-id &key (list-owner (twitter-user-screen-name *twitter-user*)))
  (apply 'twitter-op :?user/?list_id/subscribers/_delete :user list-owner :list_id list-id  :_method "delete" nil ))

(defun user-list-subscriber-p (list-id user-id &key (list-owner (twitter-user-screen-name *twitter-user*)) (include-entities t) )
  (apply 'twitter-op :?user/?list_id/subscribers/?id :user list-owner :list-id list-id :id user-id  :include-entities include-entities nil))
