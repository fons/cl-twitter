(in-package :twitter)
;;List Members resources
;;   :user/:list_id/members
;;   :user/:list_id/members
;;   :user/:list_id/create_all
;;   :user/:list_id/members
;;   :user/:list_id/members/:id
;; similarly requires the rpalcement of two ids in the url
;; =======>>>>>>>TBD

(define-command ?user/?list_id/members/_get (:get-user-list-id :cursor-user)
    (twitter-app-uri "<user>/<list-id>/members.json")
    "Returns the members of the specified list."
  :user "user"
  :list_id "The id or slug of the list."
  :cursor "Breaks the results into pages. This is recommended for users who are following many users. Provide a value of -1 to begin paging. "
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities.")

(define-command ?user/?list_id/members (:post-user-list-id :list-type)
    (twitter-app-uri "<user>/<list-id>/members.json")
    "Returns the members of the specified list."
  :user "user"
  :list_id "Required : The id or slug of the list."
  :id "The user id of the list member.")

(define-command ?user/?list_id/members/_delete (:post-user-list-id :list-type)
    (twitter-app-uri "<user>/<list-id>/members.json")
    "Returns the members of the specified list."
  :user "user"
  :list_id "Required : The id or slug of the list."
  :id "The user id of the list member."
  :_method "should be delete")

;;http://app.apigee.com/console/twitter 
;; I don't see this specfied in the apigee console so I'm not sure this is live . When called it returns code 200 but not with json.
;; For now not using/exporting it
(define-command ?user/?list_id/create_all (:post-user-list-id :identity)
    (twitter-app-uri "<user>/<list-id>/create_all.json")
    "Adds multiple members to a list, by specifying a comma-separated list of member ids or screen names. "
  :user "Required : user; owner of the list"
  :list_id "Required : The id or slug of the list."
  :user_id "A comma separated list of user IDs, up to 100 are allowed in a single request."
  :screen_name "A comma separated list of screen names, up to 100 are allowed in a single request.")

(define-command ?user/?list_id/members/?id (:get-user-list-id-id :twitter-user)
    (twitter-app-uri "<user>/<list-id>/members/<id>.json")
    "Check if a user is a member of the specified list."
  :user "Required : user; owner of the list"
  :list_id "Required : The id or slug of the list."
  :id        "Required : id of the user whose membership you're checking"
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities.")

;;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------;---
(defun user-list-members (&key  (list-owner (twitter-user-screen-name *twitter-user*)) (list-id nil) (cursor -1) (include-entities nil))
  (apply 'twitter-op :?user/?list_id/members/_get :user list-owner :list-id list-id :cursor cursor :include-entities include-entities nil))

(defun collect-user-list-members (list-owner list-id &key (max -1) (skip 0))
  (let ((ht (make-hash-table  :test 'equal :size 100)))
    (labels ((collect-it (lst)
	       (dolist (item lst)
		 (setf (gethash (twitter-user-id item) ht) item))))
      (with-cursor (:extractor #'cursor-user-users :controller #'cursor-user-next-cursor 
			       :collector #'collect-it :skip skip :max max :test (lambda() nil)) (user-list-members :list-owner list-owner :list-id list-id :cursor -1)))
    ht))


(defun add-user-list-members (list-id user-id &key (list-owner (twitter-user-screen-name *twitter-user*)))
  (apply 'twitter-op :?user/?list_id/members :user list-owner :list_id list-id :id user-id nil))

;;(defvar *HL* *) (create-user-list "haffies")
;;(defvar *H* (do-user-search "haffmans" ))
;;(maphash (lambda (k v) (format t "k :~S:~S~%" k v)) *H*)
;;(maphash (lambda (k v) (add-user-list-members (list-type-id *HL*) k)) *H*)

(defun delete-user-list-members (list-id user-id &key (list-owner (twitter-user-screen-name *twitter-user*)))
  (apply 'twitter-op :?user/?list_id/members/_delete :user list-owner :list_id list-id :id user-id :_method "delete" nil))

(defun user-list-member-p (list-id user-id &key (list-owner (twitter-user-screen-name *twitter-user*)) (include-entities nil) )
  (apply 'twitter-op :?user/?list_id/members/?id :user list-owner :list-id list-id :id user-id  :include-entities include-entities nil))

;; Doesn't seem to be working properly.
;;(defun add-members-to-user-list (list-id screen-names &key (list-owner (twitter-user-screen-name *twitter-user*)) (user-ids nil) )
;;  (apply 'twitter-op :?user/?list_id/create_all :user list-owner :list-id list-id :screen-name screen-names  :user-id user-ids))

