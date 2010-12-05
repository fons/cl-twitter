(in-package :twitter)

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
 
;;-------------------------------------------------------------------------------

(defun follow-notification (screen-name &rest args &key (user-id nil) (include-entities nil))
  (declare (ignore user-id include-entities))
  (apply 'twitter-op :notifications/follow :screen-name screen-name args))

(defun leave-notification (screen-name &rest args &key (user-id nil) (include-entities nil))
  (declare (ignore user-id include-entities))
  (apply 'twitter-op :notifications/leave :screen-name screen-name args))

