(in-package :twitter)

;;
;; Help Methods
;;
;;  Help resources
;;     help/test
;;

(define-command test (:get :string)
    "http://twitter.com/help/test.json"
    "Returns the string 'ok' in the requested format with a 200 OK HTTP status code.")

;; Legal resources
;;    legal/tos
;;    legal/privacy

(define-command legal/tos (:get :string)
    (twitter-app-uri "legal/tos.json")
    "Returns Twitter's' Terms of Service in the requested format. These are not the same as the Developer Terms of Service.")

(define-command legal/privacy (:get :string)
    (twitter-app-uri "legal/privacy.json")
    "Returns Twitter's' Terms of Service in the requested format. These are not the same as the Developer Terms of Service.")

;;
;;Spam Reporting resources
;;   report_spam
(define-command report-spam (:post :twitter-user)
    (twitter-app-uri "report_spam.json")
    "The user specified in the id is blocked by the authenticated user and reported as a spammer."
  :user_id "The ID of the user for whom to return results for. Helpful for disambiguating when a valid user ID is also a valid screen name."
  :screen_name "The screen name of the user for whom to return results for. Helpful for disambiguating when a valid screen name is also a user ID."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities")


;;--------------------------------------------------------------------

(defun twitter-test ()
  (apply 'twitter-op :test nil))

(defun twitter-terms-of-service ()
  (apply 'twitter-op :legal/tos nil))

(defun twitter-privacy ()
  (apply 'twitter-op :legal/privacy nil))

(defun report-spam (screen-name &key (user-id nil) (include-entities t))
  (apply 'twitter-op :report-spam :screen-name screen-name :user-id user-id :include-entities include-entities))