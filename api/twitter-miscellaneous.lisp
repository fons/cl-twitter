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




;;--------------------------------------------------------------------

(defun twitter-test ()
  (apply 'twitter-op :test nil))

(defun twitter-terms-of-service ()
  (apply 'twitter-op :legal/tos nil))

(defun twitter-privacy ()
  (apply 'twitter-op :legal/privacy nil))

