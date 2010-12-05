(in-package :twitter)

;; cl-oauth is used in this library.
;; These methods have not been tested and aren't used ...

;;
;; OAuth Methods
;;
;; OAuth resources
;;        oauth/request_token
;;        oauth/authorize
;;        oauth/authenticate
;;        oauth/access_token
;;

(define-command oauth/request-token (:get :string)
    (twitter-app-uri "oauth/request_token")
    "Allows a Consumer application to obtain an OAuth Request Token to request user authorization"
  :force_login "Forces the user to enter their credentials to ensure the correct users account is authorized.")

(define-command oauth/authorize (:get :string)
    (twitter-app-uri "oauth/authorize")
    "Allows a Consumer application to use an OAuth Request Token to request user authorization. ")

(define-command oauth/authenticate (:get :string)
    (twitter-app-uri "oauth/authorize")
    "Allows a Consumer application to use an OAuth request_token to request user authorization. "
  :force_login "Forces the user to enter their credentials to ensure the correct users account is authorized.")

(define-command oauth/access-token (:get :string)
    (twitter-app-uri "oauth/access_token")
    "Allows a Consumer application to exchange the OAuth Request Token for an OAuth Access Token. "
  :x_auth_username "The username of the user to obtain a token for. (required when using xAuth)"
  :x_auth_password "The password of the user for which to obtain a token for. (required when using xAuth)"
  :x_auth_mode "Set this value to 'client_auth', without the quotes. (required when using xAuth)")

;;------------------ end of oauth methods -------------------------------------------

;; cl-oauth is used in this library.
;; These methods have not been tested and aren't used ...
