(in-package :twitter)


(define-element end-session ()
  "rate limit type"
  (id         "" nil)
  (request "" nil)
  (error "" nil))

(defmethod print-object ((ref end-session) stream)
  (format stream "#<TWITTER-END-SESSION '~A:~A'>" (end-session-request ref) (end-session-error ref)))

(defun print-end-session (ref)
  (format t "~A: ~A~%" (end-session-request ref) (end-session-error ref)))

(define-element placetype ()
  "a place type"
  (id   "" nil)
  (code "" nil)
  (name "" nil))

(defmethod placetype-name ((name (eql nil)))
  nil)

(defmethod print-object ((ref placetype) stream)
  (format stream "#<TWITTER-PLACETYPE '~A'>" (placetype-name ref)))

(defun print-placetype (ref)
  (format t "~A: ~A~%" 
	  (placetype-name ref)
	  (placetype-code ref)))

(define-element trend-location ((placetype placetype))
  "a place "
  (id  "" nil)
  (url "" nil)
  (placetype "" nil)
  (country "" nil)
  (countrycode "" nil)
  (woeid  "" nil)
  (name "" nil))

(defmethod trend-location-name ((obj (eql nil)))
  "N/A")

(defmethod print-object ((ref trend-location) stream)
  (format stream "#<TWITTER-TREND-LOCATION '~A:~A'>" (trend-location-name ref) (trend-location-country ref)))

(defun print-trend-location (ref)
  (format t "~A: ~A ~A~%" 
	  (trend-location-name ref)
	  (trend-location-country ref)
	  (trend-location-woeid ref)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-element time-zone ()
  "time zone element"
  (name "" nil)
  (utc-offset "" nil)
  (tzinfo-name "" nil))

(defmethod print-object ((ref time-zone) stream)
  (format stream "#<TIME-ZONE '~A:~A:~A'>" (time-zone-name ref) (time-zone-utc-offset ref) (time-zone-tzinfo-name ref)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-element sleep-time ()
  "sleep time element"
  (enabled "" nil)
  (end-time "" nil)
  (start-time "" nil))

(defmethod print-object ((ref sleep-time) stream)
  (format stream "#<SLEEP-TIME '~A:~A:~A'>" (sleep-time-enabled ref) (sleep-time-start-time ref) (sleep-time-end-time ref)))

;;;=======================================================================================================================================================================================

(define-element account-settings ((time-zone time-zone) (trend-location (trend-location)) (sleep-time sleep-time) )
  "account settings"
  (time-zone "" nil)
  (protected "" nil)
  (screen-name "" nil)
  (always-use-https "" nil)
  (use-cookie-personalization "" nil)
  (sleep-time "" nil) 
  (geo-enabled "" nil)
  (language  "" nil)
  (discoverable-by-email "" nil)
  (discoverable-by-mobile-phone "" nil)
  (display-sensitive-media "" nil)
  (allow-contributor-request "" nil)
  (allow-dms-from  "" nil)
  (allow-dm-groups-from "" nil)
  (trend-location "" nil))
  
  
(defmethod print-object ((ref account-settings) stream)
  (format stream "#<ACCOUNT-SETTINGS '~A:~A'>" (account-settings-screen-name ref) (account-settings-trend-location ref)))

(defun print-account-settings (ref)
  (format t "~A: ~A~%" (account-settings-screen-name ref) (account-settings-trend-location ref)))

;;
;; ACCOUNT METHODS
;;
;; Unsupported calls are marked with an X
;;
;;   Account resources
;;        account/verify_credentials
;;        account/rate_limit_status
;;        account/end_session
;;        account/update_delivery_device X
;;        account/update_profile_colors
;;        account/update_profile_image X
;;        account/update_profile_background_image X
;;        account/update_profile
;;

(define-command account/settings (:get :account-settings)
    (twitter-app-uri "account/settings.json")
    "Returns settings (including current trend, geo and sleep time information) for the authenticating user. ")

(define-command account/settings (:post :account-settings)
    (twitter-app-uri "account/settings.json")
    "Ends the session of the authenticating user, returning a null cookie."
  :sleep_time_enabled "Optional: When set to true, t or 1, will enable sleep time for the user. Sleep time is the time when push or SMS notifications should not be sent to the user."
  :start_sleep_time "Optional:The hour that sleep time should begin if it is enabled in ISO8601 format (i.e. 00-23). The time is considered to be in the same timezone as the user’s time_zone setting."
  :end_sleep_time "Optional:The hour that sleep time should end if it is enabled in ISO8601 format (i.e. 00-23). The time is considered to be in the same timezone as the user’s time_zone setting."
  :time_zone "Optional :The timezone dates and times should be displayed in for the user. The timezone must be one of the Rails TimeZone names. e.g. Europe/Copenhagen, Pacific/Tongatapu"
  :trend_location_woeid "Optional: The Yahoo! Where On Earth ID to use as the user’s default trend location. Global information is available by using 1 as the WOEID. "
  :allow_contributor_request "Optional: Whether to allow others to include user as contributor. Possible values include “all” (anyone can include user), “following” (only followers can include user) or “none”."
  :lang "Optional: The language which Twitter should render in for this user. The language must be specified by the appropriate two letter ISO 639-1 representation. ")


(define-twitter-method account-settings (() &key (sleep-time-enabled nil) (start-sleep-time nil)
                                         (end-sleep-time nil) (time-zone nil) (trend-location-woeid nil) (allow-contributor-request nil) (lang nil)) :account/settings)

(define-command account/verify-credentials (:get :twitter-user)
    (twitter-app-uri "account/verify_credentials.json")
    "Returns an HTTP 200 OK response code and a representation of the requesting user if authentication was successful; returns a 401 status code and an error message if not. 
    Use this method to test if supplied user credentials are valid."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities"
  :skip_status      "When set to either true, t or 1 statuses will not be included in the returned user objects"
  :include_email    "Use of this parameter requires whitelisting. When set to true email will be returned in the user objects as a string.")

(define-command account/update-profile (:post :twitter-user)
    (twitter-app-uri "account/update_profile.json")
    "Sets values that users are able to set under the 'Account' tab of their settings page."
  :name                "Optional: Maximum of 20 characters. One or more of all these parameters must be present."
  :url                 "Optional: Maximum of 100 characters. Will be prepended with 'http://' if not present."
  :location            "Optional: Maximum of 30 characters. The contents are not normalized or geocoded in any way."
  :description         "Optional: Maximum of 160 characters."
  :profile_link_color  "Optional: Sets a hex value that controls the color scheme of links used on the authenticating user’s profile page on twitter.com"
  :include_entities    "Optional: When set to either true, t or 1, each tweet will include a node called entities"
  :skip_status         "Optional:When set to either true, t or 1 statuses will not be included in the returned user objects.")

(define-twitter-method account-update-profile (() &key (name nil)  (url nil) (location nil) (description nil) (profile-link-color nil) (include_entities t) (skip-status t)) :account/update-profile)

;;;============================================



 
(define-command account/update-profile-colors (:post :twitter-user)
    (twitter-app-uri "account/update_profile_colors.json")
    "Sets one or more hex values that control the color scheme of the authenticating user's
     profile page on twitter.com.  These values are also returned in the /users/show 
     API method."
  :profile-background-color "Optional. "
  :profile-text-color "Optional"

  :profile-sidebar-fill-color "Optional"
  :profile-sidebar-border-color "Optional"
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities")

;; not tested 
(define-command account/update-profile-image (:post :twitter-user)
    (twitter-app-uri "account/update_profile_image.json")
    "Updates the authenticating user's profile image.  Expects raw multipart data, not a URL to an image."
  :image "Required.  Must be a valid GIF, JPG, or PNG image of less than 700 kilobytes in size.  Images with width larger than 500 pixels will be scaled down."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities")


;;not tested
(define-command account/update-profile-background-image (:post :twitter-user)
    (twitter-app-uri "account/update_profile_background_image.json")
    "Updates the authenticating user's profile background image.  Expects raw multipart data, not a URL to an image."
  :image "Required.  Must be a valid GIF, JPG, or PNG image of less than 800 kilobytes in size.  Images with width larger than 2048 pixels will be scaled down."
  :tile "Whether or not to tile the background image. If set to true the background image will be displayed tiled. The image will not be tiled otherwise."
  :include_entities "When set to either true, t or 1, each tweet will include a node called entities")


;;Only the parameters specified will be updated; to only update the 'name' attribute, for example, only include that parameter in your request."


;;----------------------- end of account methods -----------------------------------------------------------------------------


(define-twitter-method account-verify-credentials (() &key (include-entities nil) (skip-status t) (include-email nil)) :account/verify-credentials)


;;;;=======================



(define-twitter-method rate-limit-status  (())                           :application/rate-limit-status )
(define-twitter-method end-session        (())                           :account/end-session)


(define-twitter-method update-profile-colors (() &key (profile-background-color nil)  (profile-text-color nil)  (profile-link-color nil)  
			      (profile-sidebar-fill-color nil)  (profile-sidebar-border-color nil)  (include_entities t))     :account-update-profile-colors )



;;;;=============

(defun verify-credentials ( &key (include-entities nil) (skip-status t) (include-email nil))
  (account-verify-credentials  :include-entities include-entities :skip-status skip-status :include-email include-email))

