(in-package :cl-twitter)

;;(defvar *seperator* "--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------")
(defvar *seperator* " ")


(defgeneric show (obj &optional s ))

(defmethod show ( (obj t) &optional (s *standard-output*))
  (format s "~&~1t~S~%" obj))

(defmethod show ((obj cons) &optional (s *standard-output*))
  (length (mapcar (lambda (el) (show el s)) obj)))

(defmethod show ((obj hash-table) &optional (s *standard-output*))
  (maphash (lambda (k v) (declare (ignore k)) (show v s)) obj))

(defmethod show ((tweet tweet) &optional (s *standard-output*))
  (format s "~&~150<~a~; ~a~>" (twitter-user-screen-name (tweet-user tweet)) (tweet-created-at tweet))
  (format s "~&~A" (tweet-text tweet))
  (format s "~&~A" *seperator*))

(defmethod show ((trend-list trend-list) &optional (s *standard-output*))
  (let ((location  (car (trend-list-locations trend-list)))
	(trend-list (trend-list-trends    trend-list)))
    (dolist (trend trend-list)
      (format s "~&~1t~A~40t~A~80t~A" (place-name location) (trend-name trend) (trend-url trend))
      (format s "~&~A" *seperator*))))

(defmethod show ((trend trend) &optional (s *standard-output*))
  (format s "~&~120<~1t~a~; ~a~>" (trend-name trend) (trend-url trend))
  (format s "~&~A" *seperator*))

(defmethod show ((search-metadata cl-twitter::search-metadata) &optional (s *standard-output*))
  (format s "~&~150<Metadata: ~a results for ~a in ~s seconds~>" (search-metadata-count search-metadata)
          (search-metadata-query search-metadata) (search-metadata-completed-in search-metadata))
  (format s "~&~A" *seperator*))

(defmethod show ((search-result cl-twitter::search-result) &optional (s *standard-output*))
  (format s "~&~150<Search: ~a results for ~a in ~s seconds~>" (search-metadata-count (search-metadata search-result))
          (search-metadata-query (search-metadata search-result)) (search-metadata-completed-in (search-metadata search-result)))
  (format s "~&~A" *seperator*))

(defmethod show ((twitter-user twitter-user) &optional (s *standard-output*))
  (format s "~&~1t~A ~30ttimezone : ~A ~70turl     : ~A ~140tcreated   : ~A" (twitter-user-screen-name twitter-user) (twitter-user-time-zone twitter-user)
	  (twitter-user-url twitter-user) (twitter-user-created-at twitter-user) )
  (format s "~&~1tname : ~S ~30tstatuses : ~A ~70tfriends : ~A ~120tfollowers : ~A ~140tfollowing : ~A" (twitter-user-name twitter-user)
	  (twitter-user-statuses-count twitter-user) (twitter-user-friends-count twitter-user)
	  (twitter-user-followers-count twitter-user) (twitter-user-following twitter-user))
  (format s "~&~1t~A" (twitter-user-description twitter-user))
  (format s "~&~A" *seperator*))

(defmethod show ((geo-place geo-place) &optional (s *standard-output*))
  (format s "~&~1t~A~15t~A " (geo-place-place-type geo-place) (geo-place-full-name geo-place))
  (format s "~80t~A" (geo-attribute-street-address (geo-place-attributes geo-place)))
  (format s "~120t~A" (geo-place-country geo-place) )
  (format s "~&~A" *seperator*))

(defmethod show ((place place) &optional (s *standard-output*))
  (format s "~&~1t~A~30t~A~60t~A" (place-name place) (place-type-name (place-placetype place))  (place-country place) )
  (format s "~90twoeid : ~A~110tcoutry code : ~A" (place-woeid place) (place-countrycode place) )
  (format s "~&~A" *seperator*))

(defmethod show ((list-type list-type) &optional (s *standard-output*))
  (format s "~&~1t~a~35t~a~90t~a" (list-type-slug list-type) (list-type-full-name list-type) (list-type-description list-type))
  (format s "~&~90tid : ~a~100towner : ~a members : ~a mode :~a " (list-type-id list-type) (twitter-user-screen-name (list-type-user list-type)) (list-type-member-count list-type) (list-type-mode list-type) )
  (format s "~&~A" *seperator*))


(defmethod show ((rate-limit rate-limit) &optional (s *standard-output*))
  (format s "~&~1tremaining : ~A/~A ~20treset : ~A/current time : ~A ~67t[~A seconds]" (rate-limit-remaining-hits rate-limit) (rate-limit-hourly-limit rate-limit)
	  (rate-limit-reset-time rate-limit) (current-utc nil) (rate-limit-reset-time-in-seconds rate-limit) ))


(defmethod show ((geo-places geo-places) &optional (s *standard-output*))
  (mapcar (lambda (el) (show el s)) (geo-places-places geo-places)))

(defmethod show ((geo-result geo-result) &optional (s *standard-output*))
  (show (geo-result-result geo-result) s))

(defmethod show ((cursor-user-lists cursor-user-lists) &optional (s *standard-output*))
  (mapcar (lambda (el) (show el s)) (cursor-user-lists-lists cursor-user-lists)))

(defmethod show ((url-entity url-entity) &optional (s *standard-output*))
  (format s "~&~1turl: ~a~30tdisplay: ~a~65texpanded: ~a~120t[~a,~a]" (url-entity-url url-entity)
	  (url-entity-display-url url-entity)
	  (url-entity-expanded-url url-entity)
	  (car (url-entity-indices url-entity))
	  (cadr (url-entity-indices url-entity))))

(defmethod show ((hashtag hashtag) &optional (s *standard-output*))
  (format s "~&~1ttext: #~a~30t[~a,~a]" (hashtag-text hashtag)
	  (car (hashtag-indices hashtag))
	  (cadr (hashtag-indices hashtag))))

(defmethod show ((user-mention user-mention) &optional (s *standard-output*))
  (format s "~&~1t@~a~25t~a~55t~a~65t[~a,~a]"
	  (user-mention-screen-name user-mention)
	  (user-mention-name user-mention)
	  (user-mention-id user-mention)
	  (car (user-mention-indices user-mention))
	  (cadr (user-mention-indices user-mention))))

(defmethod show ((media-entity media-entity) &optional (s *standard-output*))
  (format s "~&~1t~a~30turl: ~a~60tdisplay: ~a~95texpanded: ~a~120t[~a,~a]" (media-entity-type media-entity)
	  (media-entity-url media-entity)
	  (media-entity-display-url media-entity)
	  (media-entity-expanded-url media-entity)
	  (car (media-entity-indices media-entity))
	  (cadr (media-entity-indices media-entity))))
