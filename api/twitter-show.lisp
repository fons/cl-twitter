(in-package :cl-twitter)

(defvar *seperator* "--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------")

(defgeneric show (obj))

(defmethod show ( (obj t))
  (format t "~&~1t~S~%" obj))

(defmethod show ((obj cons))
  (length (mapcar #'show obj)))

(defmethod show ((obj hash-table))
  (maphash (lambda (k v) (show v)) obj))

(defmethod show ((tweet tweet))
  (format t "~&~150<~a~; ~a~>" (twitter-user-screen-name (tweet-user tweet)) (tweet-created-at tweet))
  (format t "~&~A" (tweet-text tweet))
  (format t "~&~A" *seperator*))

(defmethod show ((trend-list trend-list))
  (let ((location  (car (trend-list-locations trend-list)))
	(trend-list (trend-list-trends    trend-list)))
    (dolist (trend trend-list)
      (format t "~&~1t~A~40t~A~80t~A" (place-name location) (trend-name trend) (trend-url trend))
      (format t "~&~A" *seperator*))))

(defmethod show ((trend trend))
  (format t "~&~120<~1t~a~; ~a~>" (trend-name trend) (trend-url trend))
  (format t "~&~A" *seperator*))

(defmethod show ((search-ref cl-twitter::search-ref))
  (format t "~&~150<From: ~a~; To: ~a~>" (search-ref-from-user search-ref) (or (search-ref-to-user search-ref) ""))
  (format t "~&~A" (search-ref-text search-ref))
  (format t "~&~A" (search-ref-created-at search-ref))
  (format t "~&~A" *seperator*))

(defmethod show ((twitter-user twitter-user))
  (format t "~&~1t~A ~30ttimezone : ~A ~70turl     : ~A ~140tcreated   : ~A" (twitter-user-screen-name twitter-user) (twitter-user-time-zone twitter-user) 
	  (twitter-user-url twitter-user) (twitter-user-created-at twitter-user) )
  (format t "~&~1tname : ~S ~30tstatuses : ~A ~70tfriends : ~A ~120tfollowers : ~A ~140tfollowing : ~A" (twitter-user-name twitter-user) 
	  (twitter-user-statuses-count twitter-user) (twitter-user-friends-count twitter-user) 
	  (twitter-user-followers-count twitter-user) (twitter-user-following twitter-user)) 
  (format t "~&~1t~A" (twitter-user-description twitter-user))
  (format t "~&~A" *seperator*))

(defmethod show ((geo-place geo-place))
  (format t "~&~1t~A~15t~A " (geo-place-place-type geo-place) (geo-place-full-name geo-place)) 
  (format t "~80t~A" (geo-attribute-street-address (geo-place-attributes geo-place)))
  (format t "~120t~A" (geo-place-country geo-place) )
  (format t "~&~A" *seperator*))

(defmethod show ((place place))
  (format t "~&~1t~A~30t~A~60t~A" (place-name place) (place-type-name (place-placetype place))  (place-country place) )
  (format t "~90twoeid : ~A~110tcoutry code : ~A" (place-woeid place) (place-countrycode place) )
  (format t "~&~A" *seperator*))

(defmethod show ((geo-places geo-places))
  (mapcar #'show (geo-places-places geo-places)))
  
(defmethod show ((geo-result geo-result))
  (show (geo-result-result geo-result)))
