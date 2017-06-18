(in-package :twitter)

;;---> define place element with a place type embedded in it..
;;

#|

NIL
TWIT> (trends-place 1)
(((:Trends
   ((:NAME . "#PSYAngBatasNgApi") (:QUERY . "%23PSYAngBatasNgApi")
    (:URL . "http://twitter.com/search?q=%23PSYAngBatasNgApi")
    (:PROMOTED-CONTENT))
   ((:NAME . "#FelizJueves") (:QUERY . "%23FelizJueves")
    (:URL . "http://twitter.com/search?q=%23FelizJueves") (:PROMOTED-CONTENT))
   ((:NAME . "#MeSobranGanasDe") (:QUERY . "%23MeSobranGanasDe")
    (:URL . "http://twitter.com/search?q=%23MeSobranGanasDe")
    (:PROMOTED-CONTENT))
   ((:NAME . "#شعورك_الان_بتغريده")
    (:QUERY
     . "%23%D8%B4%D8%B9%D9%88%D8%B1%D9%83_%D8%A7%D9%84%D8%A7%D9%86_%D8%A8%D8%AA%D8%BA%D8%B1%D9%8A%D8%AF%D9%87")
    (:URL
     . "http://twitter.com/search?q=%23%D8%B4%D8%B9%D9%88%D8%B1%D9%83_%D8%A7%D9%84%D8%A7%D9%86_%D8%A8%D8%AA%D8%BA%D8%B1%D9%8A%D8%AF%D9%87")
    (:PROMOTED-CONTENT))
   ((:NAME . "#과목별_담당선생님들_명언을써보자")
    (:QUERY
     . "%23%EA%B3%BC%EB%AA%A9%EB%B3%84_%EB%8B%B4%EB%8B%B9%EC%84%A0%EC%83%9D%EB%8B%98%EB%93%A4_%EB%AA%85%EC%96%B8%EC%9D%84%EC%8D%A8%EB%B3%B4%EC%9E%90")
    (:URL
     . "http://twitter.com/search?q=%23%EA%B3%BC%EB%AA%A9%EB%B3%84_%EB%8B%B4%EB%8B%B9%EC%84%A0%EC%83%9D%EB%8B%98%EB%93%A4_%EB%AA%85%EC%96%B8%EC%9D%84%EC%8D%A8%EB%B3%B4%EC%9E%90")
    (:PROMOTED-CONTENT))
   ((:NAME . "ロッキン") (:QUERY . "%E3%83%AD%E3%83%83%E3%82%AD%E3%83%B3")
    (:URL . "http://twitter.com/search?q=%E3%83%AD%E3%83%83%E3%82%AD%E3%83%B3")
    (:PROMOTED-CONTENT))
   ((:NAME . "Chiellini") (:QUERY . "Chiellini")
    (:URL . "http://twitter.com/search?q=Chiellini") (:PROMOTED-CONTENT))
   ((:NAME . "メンタルヘルス")
    (:QUERY
     . "%E3%83%A1%E3%83%B3%E3%82%BF%E3%83%AB%E3%83%98%E3%83%AB%E3%82%B9")
    (:URL
     . "http://twitter.com/search?q=%E3%83%A1%E3%83%B3%E3%82%BF%E3%83%AB%E3%83%98%E3%83%AB%E3%82%B9")
    (:PROMOTED-CONTENT))
   ((:NAME . "Rick Perry") (:QUERY . "%22Rick+Perry%22")
    (:URL . "http://twitter.com/search?q=%22Rick+Perry%22")
    (:PROMOTED-CONTENT))
   ((:NAME . "COUNTDOWN 427화") (:QUERY . "%22COUNTDOWN+427%ED%99%94%22")
    (:URL . "http://twitter.com/search?q=%22COUNTDOWN+427%ED%99%94%22")
    (:PROMOTED-CONTENT)))
  (:AS-OF . "2015-06-04T13:22:25Z") (:CREATED-AT . "2015-06-04T13:19:50Z")
  (:LOCATIONS ((:NAME . "Worldwide") (:WOEID . 1)))))
TWIT> 
|#
(define-element trend ()
  "a trend "
  (promoted-content "" nil)
  (url              "" nil)
  (query            "" nil)
  (name             "" nil))

;;twitter doesn't provide a unique id; use name instead
(defmethod unique-id ((trend trend))
  (trend-name trend))

(defmethod print-object ((ref trend) stream)
  (format stream "#<TWITTER-TREND '~A'>" (trend-name ref)))

(defun print-trend (ref)
  (format t "~A: ~A ~A~%" 
	  (trend-name ref)
	  (trend-query ref)
	  (trend-url  ref)))


(define-element trend-list ((locations (place)) (trends (trend)))
  "a trend at a location"
  (id         "" nil)
  (created-at "" nil)
  (locations  "" nil)
  (trends     "" nil)
  (as-of      "" nil))

(defmethod print-object ((ref trend-list) stream)
  (format stream "#<TWITTER-TREND-LIST '~A:~A:~A'>" (trend-list-as-of ref) (trend-list-locations ref) (trend-list-trends ref)))

(defun print-trend-list (ref)
  (format t "~A: ~A ~%" 
	  (trend-list-trends ref)
	  (trend-list-as-of ref)))

;; override because api doesn't supply id..
;;stores react badly to cons's as unique ids
(defmethod unique-id ((trend-list trend-list))
  #+nil(format t "unique id on trendlist~%")
  (format nil "~S" (cons (trend-list-as-of trend-list) (trend-list-locations trend-list)))) 

;;
;; In some cases (like daily-trends) the data doesn't follow the format of having a predefined static key.
;; The key is in effect a time stamp. This doesn 't fit into the element definition api, as it assumes that keys are known at compile time.
;;   (symbol-name (first trend-set)) -> that's in basically handling the key as timestamp.

(defmethod parse-record (response (prim-type (eql :new-trends)))
  "Unparse and unpack trend type records"
  (labels ((parse-trend-set (trend-list)
	     (mapcar (lambda (trend) (default-make-element trend 'trend)) trend-list)) 
	   (parse-trends (trends)
	     (mapcar (lambda (trend-set) (list (symbol-name (first trend-set)) (parse-trend-set (rest trend-set)))) trends)))	     
    (cons (get-value :as-of response) (parse-trends (get-value :trends response)) )))


;;
;; Trends resources
;;     trends
;;     trends/current
;;     trends/daily
;;     trends/weekly

(define-element place-type ()
  "a place type"
  (code "" nil)
  (name "" nil))

(defmethod place-type-name ((name (eql nil)))
  nil)


(defmethod print-object ((ref place-type) stream)
  (format stream "#<TWITTER-PLACE-TYPE '~A'>" (place-type-name ref)))


(defun print-place-type (ref)
  (format t "~A: ~A~%" 
	  (place-type-name ref)
	  (place-type-code ref)))

(define-element place ((placetype place-type))
  "a place "
  (id  "" nil)
  (url "" nil)
  (placetype "" nil)
  (country "" nil)
  (countrycode "" nil)
  (woeid  "" nil)
  (name "" nil))

(defmethod place-name ((obj (eql nil)))
  "N/A")

(defmethod print-object ((ref place) stream)
  (format stream "#<TWITTER-PLACE '~A:~A'>" (place-woeid ref) (place-name ref)))

(defun print-place (ref)
  (format t "~A: ~A ~A~%" 
	  (place-name ref)
	  (place-country ref)
	  (place-woeid ref)))

(define-command trends/available (:get (place))
    (twitter-app-uri "trends/available.json")
    "Returns the locations that Twitter has trending topic information for.")


(define-twitter-method trends-available (()) :trends/available)


(define-command trends/place (:get (:trend-list))
    (twitter-app-uri "trends/place.json")
    "Returns the top 10 trending topics for a specific WOEID, if trending information is available for it."
  :id "Required: The Yahoo! Where On Earth ID of the location to return trending information for. Global information is available by using 1 as the WOEID."
  :exclude "Setting this equal to hashtags will remove all hashtags from the trends list.")

(define-twitter-method trends-place ((id)) :trends/place :id)

(define-command trends/closest (:get (:place))
    (twitter-app-uri "trends/closest.json")
    "Returns the locations that Twitter has trending topic information for."
  :lat "If provided with a long parameter the available trend locations will be sorted by distance, nearest to furthest, to the co-ordinate pair."
  :long "If provided with a lat parameter the available trend locations will be sorted by distance, nearest to furthest, to the co-ordinate pair.")

(define-twitter-method trends-closest(() &key lat long) :trends/closest)



;;---------------------- end of local trends resources ----------------------------------------------------




