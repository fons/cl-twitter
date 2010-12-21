(in-package :twitter)

;;---> define place element with a place type embedded in it..
;;
(define-element place-type ()
  "a place type"
  (id   "" nil)
  (code "" nil)
  (name "" nil))


(defmethod print-object ((ref place-type) stream)
  (format stream "#<TWITTER-PLACE-TYPE '~A'>" (place-type-name ref)))

(defun lookup-place-type (rec)
  (declare (ignore rec)))

(defun print-place-type (ref)
  (format t "~A: ~A~%" 
	  (place-type-name ref)
	  (place-type-code ref)))

(defmethod register-twitter-object ((ref place-type)))

(define-element place ((placetype place-type))
  "a place "
  (id  "" nil)
  (url "" nil)
  (placetype "" nil)
  (country "" nil)
  (countrycode "" nil)
  (woeid  "" nil)
  (name "" nil))

(defmethod print-object ((ref place) stream)
  (format stream "#<TWITTER-PLACE '~A:~A'>" (place-woeid ref) (place-name ref)))

(defun lookup-place (rec)
  (declare (ignore rec)))

(defun print-place (ref)
  (format t "~A: ~A ~A~%" 
	  (place-name ref)
	  (place-country ref)
	  (place-woeid ref)))

(defmethod register-twitter-object ((ref place)))

(define-element trend ()
  "a trend "
  (id               "" nil)
  (url              "" nil)
  (query            "" nil)
  (promoted-content "" nil)
  (events           "" nil)
  (name             "" nil))

(defmethod print-object ((ref trend) stream)
  (format stream "#<TWITTER-TREND '~A'>" (trend-name ref)))

(defun lookup-trend (rec)
  (declare (ignore rec)))

(defun print-trend (ref)
  (format t "~A: ~A ~A~%" 
	  (trend-name ref)
	  (trend-query ref)
	  (trend-url  ref)))

(defmethod register-twitter-object ((ref trend)))

(define-element trend-list ((locations (place)) (trends (trend)))
  "a trend at a location"
  (id         "" nil)
  (created-at "" nil)
  (locations  "" nil)
  (trends     "" nil)
  (as-of      "" nil))

(defmethod print-object ((ref trend-list) stream)
  (format stream "#<TWITTER-TREND-LIST '~A:~A:~A'>" (trend-list-as-of ref) (trend-list-locations ref) (trend-list-trends ref)))

(defun lookup-trend-list (rec)
  (declare (ignore rec)))

(defun print-trend-list (ref)
  (format t "~A: ~A ~%" 
	  (trend-list-trends ref)
	  (trend-list-as-of ref)))


(defmethod register-twitter-object ((ref trend-list)))

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


(define-command trends (:get :trend-list)
    (twitter-app-uri "trends.json")
    "Returns the top ten topics that are currently trending on Twitter. The response includes the time of the request, the name of each trend, and the url to the Twitter Search results page for that topic.")

(define-command trends/current (:get :new-trends)
    (twitter-app-uri "trends/current.json")
    "Returns the top ten queries that are currently trending on Twitter.  The response includes the time of the request, the name of each trending topic, and the url to the Twitter Search results page for that topic. "
  :exclude "Setting this equal to hashtags will remove all hashtags from the trends list.")

(define-command trends/daily (:get :new-trends)
    (twitter-app-uri "trends/daily.json")
    "Returns the top 20 trending topics for each hour in a given day."
  :date "The start date for the report. The date should be formatted YYYY-MM-DD. A 404 error will be thrown if the date is older than the available search index (7-10 days). "
  :exclude "Setting this equal to hashtags will remove all hashtags from the trends list")

(define-command trends/weekly (:get :new-trends)
    (twitter-app-uri "trends/weekly.json")
    "Returns the top 30 trending topics for each day in a given week."
  :date "The start date for the report. The date should be formatted YYYY-MM-DD. A 404 error will be thrown if the date is older than the available search index (7-10 days). "
  :exclude "Setting this equal to hashtags will remove all hashtags from the trends list")


;;--------------end of trends-------------------------------------
;;
;; Local Trends resources
;;     trends/available
;;     trends/1
;;
;; The response is an array of "locations" that encode the location's WOEID and some other human-readable information such as 
;; a canonical name and country the location belongs in.
;; A WOEID is a Yahoo! Where On Earth ID.

(define-command trends/available (:get (:place))
    (twitter-app-uri "trends/available.json")
    "Returns the locations that Twitter has trending topic information for."
  :lat "If provided with a long parameter the available trend locations will be sorted by distance, nearest to furthest, to the co-ordinate pair."
  :long "If provided with a lat parameter the available trend locations will be sorted by distance, nearest to furthest, to the co-ordinate pair.")


(define-command trends/1 (:get-id (:trend-list))
    (twitter-app-uri "trends/<id>.json")
    "Returns the locations that Twitter has trending topic information for."
  :id "Required woeid; The Yahoo! Where On Earth ID of the location to return trending information for. Global information is available by using 1 as the WOEID.")

;;---------------------- end of local trends resources ----------------------------------------------------

(defun trends ()
  (apply 'twitter-op :trends nil))

(defun current-trends (&rest args &key (exclude nil))
  (declare (ignore exclude))
  (apply 'twitter-op :trends/current  args))

(defun daily-trends (&rest args &key (date nil) (exclude nil))
  (declare (ignore exclude date))
  (apply 'twitter-op :trends/daily args))

(defun weekly-trends (&rest args &key (date nil) (exclude nil))
  (declare (ignore exclude date))
  (apply 'twitter-op :trends/weekly args))

(defun location-trends (&rest args &key (lat nil) (long nil))
  (declare (ignore long lat))
  (apply 'twitter-op :trends/available args))

(defun trends@location (woeid)
  (apply 'twitter-op :trends/1 :id woeid nil))

;;-----------------------------------------------------

(defun trending-locations (&key (lat nil) (long nil))
  (let ((loc-trend-list (location-trends :lat lat :long long))
	(lst ()))
    (dolist (loc-trend loc-trend-list)
      (push (trends@location (place-woeid loc-trend))) lst)
    lst))
