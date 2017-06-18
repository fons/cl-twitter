(in-package :twitter)
;;-----------------------------------------------------

(define-element geo-attribute ()
  "coordinate"
  (id   "" nil)
  (street-address "" nil))

(defmethod print-object ((ref geo-attribute) stream)
  (format stream "#<TWITTER-GEO-ATTRIBUTE '~A'>" (geo-attribute-street-address ref)))


(defun print-geo-attribute (ref)
  (format t "address :~A " (geo-attribute-street-address ref)))

;;-----------------------------------------------------
(define-element geo-coordinate ()
  "coordinate"
  (id   "" nil)
  (long "" nil)
  (lat  "" nil))


(defmethod print-object ((ref geo-coordinate) stream)
  (format stream "#<TWITTER-GEO-COORDINATE '~A:~A'>" (geo-coordinate-long ref) (geo-coordinate-lat ref)))


(defun print-geo-coordinate (ref)
  (format t "long :~A ; lat: ~A" (geo-coordinate-long ref) (geo-coordinate-lat ref)))

;;-----------------------------------------------------
;;(default-make-element '((:long 89) (:lat 67)) 'geo-coordinate)
(defun parse-geo-coordinates (ref)
  (mapcar (lambda (p) (default-make-element (list (list :long (car p)) (list :lat (cadr p))) 'geo-coordinate)) (car ref)))


;;----------------------------------------------------
(define-element geo-bounding-box ( (coordinates geo-coordinates) )
  "bounding box"
  (id  "" nil)
  (coordinates "" nil)
  (type        "" nil))


(defmethod print-object ((ref geo-bounding-box) stream)
  (format stream "#<TWITTER-GEO-BOUNDING-BOX '~A'>" (geo-bounding-box-coordinates ref)))


(defun print-geo-bounding-box (ref)
  (format t "~A" (geo-bounding-box-coordinates ref)))

;;----------------------------------------------------
(define-element geo-query ((params identity))
  "a geo query"
  (id     "" nil)
  (type   "" nil)
  (params "" nil)
  (url    "" nil) )

(defmethod print-object ((ref geo-query) stream)
  (format stream "#<TWITTER-GEO-QUERY '~A'>" (geo-query-url ref)))


(defun print-geo-query (ref)
  (format t "~A: ~A~%" 
	  (geo-query-url ref)
	  (geo-query-type ref)))

;;-------------------------------------------------------------------------
(define-element geo-result ( (result geo-places) (query geo-query))
  "a geo result"
  (id "" nil)
  (result "" nil)
  (query  "" nil))

(defmethod print-object ((ref geo-result) stream)
  (format stream "#<TWITTER-GEO-RESULT '~A':~A'>" (geo-result-result ref)  (geo-result-query ref) ))


(defun print-geo-result (ref)
  (format t "~A: ~A~%" (geo-result-query ref) (geo-result-result ref)))

;;-------------------------------------------------------------------------
(define-element geo-places ( (places (geo-place)) )
  "a geo place result"
  (id     "" nil)
  (token  "" nil)
  (places "" nil))

(defmethod print-object ((ref geo-places) stream)
  (format stream "#<TWITTER-GEO-PLACES '~A'>" (geo-places-places ref)))

(defun print-geo-places (ref)
  (format t "~A~%" (geo-places-places ref)))
	  
;;-------------------------------------------------------------------------

(define-element geo-place ( (bounding-box geo-bounding-box) (contained-within (geo-place)) (attributes geo-attribute) (geometry geo-bounding-box))
  "a geo place type"
  (id               "" nil)
  (code             "" nil)
  (name             "" nil)
  (attributes       "" nil)
  (url              "" nil)
  (place-type       "" nil)
  (full-name        "" nil)
  (country          "" nil)
  (bounding-box     "" nil)
  (geometry         "" nil)
  (country-code     "" nil)
  (polylines        "" nil)
  (contained-within "" nil))

(defmethod geo-attribute-street-address ((obj (eql nil)))
  )

(defmethod print-object ((ref geo-place) stream)
  (format stream "#<TWITTER-GEO-PLACE '~A:~A'>" (geo-place-name ref) (geo-place-id ref)))

(defvar *geo-print-offset* 0)

(defun print-geo-place (ref)
  (labels ((start ()
	     (format t "~vT{" *geo-print-offset*))
	   (end ()
	     (format t "~vT}" *geo-print-offset*))
	   (pr (k v &key (before (lambda()(format t " "))) (after (lambda() (format t "~%"))))
	     (when before (funcall before))
	     (when (and k v) (format t "~vT~20<~A:~;~>~A" *geo-print-offset* k v))
	     (when after (funcall after))))
    (pr nil nil :before #'start)
    (pr "name"         (geo-place-name ref)) 
    (pr "id"           (geo-place-id ref) )
    (pr "country"      (geo-place-country ref) ) 
    (pr "country-code" (geo-place-country-code ref) ) 
    (pr "url"          (geo-place-url ref) ) 
    (pr "place-type"   (geo-place-place-type ref) )
    (pr "full name"     (geo-place-full-name ref) ) 
    (pr "bounding box"  (geo-place-bounding-box ref) ) 
    (pr "geometry"      (geo-place-geometry ref) ) 
    (let ((contained-within (geo-place-contained-within ref)))
      (when contained-within
	(progn
	  (format t "~vTcontained within ~%" *geo-print-offset*)
	  (incf *geo-print-offset* 15) 
	  (mapcar #'print-geo-place contained-within)
	  (decf *geo-print-offset* 15))))
    (pr "attributes"   (geo-place-attributes ref))
    (pr "polylines"   (geo-place-polylines ref))
    (pr nil nil :before #'end)
  ref))

;;--------------------------helper functions----------------------

(defun geo-token (result)
  (geo-places-token (geo-result-result result)))

(defun geo-print-places (result)
  (let* ((geo-places (geo-result-result result))
	 (geo-place-list (geo-places-places geo-places)))
    (mapcar #'print-geo-place geo-place-list)))
      


(define-command geo/search (:get :geo-result)
    (twitter-app-uri "geo/search.json")
    "Search for places that can be attached to a statuses/update. Given a latitude and a longitude pair, an IP address, or a name, 
     this request will return a list of all the valid places that can be used as the place_id when updating a status."
    :lat "The latitude to search around. Ignored unless between [-90.0 +90.0] (North is positive) and :long is specified"
    :long "The longitude to search around. Ignored unless between [-180.0 to +180.0] (East is positive) inclusive. must be geo enabled and must have a :lat spec"
    :query "Free-form text to match against while executing a geo-based query, best suited for finding nearby locations by name."
    :ip    "An IP address. Used when attempting to fix geolocation based off of the user's IP address."
    :attribute->street_address "This parameter searches for places which have this given street address."
    :granularity "This is the minimal granularity of place types to return and must be one of: poi, neighborhood (default), city, admin or country."
    :accuracy "A hint on the 'region' in which to search."
    :max_results "A hint as to the number of results to return. "
    :contained_within "This is the place_id which you would like to restrict the search results to. "
    :callback "If supplied, the response will use the JSONP format with a callback of the given name.")

(define-twitter-method geo-search (() &key (lat nil) (long nil) (query nil) (granularity nil) (max-results 20) (ip nil) (accuracy nil) (contained-within) (attribute->street_address)) :geo/search )

(define-command geo/reverse_geocode (:get :geo-result)
    (twitter-app-uri "geo/reverse_geocode.json")
    "Given a latitude and a longitude, searches for up to 20 places that can be used as a place_id when updating a status."
  :lat "Required  : The latitude to search around. Ignored unless between [-90.0 +90.0] (North is positive) and :long is specified"
  :long "Required : The longitude to search around. Ignored unless between [-180.0 to +180.0] (East is positive) inclusive." 
  :accuracy "A hint on the 'region' in which to search."
  :granularity "This is the minimal granularity of place types to return and must be one of: poi, neighborhood (default), city, admin or country."
  :max_results "A hint as to the number of results to return. "
  :callback "If supplied, the response will use the JSONP format with a callback of the given name.")

(defun geo-reverse-geocode (lat long &rest args &key (granularity nil) (max-results 20) (ip nil) (accuracy nil) (contained-within))
  (declare (ignore granularity max-results ip accuracy contained-within))
  (apply 'twitter-op :geo/reverse_geocode :lat lat :long long args )) 

(define-command geo/id/?place_id (:get-id :geo-place)
    (twitter-app-uri "geo/id/<id>.json")
    "Given a latitude and a longitude, searches for up to 20 places that can be used as a place_id when updating a status."
  :id "A place in the world. These IDs can be retrieved from geo/reverse_geocode.")

(defun geo-id-place-id (place-id)
  (apply 'twitter-op :geo/id/?place_id :id place-id nil))





