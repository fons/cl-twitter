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
  (format stream "#<TWITTER-GEO-PLACE '~A'>" (geo-place-name ref)))

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
      

;;;---------------------------------------------------------------
;;
;; Geo resources
;;         geo/nearby_places ====> deprecated
;;    geo/search
;;    geo/similar_places
;;    geo/reverse_geocode
;;    geo/id/247f43d441defc03
;;    geo/place
;; Test case : (apply 'twitter-op :geo/search :lat 37.7821120598956 :long -122.400612831116  ())
;; return san fran..
;;  (apply 'twitter-op :geo/search :lat 40.88 :long -73.41  ()) --> huntington

;; TODO handle this :   :attribute:street_address "This parameter searches for places which have this given street address. "

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

;; Consult the api pages for more information

;; on accruarcy : If a number, then this is a radius in meters, but it can also take a string that is suffixed with ft to specify feet. 
;;    If this is not passed in, then it is assumed to be 0m. If coming from a device, in practice, this value is whatever accuracy the device has 
;;    measuring its location (whether it be coming from a GPS, WiFi triangulation, etc.).

(define-command geo/similar_places (:get :geo-result)
    (twitter-app-uri "geo/similar_places.json")
    "Locates places near the given coordinates which are similar in name."
    :lat "Required  : The latitude to search around. Ignored unless between [-90.0 +90.0] (North is positive) and :long is specified"
    :long "Required : The longitude to search around. Ignored unless between [-180.0 to +180.0] (East is positive) inclusive." 
    :name "Required : The name a place is known as."
    :attribute->street_address "This parameter searches for places which have this given street address."
    :contained_within "This is the place_id which you would like to restrict the search results to. "
    :callback "If supplied, the response will use the JSONP format with a callback of the given name.")


(define-command geo/reverse_geocode (:get :geo-result)
    (twitter-app-uri "geo/reverse_geocode.json")
    "Given a latitude and a longitude, searches for up to 20 places that can be used as a place_id when updating a status."
    :lat "Required  : The latitude to search around. Ignored unless between [-90.0 +90.0] (North is positive) and :long is specified"
    :long "Required : The longitude to search around. Ignored unless between [-180.0 to +180.0] (East is positive) inclusive." 
    :accuracy "A hint on the 'region' in which to search."
    :granularity "This is the minimal granularity of place types to return and must be one of: poi, neighborhood (default), city, admin or country."
    :max_results "A hint as to the number of results to return. "
    :callback "If supplied, the response will use the JSONP format with a callback of the given name.")


(define-command geo/id/?place_id (:get-id :geo-place)
    (twitter-app-uri "geo/id/<id>.json")
    "Given a latitude and a longitude, searches for up to 20 places that can be used as a place_id when updating a status."
  :id "A place in the world. These IDs can be retrieved from geo/reverse_geocode.")

;; TODO --> reasonable return structure
;; TODO not tested
(define-command geo/place (:post :identity)
    (twitter-app-uri "geo/place.json")
    "Creates a new place at the given latitude and longitude."
  :name "Required :The name a place is known as."
  :contained_within "Required :The place_id within which the new place can be found. Try and be as close as possible with the containing place. "
  :token "Required: The token found in the response from geo/similar_places."
  :lat "Required :The latitude the place is located at. This parameter will be ignored unless it is inside the range -90.0 to +90.0 (North is positive) inclusive."
  :long "Required :The longitude the place is located at. The valid ranges for longitude is -180.0 to +180.0 (East is positive) inclusive. "
  :attribute->street_address "This parameter searches for places which have this given street address."
  :callback "If supplied, the response will use the JSONP format with a callback of the given name.")  


;;----------------------------------end of geo resources --------------------------

(defun geo-search (lat long &rest args &key (query nil) (granularity nil) (max-results 20) (ip nil) (accuracy nil) (contained-within) (attribute->street_address) )
  (declare (ignore query granularity max-results ip accuracy contained-within attribute->street_address))
  (apply 'twitter-op :geo/search :lat lat :long long args )) 

(defun geo-similar-places (lat long name &rest args &key (contained-within nil) (callback nil) (attribute->street_address) )
  (declare (ignore contained-within callback attribute->street_address) )
  (apply 'twitter-op :geo/similar_places :lat lat :long long :name name args))

(defun geo-reverse-geocode (lat long &rest args &key (query nil) (granularity nil) (max-results 20) (ip nil) (accuracy nil) (contained-within))
  (declare (ignore query granularity max-results ip accuracy contained-within))
  (apply 'twitter-op :geo/reverse_geocode :lat lat :long long args )) 
  
(defun geo-place-by-id (id)
  (apply 'twitter-op :geo/id/?place_id :id id nil))

(defun geo-register-place (lat long name contained-id token)
  (apply 'twitter-op :geo/place :name name :contained_within contained-id :token token :lat lat :long long nil))


