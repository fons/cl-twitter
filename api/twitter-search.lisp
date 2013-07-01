(in-package :twitter)

;;
;; Search results
;;

(define-element search-metadata ()
  "meta data returned in the search result"
  (completed-in "" nil)
  (max-id "" nil)
  (max-id_str "" nil)
  (next-results "" nil)
  (query "" nil)
  (refresh-url "" nil)
  (count "" nil)
  (since-id "" nil)
  (since-id-str "" nil))

(defmethod print-object ((metadata search-metadata) stream)
  (format stream "#<TWITTER-SEARCH-METADATA ~S results for '~A'>" (search-metadata-count metadata) (search-metadata-query metadata)))

(define-element search-result ((statuses (tweet)) (search-metadata search-metadata))
   "This is the results of a twitter search.  Metadata plus
    a list of search references."
   (statuses "" nil)
   (search-metadata "" nil))

(defmethod print-object ((result search-result) stream)
  (format stream "#<TWITTER-SEARCH '~A'~%~A>" (search-result-search-metadata result) (search-result-statuses result)))

(defun search-results (result)
  (search-result-statuses result))

(defun print-search-results (result)
  (mapcar #'print-tweet (search-results result)))


;;
;; SEARCH API
;;
;;
;;  Search resources
;;         search

(define-command search (:get :search-result)
    (twitter-search-uri "search/tweets.json")
    "Returns tweets that match a specified query."
  :q        "Required. The search string"
  :callback "Only available for JSON format. If supplied, the response will use the JSONP format with a callback of the given name."
  :lang     "Restricts tweets to a particular language given by an ISO 639-1 code."
  :locale   "Specify the language of the query you are sending (only ja is currently effective). "
  :count    "The number of tweets to return per page, up to 100."
  :since_id "Returns tweets with status ids greater than the given id"
  :until    "Returns tweets generated before the given date. Date should be formatted as YYYY-MM-DD."
  :geocode  "Returns tweets by users located within a given radius of the given
            latitude/longitude, where the user's location is taken from their
            Twitter profile.  The parameter value is specified by
            'latitude, longitude, radius' where radius units must be
            specified as either miles or kilometers"
  :show-user "When 'true' adds '<user>:' to the beginning of the tweet.  This is
              useful for readers that do not display Atom's author field.  The
              default is 'false'"
  :result_type "Optional. Specifies what type of search results you would prefer to receive. The current default is 'mixed.'
                Valid values include:
                             mixed: Include both popular and real time results in the response.
                             recent: return only the most recent results in the response
                             popular: return only the most popular results in the response.")


;;
;; Search API
;;

(defun search-twitter (query &rest args &key (callback nil) (lang nil) (locale nil) (count nil)
		       (since-id nil) (until nil) (geocode nil) (show-user nil) (result-type nil) )
  (declare (ignore callback lang  locale count since-id until geocode show-user result-type ))
  (apply 'twitter-op :search :q query (rem-nil-keywords args '(:callback :geocode :lang :until))))

;;---------------------------------------------------------------------------------------------------------------------------



(defun do-search (query &key (max-pages 15) )
  (let ((ht (make-hash-table  :test 'equal :size 1500)))
    (labels ((collect-it (slst)
	       (dolist (item slst)
		 (setf (gethash (tweet-id item) ht) item))))
      (with-paging (:collector #'collect-it :max-pages max-pages :test #'rate-limit-exceeded ) (search-twitter query))
      ht)))


