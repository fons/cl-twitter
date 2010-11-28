(in-package :twitter)

;;
;; Search results
;;
   
(define-element search-ref-metadata ()
  "meta data returned in the search result"
  (id "" nil)
 (result-type "" nil)
 (recent-retweets "" nil))

(defmethod print-object ((ref search-ref-metadata) stream)
  (format stream "#<TWITTER-SEARCH-REF-META-DATA '~A'>" (search-ref-metadata-result-type ref)))

(defun lookup-search-ref-metadata (rec)
  (declare (ignore rec)))

(defun print-search-ref-metadata (ref)
  (format t "~A: ~A~%" 
	  (search-ref-metadata-result-type ref)
	  (search-ref-metadata-recent-retweets ref)))

(defmethod register-twitter-object ((ref search-ref-metadata)))

(define-element search-ref ( (metadata search-ref-metadata))
  "An individual search reference"
  (id "Integer ID of message" nil)
  (text "The text of the tweet" nil)
  (to-user "Screen name" nil)
  (to-user-id "ID of receiver" nil)
  (from-user "Screen name" nil)
  (from-user-id "ID of sender" nil)
  (iso-language-code "Two character language code for content" nil)
  (source "Source of the result" nil)
  (profile-image-url "The profile image of the sender")
  (created-at "The date of message creation" nil)
  (metadata nil "")
  (geo "FIXME unparsed"))

(defmethod print-object ((ref search-ref) stream)
  (format stream "#<TWITTER-SEARCH-REF '~A'>" (search-ref-from-user ref)))

(defun lookup-search-ref (rec)
  (declare (ignore rec)))

(defun print-search-ref (ref)
  (format t "~A: ~A~%" 
	  (search-ref-from-user ref)
	  (search-ref-text ref)))

(defmethod register-twitter-object ((ref search-ref)))

(define-element search-result ((results (search-ref)))
   "This is the results of a twitter search.  Metadata plus
    a list of search references."
   (id "" nil)
   (results "" nil)
   (since-id "" nil)
   (max-id "" nil)
   (warning "" nil)
   (refresh-url "" nil)
   (page "" nil)
   (previous-page "" nil)
   (total "" nil)
   (results-per-page "" nil)
   (next-page "" nil)
   (completed-in "" nil)
   (query "" nil))

(defmethod print-object ((results search-result) stream)
  (format stream "#<TWITTER-SEARCH '~A'>" (search-result-query results)))

(defun lookup-search-result (rec)
  #-allegro (declare (ignore rec)))

(defmethod register-twitter-object ((result search-result)))

(defun search-results (result)
  (search-result-results result))

(defun print-search-results (result)
  (mapcar #'print-search-ref (search-results result)))



;; 
;; SEARCH API
;;
;;
;;  Search resources
;;         search

(define-command search (:get :search-result)
    (twitter-search-uri "search.json")
    "Returns tweets that match a specified query."
  :q        "Required. The search string"
  :callback "Only available for JSON format. If supplied, the response will use the JSONP format with a callback of the given name."
  :lang     "Restricts tweets to a particular language given by an ISO 639-1 code."
  :locale   "Specify the language of the query you are sending (only ja is currently effective). "
  :rpp      "The number of tweets to return per page, up to 100."
  :page     "The page number."
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

(defun search-twitter (query &rest args &key (callback nil) (lang nil) (locale nil) (rpp nil) (page nil) 
		       (since-id nil) (until nil) (geocode nil) (show-user nil) (result-type nil) )
  (declare (ignore callback lang  locale rpp page since-id until geocode show-user result-type ))
  (apply 'twitter-op :search :q query (rem-nil-keywords args '(:callback :geocode :lang :until))))

;;---------------------------------------------------------------------------------------------------------------------------

;;(defmacro with-paging ( (&key (max 1500) (skip 0) (collector nil) ) &rest body)
;; based on the with-cursor macro

(defmacro with-paging ((&key (max 1500) (max-pages 15) (collector #'identity) (skip 0) (controller nil) (test (lambda() nil)) )  &rest body)
  (with-gensyms ($results $max $skip fn kargs _page_ args twitter-search fn_ args_ $rpp)
    `(macrolet ((unpack$ ( (,fn_ &rest ,args_) )
		  `(values (quote ,,fn_) (list ,@,args_))))
       (multiple-value-bind (,fn ,kargs) (unpack$ ,@body)
	 (destructuring-bind (,args &key callback lang locale rpp page since-id until geocode show-user result-type) ,kargs
	   (let ((,$max ,max)
		 (,$skip ,skip)
		 (,$results 0)
		 (,$rpp (or rpp 100))
		 (,_page_ (or page 1)))
	     (do () ((or (> ,_page_ ,max-pages) (> 0 (- ,$max ,$results)) (funcall ,test) ))
	       (progn
		 (let ((,twitter-search (funcall ,fn ,args :page ,_page_ :rpp ,$rpp :lang lang :callback callback :locale locale :since-id since-id 
						 :until until :geocode geocode :show-user show-user :result-type result-type )))
		   (incf ,$results (length (search-result-results ,twitter-search)))
		   (if (zerop ,$skip)
		       (funcall ,collector (search-results ,twitter-search))
		       (decf ,$skip ,$results ))
		   (if (null ,controller)
		       (incf ,_page_)
		       (setf ,_page_ (funcall ,controller ,twitter-search))))))))))))

(defun do-search (query &key (max-pages 15) )
  (let ((ht (make-hash-table  :test 'equal :size 1500)))
    (labels ((collect-it (slst)
	       (dolist (item slst)
		 (setf (gethash (search-ref-id item) ht) item))))
      (with-paging (:collector #'collect-it :max-pages max-pages :test #'rate-limit-exceeded ) (search-twitter query))
      ht)))


