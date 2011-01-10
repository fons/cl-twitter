
(defvar *Y* (json:decode-json-from-string (json:encode-json-to-string (car *T*) )))

;;(defvar *twitter-types* (list 'tweet 'cl-twitter::geo-places 'cl-twitter::geo-place 'cl-twitter::place 'cl-twitter::list-type 'cl-twitter::geo-result 'twitter-user 'search-ref 'trend-list 
;;			      'cl-twitter::rate-limit) "list of types in the twitter api for which we 're going to use the show method instead of the pp")

(defvar *twitter-db-entities* (list :user :tweet) " ")
;;(require :uuid)
;;(uuid:make-v1-uuid)

(defun decode-to-hash-table (lst)
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (el lst)
      (if (atom (cdr el))
	  (setf (gethash (car el) ht) (cdr el))
	  (setf (gethash (car el) ht) (decode-to-hash-table (cdr el)))))
    ht))


(defun print-ht (ht)
  (maphash (lambda (k v) (format t "~A : ~A ~%" k v)) ht))