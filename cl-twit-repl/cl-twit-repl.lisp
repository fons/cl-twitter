(in-package :cl-twit-repl)

(defvar *consumer-key*       "9hOStbD2Zf7x0mUoo7cYBg"                     "The consumer key for cl-twit-repl, as listed on https://twitter.com/apps. Used for OAuth.")
(defvar *consumer-secret*    "PWx9ZBZS9BVbesqlkoyiPzXtucmU7jaWe4ECcC30l0" "The consumer secret for cl-twit-repl, as listed on https://twitter.com/apps. Used for OAuth.")
(defvar *saved-print-pprint-dispatch* ()                                  "saved version of the dispatch table")
(defvar *saved-pprint-dispatch-cons*  (pprint-dispatch 'cons)             "saved version of the cons pp")
(defvar *cl-twit-repl-stream*         *standard-output*                   "prefferred output stream for the twitter repl client")

(defvar *twitter-types* (list 'tweet 'cl-twitter::geo-places 'cl-twitter::geo-place 'cl-twitter::place 'cl-twitter::list-type 'cl-twitter::geo-result 'twitter-user 'search-ref 'trend-list 
			      'cl-twitter::rate-limit) "list of types in the twitter api for which we 're going to use the show method instead of the pp")

(defun twitterp (obj)
  (member (type-of obj) *twitter-types*))

(defun pp-show (s o)
  (if (or (twitterp o) (and (consp o) (twitterp (car o))) (hash-table-p o))
      (show o (or *cl-twit-repl-stream* s))
      (funcall *saved-pprint-dispatch-cons* s o)))

(defun install-new-dispatchers ()
  (mapcar (lambda (type) (set-pprint-dispatch type #'pp-show)) *twitter-types*))

(defun cl-twit-repl ()
  (use-package :cl-twitter)
  (unless *alias-registry* (setf *alias-registry*   (make-hash-table :test 'equal)))
  (read-alias)
  (when (null *saved-print-pprint-dispatch*) (setf *saved-print-pprint-dispatch* (copy-pprint-dispatch)))
  (set-pprint-dispatch 'cons         #'pp-show)
  (set-pprint-dispatch 'hash-table   #'pp-show)
  (install-new-dispatchers)
  (unless (probe-file (access-file)) (repl-authenticate-user))
  (cl-twitter::with-error-handler (:verbose nil)
      (verify-credentials)))

(defun done-twittering ()
  (when *saved-print-pprint-dispatch* 
    (setf *print-pprint-dispatch* *saved-print-pprint-dispatch*)
    (setf *saved-print-pprint-dispatch* nil))
  ;;order is important
  (when *alias-registry* (dump-alias))
  (setf *alias-registry*  nil)
  (cl-twitter::with-error-handler (:verbose nil)
    (end-session))
  (setf *twitter-user* nil))
 
