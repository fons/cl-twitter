(in-package :twitter-cache)

(defvar *twitter-users* (make-hash-table :test #'equal) "A hash of previously seen users to avoid re-parsing")

(defmethod cl-twitter::register-twitter-object ((user twitter-user))
  (setf (gethash (twitter-user-screen-name user) *twitter-users*) user))


;(defmethod cache-lookup 
(defun lookup-twitter-user (rec)
  (let ((name (get-value :screen-name rec)))
    (gethash name *twitter-users*)))
  
