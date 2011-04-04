(in-package :cl-twitter-engine)


(defun mongodb-error-handler (id &key (db "scheduler") (collection "error") (mongo nil))
  (progn
    (cl-mongo:db.use db :mongo mongo)
    (lambda (condition)
      (db.insert collection (kv (kv "id" id) (kv "condition" (format nil "~A" condition))) :mongo mongo))))

(defun make-scheduled-job (func &key (args nil) (every 0) (iter 100) (maxerror 5) (errorhandler #'identity))
  (progn
    (unless (null args) (assert (equal (type-of args) 'cons)))
    (lambda ()
      (loop
	 (progn
	   (handler-case
	       (apply func args)
	     (error (condition)
	       (funcall errorhandler condition)
	       (decf maxerror)))
	   (when (< 0 iter) (decf iter))
	   (when (>= 0 every) (return 'done))
	   (when (= 0 iter)   (return 'iter-done))
	   (when (= 0 maxerror)  (return 'error-done))
	   (sleep every))))))

(defun submit-job (name func &key (args nil) (every 0) (iter 100) (errorhandler nil) (maxerror 5))
  (let ((errhandle (and errorhandler (mongodb-error-handler name))))
    (bordeaux-threads:make-thread  (make-scheduled-job func :args args :every every :iter iter :errorhandler errhandle :maxerror maxerror) :name name)))


;; macro start , stop, show, ---> add to hash table...  


