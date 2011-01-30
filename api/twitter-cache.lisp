(in-package :cl-twitter)

(defvar *twitter-object-cache*  (make-hash-table)   "twitter object cache, in memory")

(defun twitter-object-cache (type)
    (let ((type-cache (gethash type *twitter-object-cache*)))
      (if type-cache
	  type-cache
	  (setf (gethash type *twitter-object-cache*) (make-hash-table :test 'equal)))))

(defgeneric cache-map-reduce (collection map-fn reduce-fn &key))

(defmethod cache-map-reduce ( (collection symbol) (map-fn (eql nil)) (reduce-fn (eql nil)) &key (collect nil))
  (when collect
    (let ((lst ()))
      (labels ((collect-fn (k v)
		 (declare (ignore k))
		 (push v lst)))
	(maphash #'collect-fn (twitter-object-cache collection) ))
      lst)))

(defmethod cache-map-reduce ( (collection symbol) (map-fn function) (reduce-fn (eql nil)) &key (collect nil))
  (let ((lst ()))
    (labels ((collect-fn (k v)
	       (if collect 
		   (push (funcall map-fn k v) lst)
		   (funcall map-fn k v))))
      (maphash #'collect-fn (twitter-object-cache collection) ))
    lst))

(defmethod cache-map-reduce ( (collection symbol) (map-fn function) (reduce-fn function) &key (from-end nil) (initial-value nil))
  (let ((lst (db-map-reduce collection map-fn nil))) 
    (reduce reduce-fn lst :initial-value initial-value :from-end from-end)))

(defmacro map-reduce-cache (collection &key (map nil) (reduce nil) )
  `(progn
     (if ,map
	 (cache-map-reduce (quote ,collection) (lambda (k v) (declare (ignore k)) (funcall ,map v)) ,reduce :collect nil)
	 (cache-map-reduce (quote ,collection) nil ,reduce :collect t))))

(defmethod caches ()
  (let ((lst))
    (maphash (lambda (k v)  (push (cons k (hash-table-count v)) lst)) *twitter-object-cache*)
    lst))

(defgeneric unique-id (twitter-obj) )

(defmethod unique-id ((twitter-obj t))
  (funcall (lookup-id-name (type-of twitter-obj)) twitter-obj))

;;twitter-user is somewhat special..
;;       (setf (gethash (,(intern (format nil "~A-ID" type)) twitter-obj)  (twitter-object-cache ',type)) twitter-obj)
(defmacro twitter-object-store (type)
  `(progn 
     (defmethod register-twitter-object ((twitter-obj ,type) (lisprec cons))
       (setf (gethash (unique-id twitter-obj)  (twitter-object-cache ',type)) twitter-obj)
       (db-store-object twitter-obj lisprec))))

(defmacro create-caches (defmethod-maker-macro)
  (let ((lst (list 'progn )))
    (dolist (spec (read-cache-spec))
      (push `(,defmethod-maker-macro ,spec) lst))
    `,(nreverse lst)))

(defmacro use-cache ()
  `(progn
     (create-caches twitter-object-store)
     ;;twitter-user is a bit special so we ovveride it here
     (defmethod register-twitter-object ((user twitter-user) (lisprec cons))
       (setf (gethash (twitter-user-screen-name user) (twitter-object-cache 'twitter-user)) user)
       (db-store-object user lisprec))))

(defmacro drop-cache (type) 
  `(remhash (quote ,type) *twitter-object-cache*))

(defmethod lookup-twitter-object ((ref (eql 'twitter-user)) (lisprec cons))
    (let ((name (cl-twitter::get-value :screen-name lisprec)))
      (gethash name (twitter-object-cache 'twitter-user))))

(defmethod lookup-twitter-object ((ref (eql 'twitter-user)) (screen-name string))
    (gethash screen-name (twitter-object-cache 'twitter-user) ))

(defmethod lookup-twitter-object ((ref (eql 'twitter-user)) (lisprec (eql nil)))
    (let ((name (twitter-user-screen-name ref)))
      (gethash name (twitter-object-cache 'twitter-user) )))

(defvar *cache-spec-file* "cache.spec" "cache specification; contains types to cache")

(defun cache-spec ()
  (default-file-path "db/" *cache-spec-file*))

(defun read-cache-spec ()
  (let ((lst ()))
    (with-open-file (stream (cache-spec) :direction :input :if-does-not-exist :create)
      (do ((line (read stream nil) (read stream nil))) 
	  ((null line))
	(push line lst)))
    lst))

(defun write-cache-spec (args &key (supersede nil))
  (let ((lst (read-cache-spec)))
    (if supersede (setf lst args)
	(dolist (item args)
	  (unless (member item lst) (push item lst))))
    (with-open-file (stream (cache-spec) :direction :output :if-exists :supersede :if-does-not-exist :create)
      (mapcar (lambda (l) (format stream "~A~%" l)) lst))))


