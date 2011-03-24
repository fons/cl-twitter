(in-package :twitter-bdb-driver)

;;Note that the spec needs to a fully specified elephant spec..

;;(defvar *twitter-db-spec* '(:BDB "/Users/alfons/Data/BDB")  "Use to setup the elephant spec to store stuff at")
(defvar *twitter-db-spec* nil  "Use to setup the elephant spec to store stuff at")
;;
;; Basic database operations
;;

(defvar *twitter-db* nil                           "Stores the open twitter db controller")
(defvar *twitter-db-collections* (make-hash-table) "collections")

(defun open-twitter-db (&optional spec)
  (when spec (setf *twitter-db-spec* spec))
  (unless *twitter-db-spec*
    (error "You must define or provide *twitter-db-spec*, a valid elephant spec; should be valid elephant spec"))
  (let ((*store-controller* nil))
    (declare (special *store-controller*))
    (setf *twitter-db* (open-store *twitter-db-spec*))))

(defmacro with-twitter-db (() &body body)
  `(when *twitter-db*
     (with-transaction (:store-controller *twitter-db*)
       ,@body)))

(defun btree (collection)
  (let ((bt (get-from-root collection :sc *twitter-db*)))
    (if bt
	bt
	(add-to-root collection (make-indexed-btree *twitter-db*) :sc *twitter-db*))))

(defun twitter-object-db (type) 
  (let ((collection (gethash type *twitter-db-collections*)))
    (if collection
	collection
	(with-twitter-db ()
	  (setf (gethash collection *twitter-db-collections*) (btree type))))))
	  
(defun close-twitter-db ()
  (when *twitter-db*
    (close-store *twitter-db*))
  (setf *twitter-db-collections* nil))

(defmacro store-twitter-object (type)
 `(progn 
    #+nil(format t "store-twitter-object =====================>~A~%" (quote ,type))
    (defmethod db-store-object ((collection ,type) (lisprec cons))
      (with-twitter-db ()
	(let ((twitter-obj-db (twitter-object-db (type-of collection)))
	      (db-key        (unique-id collection)))
	  (unless (get-value db-key twitter-obj-db)
	    (setf (get-value db-key twitter-obj-db) lisprec)))))))  

(defun db-cache-size (type)
  (length (db-map-reduce type (lambda (k v) (declare (ignore k )) v) nil )))

(defun delete-type-store (type)
  (remove-from-root type :sc *twitter-db*))
 
;;---------------- twitter db interface implementation -----------

(defmethod db-initialize (&rest args)
  (open-twitter-db (car args))
  (create-caches store-twitter-object))


(defmethod db-status (&rest args)
  (declare (ignore args))
  (format t "store : ~A~%" *twitter-db*) 
  (format t "walking over packages in the root~%")
  (let ((lst ()))
    (map-root (lambda (k v) (format t "~1troot~21<~A ~>: ~10t ~A~%" k v) (push k lst)) :sc *twitter-db*)
    (dolist (type lst)
      (format t "~1t~25<~A ~>: ~10t ~A ~%" type (db-cache-size type)))))

(defmethod db-shutdown  (&rest args)
  (declare (ignore args))
  (close-twitter-db))

(defmethod db-map-reduce ( (collection symbol) (map-fn function) (reduce-fn function) &key (from nil) (to nil) (from-end nil) (initial-value nil))
  (let ((lst (map-btree map-fn (twitter-object-db collection) :collect t :from-end from-end :start from :end to)))
    (reduce reduce-fn lst :initial-value initial-value :from-end from-end)))

(defmethod db-map-reduce ( (collection symbol) (map-fn function) (reduce-fn (eql nil)) &key (from nil) (to nil) (from-end nil))
  (let ((lst (map-btree map-fn (twitter-object-db collection) :collect t :from-end from-end :start from :end to)))
    lst))
