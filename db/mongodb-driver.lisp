(in-package :twitter-mongodb-driver)

(defvar *mongodb-host* nil)
(defvar *mongodb-port* nil)
(defvar *mongodb-db*   nil)

(defun connection ()
  (handler-case 
      (cl-mongo:mongo :host *mongodb-host* :port *mongodb-port* :db *mongodb-db* :name :twitter)      
    (error (c)
      (progn
	(format t "error detected when opening connection : ~S~%" c)
	(cl-mongo:mongo-close :twitter)))))

(defun mongodb-connect (connection-plist)  (format t "initializing mongodb..~%")
  (setf *mongodb-host* (or (getf connection-plist :host) "localhost"))
  (setf *mongodb-port* (or (getf connection-plist :port) cl-mongo::+MONGO-PORT+))
  (setf *mongodb-db*   (or (getf connection-plist :db)   "twitter"))
  (handler-case 
      (connection)
    (error (c)
      (progn
	(format t "error detected when opening connection : ~S~%" c)
	(cl-mongo:mongo-close :all)))))

(defvar *symbol->string* (make-hash-table  :test 'equal))

(defun symbol->string (type)
  (labels ((add-to-table (type)
	     (setf (gethash type *symbol->string*) (string-downcase (symbol-name type)))))
    (or (gethash type *symbol->string*) (add-to-table type))))


(defun split-pos-helper (str char accum)
  (let ((pos (position char str)))
    (if pos
	(split-pos-helper (subseq str (+ 1 pos)) char (cons (+ pos 1 (or (car accum) -1)) accum))
	accum)))

(defun split-pos (char str)
  (nreverse (cons (length str) (split-pos-helper str char ()))))

(defun split-string (char str)
  (let* ((locations (split-pos char str))
	 (starts    (nreverse (cdr (nreverse (cons 0 (mapcar (lambda (x) (+ 1 x)) locations))))))
	 (boundaries (nreverse (pairlis starts locations)))
	 (lst ()))
    (dolist (item boundaries)
      (push (subseq str (car item) (cdr item)) lst))
    (nreverse lst)))

(defun decode-to-hash-table (lst)
  (let ((ht (make-hash-table :test 'equal)))
    (labels ((assocp (lst)
	       (and (consp lst) (consp (car lst)) (symbolp (car (car lst)))))
	     (item-listp (lst)
	       (and (consp lst) (consp (car lst)) ))
	     (map-fn (el)
	       (if (assocp el)
		   (decode-to-hash-table el)
		   el))
	     (add (key value)
	       (setf (gethash (symbol->string key) ht) value)))
      (dolist (el lst)
	(cond ((and (symbolp (car el)) (atom (cdr el)))       (add (car el) (cdr el)))
	      ((and (symbolp (car el)) (assocp (cdr el)))     (add (car el) (decode-to-hash-table (cdr el))))
	      ((and (symbolp (car el)) (item-listp (cdr el))) (add (car el) (mapcar #'map-fn (cdr el))))     
	      (t	                                      (add (car el) (cdr el)))))
      ht)))

;; IMPORTANT : use 'safe mode' i.e chech get-last-error before continuing....

(defmacro store-twitter-object (type)
 `(progn 
    (defmethod db-store-object ((collection ,type) (lisprec cons))
      (let ((ht (decode-to-hash-table lisprec)))
	(setf (gethash "_id" ht) (unique-id collection))
	(cl-mongo:db.update  (symbol->string (type-of collection)) ($ "_id" (gethash "_id" ht))  ht :upsert t :mongo (connection))))))

;;---------------- twitter db interface implementation -----------

(defmethod db-initialize (&rest args)
  (mongodb-connect (car args))
  (create-caches store-twitter-object))

(defmethod db-shutdown (&rest args)
  (declare (ignore args))
  (cl-mongo:mongo-close :all))

(defmethod db-map-reduce ((collection t) (map-fn function) (reduce-fn function) &key (selector :all) (initial-value nil) (from-end nil))
  (let ((lst (mapcar map-fn (docs (cl-mongo:iter (cl-mongo:db.find collection selector :mongo (connection)) :mongo (connection) )))))
    (reduce reduce-fn lst :initial-value initial-value :from-end from-end)))

(defmethod db-map-reduce ((collection t) (map-fn function) (reduce-fn (eql nil)) &key (selector :all))
  (mapcar map-fn (docs (iter (db.find collection selector  :mongo (connection)) :mongo (connection) ))))

(defmethod db-status (&rest args)
  (declare (ignore args))
  (format t "mongodb driver loaded~%")
  (cl-mongo:show :errors)
  (format t "~S~%" (connection)) 
  (let ((collections (docs (db.collections :mongo (connection))))
	(collnames ()))
    (dolist (item collections)
      (let ((split (split-string #\. (cl-mongo:get-element "name" item))))
	(when (and (eq 2 (length split)) (string/= (nth 1 split) "system")) (push (nth 1 split) collnames))))
    (dolist (name collnames)
      (format t "~1t~25<~A ~>: ~10t ~A ~%" name (ret (db.count name :all :mongo (connection)))))))
	   

