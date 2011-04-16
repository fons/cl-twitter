(in-package :cl-twitter)

;;interface specification for drivers

(defgeneric db-initialize (&rest args))
(defgeneric db-shutdown   (&rest args))
(defgeneric db-store-object (collection lisprec))
(defgeneric db-map-reduce   (collection map-fn reduce-fn &key))
(defgeneric db-status (&rest args))  


(defmethod db-initialize (&rest args)
  (declare (ignore args)))
(defmethod db-store-object ((collection t) (lisprec t)))
(defmethod db-shutdown  (&rest args)
  (declare (ignore args)))
(defmethod db-map-reduce ((collection t) (map-fn function) (reduce-fn function) &key))
(defmethod db-status (&rest args)
  (declare (ignore args))
  (format t "no driver loaded...~%"))

(defvar *driver-spec-file* "driver.spec" "file which stores/contains the driver specification")

(defun driver-spec ()
  (default-file-path "access/" *driver-spec-file*))

(defun read-driver-spec ()
  (labels ((gv (tag lst)
	     (cadr (member tag lst))))
    (let ((ht (make-hash-table :test 'equal)))
      (with-open-file (stream (driver-spec) :direction :input :if-does-not-exist :create)
	(do ((line (read stream nil) (read stream nil))) 
	    ((null line))
	  (let ((key (cons (gv :TAG line) (gv :DRIVER line))))
	    (setf (gethash key ht) line)))) 
      ht)))


(defun write-driver-spec (driver-spec)
  (labels ((gv (tag lst)
	     (cadr (member tag lst))))
    (let ((ht  (read-driver-spec))
	  (key (cons (gv :TAG driver-spec) (gv :DRIVER driver-spec))))
      (setf (gethash key ht)  driver-spec) 
      (with-open-file (stream (driver-spec) :direction :output :if-exists :supersede :if-does-not-exist :create)
	(maphash (lambda (key lst) (declare (ignore key)) (format stream "~S~%" lst)) ht)))))

(defun use-db (driver-package &key (spec nil specp) (tag :default))
  (when driver-package
    (require driver-package)
    (use-package driver-package)
    (when specp (write-driver-spec (list :TAG tag :DRIVER driver-package :SPEC spec)))
    (let ((spec (gethash (cons tag driver-package) (read-driver-spec))))
      (when spec (db-initialize (cadr (member :SPEC spec)))))))

  
;;;-------------------------------------------------------------------------------------------------------------
(defmethod  lookup-twitter-object ((ref t) (lisprec t)))
(defmethod  register-twitter-object ((twitter-object t) (lisprec cons)))



