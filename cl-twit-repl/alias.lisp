(in-package :cl-twit-repl)

(defvar *alias-registry*   (make-hash-table :test 'equal))
(defvar *alias-file-name*  "cl-twit-repl.alias")

(defun alias-file ()
  (default-file-path "alias/" *alias-file-name*))

(defmacro testit (x y)
  `(destructuring-bind (name (&rest args) &rest body) ',y
     (format nil "(defun ~S (&rest args) (apply (~S  ~S   ~S  ) args)) " ',x name  args (car body))))


(defmacro construct-alias-func (x y)
  `(if (listp ,y)
       (progn
	 (if (equal (car ,y) 'lambda)
	     (destructuring-bind (name (&rest args) &rest body) ,y
	       (format nil "(defun ~S (&rest args) (apply (~S ~A ~S) args)) " ,x name  args (car body)))
	     (destructuring-bind (fn &rest rest) ,y
	       (format nil "(defun ~S (&rest args) (apply '~S  ~{ ~S ~} args))" ,x fn rest))))
       (format nil "(defun ~S (&rest args) (apply '~S args))" ,x ,y)))


(defmacro alias (&optional x y)
  `(if ',x
	 (if ',y
	     (progn
	       (eval (read (make-string-input-stream (cdr (setf (gethash ',x  *alias-registry*) (cons ',y (construct-alias-func ',x ',y)))))))
	       (dump-alias))
	     (car (gethash ',x *alias-registry*)))
       (maphash (lambda (k v) (format t "~1t~S ~20t=> ~25t~S~%" k  (car v))) *alias-registry*))) 


  
(defmacro unalias (&optional x)
  `(progn (if ',x
	      (progn (remhash ',x *alias-registry*)
		     (fmakunbound ',x))
	      (progn (maphash (lambda (k v) (declare (ignore v)) (fmakunbound k)) *alias-registry*)
		     (setf *alias-registry* (make-hash-table :test 'equal))))))



(defun dump-alias ()
  (with-open-file (stream (alias-file) :direction :output :if-exists :supersede)
    (maphash (lambda (key lst) (format stream "~S~%" (list key lst))) *alias-registry*)))

(defun read-alias ()
  (when (probe-file (alias-file))
    (with-open-file (stream (alias-file) :direction :input )
      (do ((line (read stream nil) (read stream nil))) 
	  ((null line))
	(eval (read (make-string-input-stream (cdr (setf (gethash (car line)  *alias-registry*) (cadr line))))))))))

      
