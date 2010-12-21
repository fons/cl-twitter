(in-package :cl-twit-repl)

;; used to serialize the access tokes to a stream/file
;;;
;;secret user-data token-consumer session-handle expires authorization-expires origin-uri)
(defvar *access-file* "access.ht")

(defun default-access-path ()
  (let ((dirs (mapcar #'sb-ext:native-namestring (directory "./."))))
    (labels ((cl-twitter-root (path)
	       (multiple-value-bind (start end reg1 reg2) (ppcre:scan "/cl-twitter/" path)
		 (declare (ignore start reg1 reg2))
		 (subseq path 0 end))))
      (let ((root-dirs (mapcar #'cl-twitter-root dirs)))
	(if root-dirs
	    (concatenate 'string (car root-dirs) "access/" *access-file*)
	    ())))))

(defun access-file ()
  (default-access-path))

(defun serialize-user-data (token)
  (mapcar (lambda (e) (list (car e) (cdr e))) (oauth::token-user-data token)))

(defvar *CONSUMER-TOKEN-SERIALIZER* '( (:key                   oauth::token-key)
				      (:secret                oauth::token-secret)
				      (:user-data             serialize-user-data)))

(defun serialize-consumer-token (token)
  (let ((consumer-token (oauth:token-consumer token)))
    (mapcar (lambda (e) (list (car e) (funcall (cadr e) consumer-token) )) *CONSUMER-TOKEN-SERIALIZER*)))


(defvar  *ACCESS-TOKEN-SERIALIZER* '( (:key                   oauth::token-key)
				     (:secret                oauth::token-secret)
				     (:user-data             serialize-user-data)
				     (:consumer              serialize-consumer-token)
				     (:expires               oauth::access-token-expires)
				     (:authorization-expires oauth::access-token-authorization-expires)
				     (:origin-uri            oauth::access-token-origin-uri)
				     (:session-handle oauth::access-token-session-handle)))

(defun user-name (token)
  (cdr (assoc "screen_name" (oauth:token-user-data token)  :test #'equal)))

(defun serialize-access-token (token)
    (list (user-name token) (cons :access (list (mapcar (lambda (e) (list (car e) (funcall (cadr e) token) )) *ACCESS-TOKEN-SERIALIZER*)))))

(defun write-access-info (twitter-user)
  (let ((ht  (read-access-info))
	(lst (serialize-access-token (twitter-user-access-token twitter-user))))
    (setf (gethash (car lst) ht) lst)
    (with-open-file (stream (access-file) :direction :output :if-exists :supersede)
      (maphash (lambda (key lst) (format stream "~S~%" lst)) ht)))) 
		 

(defun read-access-info()
  (let ((ht (make-hash-table :test 'equal)))
    (with-open-file (stream (access-file) :direction :input )
      (do ((line (read stream nil) (read stream nil))) 
	  ((null line))
	(setf (gethash (car line) ht) line))) 
    ht))
      

(defvar *N* (pairlis '(:access :consumer) '(oauth:make-access-token oauth:make-consumer-token) ))

(defun specp (lst)
  (and (symbolp (car lst)) (assoc (car lst) *N*) (consp (cadr lst)) (consp (car (cadr lst)))))

(defun make-keyword (name) (values (intern (string-upcase name) "KEYWORD")))

(defun make-cons-lst (lst accum)
  (if (null lst)
      (nreverse accum)
      (make-cons-lst (cdr lst) (cons (cons (car (car lst)) (cadr (car lst))) accum))))

(defun make-user-data (args)
  (make-cons-lst args ()))

(defun unpack (kv)
  (cond ((specp kv)                    (list (make-keyword (car kv)) (maker kv)))
	((string= (car kv) "USER-DATA") (list (make-keyword (car kv)) `(quote ,(make-user-data(cadr kv)))))
	(t                             (list (make-keyword (car kv)) (cadr kv)))))


(defun maker (lst)
  (let ((fun (cdr (assoc (car lst) *N*)))
	(arglist  (cadr lst)))
    (cons fun (reduce (lambda (l r) (append l r)) (mapcar #'unpack  arglist) ))))

(defun get-access-token (user)
  (labels ((check-name (ht)
	     (let ((lst (gethash user ht)))
	       (if lst
		   (cadr lst)
		   (error (format nil "access credentials for user ~A not found~%" user)))))) 
    (eval (maker (check-name (read-access-info))))))

