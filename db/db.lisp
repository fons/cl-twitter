(in-package :twitter-db)


(defvar *twitter-db-spec* nil  "Use to setup the elephant spec to store stuff at")

;;
;; Basic database operations
;;

(defvar *twitter-db* nil "Stores the open twitter db controller")

(defvar *users* nil)
(defvar *tweets* nil)
(defvar *msgs* nil)

(defun open-twitter-db (&optional spec)
  (when spec (setf *twitter-db-spec* spec))
  (unless *twitter-db-spec*
    (error "You must define or provide *twitter-db-spec*, a valid elephant spec"))
  (let ((*store-controller* nil))
    (declare (special *store-controller*))
    (setf *twitter-db* (open-store *twitter-db-spec*))
    (with-transaction (:store-controller *twitter-db*)
      (if (get-from-root 'users :sc *twitter-db*)
	  (setf *users* (get-from-root 'users :sc *twitter-db*)
		*tweets* (get-from-root 'tweets :sc *twitter-db*)
		*msgs* (get-from-root 'msgs :sc *twitter-db*))
	  (setup-twitter-db)))))

(defmacro db-key-form (fn)
  `'(lambda (i k v)
     (declare (ignore i k))
     (let ((value (,fn v)))
       (when value (values t value)))))

(defun setup-twitter-db ()
  ;; Users
  (setf *users* (add-to-root 'users (make-indexed-btree *twitter-db*) 
			     :sc *twitter-db*))
  (add-index *users* :index-name 'screen-name 
	     :key-form (db-key-form twitter-user-screen-name))
  (add-index *users* :index-name 'name 
	     :key-form (db-key-form twitter-user-name))
  ;; Tweets
  (setf *tweets* (add-to-root 'tweets (make-indexed-btree *twitter-db*) 
			      :sc *twitter-db*))
  (add-index *tweets* :index-name 'time 
	     :key-form (db-key-form tweet-created-at))
  (add-index *tweets* :index-name 'reply-to-id 
	     :key-form (db-key-form tweet-in-reply-to-status-id))
  (add-index *tweets* :index-name 'user-id 
	     :key-form '(lambda (index id tweet)
			 (declare (ignore index id))
			 (values t (twitter-user-id
				    (tweet-user tweet)))))
  ;; Messages
  (setf *msgs* (add-to-root 'msgs (make-indexed-btree *twitter-db*) 
			    :sc *twitter-db*))
  (add-index *msgs* :index-name 'time 
	     :key-form (db-key-form twitter-message-created-at))
  (add-index *msgs* :index-name 'sender-id 
	     :key-form (db-key-form twitter-message-sender-id))
  (add-index *msgs* :index-name 'recipient-id 
	     :key-form (db-key-form twitter-message-recipient-id)))

(defun close-twitter-db ()
  (when *twitter-db*
    (close-store *twitter-db*))
  (setf *users* nil)
  (setf *tweets* nil)
  (setf *msgs* nil))

;;
;; Record new structures
;;

(defmacro with-twitter-db (() &body body)
  `(when *twitter-db*
     (with-transaction (:store-controller *twitter-db*)
       ,@body)))

;;(defmethod register-twitter-object :after ((user twitter-user))
;;  (format t "-----> calling after twitter user method on ~S~%" user)
;;  )


;;  (with-twitter-db ()
;;    (unless (get-value (twitter-user-id user) *users*))
;;    (setf (get-value (twitter-user-id user) *users*) user)))

;;(defmethod register-twitter-object :after ((tweet tweet))
;;  (format t "-----> calling after tweet method on ~S~%" tweet)
;;  )

;;  (with-twitter-db ()
;;    (unless (get-value (tweet-id tweet) *tweets*)
;;      (setf (get-value (tweet-id tweet) *tweets*) tweet))))

;;(defmethod register-twitter-object :after ((msg twitter-message))
;;  (with-twitter-db ()
;;    (unless (get-value (twitter-message-id msg) *msgs*)
;;      (setf (get-value (twitter-message-id msg) *msgs*) msg))))
  
;;
;; Retrieval
;;

;; Users

(defun user-name-index () (get-index *users* 'name))
(defun user-screen-index () (get-index *users* 'screen-name))

(defun find-twitter-user (ref &key name)
  (cond ((twitter::twitter-user-p ref)
	 ref)
	((integerp ref)
	 (get-value ref *users*))
	((stringp ref)
	 (get-value ref (user-screen-index)))
	(name
	 (get-value ref (user-name-index)))))

(defun map-twitter-users (fn)
  (map-btree (lambda (k v) 
	       (declare (ignore k))
	       (funcall fn v)) 
	     *users*))

;; Tweets

(defun tweet-time-index () (get-index *tweets* 'time))
(defun tweet-user-index () (get-index *tweets* 'user-id))
(defun tweet-reply-index () (get-index *tweets* 'reply-to-id))

(defun get-tweet (id)
  (get-value id *tweets*))

(defun map-tweets (fn)
  (map-btree (lambda (k v) 
	       (declare (ignore k))
	       (funcall fn v))
	     *tweets*))

(defun map-user-tweets (fn user &optional collect)
  (let ((mfn (lambda (k v) 
	       (declare (ignore k))
	       (funcall fn v))))
    (if (twitter::twitter-user-p user)
	(map-btree mfn (tweet-user-index) 
		   :value (twitter-user-id user)
		   :collect collect)
	(map-btree mfn (tweet-user-index)
		   :value (twitter-user-id
			   (find-twitter-user user))
		   :collect t))))

(defun user-tweets (user)
  (map-user-tweets #'identity user t))


;; Messages

(defun msg-time-index () (get-index *msgs* 'time))
(defun msg-sender-index () (get-index *msgs* 'sender-id))
(defun msg-recipient-index () (get-index *msgs* 'recipient-id))


(defun get-tweet-msg (id)
  (get-value id *msgs*))

(defun map-tweet-msgs (fn)
  (map-btree (lambda (k v) 
	       (declare (ignore k))
	       (funcall fn v)) 
	     *msgs*))

(defun map-user-msgs (fn user &optional collect)
  (let ((mfn (lambda (k v) 
	      (declare (ignore k))
	      (funcall fn v))))
      (cond ((twitter::twitter-user-p user)
	     (map-btree mfn (msg-sender-index) :value (twitter-user-id user) 
			:collect collect))
	    ((stringp user)
	     (map-btree mfn (msg-sender-index) 
			:value (find-twitter-user user)
			:collect collect))
	    ((integerp user)
	     (map-btree mfn *tweets* :value user :collect collect))
	    (t (error "User reference not understood: ~A" user)))))

(defun user-msgs (user)
  (flet ((identity-v (k v)
	   (declare (ignore k)) 
	   v))
    (map-user-msgs #'identity-v user t)))
  

;;-------------

