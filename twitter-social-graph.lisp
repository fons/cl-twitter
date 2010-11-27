(in-package :cl-twitter)


;; A quick note
;; These calls can be made with or without a cursor supplied.
;; If no cursor is supplied than an attempt is made to return all followers and freinds, even though that may exceed the alloted 
;; list size of 5000. When that happens, a error is returned.
;; In order to make this all useful a cursor of -1 is set as the default. This will start a  cursor in all cases. 
;; The cursor-id element below is a data structure setup to capture bth the list of ids as well as the cursor.
;; The with-cursor macro will continue the cursoring...

;;((:NEXT-CURSOR-STR . "1353128277932682391") (:PREVIOUS-CURSOR-STR . "0")
;; (:NEXT-CURSOR . 1353128277932682391)
;;(:IDS 216401816 21755021 212199048 23127160 24453936 171180765 219681711
;;  137153890 22498181 75816735 14456639 168650976 219678703 24814522 219677889
;;  219678963 56450976 28145041 132592341 14315451 212233581 45806371 58472385
;;  95968614 16939709 24726967 41449511)
;; (:PREVIOUS-CURSOR . 0))

(defun parse-identity (ref)
  ref)

(define-element cursor-id ((ids (identity)))
  "a saved search "
  (id                  "" nil)
  (next-cursor-str     ""  nil)
  (previous-cursor-str "" nil)
  (next-cursor         ""  nil)
  (ids                 ""  nil)
  (previous-cursor     ""  nil))

(defmethod print-object ((ref cursor-id) stream)
  (format stream "#<TWITTER-CURSOR-ID '~A:~A'>" (cursor-id-next-cursor ref) (length (cursor-id-ids ref)) ))

(defun lookup-cursor-id (rec)
  (declare (ignore rec)))

(defun print-cursor-id (ref)
  (format t "~A: ~A ~A~%" (cursor-id-previous-cursor ref) (cursor-id-next-cursor ref) (length (cursor-id-ids ref))))

(defmethod register-twitter-object ((ref cursor-id)))


;; Social Graph Methods
;;
;;  Friends and Followers resources :
;;     friends/ids
;;     followers/ids
;;
;; To begin paging provide a value of -1 as the cursor. 
;; The response from the API will include a previous_cursor and next_cursor to allow paging back and forth. 
;; If the cursor is not provided the API will attempt to return all IDs. For users with many connections this will probably fail. 
;; Querying without the cursor parameter is deprecated and should be avoided. The API is being updated to force the cursor to be -1 if it isn't supplied.
;; TODO : handle the cursor...

;;(define-command friends/ids (:get (:identity))
(define-command friends/ids (:get :cursor-id)
    (twitter-app-uri "friends/ids.json")
    "Returns an array of numeric IDs for every user the specified user is following."
  :user_id "Optional.  The ID or screen name of (the user for whom to request a list of friends."
  :screen_name "Optional.  The ID or screen name of (the user for whom to request a list of friends."
  :cursor "Causes the list of connections to be broken into pages of no more than 5000 IDs at a time. ")


;;(define-command followers/ids (:get (:identity))
(define-command followers/ids (:get :cursor-id)
    (twitter-app-uri "followers/ids.json")
    "Returns an array of numeric IDs for every user following the specified user."
  :user_id "Optional.  The ID or screen name of (the user for whom to request a list of friends."
  :screen_name "Optional.  The ID or screen name of (the user for whom to request a list of friends."
  :cursor "Causes the list of connections to be broken into pages of no more than 5000 IDs at a time." )

;;---------------------------------------------------------------------------------

(defun friend-ids (screen-name &key (cursor -1))
  (apply 'twitter-op :friends/ids :screen-name screen-name :cursor cursor nil ))

(defun follower-ids (screen-name &key (cursor -1))
  (apply 'twitter-op :followers/ids :screen-name screen-name :cursor cursor nil ))

(defmacro with-gensyms ((&rest names) &rest body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))


;;
;; A quick nore on the controller : ordinarily I would have set the controllor default to
;; cursor-id-next-cursor. Unfortunatly that leads to an error message in sbcl because the
;; defintion for generic methods cannot be dumped into fasl files.
;; Hence this work-around...
;;
(defmacro with-cursor ((&key (max -1) (collector #'identity) (skip 0) (controller nil) )  &rest body)
     (with-gensyms ($max $skip fn kargs _cursor_ args cursor-id fn_ args_)
       `(macrolet ((unpack$ ( (,fn_ &rest ,args_) )
		     `(values (quote ,,fn_) (list ,@,args_))))
	  (multiple-value-bind (,fn ,kargs) (unpack$ ,@body)
	    (destructuring-bind (,args &key cursor) ,kargs
	      (let ((,$max (+ 1 ,max ,skip))
		    (,$skip ,skip)
		    (,_cursor_ (or cursor -1)))
		(do () ((or (zerop ,_cursor_) (zerop (decf ,$max))))
		  (progn
		    (let ((,cursor-id (funcall ,fn ,args :cursor ,_cursor_)))
		      (if (zerop ,$skip)
			  (funcall ,collector (cursor-id-ids ,cursor-id))
			  (decf ,$skip))
		      (if (null ,controller)
 			   (setf ,_cursor_ (cursor-id-next-cursor ,cursor-id) )
			   (setf ,_cursor_ (funcall ,controller ,cursor-id))))))))))))

(defun collect-followers (screen-name)
  (let ((lst))
    (labels ((collect-it (l)
	       (setf lst (nconc lst l))))
      (with-cursor (:collector #'collect-it) (follower-ids screen-name)))
    lst))

(defun collect-friends (screen-name)
  (let ((lst))
    (labels ((collect-it (l)
	       (setf lst (nconc lst l))))
      (with-cursor (:collector #'collect-it) (friend-ids screen-name)))
    lst))
    
