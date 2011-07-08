(in-package :twitter)

;;
;; Element record sugar
;;

;; Elements are structures that mirror the fields of twitter elements.
;; We allow specific types to maintain identity and effectively make 
;; objects immutable.  The macro records description information and
;; provides some convenience conversions of _ symbols to - symbols.

(defvar *element-descriptions* (make-hash-table :test #'eq)
  "Lookup element descriptions")

(defmacro define-element (name embedded &body arguments)
  "A macro for compactly defining a twitter element.  Args are lists of
   (slotname description value-type).  First two elts are optional unique-p
    and an optional description"
  (let* ((has-tag-p (symbolp (first arguments)))
	 (unique-p (when has-tag-p (first arguments)))
	 (description (if has-tag-p (second arguments) (first arguments)))
	 (args (if has-tag-p (cddr arguments) (cdr arguments))))
    `(progn
       (defclass ,name ()
	 ,(mapcar #'(lambda (arg) (make-slot-init name arg)) args))
       (defun ,(parse-fn-name name) (rec)
	 (parse-element-record ',name rec ',embedded ,unique-p))
       (defun ,(intern (format nil "~A-P" name)) (inst)
	 (eql (type-of inst) ',name))
       (let ((args ',args))
	 (add-conversions args)
	 (record-type-args ',name args)
	 (record-arg-descriptions ',name ,description args)))))

(eval-when (:compile-toplevel :load-toplevel)

  (defun make-slot-init (type arg)
    "Make a defclass slot initialization"
    (let ((slotname (first arg))
	  (initform (third arg)))
      `(,slotname :accessor ,(accessor-name type slotname)
		  :initarg ,(as-keyword slotname)
		  :initform ,initform)))
  
  (defun as-keyword (symbol)
    (intern (symbol-name symbol)
	    #.(find-package :keyword)))
  
  (defun add-conversions (args)
    "Add twitter->lisp conversions for define-element args"
    (mapcar #'maybe-add-conversion (mapcar #'car args)))
  
  (defun record-type-args (type args)
    "Record arguments for type checking prior to creation"
    (declare (ignore type args)))
  
  (defun record-arg-descriptions (type description args)
    "Record the descriptions for specific slot types.  This
   works because they're universal in the twitter API"
    (setf (gethash type *element-descriptions*) 
	  (list description (mapcar #'first args)))
    (loop for arg in args do
	 (let ((keyarg (first arg)))
	   (setf (gethash keyarg *element-descriptions*)
		 (rest arg)))))

  ;; Lookup default functions
  
  (defun accessor-name (type slotname)
    "Create accessor name"
    (intern (format nil "~A-~A" type slotname)
	    #.(find-package :twitter)))
  
  (defun parse-fn-name (type)
    "Given a type, return the canonical parser name.  Created automatically"
    (intern (concatenate 'string "PARSE-" (symbol-name type))
	    #.(find-package :twitter)))
  
  (defun lookup-fn-name (type)
    "Given a type, return the name lookup fn. User defined."
    (intern (concatenate 'string "LOOKUP-" (symbol-name type))
	    #.(find-package :twitter)))

  ) ;;end of toplevel compile

(defun lookup-id-name (type)
  "Given a type, return the name lookup fn. User defined."
  (intern (concatenate 'string (symbol-name type) "-ID")  #.(find-package :twitter)))


;;
;; Parsing records
;;


(defgeneric parse-record (response prim-type)
  (:documentation "Default response parser for primitive types; assume target structure")
  (:method (response prim-type)
    (funcall (parse-fn-name prim-type) response))
  (:method (response (prim-type (eql :integer)))
    (parse-integer response))
  (:method (response (prim-type (eql :string)))
    response)
  (:method (response (prim-type (eql :identity)))
    response))




;;move to after register-twitter-object is defined
;;	 (existing (when (fboundp (lookup-fn-name type)) (funcall (lookup-fn-name type) lisprec))))
(defun parse-element-record (type rec embedded unique-p)
  "Generic parsing function"		     
  ;;(format t "generic parsing function *) ~A:~A:~A:~A~%" type rec embedded unique-p)
  (let* ((lisprec  (twitter->lisp-alist rec))
	 (existing (lookup-twitter-object type lisprec)))
    ;;(format t "~S:~A~%" lisprec existing)
    (if existing
	(prog1 existing
	  (unless unique-p ;; maintains eq, but refreshes values
	    (update-element-fields existing lisprec)
	    (create-embedded-elements existing embedded)))
	(let ((new (default-make-element lisprec type)))
	  (create-embedded-elements new embedded)
	  (register-twitter-object new lisprec)
	  new))))

(defgeneric register-twitter-object (ref lisprec))
(defgeneric lookup-twitter-object (ref lisprec))

(defun default-make-element (rec type)
  "Make an element of type from record."
  ;;(format t "=====> ~S:~S <<====~%" rec type)
  (let* ((valid-initargs (mapcar (lambda (slot)
				   (intern (symbol-name (closer-mop:slot-definition-name slot)) :keyword))
				 (closer-mop:class-direct-slots (find-class type))))
	 (apply-args (alist->plist (remove-if-not #'(lambda (initarg)
						      (progn
							;;(format t "[~S]~%" initarg)
							(member initarg valid-initargs))) rec :key #'car))))
    ;;#+nil
    ;;(format t "valid : ~S - apply ~S ~%" valid-initargs apply-args)
    ;;(format t "Making element: ~A ~S~%" type apply-args)
    (apply #'make-instance type apply-args)))

(defun update-element-fields (element rec)
  "Since we don't know whether we are updating an existing or new element."
  (loop :for (field . value) :in rec :do
	(let ((slot-name (localize-symbol field #.(find-package :twitter))))
	  (if (slot-exists-p element slot-name)
	      (setf (slot-value element slot-name) value)
	      #+nil
	      (format t "Invalid slot name ~A for object ~S" slot-name element)))))

(defun create-embedded-elements (elt embedded)
  "For any slot value that is designated embedded, take the alist
   and create an instance of that embedded object"
  ;;(format t "***) ~A:~A~%" elt embedded)
  ;;NOT ALL ID'S are integers
;;  (when (stringp (slot-value elt 'id))
;;    (setf (slot-value elt 'id) (parse-integer (slot-value elt 'id))))
	  
  (loop for (argname type) in embedded do
       (let ((value (slot-value elt argname)))
	 ;;(format t "***??) ~A:~A~%" elt argname)
	 ;;(format t "-----> value : ~A~%" value)
	 (when value
	   (setf (slot-value elt argname)
		 (parse-embedded-element type value))))))

(defun parse-embedded-element (type value)
  "Parse elements of type from value, map over the value
   as a list of type is of type consp"
  (when (consp value)
    (if (consp type)
	(mapcar (lambda (entry)
		  (funcall (parse-fn-name (first type)) entry))
		value)
	(funcall (parse-fn-name type) value))))
  
			
;;
;; Describe an element
;;

(defun get-description (type)
  (gethash type *element-descriptions*))

(defun element-help (type)
  (destructuring-bind (desc slots) (get-description type)
    (format t "~A~%~A~%~%Slots:~%~{~A: ~A~%~}~%"
	    type desc 
	    (loop for slot in slots 
		 nconc (list slot (car (get-description slot)))))))
  

;;
;; User elements
;;

(defvar *twitter-user* nil
  "The authenticated user")


(defun user-http-auth (user)
  "If the given USER has no login credentials, returns NIL.  If the
user has been logged in via OAUTH, returns

    (:oauth access-token).

If the user has been logged in via basic authorization, returns

    (:basic-authorization username password)"
  (declare (optimize debug))
  (when user
    (acond ((twitter-user-access-token user)
	    (list :oauth  it))
	   ((and (twitter-user-screen-name user) (twitter-user-password user))
	    (list :basic-authorization
		  (twitter-user-screen-name user)
		  (twitter-user-password user)))
	   (t (cerror "Continue" "User has no auth credentials: ~S" user)))))





