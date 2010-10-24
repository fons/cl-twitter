(in-package :twitter)

;;
;; Support for parsing element records
;;

(defun get-value (name object)
  (awhen (assoc name object)
    (cdr it)))
(defun set-value (value name object) 
  (setf (cdr (assoc name object)) value))
(defsetf get-value set-value)

;;
;; Convenient symbol conversions
;;

(defvar *lisp->twitter-symbols* (make-hash-table :test 'eq)
  "Maintain conversions between _ and - forms for simplicity")
(defvar *twitter->lisp-symbols* (make-hash-table :test 'eq)
  "Maintain conversions between _ and - forms for simplicity")

(defun lisp->twitter (sym)
  (aif (gethash sym *lisp->twitter-symbols*) it
       sym))

(defun lisp->twitter-plist (plist)
  (loop for elt in plist 
       for i from 1 collect
       (if (oddp i) 
	   (lisp->twitter elt)
	   elt)))

(defun twitter->lisp (sym)
  (aif (gethash sym *twitter->lisp-symbols*) it
       sym))

(defun twitter->lisp-alist (alist)
  (loop for cell in alist do
       (setf (car cell) (twitter->lisp (car cell))))
  alist)

(defun maybe-add-conversion (lisp-sym)
  "Add a conversion between _ and - forms of argument symbols"
  (let* ((lisp-key (as-keyword lisp-sym))
	 (lisp-name (symbol-name lisp-key)))
    (when (find #\- lisp-name)
      (let ((twitter-sym (convert-to-twitter lisp-name)))
	(setf (gethash lisp-key *lisp->twitter-symbols*) twitter-sym
	      (gethash twitter-sym *twitter->lisp-symbols*) lisp-key)))))

(defun convert-to-twitter (string)
  (intern (string-upcase (substitute #\_ #\-  (copy-seq string))) :keyword))

(defun convert-from-twitter (string)
  (string-upcase (substitute #\- #\_ (copy-seq string))))

(defun rem-keywords (list keywords)
  "Remove keywords from a keylist"
  (loop for (value indicator) on list by #'cddr
	unless (member value keywords)
	  nconc (list value indicator)))


;;
;; Property list utilities
;;

(defun plist->alist (plist)
  (when (valid-plist plist)
    (cons (cons (first plist) (second plist))
	  (plist->alist (cddr plist)))))

(defun alist->plist (alist)
  (when (consp alist)
    (cons (caar alist)
	  (cons (cdar alist)
		(alist->plist (cdr alist))))))

(defun plist->uri-params (plist &optional escape-p)
  (when (valid-plist plist)
    (cons (string-downcase (as-string (first plist)))
	  (cons (to-uri-param (second plist) escape-p)
		(plist->uri-params (cddr plist))))))

(defun plist-keywords (plist)
  (when (valid-plist plist)
    (cons (first plist) (plist-keywords (cddr plist)))))

(defun valid-plist (plist)
  (and (consp plist) (consp (cdr plist))))

(defun strip-keyword (keyword plist)
  (when (valid-plist plist)
    (if (eq (car plist) keyword)
	(strip-keyword keyword (cddr plist))
	(cons (first plist)
	      (cons (second plist)
		    (strip-keyword keyword (cddr plist)))))))

(defun strip-keyword-if (predicate plist)
  (when (valid-plist plist)
    (if (funcall predicate (car plist))
	(strip-keyword-if predicate (cddr plist))
	(cons (first plist)
	      (cons (second plist)
		    (strip-keyword-if predicate (cddr plist)))))))

;;
;; Other
;;

(defun as-string (arg)
  (typecase arg
    (string arg)
    (symbol (symbol-name arg))))

(defun as-keyword (arg)
  (typecase arg
    (string (intern arg :keyword))
    (symbol (intern (symbol-name arg) :keyword))))

(defun localize-symbol (sym &optional (package *package*))
  (typecase sym
    (string (intern sym package))
    (symbol (intern (symbol-name sym) package))))

(defun to-uri-param (arg escape-p)
  (if escape-p 
      (trivial-http:escape-url-query 
       (princ-to-string arg))
      (princ-to-string arg)))
