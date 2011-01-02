(in-package :twitter)

(defun day-of-week (i)
  (nth i *day-of-week-strings*))

(defun month (i)
  (nth i *month-strings*))

(defun current-utc (stream)
  (multiple-value-bind (s m h date month year dow &rest args) (decode-universal-time (get-universal-time) 0)
    (format stream "~A ~A ~A ~A:~A:~A ~A" (day-of-week dow) (month month) date h m s year)))

(defun twitter-app-uri (method)
  (concatenate 'string *twitter-app-uri* method))

(defun twitter-search-uri (method)
  (concatenate 'string *twitter-search-uri* method))

(defun twitter-oauth-uri (method)
  (concatenate 'string *twitter-oauth-uri* method))

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

(defun maybe-add-conversion-from-twitter (twitter-sym)
  "Add a conversion between _ and - forms of argument symbols starting with a twitter symbol"
  (let* ((twitter-key (as-keyword twitter-sym))
	 (twitter-name (symbol-name twitter-key)))
    (when (find #\_ twitter-name)
      (let ((lisp-key (intern (convert-from-twitter twitter-name) :keyword)))
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

(defun rem-nil-keywords (list keywords)
  "Remove keywords from a keylist"
  (loop for (value indicator) on list by #'cddr
	unless (and (member value keywords) (null indicator))
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
    (cons (fix-attribute (string-downcase (as-string (first plist)))) (cons (to-uri-param (second plist) escape-p) (plist->uri-params (cddr plist) escape-p)))))
		

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

(defun strip-keywords (keyword-list plist)
  (if keyword-list
      (strip-keywords (cdr keyword-list) (strip-keyword (car keyword-list) plist))
      plist))

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

;;this is for attributes in the geo specification. in lisp I use attribute->entity syntax; twitter expects attribute:entity but :k:l is invalid syntax in lisp.
(defun fix-attribute (arg)
  (ppcre:regex-replace "->" arg ":" ))

(defun to-uri-param (arg escape-p)
  (if escape-p 
      (url-rewrite:url-encode (princ-to-string arg))
      (princ-to-string arg)))

(defun split-to-key (lst)
  (let ((e))
    (do () ((or (null lst) (keywordp (car lst)))) 
      (push (pop lst) e))
    (values (nreverse e) lst)))
