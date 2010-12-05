(in-package :twitter)

(defmacro fixup (lst)
  `(and (keywordp (car ,lst)) (cons nil ,lst))) 

(defmacro with-gensyms ((&rest names) &rest body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro with-cursor ((&key (max -1) (collector #'identity) (skip 0) (extractor nil) (controller (lambda (x) (declare (ignore x)) 0))  (test (lambda() nil) ))  &rest body)
     (with-gensyms ($max $skip fn targs kargs _cursor_ cursor-id fn_ args_ args)
       `(macrolet ((unpack$ ( (,fn_ &rest ,args_) )
		     `(values (quote ,,fn_) (list ,@,args_))))
	  (multiple-value-bind (,fn ,targs) (unpack$ ,@body)
	    (multiple-value-bind (,args ,kargs) (split-to-key ,targs)
	      (let ((,$max (+ 1 ,max ,skip))
		    (,$skip ,skip)
		    (,_cursor_ (or (cadr (member :cursor ,kargs)) -1)))
		(do () ((or (zerop ,_cursor_) (zerop (decf ,$max)) (funcall ,test)))
		  (progn
		    ;;(format t "[~A] ~A ~S~%" ,kargs ,fn (nconc ,args (strip-keyword :cursor ,kargs) (list :cursor ,_cursor_)))
		    (let ((,cursor-id (apply ,fn (nconc ,args (strip-keyword :cursor ,kargs) (list :cursor ,_cursor_)))))
		      (if (zerop ,$skip)
			  (funcall ,collector (funcall ,extractor ,cursor-id))
			  (decf ,$skip))
		      (setf ,_cursor_ (funcall ,controller ,cursor-id)))))))))))

;;(defmacro with-paging ( (&key (max 1500) (skip 0) (collector nil) ) &rest body)
;; based on the with-cursor macro

(defmacro with-paging ((&key (max 1500) (max-pages 15) (collector #'identity) (skip 0) (controller nil) (test (lambda() nil)) )  &rest body)
  (with-gensyms ($results $max $skip fn kargs _page_ args twitter-search fn_ args_ $rpp)
    `(macrolet ((unpack$ ( (,fn_ &rest ,args_) )
		  `(values (quote ,,fn_) (list ,@,args_))))
       (multiple-value-bind (,fn ,kargs) (unpack$ ,@body)
	 (destructuring-bind (,args &key callback lang locale rpp page since-id until geocode show-user result-type) ,kargs
	   (let ((,$max ,max)
		 (,$skip ,skip)
		 (,$results 0)
		 (,$rpp (or rpp 100))
		 (,_page_ (or page 1)))
	     (do () ((or (> ,_page_ ,max-pages) (> 0 (- ,$max ,$results)) (funcall ,test) ))
	       (progn
		 (let ((,twitter-search (funcall ,fn ,args :page ,_page_ :rpp ,$rpp :lang lang :callback callback :locale locale :since-id since-id 
						 :until until :geocode geocode :show-user show-user :result-type result-type )))
		   (incf ,$results (length (search-result-results ,twitter-search)))
		   (if (zerop ,$skip)
		       (funcall ,collector (search-results ,twitter-search))
		       (decf ,$skip ,$results ))
		   (if (null ,controller)
		       (incf ,_page_)
		       (setf ,_page_ (funcall ,controller ,twitter-search))))))))))))
