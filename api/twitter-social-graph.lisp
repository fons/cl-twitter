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

(define-element social-graph-cursor-id ((ids (identity)))
  "a cursor element "
  (id                  "recompile override below" nil)
  (next-cursor-str     ""  nil)
  (previous-cursor-str "" nil)
  (next-cursor         ""  nil)
  (screen-name         ""  nil)
  (command             ""  nil)
  (ids                 ""  nil)
  (previous-cursor     ""  nil))

(defmethod print-object ((ref social-graph-cursor-id) stream)
  (format stream "#<TWITTER-SOCIAL-GRAPH-CURSOR-ID '~A:~A'>" (social-graph-cursor-id-screen-name ref) (length (social-graph-cursor-id-ids ref)) ))
;;override because api doesn't return an id
;; the id is constructed with the assumption that the head of the list is always the latest follower/friend

(defmethod unique-id ((ref social-graph-cursor-id))
  (let ((id (cons (social-graph-cursor-id-screen-name ref) (cons (car (social-graph-cursor-id-ids ref)) (social-graph-cursor-id-command ref)))))
    #+nil(format t "unique cursor id ~S [~A : ~A] ~%" id (social-graph-cursor-id-previous-cursor ref) (social-graph-cursor-id-next-cursor ref))
    (format nil "~S" id)))
  

(defun print-social-graph-cursor-id (ref)
  (format t "~A: ~A ~A~%" (social-graph-cursor-id-previous-cursor ref) (social-graph-cursor-id-next-cursor ref) (length (social-graph-cursor-id-ids ref))))


(define-element social-graph-cursor-user ((users (twitter-user)))
  "a cursor element "
  (id                  "recompile override below" nil)
  (next-cursor-str     ""  nil)
  (previous-cursor-str "" nil)
  (next-cursor         ""  nil)
  (screen-name         ""  nil)
  (command             ""  nil)
  (users                 ""  nil)
  (previous-cursor     ""  nil))

(defmethod print-object ((ref social-graph-cursor-user) stream)
  (format stream "#<TWITTER-SOCIAL-GRAPH-CURSOR-ID <~A:~A:~A>" (social-graph-cursor-user-screen-name ref) (social-graph-cursor-user-command ref)  (length (social-graph-cursor-user-users ref)) ))


;;override because api doesn't return an id
;; the id is constructed with the assumption that the head of the list is always the latest follower/friend

(defmethod unique-id ((ref social-graph-cursor-user))
  (let ((id (cons (social-graph-cursor-id-screen-name ref) (cons (twitter-user-id (car (social-graph-cursor-user-users ref))) (social-graph-cursor-id-command ref)))))
    #+nil(format t "unique cursor id ~S [~A : ~A] ~%" id (social-graph-cursor-id-previous-cursor ref) (social-graph-cursor-id-next-cursor ref))
    (format nil "~S" id)))
  

(defun print-social-graph-cursor-user (ref)
  (format t "~A: ~A ~A~%" (social-graph-cursor-id-previous-cursor ref) (social-graph-cursor-id-next-cursor ref) (length (social-graph-cursor-user-users ref))))


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


(define-command friends/ids (:get :social-graph-cursor-id)
    (twitter-app-uri "friends/ids.json")
    "Returns an array of numeric IDs for every user the specified user is following."
  :user_id "Optional.  The ID or screen name of (the user for whom to request a list of friends."
  :screen_name "Optional.  The ID or screen name of (the user for whom to request a list of friends."
  :cursor "Causes the list of connections to be broken into pages of no more than 5000 IDs at a time. "
  :stringify_ids "any programming environments will not consume our Tweet ids due to their size."
  :count "Specifies the number of IDs attempt retrieval of, up to a maximum of 5,000 per distinct request.")

(define-command followers/ids (:get :social-graph-cursor-id)
    (twitter-app-uri "followers/ids.json")
    "Returns an array of numeric IDs for every user following the specified user."
  :user_id "Optional.  The ID or screen name of (the user for whom to request a list of friends."
  :screen_name "Optional.  The ID or screen name of (the user for whom to request a list of friends."
  :cursor "Causes the list of connections to be broken into pages of no more than 5000 IDs at a time."
  :stringify_ids "any programming environments will not consume our Tweet ids due to their size."
  :count "Specifies the number of IDs attempt retrieval of, up to a maximum of 5,000 per distinct request.")

(define-command friends/list (:get :social-graph-cursor-user)
    (twitter-app-uri "friends/list.json")
    "Returns a cursored collection of user objects for every user the specified user is following (otherwise known as their “friends”)."
  :user_id "Optional.  The ID or screen name of (the user for whom to request a list of friends."
  :screen_name "Optional.  The ID or screen name of (the user for whom to request a list of friends."
  :cursor      "Causes the list of connections to be broken into pages of no more than 5000 IDs at a time. "
  :count "Specifies the number of IDs attempt retrieval of, up to a maximum of 5,000 per distinct request."
  :skip_status "When set to either true, t or 1 statuses will not be included in the returned user objects."
  :include_user_entities  "When set to either true, t or 1 statuses will not be included in the returned user objects.")

(define-command followers/list (:get :social-graph-cursor-user)
    (twitter-app-uri "followers/list.json")
    "Returns a cursored collection of user objects for users following the specified user."
  :user_id "Optional.  The ID or screen name of (the user for whom to request a list of friends."
  :screen_name "Optional.  The ID or screen name of (the user for whom to request a list of friends."
  :cursor "Causes the list of connections to be broken into pages of no more than 5000 IDs at a time."
  :count "Specifies the number of IDs attempt retrieval of, up to a maximum of 5,000 per distinct request."
  :skip_status "When set to either true, t or 1 statuses will not be included in the returned user objects."
  :include_user_entities  "When set to either true, t or 1 statuses will not be included in the returned user objects.")

;;
;;---------------------------------------------------------------------------------------------------------------------
;;max and skip refer to pages (of 5000 each) !!
;;
;;

(defmethod friends-ids ((screen-name string) &key (cursor -1) (count 20))
  (apply 'twitter-op-ext :friends/ids :screen-name screen-name :cursor cursor :count count nil ))

(defmethod friends-ids ((user-id integer) &key (cursor -1) (count 20))
  (apply 'twitter-op-ext :friends/ids :user-id user-id :cursor cursor :count count nil ))

(defmethod followers-ids ((screen-name string) &key (cursor -1) (count 20))
  (apply 'twitter-op-ext :followers/ids :screen-name screen-name :cursor cursor :count count nil ))

(defmethod followers-ids ((user-id integer) &key (cursor -1) (count 20))
  (apply 'twitter-op-ext :followers/ids :user-id user-id :cursor cursor :count count nil ))

(defmethod friends-list ((screen-name string) &key (cursor -1) (count 20) (skip-status nil) (include-user-entities nil))
  (apply 'twitter-op-ext :friends/list :screen-name screen-name :cursor cursor :count count :skip-status skip-status :include-user-entities include-user-entities nil ))

(defmethod friends-list ((user-id integer) &key (cursor -1) (count 20) (skip-status nil) (include-user-entities nil))
  (apply 'twitter-op-ext :friends/list :user-id user-id :cursor cursor :count count :skip-status skip-status :include-user-entities include-user-entities nil ))

(defmethod followers-list ((screen-name string) &key (cursor -1) (count 20) (skip-status nil) (include-user-entities nil))
  (apply 'twitter-op-ext :followers/list :screen-name screen-name :cursor cursor :count count :skip-status skip-status :include-user-entities include-user-entities nil ))

(defmethod followers-list ((user-id integer) &key (cursor -1) (count 20) (skip-status nil) (include-user-entities nil))
  (apply 'twitter-op-ext :followers/list :user-id user-id :cursor cursor :count count :skip-status skip-status :include-user-entities include-user-entities nil ))

;;
;;--------------------------------------------------------------------------------------------------------------------------------------
;;

(defun followers-as-ids (user &key (max -1) (skip 0) (count 20))
  (let ((lst))
    (labels ((collect-it (l)
	       (setf lst (nconc lst l))))
      (with-cursor (:skip skip :max max :extractor #'social-graph-cursor-id-ids :controller #'social-graph-cursor-id-next-cursor :collector #'collect-it :test #'rate-limit-exceeded )
        (followers-ids user :count count)))
    lst))


(defun friends-as-ids (user &key (max -1) (skip 0) (count 20))
  (let ((lst))
    (labels ((collect-it (l)
	       (setf lst (nconc lst l))))
      (with-cursor (:skip skip :max max :extractor #'social-graph-cursor-id-ids :controller #'social-graph-cursor-id-next-cursor :collector #'collect-it :test #'rate-limit-exceeded )
        (friends-ids user :count count)))
    lst))


(defun followers (user &key (max -1) (skip 0) (count 20) (skip-status nil) (include-user-entities nil))
  (let ((lst))
    (labels ((collect-it (l)
	       (setf lst (nconc lst l))))
      (with-cursor (:skip skip :max max :extractor #'social-graph-cursor-user-users :controller #'social-graph-cursor-user-next-cursor :collector #'collect-it :test #'rate-limit-exceeded )
        (followers-list user :count count :skip-status skip-status :include-user-entities include-user-entities)))
    lst))


(defun friends (user &key (max -1) (skip 0) (count 20) (skip-status nil) (include-user-entities nil))
  (let ((lst))
    (labels ((collect-it (l)
	       (setf lst (nconc lst l))))
      (with-cursor (:skip skip :max max :extractor #'social-graph-cursor-user-users :controller #'social-graph-cursor-user-next-cursor :collector #'collect-it :test #'rate-limit-exceeded )
        (friends-list user :count count :skip-status skip-status :include-user-entities include-user-entities)))
    lst))


;;;;;;;;

(defun start-parse(expr)
  #+nil(format t "start parse expression : ~A~%" expr)
  (list (list (list :START)) expr))

(defun parse-add-type (type)
  (lambda (parser-state)
    (let ((parsed       (car parser-state))
          (remainder    (cadr parser-state)))
      (list (cons (list :TYPE type) parsed) remainder))))

(defun end-parse(parser-state)
  (let ((parsed       (car parser-state))
        (remainder    (cadr parser-state)))
    (nreverse parsed)))

(defun parser-error (parser-state)
  (let ((remainder    (cadr parser-state)))
    (if remainder
        (format t "parse error detected : remainder ~S~%" remainder)
        parser-state)))


(defun parser-read (type keyword)
  (lambda (a)
    (list keyword type a)))


(defun parser-done (parser-state)
  (let* ((parsed       (car parser-state))
         (remainder    (cadr parser-state)))
    (cond ((and (car parser-state) (not (cadr parser-state))) (list (cons (list :DONE) parsed) remainder))
          (t  parser-state) )))


(defun parser-validate (keyword token)
  (lambda (a)
    (cond
      ((eql token a) (list keyword token))
      (t nil))))

(defun parser-print (a)
  (progn
    #+nil (format t "parser state ~S~%" a)
    a))

(defun valid-parser-state (parser-state)
  (if (and (car parser-state) (cadr parser-state)) t nil))

(defun invalidate-parser-state (parser-state)
  #+nil (format t "parser state : ~A~%" parser-state)
  (cond ((not (car parser-state)) parser-state) ;; already invalid
        ((and (car parser-state) (not (cadr parser-state))) (list () (list :ERROR :MISSING-ARGUMENTS)))
        (t    (error  (format nil "incoherent parser-state : ~S~%" parser-state)))))
         
(defun parser-op (op)
  (lambda (parser-state)
    (if (valid-parser-state parser-state)
        (let* ((parsed       (car parser-state))
               (remainder    (cadr parser-state))
               (next-symbol (car  remainder))                          
               (parse-result (funcall op next-symbol)))
          (cond (parse-result (list (cons parse-result parsed) (cdr remainder)))
                (t  (list () remainder))))
        (invalidate-parser-state parser-state))))

(defun parser-op-cond(if-op then-op)
  (lambda (parser-state)
    (funcall (parser-op then-op) (funcall (parser-op if-op) parser-state))))

(defun parser-op-or(left-op right-op)
  (lambda (parser-state)
    (let ((next-parser-state (funcall left-op parser-state)))
      (if (valid-parser-state next-parser-state) next-parser-state (funcall right-op parser-state)))))
  

(defun parser-or(left right)
  (lambda (a)
    (let ((vl (funcall left a)))
      (if vl vl (funcall right a)))))

;;(social-graph all friends of darealmaozedong as list of users)

(defun intern-all(command)
  (if (symbolp command)
      (intern (symbol-name command) :keyword) command))

(defun stringify-all(command)
  (if (symbolp command)
      (format nil "~A" command) command))



;;(run-parser (social-graph-parser) all followers of user darealmaozedong as set of users)

(defmacro run-parser (parser &rest expr)
  (let ((symbol-expr (mapcar #'intern-all expr)))
    `(reduce (lambda(parse-state fun) (funcall fun parse-state)) (nreverse ,parser) :initial-value (start-parse (list ,@symbol-expr)))))

(defun social-graph-parser ()
  (list #'end-parse
        #'parser-done
        #'parser-error
        (parser-op (parser-or (parser-validate :RETURN :USERS) (parser-validate :RETURN :IDS)))
        (parser-op (parser-validate :KEYWORD :OF))
        (parser-op (parser-or (parser-validate :CONTAINER :SET) (parser-validate :CONTAINER :LIST)))
        (parser-op (parser-validate :KEYWORD :AS))
        (parser-op-or (parser-op-cond (parser-validate :KEYWORD :USER) (parser-read :STRING :USER))
                      (parser-op-cond (parser-validate :KEYWORD :USERID) (parser-read :INTEGER :USERID)))
        (parser-op (parser-validate :KEYWORD :OF))
        (parser-op (parser-or (parser-validate :ACTION :FRIENDS) (parser-validate :ACTION :FOLLOWERS)))
        (parser-op-or (parser-op-cond (parser-validate :METHOD :SELECT) (parser-or (parser-validate :COUNT :ALL) (parser-read :INTEGER :COUNT)))
                      (parser-op-cond (parser-validate :METHOD :COUNT) (parser-validate :COUNT :ALL)))
        (parse-add-type :SOCIAL-GRAPH)
        ))

(defun parse-tree-start? (parse-tree)
  (eql (caar parse-tree) :START))

(defun parse-tree-done? (parse-tree)
  (eql (caar (reverse parse-tree)) :DONE)) 

(defun run-verify-parse-tree(parse-tree &rest checks)
  (reduce (lambda (l r) (and l r)) (mapcar (lambda (fun) (funcall fun parse-tree)) checks)))
   
(defun verify-parse-tree (parse-tree)
  (if (run-verify-parse-tree parse-tree #'parse-tree-start? #'parse-tree-done?) parse-tree nil))


 (defun abstract-syntax-tree (parse-tree)
   (remove-if (lambda(element) (or (eql (car element) :KEYWORD) (eql (car element) :START) (eql (car element) :DONE))) parse-tree))

(defmacro social-graph(&rest expr)
  `(abstract-syntax-tree (verify-parse-tree (run-parser (social-graph-parser) ,@expr))))

(defun dispatch(tree)
  (if tree
    (destructuring-bind ((&key type) &rest rest) tree
      #+nil (format t "type ~A ~A ~%" type rest)
      (dispatch-on-type type rest))))
 
(defgeneric dispatch-on-type (type tree) )

(defmethod dispatch-on-type((type (eql :social-graph)) (tree cons))
  (destructuring-bind ( (&key method) (count qualifier &optional number) (&key action) (user datatype value) (&key container) (&key return)) tree
    (let ((func (intern (string-upcase (format nil "~A-type" type)))))
      #+nil(format t "~A ~A ~A ~A ~A ~A ~A ~A ~A ~A ~A ~A ~%" func type method count qualifier number action user datatype (symbol-name value) container return)
      (funcall func method (interp-number count qualifier number) action (symbol-name value) container return))))

  
(defgeneric interp-number (count qualifier number))

(defmethod interp-number ( (count (eql :count)) (qualifier (eql :all)) (number t))
  :ALL)

(defmethod interp-number ( (count (eql :count)) (qualifier (eql :integer)) (number integer))
  number)

;;;=======================================

;;((:TYPE :SOCIAL-GRAPH) (:METHOD :SELECT) (:COUNT :ALL) (:ACTION :FOLLOWERS) (:USER :STRING :DAREALMAOZEDONG) (:CONTAINER :LIST) (:RETURN :USERS))

(defun user-list-to-set (user-lst)
  (let ((ht (make-hash-table  :test 'equal :size 1500)))
    (labels ((collect (user)
               (setf (gethash (twitter-user-id user) ht) user)))
      (progn
        (mapcar #'collect user-lst)
        ht))))

(defgeneric social-graph-type(method count action user container return) )

(defmethod social-graph-type((method t) (count t) (action t) (user t) (container t) (return t) )
  (format t "no implementation found for social-graph-type with arguments ~A" (list method count action user container return)))

(defmethod social-graph-type((method t) (count t) (action t) (user t) (container (eql :SET)) (return (eql :USERS)) )
  (user-list-to-set (social-graph-type method count action user :LIST return)))

(defmethod social-graph-type((method (eql :COUNT)) (count (eql :ALL)) (action (eql :FOLLOWERS)) (user string) (container t) (return (eql :USERS)) )
  (list action (twitter-user-followers-count (show-user user))  :USER user ))

(defmethod social-graph-type((method (eql :COUNT)) (count (eql :ALL)) (action (eql :FRIENDS)) (user string) (container t) (return (eql :USERS)) )
  (twitter-user-friends-count (show-user user)))

(defun determine-page-size(count)
  (let* ((page-size  (if (< count 200) count 200))
         (max-calls  (ceiling (/ count page-size))))
    (values page-size max-calls)))
 
(defmethod social-graph-type((method (eql :SELECT)) (count (eql :ALL)) (action (eql :FOLLOWERS)) (user string) (container (eql :LIST)) (return (eql :USERS)) )
  (followers user :count 200))


(defmethod social-graph-type((method (eql :SELECT)) (count integer) (action (eql :FOLLOWERS)) (user string) (container (eql :LIST)) (return (eql :USERS)) )
  (multiple-value-bind (page-size max-calls) (determine-page-size count)
    (followers user :count page-size :max max-calls)))

(defmethod social-graph-type((method (eql :SELECT)) (count (eql :ALL)) (action (eql :FRIENDS)) (user string) (container (eql :LIST)) (return (eql :USERS)) )
  (friends user :count 200))

(defmethod social-graph-type((method (eql :SELECT)) (count integer) (action (eql :FRIENDS)) (user string) (container (eql :LIST)) (return (eql :USERS)) )
  (multiple-value-bind (page-size max-calls) (determine-page-size count)
    (friends user :count page-size :max max-calls)))


(defmethod social-graph-type((method (eql :SELECT)) (count (eql :ALL)) (action (eql :FOLLOWERS)) (user string) (container (eql :LIST)) (return (eql :IDS)) )
  (followers-as-ids user :count 200))


(defmethod social-graph-type((method (eql :SELECT)) (count integer) (action (eql :FOLLOWERS)) (user string) (container (eql :LIST)) (return (eql :IDS)) )
  (multiple-value-bind (page-size max-calls) (determine-page-size count)
    (followers-as-ids user :count page-size :max max-calls)))

(defmethod social-graph-type((method (eql :SELECT)) (count (eql :ALL)) (action (eql :FRIENDS)) (user string) (container (eql :LIST)) (return (eql :IDS)) )
  (friends-as-ids user :count 200))


(defmethod social-graph-type((method (eql :SELECT)) (count integer) (action (eql :FRIENDS)) (user string) (container (eql :LIST)) (return (eql :IDS)) )
  (multiple-value-bind (page-size max-calls) (determine-page-size count)
    (friends-as-ids user :count page-size :max max-calls)))



;; social-graph all of darealmaozedong friends as list of users
;; social-graph 200 of darealmaozedong friends as list of ids

;; social-graph friends of darealmaozedong as list of users
;;
;; social-graph of darealmaozedong friends as list of users .. as list of ids
;;
