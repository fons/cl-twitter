(in-package :cl-twitter)

;;(((:QUERY . "natural language")
;;  (:CREATED-AT . "Mon Nov 15 19:42:01 +0000 2010") (:ID-STR . "17468711")
;;  (:NAME . "natural language") (:POSITION) (:ID . 17468711))
;; ((:QUERY . "land of lisp") (:CREATED-AT . "Mon Nov 15 19:52:50 +0000 2010")
;;  (:ID-STR . "17469306") (:NAME . "land of lisp") (:POSITION) (:ID . 17469306)))

(define-element saved-search ()
  "a saved search "
  (id               "" nil)
  (query            "" nil)
  (created-at       "" nil)
  (id-str           "" nil)
  (name             "" nil)
  (position         "" nil))


(defmethod print-object ((ref saved-search) stream)
  (format stream "#<TWITTER-SAVED-SEARCH '~A:~A'>" (saved-search-id ref)(saved-search-name ref) ))

(defun lookup-saved-search (rec)
  (declare (ignore rec)))

(defun print-saved-search (ref)
  (format t "~A: ~A ~A~%" (saved-search-name ref) (saved-search-query ref) (saved-search-id  ref)))

(defmethod register-twitter-object ((ref saved-search)))

;;
;; Saved Searches resources
;;          saved_searches
;;          saved_searches/show/:id
;;          saved_searches/create
;;          saved_searches/destroy/:id
;; -> in general for all the save searches : ;; TODO : sensible return structure

(define-command saved-searches (:get (:saved-search))
    (twitter-app-uri "saved_searches.json")
    "Returns the authenticated user's saved search queries.")

;; TODO : sensible return structure
(define-command saved-searches/show/?id (:get-id :saved-search)
    (twitter-app-uri "saved_searches/show/<id>.json")
    "Retrieve the data for a saved search owned by the authenticating user specified by the given id."
  :id "The ID of the saved search.")


(define-command saved-searches/create (:post :saved-search)
    (twitter-app-uri "saved_searches/create.json")
    "Creates a saved search for the authenticated user."
  :query "The query of the search the user would like to save.")


(define-command saved-searches/destroy/?id (:post-id :saved-search)
    (twitter-app-uri "saved_searches/destroy/<id>.json")
    "Destroys a saved search for the authenticated user. The search specified by id must be owned by the authenticating user."
  :id "The ID of the saved search.")

;;---------------------------------------------------------------------------------------------------------------------------

(defun saved-searches (&rest args)
  (apply 'twitter-op :saved-searches args))

(defun saved-search (id) 
  (apply 'twitter-op :saved-searches/show/?id :id id nil))

(defun save-search (query)
  (apply 'twitter-op :saved-searches/create :query query nil))

(defun delete-search (id)
  (apply 'twitter-op :saved-searches/destroy/?id :id id nil))


;;--------------------------------------------------------------------

(defun show-search (saved-search)
  (saved-search (saved-search-id saved-search)))

(defun rm-search (saved-search)
  (delete-search (saved-search-id saved-search)))