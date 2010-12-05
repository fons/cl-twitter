(in-package :twitter)
;;
;; Tiny URL API
;;

;;; TinyURL-ize
;;; Using the very simple TinyURL API

(defparameter *tinyurl-url* "http://tinyurl.com/api-create.php")
(defconstant +http-ok+ 200)

(defun get-tinyurl (url)
  "Get a TinyURL for the given URL. Uses the TinyURL API service.
   (c) by Chaitanaya Gupta via cl-twit"
  (multiple-value-bind (body status-code)
      (funcall *http-request-function*
	       *tinyurl-url*
	       :parameters `(("url" . ,url)))
    (if (= status-code +http-ok+)
        body
        (error 'http-error
               :status-code status-code
               :url url
               :body body))))

(defun convert-to-tinyurl (text)
  (let ((result text)
	(regex (ppcre:create-scanner "http:[^\\s\\)\\]\\'\\\"]+")))
    (ppcre:do-matches (start end regex result result)
      (when (> (- end start) 24)
	(setf result (ppcre:regex-replace 
		      regex result 
		      (get-tinyurl (subseq result start end))))))))



