(in-package :cl-twit-repl)

(defun cursor ()
  (if *twitter-user*
      (format t "~&~A> " (twitter-user-screen-name *twitter-user*))
      (format t "~&~A> " "not_authenticated")))
  
(defun cl-twit-read ()
  (read-from-string (read-line) nil nil))

(defun cl-twit-eval (sexp)
  (eval sexp))

(defun cl-twit-print (lst)
  (format t "~S" (show lst)))

(defun cl-twit-repl ()
  (cursor)
  (let ((cmd (cl-twit-read)))
    (unless (and (consp cmd) (eq (car cmd ) 'quit))
      (cl-twit-print (cl-twit-eval cmd))
      (cl-twit-repl))))