(in-package :cl-twitter-engine)

(defun initialize (user)
  (progn
    (cl-twit-repl:get-authenticated-user user)
    (use-cache)
    (use-db :twitter-mongodb-driver)
    (db-status)))

(defun job-monitor-home-timeline (user)
  (when (zerop (cl-twitter:rate-limit-remaining-hits (cl-twitter:rate-limit-status))) (error "rate limit exceeded for user ~A" user))
  (cl-twitter:home-timeline :count 201))

(defun monitor-home-timeline (user)
  (initialize user)
  (submit-job "monitor-home-timeline" #'job-monitor-home-timeline :args (list "mohegskunkworks") :every 600 :iter 2000 :errorhandler t))

(defun job-monitor-public-timeline (user)
  (when (zerop (cl-twitter:rate-limit-remaining-hits (cl-twitter:rate-limit-status))) (error "rate limit exceeded for user ~A" user))
  (cl-twitter:public-timeline))

(defun monitor-public-timeline (user)
  (initialize user)
  (submit-job "monitor-public-timeline" #'job-monitor-public-timeline :args (list "mohegskunkworks") :every 61 :iter 2000 :maxerror 20 :errorhandler t))