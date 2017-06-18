(in-package :cl-user)

(defpackage #:cl-twitter-asd
  (:use :cl :asdf))

(in-package :cl-twitter-asd)

(defsystem cl-twitter
    :name "CL-TWITTER"
    :version "0.5"
    :maintainer "Fons Haffmans"
    :author "Ian Eslick"
    :licence "LLGPL"
    :description "An interface to Twitter"
    :components 
    ((:module "api"
      :serial t
      :components ((:file "package")
		   (:file "twitter-vars")
		   (:file "utils")
		   (:file "twitter-macro")
		   (:file "url-shortners")
		   (:file "commands")
		   (:file "elements")
		   (:file "dictionary")
		   (:file "conditions")
		   (:file "twitter-op")
		   (:file "twitter-user")
		   (:file "twitter-tweet-status")
                   (:file "twitter-application")
		   (:file "twitter-entity")
		   (:file "twitter-timeline")
		   (:file "twitter-trends")
		   (:file "twitter-social-graph")
		   (:file "twitter-account")
		   (:file "twitter-blocks") 
		   (:file "twitter-messages")
		   (:file "twitter-friendship")
		   (:file "twitter-favorites")
		   (:file "twitter-saved-searches")
		   (:file "twitter-search")
		   (:file "twitter-notifications")
		   (:file "twitter-lists")
		   (:file "twitter-list-members")
		   (:file "twitter-list-subscribers")
		   (:file "twitter-miscellaneous")
		   (:file "twitter-geo")
                   (:file "twitter-generic-lookup")
		   (:file "twitter-show")
		   (:file "twitter-db")
		   (:file "twitter-cache"))))
    :depends-on (:cl-json :trivial-http :drakma :anaphora :cl-ppcre :closer-mop :cl-oauth :url-rewrite))


