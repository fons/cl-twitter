# CL-TWITTER : A common lisp API for Twitter

## API Status:

All of [twitter's rest api calls](http://dev.twitter.com/doc/) are covered, asof 12/13/2010.

The api consists of two layers. The first layer is relatively low level and is nothing more than a wrapper around the rest api call. 
The second layer provides the same functionality, but is usually a littel easier to use. 

For example the first level call to get the user id's for a user's followers looks like :

    (defun follower-ids (screen-name &key (cursor -1))
       (apply 'twitter-op :followers/ids :screen-name screen-name :cursor cursor nil ))


The cursor keyword is used to used to retrieve subsequent pages of ids. 
The *collect-follower-ids* uses *follower-ids* and the *with-cursor* macro to retrieve a list of all the followers.

    (defun collect-follower-ids (screen-name &key (max -1) (skip 0))
       (let ((lst))
           (labels ((collect-it (l)
	        (setf lst (nconc lst l))))
           (with-cursor (:skip skip :max max :extractor #'cursor-id-ids :controller #'cursor-id-next-cursor :collector #'collect-it :test #'rate-limit-exceeded ) (follower-ids screen-name)))
        lst))



Data is returned as json. cl-twitter has a way of defining the return type, and using the json to instantiate that type. 

*follower-ids* in the previous example returns a list of ids, so that pretty straightforward. *tweets* however carry a lot more information. Internally a tweet is defined using the *define-element*
macro :

    (define-element tweet ((user twitter-user))
      "A status element consisting of information on a status and a nested
       user element"
      (id "" nil)
      (contributors nil nil)
      (created-at "" nil)
      (text "" nil)
      (source "" nil)
      (truncated "" nil)
      (favorited "" nil)
      (in-reply-to-status-id "" nil)
      (in-reply-to-user-id "" nil)
      (in-reply-to-screen-name "" nil)
      (geo "" nil)
      (geo-enabled "" nil)
      ;; embedded user
     (user "" nil))


The caller can access the tweet data using standard *(tweet-... )* generic functions, like *tweet-id* or *tweet-text*.

## API Overview
 
TBD 