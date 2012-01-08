(in-package :cl-twitter)

;;indices are lists of ids. See the twitter-social-graph where idendity is defined...
;;
(defun parse-identity (ref)
  ref)

(define-element url-entity ((indices (identity)))
  "a url entity "
  (id   "" nil)
  (url  "" nil)
  (display-url "" nil)
  (expanded-url "" nil)
  (indices "" nil))


(defmethod print-object ((url-entity url-entity) stream)
  (format stream "#<URL '~A' ~S>"   (url-entity-url url-entity) (url-entity-indices url-entity)))

;;-------
(define-element hashtag ((indices (identity)))
  "a hash tag entity"
  (id "" nil)
  (text "" nil)
  (indices "" nil))

(defmethod print-object ((hashtag hashtag) stream)
  (format stream "#<HASHTAG '#~A' ~S>"   (hashtag-text hashtag) (hashtag-indices hashtag)))
;;--------

(define-element user-mention ((indices (identity)))
  "a user mention"
  (id          "" nil)
  (id-str      "" nil)
  (screen-name "" nil)
  (name        "" nil)
  (indices     "" nil))


(defmethod print-object ((user-mention user-mention) stream)
  (format stream "#<USER-MENTION id :~A '@~A' ~S>" (user-mention-id user-mention)  (user-mention-screen-name user-mention)   
	  (user-mention-indices user-mention) ))
;;--------
(define-element media-size ()
  "generic media size"
  (id     "" nil)
  (w      "" nil)
  (h      "" nil)
  (resize "" nil))

(define-element media-sizes ((large media-size) (medium media-size) (small media-size) (thumb media-size))
  "media entity size.."
  (id      "" nil)
  (large   "" nil)
  (medium  "" nil)
  (small   "" nil)
  (thumb   "" nil))

(define-element media-entity ((sizes media-sizes) (indices (identity)))
  " a media entity.."
  (id               ""   nil)
  (id-str           ""   nil)
  (media-url        ""   nil)
  (media-url-https  ""   nil)
  (url              ""   nil)
  (display-url      ""   nil)
  (expanded-url     ""   nil)
  (type             ""   nil)
  (sizes            ""   nil)
  (indices          ""   nil))


(defmethod print-object ((media-entity media-entity) stream)
  (format stream "#<MEDIA id :~A ~A url :~A ~S>" 
	  (media-entity-id media-entity) 
	  (media-entity-type media-entity)   
	  (media-entity-display-url media-entity)  
	  (media-entity-indices media-entity)))

;;------
(define-element twitter-entities ((urls (url-entity)) (hashtags (hashtag)) (media (media-entity)) (user-mentions (user-mention)))
  "twitter entities.."
  (id            "" nil)
  (hashtags      "" nil)
  (user-mentions "" nil)
  (media         "" nil)
  (urls          "" nil))

;;---helper functions....
(defmethod entity-hashtags ((tweet tweet))
  (twitter-entities-hashtags (tweet-entities tweet)))


(defmethod entity-user-mentions ((tweet tweet))
    (twitter-entities-user-mentions (tweet-entities tweet)))

(defmethod entity-media ((tweet tweet))
  (twitter-entities-media (tweet-entities tweet)))

(defmethod entity-urls ((tweet tweet))
  (twitter-entities-urls (tweet-entities tweet)))


;;---------------------------------

    
