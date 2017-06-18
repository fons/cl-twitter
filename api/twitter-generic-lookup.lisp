(in-package :cl-twitter)

;; fmakunbound
;; Generic lookup function
;; Encapsulates all lookups twitter has implemented
;;
(defgeneric verify(type obj lst)
  (:documentation "verify the that rest elements are all of the same type"))

;;;;
(defmethod verify( (type (eql :string)) (string t) (cons t)) nil)

(defmethod verify( (type (eql :string)) (string string) (cons cons))
  (if cons
      (verify :string (car cons) (cdr cons))
      t))

(defmethod verify((type (eql :string)) (string string) (cons (eql nil))) t)


(defmethod verify( (type (eql :integer)) (integer t) (cons t))
  (error "expected a list of integers.."))

(defmethod verify( (type (eql :integer)) (integer integer) (cons cons))
  (if cons
      (verify :integer (car cons) (cdr cons))
      t))

(defmethod verify((type (eql :integer)) (integer integer) (cons (eql nil))) t)



(defgeneric lookup(entity  obj &rest rest)
  (:documentation "generic lookup function"))


(defmethod lookup (entity (obj (eql nil)) &rest rest)
  (declare (ignore rest))
  nil)

(defmethod lookup ((entity (eql :statuses)) (cons cons) &rest ignore)
  (unless ignore
    (if (verify :integer (car cons) (cdr cons))
        (statuses-lookup  (format nil "~{~a,~}" cons) :include-entities t :_map t))))

(defmethod lookup ((entity (eql :statuses)) (id integer) &rest rest)
  (if (verify :integer id rest)
      (statuses-lookup  (format nil "~a,~{~a,~}" id rest) :include-entities t :_map t)))



(defmethod lookup ((entity (eql :users)) (screen-name string) &rest rest)
  (if (verify :string screen-name rest)
      (users-lookup :screen-name (format nil "~a,~{~a~}" screen-name rest))))

(defmethod lookup ((entity (eql :users)) (user-id integer) &rest rest)
  (if (verify :integer user-id rest)
      (users-lookup :user-id (format nil "~a,~{~a~}" user-id rest))))

(defmethod lookup ((entity (eql :users)) (cons cons) &rest ignore)
  (unless ignore
    (cond ((verify :integer (car cons) (cdr cons)) (users-lookup :user-id (format nil "~{~a~}" cons)))
          ((verify :string (car cons) (cdr cons)) (users-lookup :screen-name (format nil "~{~a~}" cons)))
          (t nil))))



(defmethod lookup ((entity (eql :friendships)) (screen-name string) &rest rest)
  (if (verify :string screen-name rest)
      (friendships-lookup :screen-name (format nil "~a,~{~a~}" screen-name rest))))

(defmethod lookup ((entity (eql :friendships)) (user-id integer) &rest rest)
  (if (verify :integer user-id rest)
      (friendships-lookup :user-id (format nil "~a,~{~a~}" user-id rest))))

(defmethod lookup ((entity (eql :friendships)) (cons cons) &rest ignore)
  (unless ignore
    (cond ((verify :integer (car cons) (cdr cons)) (friendships-lookup :user-id (format nil "~{~a~}" cons)))
          ((verify :string (car cons) (cdr cons)) (friendships-lookup :screen-name (format nil "~{~a~}" cons)))
          (t nil))))








