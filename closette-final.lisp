;;; -*- mode:lisp; coding:utf-8 -*-

(in-package #:closette)


(defgeneric print-object (instance stream))

(defmethod print-object ((instance standard-object) stream)
    (print-unreadable-object (instance stream :identity t)
        (format stream "(~S)"
                (class-name (class-of instance))))
    instance)

;;; Slot access

(defgeneric slot-value-using-class (class instance slot-name))
(defmethod slot-value-using-class ((class standard-class) instance slot-name)
    (std-slot-value instance slot-name))

(defgeneric set-slot-value-using-class (instance slot-name new-value))
(defmethod set-slot-value-using-class  ((instance standard-class) slot-name new-value)
    (setf (std-slot-value instance slot-name) new-value))

;;; N.B. To avoid making a forward reference to a (setf xxx) generic function:
(defsetf slot-value-using-class set-slot-value-using-class)

(defun setf-slot-value-using-class (new-value class object slot-name)
    (setf (slot-value-using-class class object slot-name) new-value))

(defgeneric slot-exists-p-using-class (class instance slot-name))
(defmethod slot-exists-p-using-class
    ((class standard-class) instance slot-name)
    (std-slot-exists-p instance slot-name))

(defgeneric slot-boundp-using-class (class instance slot-name))
(defmethod slot-boundp-using-class
    ((class standard-class) instance slot-name)
    (std-slot-boundp instance slot-name))

(defgeneric slot-makunbound-using-class (class instance slot-name))
(defmethod slot-makunbound-using-class
    ((class standard-class) instance slot-name)
    (std-slot-makunbound instance slot-name))

;;; Instance creation and initialization

(defgeneric allocate-instance (class))

(defmethod allocate-instance ((class standard-class))
    (std-allocate-instance class))

(defgeneric make-instance (class &key))

(defmethod make-instance ((class standard-class) &rest initargs)
    (let ((instance (allocate-instance class)))
        (apply #'initialize-instance instance initargs)
        instance))

(defmethod make-instance ((class symbol) &rest initargs)
    (apply #'make-instance (find-class class) initargs))

(defgeneric initialize-instance (instance &key))

(defmethod initialize-instance ((instance standard-object) &rest initargs)
    (apply #'shared-initialize instance t initargs))

(defgeneric reinitialize-instance (instance &key))
(defmethod reinitialize-instance
    ((instance standard-object) &rest initargs)
    (apply #'shared-initialize instance () initargs))

(defgeneric shared-initialize (instance slot-names &key))
(defmethod shared-initialize ((instance standard-object) slot-names &rest all-keys)
    (dolist (slot (class-slots (class-of instance)))
        (let ((slot-name (slot-definition-name slot)))
            (multiple-value-bind (init-key init-value foundp)
                (get-properties all-keys (slot-definition-initargs slot))
                (if foundp
                    (setf (slot-value instance slot-name) init-value)
                    (when (and (not (slot-boundp instance slot-name))
                               (not (null (slot-definition-initfunction slot)))
                               (or (eq slot-names t)
                                   (member slot-name slot-names)))
                        (setf (slot-value instance slot-name)
                              (funcall (slot-definition-initfunction slot))))))))
    instance)

;;; change-class

;;; bug: rotatef not release
(defgeneric change-class (instance new-class &key))
(defmethod change-class
    ((old-instance standard-object)
     (new-class standard-class)
     &rest initargs)
    (let ((new-instance (allocate-instance new-class)))
        (dolist (slot-name (mapcar #'slot-definition-name
                                   (class-slots new-class)))
            (when (and (slot-exists-p old-instance slot-name)
                       (slot-boundp old-instance slot-name))
                (setf (slot-value new-instance slot-name)
                      (slot-value old-instance slot-name))))
        (rotatef (std-instance-slots new-instance)
                 (std-instance-slots old-instance))
        (rotatef (std-instance-class new-instance)
                 (std-instance-class old-instance))
        (apply #'update-instance-for-different-class
               new-instance old-instance initargs)
        old-instance))

(defmethod change-class
    ((instance standard-object) (new-class symbol) &rest initargs)
    (apply #'change-class instance (find-class new-class) initargs))

(defgeneric update-instance-for-different-class (old new &key))
(defmethod update-instance-for-different-class
    ((old standard-object) (new standard-object) &rest initargs)
    (let ((added-slots
            (remove-if #'(lambda (slot-name)
                             (slot-exists-p old slot-name))
                       (mapcar #'slot-definition-name
                               (class-slots (class-of new))))))
        (apply #'shared-initialize new added-slots initargs)))

;;;
;;;  Methods having to do with class metaobjects.
;;;

(defmethod print-object ((class standard-class) stream)
    (print-unreadable-object (class stream :identity t)
        (format stream "(~S) ~S"
                (class-name (class-of class))
                (class-name class)))
    class)

(defmethod initialize-instance :after ((class standard-class) &rest args)
    (apply #'std-after-initialization-for-classes class args))

;;; Finalize inheritance

(defgeneric finalize-inheritance (class))
(defmethod finalize-inheritance ((class standard-class))
    (std-finalize-inheritance class)
    (values))

;;; Class precedence lists

(defgeneric compute-class-precedence-list (class))
(defmethod compute-class-precedence-list ((class standard-class))
    (std-compute-class-precedence-list class))

;;; Slot inheritance

(defgeneric compute-slots (class))
(defmethod compute-slots ((class standard-class))
    (std-compute-slots class))

(defgeneric compute-effective-slot-definition (class direct-slots))
(defmethod compute-effective-slot-definition
    ((class standard-class) direct-slots)
    (std-compute-effective-slot-definition class direct-slots))

;;;
;;; Methods having to do with generic function metaobjects.
;;;

(defmethod print-object ((gf standard-generic-function) stream)
    (print-unreadable-object (gf stream :identity t)
        (format stream "(~S) ~S"
                (class-name (class-of gf))
                (generic-function-name gf)))
    gf)

(defmethod initialize-instance :after ((gf standard-generic-function) &key)
    (finalize-generic-function gf))

;;;
;;; Methods having to do with method metaobjects.
;;;

(defmethod print-object ((method standard-method) stream)
    (print-unreadable-object (method stream :identity t)
        (format stream "(~S) ~S ~S  ~S"
                (class-name (class-of method))
                (generic-function-name
                 (method-generic-function method))
                (method-qualifiers method)
                (mapcar #'class-name
                        (method-specializers method))))
    method)

(defmethod initialize-instance :after ((method standard-method) &key)
    (setf (method-function method) (compute-method-function method)))

;;;
;;; Methods having to do with generic function invocation.
;;;

(defgeneric compute-discriminating-function (gf))
(defmethod compute-discriminating-function ((gf standard-generic-function))
    (std-compute-discriminating-function gf))

(defgeneric method-more-specific-p (gf method1 method2 required-classes))
(defmethod method-more-specific-p
    ((gf standard-generic-function) method1 method2 required-classes)
    (std-method-more-specific-p gf method1 method2 required-classes))

(defgeneric compute-effective-method-function (gf methods))
(defmethod compute-effective-method-function
    ((gf standard-generic-function) methods)
    (std-compute-effective-method-function gf methods))

(defgeneric compute-method-function (method))
(defmethod compute-method-function ((method standard-method))
    (std-compute-method-function method))

;;; describe-object is a handy tool for enquiring minds:

(defgeneric describe-object-content (obj))

(defmethod describe-object-content ((object standard-object)) (list 'cn (class-name object)))
(defmethod describe-object-content ((object list)) (list 'list 'length (length object)))
(defmethod describe-object-content ((object array)) (list 'array 'length (length object)))
(defmethod describe-object-content ((object string)) (list 'string 'length (length object) object))
(defmethod describe-object-content ((object symbol)) (list 'symbol object))
(defmethod describe-object-content ((object number)) (list 'number object))
(defmethod describe-object-content (object) "Damn know what is it")


(defgeneric describe-object (object stream))

(defmethod describe-object ((object standard-object) stream)
    (let ((value))
        (format t "A Closette object ~S ~S  representation:~%"  (class-name object)
                (class-name (class-of object)))
        (dolist (sn (mapcar #'closette::slot-definition-name (closette::class-slots (class-of object))))
            (format t "    ~a ~a <- "  sn  (if (slot-boundp object sn) "{bound}" "{unbound}"))
            (setq value (and (slot-boundp object sn) (slot-value object sn)))
            (case sn
              (closette::name
               (format t "~a~%" value))
              ((closette::superclasses closette::class-precedence-list closette::direct-subclasses)
               (format t "~a~%" (mapcar #'class-name value)))
              (closette::effective-slots
               (format t "~a~%" (describe-object-content value)))
              (closette::direct-slots
               (format t "~a~%" (mapcar #'closette::slot-definition-name  value)))
              (closette::direct-methods (describe-object-content value))
              (otherwise (format t "~a~%" (describe-object-content value)))))
        (values)))

(defmethod describe-object ((object t) stream)
    (describe object)
    (values))


;;; ancient as a mammoth shit
;;; (format t "~%Closette is a Knights of the Lambda Calculus production.")

(in-package :cl-user)

;;; EOF
