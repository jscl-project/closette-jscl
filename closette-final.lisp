;;; -*- mode:lisp; coding:utf-8 -*-

(in-package #:amop)

#+jscl (defgeneric print-object (instance stream))

#+jscl (defmethod print-object ((instance standard-object) stream)
           (print-unreadable-object (instance stream :identity t)
               (format stream "(~S)"
                       (class-name (class-of instance))))
           instance)

;;; Slot access


#+jscl (defgeneric slot-value-using-class (class instance slot-name))
#+jscl (defmethod slot-value-using-class ((class standard-class) instance slot-name)
           (std-slot-value instance slot-name))

#+jscl (defgeneric set-slot-value-using-class (instance slot-name new-value))
#+jscl (defmethod set-slot-value-using-class  ((instance standard-class) slot-name new-value)
           (setf (std-slot-value instance slot-name) new-value))

;;; N.B. To avoid making a forward reference to a (setf xxx) generic function:
#+jscl (defsetf slot-value-using-class set-slot-value-using-class)

#+jscl (defun setf-slot-value-using-class (new-value class object slot-name)
           (setf (slot-value-using-class class object slot-name) new-value))

#+jscl (defgeneric slot-exists-p-using-class (class instance slot-name))
#+jscl (defmethod slot-exists-p-using-class
           ((class standard-class) instance slot-name)
           (std-slot-exists-p instance slot-name))

#+jscl (defgeneric slot-boundp-using-class (class instance slot-name))
#+jscl (defmethod slot-boundp-using-class
           ((class standard-class) instance slot-name)
           (std-slot-boundp instance slot-name))

#+jscl (defgeneric slot-makunbound-using-class (class instance slot-name))
#+jscl (defmethod slot-makunbound-using-class
           ((class standard-class) instance slot-name)
           (std-slot-makunbound instance slot-name))

;;; Instance creation and initialization


#+jscl (defgeneric allocate-instance (class))

#+jscl (defmethod allocate-instance ((class standard-class))
           (std-allocate-instance class))

#+jscl (defgeneric make-instance (class &key))

;;;(defgeneric make-instance (class &rest))


#+jscl (defmethod make-instance ((class standard-class) &rest initargs)
           (#j:console:log "make-instance-class" (format nil "~a" (class-name class)) initargs)
           (let ((instance (allocate-instance class)))
               (apply #'initialize-instance instance initargs)
               instance))

#+jscl (defmethod make-instance ((class symbol) &rest initargs)
           (apply #'make-instance (find-class class) initargs))

#+jscl (defgeneric initialize-instance (instance &key))

#+jscl (defmethod initialize-instance ((instance standard-object) &rest initargs)
           (apply #'shared-initialize instance t initargs))

#+jscl (defgeneric reinitialize-instance (instance &key))
#+jscl (defmethod reinitialize-instance
           ((instance standard-object) &rest initargs)
           (apply #'shared-initialize instance () initargs))

#+jscl (defgeneric shared-initialize (instance slot-names &key))
#+jscl (defmethod shared-initialize ((instance standard-object) slot-names &rest all-keys)
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
#+jscl (defgeneric change-class (instance new-class &key))
#+jscl (defmethod change-class
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

#+jscl (defmethod change-class
           ((instance standard-object) (new-class symbol) &rest initargs)
           (apply #'change-class instance (find-class new-class) initargs))

#+jscl (defgeneric update-instance-for-different-class (old new &key))
#+jscl (defmethod update-instance-for-different-class
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

#+jscl (defmethod print-object ((class standard-class) stream)
           (print-unreadable-object (class stream :identity t)
               (format stream "(~S) ~S"
                       (class-name (class-of class))
                       (class-name class)))
           class)

#+jscl (defmethod initialize-instance :after ((class standard-class) &rest args)
           (apply #'std-after-initialization-for-classes class args))

;;; Finalize inheritance

#+jscl (defgeneric finalize-inheritance (class &rest))
#+jscl (defmethod finalize-inheritance ((class standard-class) &rest all-keys)
           (std-finalize-inheritance class all-keys)
           (values))

;;; Class precedence lists

#+jscl (defgeneric compute-class-precedence-list (class))
#+jscl (defmethod compute-class-precedence-list ((class standard-class))
           (std-compute-class-precedence-list class))

;;; Slot inheritance

#+jscl (defgeneric compute-slots (class))
#+jscl (defmethod compute-slots ((class standard-class))
           (std-compute-slots class))

#+jscl (defgeneric compute-effective-slot-definition (class direct-slots))
#+jscl (defmethod compute-effective-slot-definition
           ((class standard-class) direct-slots)
           (std-compute-effective-slot-definition class direct-slots))

;;;
;;; Methods having to do with generic function metaobjects.
;;;

#+jscl (defmethod print-object ((gf standard-generic-function) stream)
           (print-unreadable-object (gf stream :identity t)
               (format stream "(~S) ~S"
                       (class-name (class-of gf))
                       (generic-function-name gf)))
           gf)

#+nil (defmethod initialize-instance :after ((gf standard-generic-function) &key)
          (finalize-generic-function gf))

;;; mvk change &key to &rest args
#+jscl (defmethod initialize-instance :after ((gf standard-generic-function) &rest args)
           (finalize-generic-function gf))



;;;
;;; Methods having to do with method metaobjects.
;;;

#+jscl (defmethod print-object ((method standard-method) stream)
           (print-unreadable-object (method stream :identity t)
               (format stream "(~S) ~S ~S  ~S"
                       (class-name (class-of method))
                       (generic-function-name
                        (method-generic-function method))
                       (method-qualifiers method)
                       (mapcar #'class-name
                               (method-specializers method))))
           method)

;;; mvk add &rest
#+jscl (defmethod initialize-instance :after ((method standard-method) &rest args)
           (setf (method-function method) (compute-method-function method)))

;;;
;;; Methods having to do with generic function invocation.
;;;

#+jscl (defgeneric compute-discriminating-function (gf))
#+jscl (defmethod compute-discriminating-function ((gf standard-generic-function))
           (std-compute-discriminating-function gf))

#+jscl (defgeneric method-more-specific-p (gf method1 method2 required-classes))
#+jscl (defmethod method-more-specific-p
           ((gf standard-generic-function) method1 method2 required-classes)
           (std-method-more-specific-p gf method1 method2 required-classes))

#+jscl (defgeneric compute-effective-method-function (gf methods))
#+jscl (defmethod compute-effective-method-function
           ((gf standard-generic-function) methods)
           (std-compute-effective-method-function gf methods))

#+jscl (defgeneric compute-method-function (method))
#+jscl (defmethod compute-method-function ((method standard-method))
           (std-compute-method-function method))

;;; describe-object is a handy tool for enquiring minds:

#+jscl (defgeneric describe-object-content (obj))

#+jscl (defmethod describe-object-content ((object standard-object)) (list 'cn (class-name object)))
#+jscl (defmethod describe-object-content ((object list)) (list 'list 'length (length object)))
#+jscl (defmethod describe-object-content ((object array)) (list 'array 'length (length object)))
#+jscl (defmethod describe-object-content ((object string)) (list 'string 'length (length object) object))
#+jscl (defmethod describe-object-content ((object symbol)) (list 'symbol object))
#+jscl (defmethod describe-object-content ((object number)) (list 'number object))
#+jscl (defmethod describe-object-content (object) "Damn know what is it")


#+jscl (defgeneric describe-object (object stream))

#+jscl (defmethod describe-object ((object standard-object) stream)
           (let ((value))
               (format t "A Closette object ~S ~S  representation:~%"  (class-name object)
                       (class-name (class-of object)))
               (dolist (sn (mapcar #'slot-definition-name (class-slots (class-of object))))
                   (format t "    ~a ~a <- "  sn  (if (slot-boundp object sn) "{bound}" "{unbound}"))
                   (setq value (and (slot-boundp object sn) (slot-value object sn)))
                   (case sn
                     (name
                      (format t "~a~%" value))
                     ((superclasses class-precedence-list direct-subclasses)
                      (format t "~a~%" (mapcar #'class-name value)))
                     (effective-slots
                      (format t "~a~%" (describe-object-content value)))
                     (direct-slots
                      (format t "~a~%" (mapcar #'clos:slot-definition-name  value)))
                     (direct-methods (describe-object-content value))
                     (otherwise (format t "~a~%" (describe-object-content value)))))
               (values)))

#+jscl (defmethod describe-object ((object t) stream)
           (describe object)
           (values))


;;; ancient as a mammoth shit
;;; (format t "~%Closette is a Knights of the Lambda Calculus production.")


(in-package :cl-user)

;;; EOF
