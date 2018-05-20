;;; -*- mode:lisp; coding:utf-8 -*-


;;;
;;; Closette Version 1.0 (February 10, 1991)
;;;
;;; Minor revisions of September 27, 1991 by desRivieres@parc.xerox.com:
;;;   - remove spurious "&key" in header of initialize-instance method
;;;     for standard-class (bottom of pg.310 of AMOP)
;;;   - add recommendation about not compiling this file
;;;   - change comment to reflect PARC ftp server name change
;;;   - add a BOA constructor to std-instance to please AKCL
;;;   - by default, efunctuate methods rather than compile them
;;;   - also make minor changes to newcl.lisp
;;;
;;; Copyright (c) 1990, 1991 Xerox Corporation.
;;; All rights reserved.
;;;
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted.  Any distribution of this
;;; software or derivative works must comply with all applicable United
;;; States export control laws.
;;;
;;; This software is made available AS IS, and Xerox Corporation makes no
;;; warranty about the software, its performance or its conformity to any
;;; specification.
;;;
;;;
;;; Closette is an implementation of a subset of CLOS with a metaobject
;;; protocol as described in "The Art of The Metaobject Protocol",
;;; MIT Press, 1991.
;;;
;;; This program is available by anonymous FTP, from the /pub/pcl/mop
;;; directory on parcftp.xerox.com.
;;; This is the file closette.lisp

(in-package :closette)

;;; Some modification for JSCL
;;; mvk. May, 2018
;;;


;;;
;;; Standard instances
;;;

;;; This implementation uses structures for instances, because they're the only
;;; kind of Lisp object that can be easily made to print whatever way we want.


(jscl::def!struct (std-instance (:constructor allocate-std-instance (class slots))
                                (:predicate std-instance-p))
    class
    slots)


;;; :todo: print-object

(defun print-std-instance (instance stream depth)
    (declare (ignore depth))
    (error "TBD print-object")
    (print-object instance stream))

;;; Standard instance allocation

#+nil (defparameter secret-unbound-value (list "slot unbound"))
(defparameter *secret-unbound-value* (list "slot unbound"))

(defun instance-slot-p (slot)
    (eql (slot-definition-allocation slot) ':instance))


;;; :note: lambda!!!
(defun std-allocate-instance (class)
    (allocate-std-instance
     :class (lambda () class)
     :slots (allocate-slot-storage (count-if #'instance-slot-p (class-slots class))
                                   *secret-unbound-value*)))

;;; Simple vectors are used for slot storage.

(defun allocate-slot-storage (size initial-value)
    (make-array size :initial-element initial-value))

;;; Standard instance slot access

;;; N.B. The location of the effective-slots slots in the class metaobject for
;;; standard-class must be determined without making any further slot
;;; references.

;;; todo: to global vars
#+nil (defvar the-slots-of-standard-class) ;standard-class's class-slots
#+nil (defvar the-class-standard-class)    ;standard-class's class metaobject

(defvar *the-slots-of-standard-class*) ;standard-class's class-slots
(defvar *the-class-standard-class*)    ;standard-class's class metaobject


(defun slot-location (class slot-name)
    (if (and (eq slot-name 'effective-slots)
             (eq class *the-class-standard-class*))
        (position 'effective-slots *the-slots-of-standard-class*
                  :key #'slot-definition-name)
        (let ((slot (find slot-name
                          (class-slots class)
                          :key #'slot-definition-name)))
            (if (null slot)
                (error "The slot ~S is missing from the class ~S."
                       slot-name class)
                (let ((pos (position slot
                                     (remove-if-not #'instance-slot-p
                                                    (class-slots class)))))
                    (if (null pos)
                        (error "The slot ~S is not an instance~@
                           slot in the class ~S."
                               slot-name class)
                        pos))))))

;;; slot-contents


(defun slot-contents (slots location)
    (aref slots location))

#|
(defun setf-slot-contents (slots location new-value)
    (setf (aref slots location) new-value))

(defsetf slot-contents setf-slot-contents)
|#

(defun* (setf slot-contents) (slots location new-value)
    (setf (aref slots location) new-value))




;;; std-slot-value
(defun std-slot-value (instance slot-name)
    (let* ((location (slot-location (class-of instance) slot-name))
           (slots (std-instance-slots instance))
           (val (slot-contents slots location)))
        (if (equal *secret-unbound-value* val)
            (error "The slot ~S is unbound in the object ~S."
                   slot-name instance)
            val)))
#|
(defun setf-std-slot-value (instance slot-name new-value)
    (let ((location (slot-location (class-of instance) slot-name))
          (slots (std-instance-slots instance)))
        (setf (slot-contents slots location) new-value)))

(defsetf std-slot-value setf-std-slot-value)
|#


(defun* (setf std-slot-value) (instance slot-name new-value)
    (let ((location (slot-location (class-of instance) slot-name))
          (slots (std-instance-slots instance)))
        (setf (slot-contents slots location) new-value)))


;;; slot-value

(defun slot-value (object slot-name)
    (if (eq (class-of (class-of object)) *the-class-standard-class*)
        (std-slot-value object slot-name)
        (slot-value-using-class (class-of object) object slot-name)))

#|
(defun setf-slot-value (object slot-name new-value)
    (if (eq (class-of (class-of object)) the-class-standard-class)
        (setf (std-slot-value object slot-name) new-value)
        (setf-slot-value-using-class
         new-value (class-of object) object slot-name)))


(defsetf slot-value setf-slot-value)
|#

(defun* (setf slot-value) (object slot-name new-value)
    (if (eq (class-of (class-of object)) *the-class-standard-class*)
        (setf (std-slot-value object slot-name) new-value)
        (setf-slot-value-using-class
         new-value (class-of object) object slot-name)))


;;; std-slot-boundp
(defun std-slot-boundp (instance slot-name)
    (let ((location (slot-location (class-of instance) slot-name))
          (slots (std-instance-slots instance)))
        (not (equal *secret-unbound-value* (slot-contents slots location)))))

(defun slot-boundp (object slot-name)
    (if (eq (class-of (class-of object)) *the-class-standard-class*)
        (std-slot-boundp object slot-name)
        (slot-boundp-using-class (class-of object) object slot-name)))

;;; std-slot-makunbound
(defun std-slot-makunbound (instance slot-name)
    (let ((location (slot-location (class-of instance) slot-name))
          (slots (std-instance-slots instance)))
        (setf (slot-contents slots location) *secret-unbound-value*))
    instance)

(defun slot-makunbound (object slot-name)
    (if (eq (class-of (class-of object)) *the-class-standard-class*)
        (std-slot-makunbound object slot-name)
        (slot-makunbound-using-class (class-of object) object slot-name)))

;;; std-slot-exists-p
(defun std-slot-exists-p (instance slot-name)
    (not (null (find slot-name (class-slots (class-of instance))
                     :key #'slot-definition-name))))

(defun slot-exists-p (object slot-name)
    (if (eq (class-of (class-of object)) *the-class-standard-class*)
        (std-slot-exists-p object slot-name)
        (slot-exists-p-using-class (class-of object) object slot-name)))


;;; class-of

;;; note: lambda!!!
(defun class-of (x)
    (if (std-instance-p x)
        (funcall (std-instance-class x))
        (built-in-class-of x)))


(defun built-in-class-of (x)
    (typecase x
      ;;(null                                          (find-class 'null))
      (symbol                       (find-class 'symbol))
      ;;((complex *)                                   (find-class 'complex))
      (integer                                  (find-class 'integer))
      (float                                   (find-class 'float))
      (cons                                          (find-class 'cons))
      (character                                     (find-class 'character))
      ;;(hash-table                                    (find-class 'hash-table))
      (package                                       (find-class 'package))
      ;;(pathname                                      (find-class 'pathname))
      ;;(readtable                                     (find-class 'readtable))
      ;;(stream                                        (find-class 'stream))
      ;;(number (find-class 'number))
      (string                                    (find-class 'string))
      ;;((bit-vector *)                                (find-class 'bit-vector))
      (vector  (find-class 'vector))
      (array                (find-class 'array))
      (sequence         (find-class 'sequence))
      (function                                      (find-class 'function))
      (t                                             (find-class 't))))



;;; subclassp and sub-specializer-p

(defun subclassp (c1 c2)
    (not (null (find c2 (class-precedence-list c1)))))

(defun sub-specializer-p (c1 c2 c-arg)
    (let ((cpl (class-precedence-list c-arg)))
        (not (null (find c2 (cdr (member c1 cpl)))))))

;;;
;;; Class metaobjects and standard-class
;;;

;;; todo: to global vars

(defparameter *the-defclass-standard-class*  ;standard-class's defclass form
  '(defclass standard-class ()
    ((name :initarg :name)              ; :accessor class-name
     (direct-superclasses               ; :accessor class-direct-superclasses
      :initarg :direct-superclasses)
     (direct-slots)                     ; :accessor class-direct-slots
     (class-precedence-list)            ; :accessor class-precedence-list
     (effective-slots)                  ; :accessor class-slots
     (direct-subclasses :initform ())   ; :accessor class-direct-subclasses
     (direct-methods :initform ()))))   ; :accessor class-direct-methods

;;; Defining the metaobject slot accessor function as regular functions
;;; greatly simplifies the implementation without removing functionality.


;;; class-name
(defun class-name (class) (std-slot-value class 'name))

#|
(defun setf-class-name (class new-value)
    (setf (slot-value class 'name) new-value))
(defsetf class-name setf-class-name)
|#

(defun* (setf class-name) (class new-value)
    (setf (slot-value class 'name) new-value))



;;; class-direct-superclasses
(defun class-direct-superclasses (class)
    (slot-value class 'direct-superclasses))
#+nil (defun setf-class-direct-superclasses (class new-value)
          (setf (slot-value class 'direct-superclasses) new-value))
#+nil (defsetf class-direct-superclasses setf-class-direct-superclasses)

(defun* (setf class-direct-superclasses) (class new-value)
    (setf (slot-value class 'direct-superclasses) new-value))


;;; class-direct-slots
(defun class-direct-slots (class)
    (slot-value class 'direct-slots))
#+nil (defun setf-class-direct-slots (class new-value)
          (setf (slot-value class 'direct-slots) new-value))
#+nil (defsetf class-direct-slots setf-class-direct-slots)

(defun* (setf class-direct-slots) (class new-value)
    (setf (slot-value class 'direct-slots) new-value))


;;; class-precedence-list
(defun class-precedence-list (class)
    (slot-value class 'class-precedence-list))
#+nil (defun setf-class-precedence-list (class new-value)
          (setf (slot-value class 'class-precedence-list) new-value))
#+nil (defsetf class-precedence-list setf-class-precedence-list)

(defun* (setf class-precedence-list) (class new-value)
    (setf (slot-value class 'class-precedence-list) new-value))



;;; class-slots
(defun class-slots (class)
    (slot-value class 'effective-slots))
#+nil (defun setf-class-slots (class new-value)
          (setf (slot-value class 'effective-slots) new-value))
#+nil (defsetf class-slots setf-class-slots)

(defun* (setf class-slots) (class new-value)
    (setf (slot-value class 'effective-slots) new-value))


;;; class-direct-subclasses
(defun class-direct-subclasses (class)
    (slot-value class 'direct-subclasses))
#+nil (defun setf-class-direct-subclasses (class new-value)
          (setf (slot-value class 'direct-subclasses) new-value))
#+nil (defsetf class-direct-subclasses setf-class-direct-subclasses)

(defun* (setf class-direct-subclasses) (class new-value)
    (setf (slot-value class 'direct-subclasses) new-value))



;;; class-direct-methods
(defun class-direct-methods (class)
    (slot-value class 'direct-methods))
#+nil (defun setf-class-direct-methods (class new-value)
          (setf (slot-value class 'direct-methods) new-value))
#+nil (defsetf class-direct-methods setf-class-direct-methods)

(defun* (setf class-direct-methods) (class new-value)
    (setf (slot-value class 'direct-methods) new-value))



;;; defclass

(defmacro defclass (name direct-superclasses direct-slots &rest options)
    `(ensure-class ',name
                   :direct-superclasses
                   ,(canonicalize-direct-superclasses direct-superclasses)
                   :direct-slots
                   ,(canonicalize-direct-slots direct-slots)
                   ,@(canonicalize-defclass-options options)))

(defun canonicalize-direct-superclasses (direct-superclasses)
    (if direct-superclasses
        `(list ,@(mapcar #'canonicalize-direct-superclass direct-superclasses))
        ()))

(defun canonicalize-direct-superclass (class-name)
    `(find-class ',class-name))

(defun canonicalize-defclass-options (options)
    (mapappend #'canonicalize-defclass-option options))

(defun canonicalize-direct-slots (direct-slots)
    (if direct-slots
        `(list ,@(mapcar #'canonicalize-direct-slot direct-slots))
        ()))

(defun canonicalize-direct-slot (spec)
    (if (symbolp spec)
        `(list :name ',spec)
        (let ((name (car spec))
              (initfunction nil)
              (initform nil)
              (initargs ())
              (readers ())
              (writers ())
              (other-options ()))
            (do ((olist (cdr spec) (cddr olist)))
                ((null olist))
                (case (car olist)
                  (:initform
                   (setq initfunction
                         `(function (lambda () ,(cadr olist))))
                   (setq initform `',(cadr olist)))
                  (:initarg
                   (push-on-end (cadr olist) initargs))
                  (:reader
                   (push-on-end (cadr olist) readers))
                  (:writer
                   (push-on-end (cadr olist) writers))
                  (:accessor
                   ;; accessor reader fn
                   (push-on-end (cadr olist) readers)
                   ;; accessor writer fn
                   ;; make (setf name) symbolic name
                   (push-on-end `(setf ,(cadr olist)) writers))
                  (otherwise
                   (push-on-end `',(car olist) other-options)
                   (push-on-end `',(cadr olist) other-options))))
            `(list
              :name ',name
              ,@(when initfunction
                    `(:initform ,initform
                      :initfunction ,initfunction))
              ,@(when initargs `(:initargs ',initargs))
              ,@(when readers `(:readers ',readers))
              ,@(when writers `(:writers ',writers))
              ,@other-options))))

(defun canonicalize-defclass-option (option)
    (case (car option)
      (:metaclass
       (list ':metaclass
             `(find-class ',(cadr option))))
      (:default-initargs
       (list
        ':direct-default-initargs
        `(list ,@(mapappend
                  #'(lambda (x) x)
                  (mapplist
                   #'(lambda (key value)
                         `(',key ,value))
                   (cdr option))))))
      (t (list `',(car option) `',(cadr option)))))

;;; find-class

(defparameter *class-table* (make-hash-table :test #'eq))

(defun find-class (symbol &optional (errorp t))
    (let ((class (gethash symbol *class-table* nil)))
        (if (and (null class) errorp)
            (error "No class named ~S." symbol)
            class)))

(defun setf-find-class (symbol new-value)
    (setf (gethash symbol *class-table*) new-value))

#+nil (defsetf find-class setf-find-class)

(defun forget-all-classes ()
    (setf *class-table* (make-hash-table :test #'eq))
    (values))

(defun* (setf find-class) (symbol new-value)
    (setf (gethash symbol *class-table*) new-value))


;;; Ensure class

;;;
(defun get-keyword-from (args key &optional default)
    (let ((val (getf args key)))
        (if val val default)))


(defun ensure-class (name &rest all-keys)
    (if (find-class name nil)
        (error "Can't redefine the class named ~S." name)
        (let* ((metaclass (get-keyword-from all-keys :metaclass *the-class-standard-class*))
               (class (apply (if (eq metaclass *the-class-standard-class*)
                                 'make-instance-standard-class
                                 'make-instance)
                             metaclass :name name all-keys)))
            (setf (find-class name) class)
            class)))


;;; make-instance-standard-class creates and initializes an instance of
;;; standard-class without falling into method lookup.  However, it cannot be
;;; called until standard-class itself exists.

(defun make-instance-standard-class
    (metaclass &key name direct-superclasses direct-slots
     &allow-other-keys)
    (declare (ignore metaclass ))
    (let ((class (std-allocate-instance *the-class-standard-class*)))
        (setf (class-name class) name)
        (setf (class-direct-subclasses class) ())
        (setf (class-direct-methods class) ())
        (std-after-initialization-for-classes class
                                              :direct-slots direct-slots
                                              :direct-superclasses direct-superclasses)
        class))

(defun std-after-initialization-for-classes
    (class &key direct-superclasses direct-slots &allow-other-keys)
    (let ((supers
            (or direct-superclasses
                (list (find-class 'standard-object)))))
        (setf (class-direct-superclasses class) supers)
        (dolist (superclass supers)
            (push class (class-direct-subclasses superclass))))
    (let ((slots
            (mapcar #'(lambda (slot-properties)
                          (apply #'make-direct-slot-definition slot-properties))
                    direct-slots)))
        (setf (class-direct-slots class) slots)
        (dolist (direct-slot slots)
            (dolist (reader (slot-definition-readers direct-slot))
                (add-reader-method
                 class reader (slot-definition-name direct-slot)))
            (dolist (writer (slot-definition-writers direct-slot))
                (add-writer-method
                 class writer (slot-definition-name direct-slot)))))
    (funcall (if (eq (class-of class) *the-class-standard-class*)
                 #'std-finalize-inheritance
                 #'finalize-inheritance)
             class)
    (values))

;;; Slot definition metaobjects

;;; N.B. Quietly retain all unknown slot options (rather than signaling an
;;; error), so that it's easy to add new ones.

(defun make-direct-slot-definition
    (&rest properties
     &key name (initargs ()) (initform nil) (initfunction nil)
       (readers ()) (writers ()) (allocation :instance)
     &allow-other-keys)
    (let ((slot (copy-list properties))) ; Don't want to side effect &rest list
        (setf (getf* slot ':name) name)
        (setf (getf* slot ':initargs) initargs)
        (setf (getf* slot ':initform) initform)
        (setf (getf* slot ':initfunction) initfunction)
        (setf (getf* slot ':readers) readers)
        (setf (getf* slot ':writers) writers)
        (setf (getf* slot ':allocation) allocation)
        slot))

(defun make-effective-slot-definition
    (&rest properties
     &key name (initargs ()) (initform nil) (initfunction nil)
       (allocation :instance)
     &allow-other-keys)
    (let ((slot (copy-list properties)))  ; Don't want to side effect &rest list
        (setf (getf* slot ':name) name)
        (setf (getf* slot ':initargs) initargs)
        (setf (getf* slot ':initform) initform)
        (setf (getf* slot ':initfunction) initfunction)
        (setf (getf* slot ':allocation) allocation)
        slot))


;;; slot-definition-name
(defun slot-definition-name (slot)
    (getf slot ':name))
#+nil (defun setf-slot-definition-name (slot new-value)
          (setf (getf* slot ':name) new-value))
#+nil (defsetf slot-definition-name setf-slot-definition-name)

(defun* (setf slot-definition-name) (slot new-value)
    (setf (getf* slot ':name) new-value))


;;; slot-definition-initfunction
(defun slot-definition-initfunction (slot)
    (getf slot ':initfunction))
#+nil (defun setf-slot-definition-initfunction (slot new-value)
          (setf (getf* slot ':initfunction) new-value))
#+nil (defsetf slot-definition-initfunction setf-slot-definition-initfunction)

(defun* (setf slot-definition-initfunction) (slot new-value)
    (setf (getf* slot ':initfunction) new-value))



;;; slot-definition-initform
(defun slot-definition-initform (slot)
    (getf slot ':initform))
#+nil (defun setf-slot-definition-initform (slot new-value)
          (setf (getf* slot ':initform) new-value))
#+nil (defsetf slot-definition-initform setf-slot-definition-initform)

(defun* (setf slot-definition-initform) (slot new-value)
    (setf (getf* slot ':initform) new-value))


;;; slot-definition-initargs
(defun slot-definition-initargs (slot)
    (getf slot ':initargs))
#+nil (defun setf-slot-definition-initargs (slot new-value)
          (setf (getf* slot ':initargs) new-value))
#+nil (defsetf slot-definition-initargs setf-slot-definition-initargs)

(defun* (setf slot-definition-initargs) (slot new-value)
    (setf (getf* slot ':initargs) new-value))


;;; slot-definition-readers
(defun slot-definition-readers (slot)
    (getf slot ':readers))
#+nil (defun setf-slot-definition-readers (slot new-value)
          (setf (getf* slot ':readers) new-value))
#+nil (defsetf slot-definition-readers setf-slot-definition-readers)

(defun* (setf slot-definition-readers) (slot new-value)
    (setf (getf* slot ':readers) new-value))


;;; slot-definition-writers
(defun slot-definition-writers (slot)
    (getf slot ':writers))
#+nil (defun setf-slot-definition-writers (slot new-value)
          (setf (getf* slot ':writers) new-value))
#+nil (defsetf slot-definition-writers setf-slot-definition-writers)

(defun* (setf slot-definition-writers) (slot new-value)
    (setf (getf* slot ':writers) new-value))



;;; slot-definition-allocation
(defun slot-definition-allocation (slot)
    (getf slot ':allocation))
#+nil (defun setf-slot-definition-allocation (slot new-value)
          (setf (getf* slot ':allocation) new-value))
#+nil (defsetf slot-definition-allocation setf-slot-definition-allocation)

(defun* (setf slot-definition-allocation) (slot new-value)
    (setf (getf* slot ':allocation) new-value))


;;; finalize-inheritance
(defun std-finalize-inheritance (class)
    (setf (class-precedence-list class)
          (funcall (if (eq (class-of class) *the-class-standard-class*)
                       #'std-compute-class-precedence-list
                       #'compute-class-precedence-list)
                   class))
    (setf (class-slots class)
          (funcall (if (eq (class-of class) *the-class-standard-class*)
                       #'std-compute-slots
                       #'compute-slots)
                   class))
    (values))

;;; Class precedence lists

(defun std-compute-class-precedence-list (class)
    (let ((classes-to-order (collect-superclasses* class)))
        (topological-sort classes-to-order
                          (remove-duplicates
                           (mapappend #'local-precedence-ordering
                                      classes-to-order))
                          #'std-tie-breaker-rule)))

;;; topological-sort implements the standard algorithm for topologically
;;; sorting an arbitrary set of elements while honoring the precedence
;;; constraints given by a set of (X,Y) pairs that indicate that element
;;; X must precede element Y.  The tie-breaker procedure is called when it
;;; is necessary to choose from multiple minimal elements; both a list of
;;; candidates and the ordering so far are provided as arguments.
(defun topological-sort (elements constraints tie-breaker)
    (let ((remaining-constraints constraints)
          (remaining-elements elements)
          (result ()))
        (loop
          (let ((minimal-elements
                  (remove-if
                   #'(lambda (class)
                         (member class remaining-constraints
                                 :key #'cadr))
                   remaining-elements)))
              (when (null minimal-elements)
                  (if (null remaining-elements)
                      (return-from topological-sort result)
                      (error "Inconsistent precedence graph.")))
              (let ((choice (if (null (cdr minimal-elements))
                                (car minimal-elements)
                                (funcall tie-breaker
                                         minimal-elements
                                         result))))
                  (setq result (append result (list choice)))
                  (setq remaining-elements
                        (remove choice remaining-elements))
                  (setq remaining-constraints
                        (remove choice
                                remaining-constraints
                                :test #'member)))))))

;;; In the event of a tie while topologically sorting class precedence lists,
;;; the CLOS Specification says to "select the one that has a direct subclass
;;; rightmost in the class precedence list computed so far."  The same result
;;; is obtained by inspecting the partially constructed class precedence list
;;; from right to left, looking for the first minimal element to show up among
;;; the direct superclasses of the class precedence list constituent.
;;; (There's a lemma that shows that this rule yields a unique result.)

(defun std-tie-breaker-rule (minimal-elements cpl-so-far)
    (dolist (cpl-constituent (reverse cpl-so-far))
        (let* ((supers (class-direct-superclasses cpl-constituent))
               (common (intersection minimal-elements supers)))
            (when (not (null common))
                (return-from std-tie-breaker-rule (car common))))))

;;; This version of collect-superclasses* isn't bothered by cycles in the class
;;; hierarchy, which sometimes happen by accident.

(defun collect-superclasses* (class)
    (labels ((all-superclasses-loop (seen superclasses)
                 (let ((to-be-processed
                         (set-difference superclasses seen)))
                     (if (null to-be-processed)
                         superclasses
                         (let ((class-to-process
                                 (car to-be-processed)))
                             (all-superclasses-loop
                              (cons class-to-process seen)
                              (union (class-direct-superclasses
                                      class-to-process)
                                     superclasses)))))))
        (all-superclasses-loop () (list class))))

;;; The local precedence ordering of a class C with direct superclasses C_1,
;;; C_2, ..., C_n is the set ((C C_1) (C_1 C_2) ...(C_n-1 C_n)).

(defun local-precedence-ordering (class)
    (mapcar #'list
            (cons class
                  (butlast (class-direct-superclasses class)))
            (class-direct-superclasses class)))

;;; Slot inheritance
(defun std-compute-slots (class)
    (let* ((all-slots (mapappend #'class-direct-slots
                                 (class-precedence-list class)))
           (all-names (remove-duplicates
                       (mapcar #'slot-definition-name all-slots))))
        (mapcar #'(lambda (name)
                      (funcall
                       (if (eq (class-of class) *the-class-standard-class*)
                           #'std-compute-effective-slot-definition
                           #'compute-effective-slot-definition)
                       class
                       (remove name all-slots
                               :key #'slot-definition-name
                               :test-not #'eq)))
                all-names)))

(defun std-compute-effective-slot-definition (class direct-slots)
    (declare (ignore class))
    (let ((initer (find-if-not #'null direct-slots
                               :key #'slot-definition-initfunction)))
        (make-effective-slot-definition
         :name (slot-definition-name (car direct-slots))
         :initform (if initer
                       (slot-definition-initform initer)
                       nil)
         :initfunction (if initer
                           (slot-definition-initfunction initer)
                           nil)
         :initargs (remove-duplicates
                    (mapappend #'slot-definition-initargs
                               direct-slots))
         :allocation (slot-definition-allocation (car direct-slots)))))

;;;
;;; Generic function metaobjects and standard-generic-function
;;;

#+nil (defparameter the-defclass-standard-generic-function
        '(defclass standard-generic-function ()
          ((name :initarg :name)      ; :accessor generic-function-name
           (lambda-list               ; :accessor generic-function-lambda-list
            :initarg :lambda-list)
           (methods :initform ())     ; :accessor generic-function-methods
           (method-class              ; :accessor generic-function-method-class
            :initarg :method-class)
           (discriminating-function)  ; :accessor generic-function-
                                        ;    -discriminating-function
           (classes-to-emf-table      ; :accessor classes-to-emf-table
            :initform (make-hash-table :test #'eq)))))

(defparameter *the-defclass-standard-generic-function*
  '(defclass standard-generic-function ()
    ((name :initarg :name)      ; :accessor generic-function-name
     (lambda-list               ; :accessor generic-function-lambda-list
      :initarg :lambda-list)
     (methods :initform ())     ; :accessor generic-function-methods
     (method-class              ; :accessor generic-function-method-class
      :initarg :method-class)
     (discriminating-function)  ; :accessor generic-function-
                                        ;    -discriminating-function
     (classes-to-emf-table      ; :accessor classes-to-emf-table
      :initform (make-hash-table :test #'eq)))))


#+nil (defvar the-class-standard-gf) ;standard-generic-function's class metaobject
(defvar *the-class-standard-gf*) ;standard-generic-function's class metaobject


;;; generic-function-name
(defun generic-function-name (gf)
    (slot-value gf 'name))
#|
(defun setf-generic-function-name (gf new-value)
    (setf (slot-value gf 'name) new-value))
(defsetf generic-function-name setf-generic-function-name)
|#

(defun* (setf generic-function-name) (gf new-value)
    (setf (slot-value gf 'name) new-value))




;;; generic-function-lambda-list
(defun generic-function-lambda-list (gf)
    (slot-value gf 'lambda-list))

#+nil (defun setf-generic-function-lambda-list (gf new-value)
          (setf (slot-value gf 'lambda-list) new-value))
#+nil (defsetf generic-function-lambda-list setf-generic-function-lambda-list)

(defun* (setf generic-function-lambda-list) (gf new-value)
    (setf (slot-value gf 'lambda-list) new-value))



;;; generic-function-methods
(defun generic-function-methods (gf)
    (slot-value gf 'methods))
#+nil (defun setf-generic-function-methods (gf new-value)
          (setf (slot-value gf 'methods) new-value))
#+nil (defsetf generic-function-methods setf-generic-function-methods)

(defun* (setf generic-function-methods) (gf new-value)
    (setf (slot-value gf 'methods) new-value))



;;; generic-function-discriminating-function
(defun generic-function-discriminating-function (gf)
    (slot-value gf 'discriminating-function))
#+nil (defun setf-generic-function-discriminating-function (gf new-value)
          (setf (slot-value gf 'discriminating-function) new-value))
#+nil (defsetf generic-function-discriminating-function setf-generic-function-discriminating-function)

(defun* (setf generic-function-discriminating-function) (gf new-value)
    (setf (slot-value gf 'discriminating-function) new-value))


;;; generic-function-method-class
(defun generic-function-method-class (gf)
    (slot-value gf 'method-class))
#+nil (defun setf-generic-function-method-class (gf new-value)
          (setf (slot-value gf 'method-class) new-value))
#+nil (defsetf generic-function-method-class setf-generic-function-method-class)

(defun* (setf generic-function-method-class) (gf new-value)
    (setf (slot-value gf 'method-class) new-value))



;;; Internal accessor for effective method function table

(defun classes-to-emf-table (gf)
    (slot-value gf 'classes-to-emf-table))
#+nil (defun setf-classes-to-emf-table (gf new-value)
          (setf (slot-value gf 'classes-to-emf-table) new-value))
#+nil (defsetf classes-to-emf-table setf-classes-to-emf-table)

(defun* (setf classes-to-emf-table) (gf new-value)
    (setf (slot-value gf 'classes-to-emf-table) new-value))



;;;
;;; Method metaobjects and standard-method
;;;

#+nil (defparameter the-defclass-standard-method
        '(defclass standard-method ()
          ((lambda-list :initarg :lambda-list)     ; :accessor method-lambda-list
           (qualifiers :initarg :qualifiers)       ; :accessor method-qualifiers
           (specializers :initarg :specializers)   ; :accessor method-specializers
           (body :initarg :body)                   ; :accessor method-body
           (environment :initarg :environment)     ; :accessor method-environment
           (generic-function :initform nil)        ; :accessor method-generic-function
           (function))))                           ; :accessor method-function

(defparameter *the-defclass-standard-method*
  '(defclass standard-method ()
    ((lambda-list :initarg :lambda-list)     ; :accessor method-lambda-list
     (qualifiers :initarg :qualifiers)       ; :accessor method-qualifiers
     (specializers :initarg :specializers)   ; :accessor method-specializers
     (body :initarg :body)                   ; :accessor method-body
     (environment :initarg :environment)     ; :accessor method-environment
     (generic-function :initform nil)        ; :accessor method-generic-function
     (function))))                           ; :accessor method-function



#+nil (defvar the-class-standard-method)    ;standard-method's class metaobject
(defvar *the-class-standard-method*)    ;standard-method's class metaobject


;;; method-lambda-list
(defun method-lambda-list (method) (slot-value method 'lambda-list))
#+nil (defun setf-method-lambda-list (method new-value)
          (setf (slot-value method 'lambda-list) new-value))
#+nil (defsetf method-lambda-list setf-method-lambda-list)

(defun* (setf method-lambda-list) (method new-value)
    (setf (slot-value method 'lambda-list) new-value))




;;; method-qualifiers
(defun method-qualifiers (method) (slot-value method 'qualifiers))
#+nil (defun setf-method-qualifiers (method new-value)
          (setf (slot-value method 'qualifiers) new-value))
#+nil (defsetf method-qualifiers setf-method-qualifiers)

(defun* (setf method-qualifiers) (method new-value)
    (setf (slot-value method 'qualifiers) new-value))


;;; method-specializers
(defun method-specializers (method) (slot-value method 'specializers))
#+nil (defun setf-method-specializers (method new-value)
          (setf (slot-value method 'specializers) new-value))
#+nil (defsetf method-specializers setf-method-specializers)

(defun* (setf method-specializers) (method new-value)
    (setf (slot-value method 'specializers) new-value))



;;; method-body
(defun method-body (method) (slot-value method 'body))
#+nil (defun setf-method-body (method new-value)
          (setf (slot-value method 'body) new-value))
#+nil (defsetf method-body setf-method-body)

(defun* (setf method-body) (method new-value)
    (setf (slot-value method 'body) new-value))




;;; method-environment
(defun method-environment (method) (slot-value method 'environment))
#+nil (defun setf-method-environment (method new-value)
          (setf (slot-value method 'environment) new-value))
#+nil (defsetf method-environment setf-method-environment)

(defun* (setf method-environment) (method new-value)
    (setf (slot-value method 'environment) new-value))


;;; method-generic-function
(defun method-generic-function (method)
    (slot-value method 'generic-function))
#+nil (defun setf-method-generic-function (method new-value)
          (setf (slot-value method 'generic-function) new-value))
#+nil (defsetf method-generic-function setf-method-generic-function)

(defun* (setf method-generic-function) (method new-value)
    (setf (slot-value method 'generic-function) new-value))



;;; method-function
(defun method-function (method) (slot-value method 'function))
#+nil (defun setf-method-function (method new-value)
          (setf (slot-value method 'function) new-value))
#+nil (defsetf method-function setf-method-function)

(defun* (setf method-function) (method new-value)
    (setf (slot-value method 'function) new-value))



;;; defgeneric
(defmacro defgeneric (function-name lambda-list &rest options)
    `(prog1 ',function-name
         (ensure-generic-function
          ',function-name
          :lambda-list ,(canonicalize-defgeneric-ll lambda-list)
          ,@(canonicalize-defgeneric-options options))))

(defun canonicalize-defgeneric-ll (lst)
    (if lst
        `',lst
        '()))

(defun canonicalize-defgeneric-options (options)
    (mapappend #'canonicalize-defgeneric-option options))

(defun canonicalize-defgeneric-option (option)
    (case (car option)
      (:generic-function-class
       (list ':generic-function-class
             `(find-class ',(cadr option))))
      (:method-class
       (list ':method-class
             `(find-class ',(cadr option))))
      (t (list `',(car option) `',(cadr option)))))

;;; find-generic-function looks up a generic function by name.  It's an
;;; artifact of the fact that our generic function metaobjects can't legally
;;; be stored a symbol's function value.

(defparameter *generic-function-table* (make-hash-table :test #'eq))

(defun find-generic-function (symbol &optional (errorp t))
    (let ((gf (gethash symbol *generic-function-table* nil)))
        (if (and (null gf) errorp)
            (error "No generic function named ~S." symbol)
            gf)))

#+nil (defun setf-find-generic-function (symbol new-value)
          (setf (gethash symbol *generic-function-table*) new-value))
#+nil (defsetf find-generic-function setf-find-generic-function)

(defun* (setf find-generic-function) (symbol new-value)
    (setf (gethash symbol *generic-function-table*) new-value))

(defun forget-all-generic-functions ()
    (setq *generic-function-table* (make-hash-table :test #'eq))
    (values))


;;; ensure-generic-function
(defun ensure-generic-function (function-name &rest all-keys)
    ;;(print (list 'ensure-generic-function function-name ))
    (if (find-generic-function function-name nil)
        (find-generic-function function-name)
        (let*
            ((generic-function-class (get-keyword-from all-keys :generic-function-class *the-class-standard-gf*))
             (method-class (get-keyword-from all-keys :method-class *the-class-standard-method*))
             (gf (apply (if (eq generic-function-class *the-class-standard-gf*)
                            #'make-instance-standard-generic-function
                            #'make-instance)
                        generic-function-class
                        :name function-name
                        :method-class method-class
                        all-keys)))
            (setf (find-generic-function function-name) gf)
            gf)))

;;; finalize-generic-function

;;; N.B. Same basic idea as finalize-inheritance.  Takes care of recomputing
;;; and storing the discriminating function, and clearing the effective method
;;; function table.


(defun finalize-generic-function (gf)
    #+nil (let ((fname (generic-function-name gf)))
              (if (and (consp fname) (equal (car fname) 'setf))
                  (print (list 'defsetf! fname))))

    (setf (generic-function-discriminating-function gf)
          (funcall (if (eq (class-of gf) *the-class-standard-gf*)
                       #'std-compute-discriminating-function
                       #'compute-discriminating-function)
                   gf))

    (jscl::fset (generic-function-name gf)
                (generic-function-discriminating-function gf))
    (setf (classes-to-emf-table gf) (make-hash-table :test #'eq))
    (values))




;;; make-instance-standard-generic-function creates and initializes an
;;; instance of standard-generic-function without falling into method lookup.
;;; However, it cannot be called until standard-generic-function exists.

(defun make-instance-standard-generic-function (generic-function-class &rest all-keys)
    (declare (ignore generic-function-class))
    (let ((name (get-keyword-from all-keys :name))
          (lambda-list (get-keyword-from all-keys :lambda-list))
          (method-class (get-keyword-from all-keys :method-class))
          (gf (std-allocate-instance *the-class-standard-gf*)))
        ;;(print (list 'make-instance-standard-generic-function name))
        ;;(print all-keys)
        ;;(print lambda-list)
        ;;(print name)
        (setf (generic-function-name gf) name)
        (setf (generic-function-lambda-list gf) lambda-list)
        (setf (generic-function-methods gf) ())
        (setf (generic-function-method-class gf) method-class)
        ;;(setf (classes-to-emf-table gf) (make-hash-table :test #'equal))
        ;;(print (list 'make-instance-std-generic name 'call-finilizer))
        (finalize-generic-function gf)
        gf))


;;; defmethod
(defmacro defmethod (&rest args)
    (multiple-value-bind (function-name qualifiers lambda-list specializers body)
        (parse-defmethod args)
        `(prog1 ',function-name
             (ensure-method (find-generic-function ',function-name)
                            :lambda-list ,(canonicalize-defgeneric-ll lambda-list)
                            :qualifiers ,(canonicalize-defgeneric-ll qualifiers)
                            :specializers ,(canonicalize-specializers specializers)
                            :body ',body
                            :environment (top-level-environment)))))

(defun canonicalize-specializers (specializers)
    (if specializers
        `(list ,@(mapcar #'canonicalize-specializer specializers))
        '()))


(defun canonicalize-specializer (specializer)
    `(find-class ',specializer))

(defun parse-defmethod (args)
    (let ((fn-spec (car args))
          (qualifiers ())
          (specialized-lambda-list nil)
          (body ())
          (parse-state :qualifiers))
        (dolist (arg (cdr args))
            (ecase parse-state
              (:qualifiers
               (if (and (atom arg) (not (null arg)))
                   (push-on-end arg qualifiers)
                   (progn (setq specialized-lambda-list arg)
                          (setq parse-state :body))))
              (:body (push-on-end arg body))))
        (values fn-spec
                qualifiers
                (extract-lambda-list specialized-lambda-list)
                (extract-specializers specialized-lambda-list)
                (list* 'block
                       (if (consp fn-spec)
                           (cadr fn-spec)
                           fn-spec)
                       body))))

;;; Several tedious functions for analyzing lambda lists
(defun required-portion (gf args)
    (let ((number-required (length (gf-required-arglist gf))))
        (when (< (length args) number-required)
            (error "Too few arguments to generic function ~S." gf))
        (subseq args 0 number-required)))

(defun gf-required-arglist (gf)
    (let ((plist
            (analyze-lambda-list
             (generic-function-lambda-list gf))))
        (getf plist ':required-args)))

(defun extract-lambda-list (specialized-lambda-list)
    (let* ((plist (analyze-lambda-list specialized-lambda-list))
           (requireds (getf plist ':required-names))
           (rv (getf plist ':rest-var))
           (ks (getf plist ':key-args))
           (aok (getf plist ':allow-other-keys))
           (opts (getf plist ':optional-args))
           (auxs (getf plist ':auxiliary-args)))
        `(,@requireds
          ,@(if rv `(&rest ,rv) ())
          ,@(if (or ks aok) `(&key ,@ks) ())
          ,@(if aok '(&allow-other-keys) ())
          ,@(if opts `(&optional ,@opts) ())
          ,@(if auxs `(&aux ,@auxs) ()))))

(defun extract-specializers (specialized-lambda-list)
    (let ((plist (analyze-lambda-list specialized-lambda-list)))
        (getf plist ':specializers)))

#+nil (defvar lambda-list-keywords '(&optional &rest &key &aux &allow-other-keys))
(defvar *lambda-list-keywords* '(&optional &rest &key &aux &allow-other-keys))

(defun analyze-lambda-list (lambda-list)
    (labels ((make-keyword (symbol)
                 (intern (symbol-name symbol)
                         (find-package 'keyword)))
             (get-keyword-from-arg (arg)
                 (if (listp arg)
                     (if (listp (car arg))
                         (caar arg)
                         (make-keyword (car arg)))
                     (make-keyword arg))))
        (let ((keys ())           ; Just the keywords
              (key-args ())       ; Keywords argument specs
              (required-names ()) ; Just the variable names
              (required-args ())  ; Variable names & specializers
              (specializers ())   ; Just the specializers
              (rest-var nil)
              (optionals ())
              (auxs ())
              (allow-other-keys nil)
              (state :parsing-required))
            (dolist (arg lambda-list)
                (if (member arg *lambda-list-keywords*)
                    (ecase arg
                      (&optional
                       (setq state :parsing-optional))
                      (&rest
                       (setq state :parsing-rest))
                      (&key
                       (setq state :parsing-key))
                      (&allow-other-keys
                       (setq allow-other-keys 't))
                      (&aux
                       (setq state :parsing-aux)))
                    (case state
                      (:parsing-required
                       (push-on-end arg required-args)
                       (if (listp arg)
                           (progn (push-on-end (car arg) required-names)
                                  (push-on-end (cadr arg) specializers))
                           (progn (push-on-end arg required-names)
                                  (push-on-end 't specializers))))
                      (:parsing-optional (push-on-end arg optionals))
                      (:parsing-rest (setq rest-var arg))
                      (:parsing-key
                       (push-on-end (get-keyword-from-arg arg) keys)
                       (push-on-end arg key-args))
                      (:parsing-aux (push-on-end arg auxs)))))
            (list  :required-names required-names
                   :required-args required-args
                   :specializers specializers
                   :rest-var rest-var
                   :keywords keys
                   :key-args key-args
                   :auxiliary-args auxs
                   :optional-args optionals
                   :allow-other-keys allow-other-keys))))

;;; ensure method
(defun ensure-method (gf &rest all-keys)
    #+nil (let ((fn-name (generic-function-name gf)))
              (print (list 'ensure-method fn-name 'type (type-of fn-name))))
    (let ((new-method
            (apply
             (if (eq (generic-function-method-class gf) *the-class-standard-method*)
                 #'make-instance-standard-method
                 #'make-instance)
             (generic-function-method-class gf)
             all-keys)))
        (add-method gf new-method)
        new-method))

;;; make-instance-standard-method creates and initializes an instance of
;;; standard-method without falling into method lookup.  However, it cannot
;;; be called until standard-method exists.
(defun make-instance-standard-method (method-class
                                      &key lambda-list qualifiers
                                        specializers body environment)
    (declare (ignore method-class))
    (let ((method (std-allocate-instance *the-class-standard-method*)))
        (setf (method-lambda-list method) lambda-list)
        (setf (method-qualifiers method) qualifiers)
        (setf (method-specializers method) specializers)
        (setf (method-body method) body)
        (setf (method-environment method) environment)
        (setf (method-generic-function method) nil)
        (setf (method-function method)
              (std-compute-method-function method))
        method))

;;; add-method
;;; N.B. This version first removes any existing method on the generic function
;;; with the same qualifiers and specializers.  It's a pain to develop
;;; programs without this feature of full CLOS.
(defun add-method (gf method)
    #+nil (let ((fn-name (generic-function-name gf)))
              (print (list 'add-method fn-name 'type (type-of fn-name))))
    (let ((old-method
            (find-method gf (method-qualifiers method)
                         (method-specializers method) nil)))
        (when old-method (remove-method gf old-method)))
    (setf (method-generic-function method) gf)
    (push method (generic-function-methods gf))
    (dolist (specializer (method-specializers method))
        (pushnew method (class-direct-methods specializer)))
    (finalize-generic-function gf)
    method)

(defun remove-method (gf method)
    #+nil (let ((fn-name (generic-function-name gf)))
              (print (list 'remove-method fn-name 'type (type-of fn-name))))
    (setf (generic-function-methods gf)
          (remove method (generic-function-methods gf)))
    (setf (method-generic-function method) nil)
    (dolist (class (method-specializers method))
        (setf (class-direct-methods class)
              (remove method (class-direct-methods class))))
    (finalize-generic-function gf)
    method)

(defun find-method (gf qualifiers specializers &optional (errorp t))
    #+nil (let ((fn-name (generic-function-name gf)))
              (print (list 'find-method fn-name 'type (type-of fn-name))))
    (let ((method
            (find-if #'(lambda (method)
                           (and (eq qualifiers
                                    (method-qualifiers method))
                                (eq specializers
                                    (method-specializers method))))
                     (generic-function-methods gf))))
        (if (and (null method) errorp)
            (error "No such method for ~S." (generic-function-name gf))
            method)))

;;; Reader and write methods
(defun add-reader-method (class fn-name slot-name)
    #+nil (print (list 'add-reader fn-name 'type (type-of fn-name)))
    (ensure-method
     (ensure-generic-function fn-name :lambda-list '(object))
     :lambda-list '(object)
     :qualifiers ()
     :specializers (list class)
     :body `(slot-value object ',slot-name)
     :environment (top-level-environment))
    (values))

(defun add-writer-method (class fn-name slot-name)
    #+nil (print (list 'add-writer fn-name 'type (type-of fn-name)))
    (ensure-method
     (ensure-generic-function  fn-name :lambda-list '(object new-value))
     :lambda-list '(object new-value)
     :qualifiers ()
     :specializers (list (find-class 't) class)
     :body `(setf (slot-value object ',slot-name) new-value)
     :environment (top-level-environment))
    (values))



;;;
;;; Generic function invocation
;;;

;;; apply-generic-function ???
(defun apply-generic-function (gf args)
    ;;(#j:console:log "apply-generic-fn" (format nil "~a" args))
    (apply (generic-function-discriminating-function gf) args))

;;; compute-discriminating-function
(defun std-compute-discriminating-function (gf)
    #'(lambda (&rest args)
          (let* ((classes (mapcar #'class-of
                                  (required-portion gf args)))
                 (emfun (gethash classes (classes-to-emf-table gf) nil)))
              (if emfun
                  (funcall emfun args)
                  (slow-method-lookup gf args classes)))))

(defun slow-method-lookup (gf args classes)
    (let* ((applicable-methods
             (compute-applicable-methods-using-classes gf classes))
           (emfun
             (funcall
              (if (eq (class-of gf) *the-class-standard-gf*)
                  #'std-compute-effective-method-function
                  #'compute-effective-method-function)
              gf applicable-methods)))
        (setf (gethash classes (classes-to-emf-table gf)) emfun)
        (funcall emfun args)))

;;; compute-applicable-methods-using-classes
(defun compute-applicable-methods-using-classes (gf required-classes)
    (sort
     (copy-list
      (remove-if-not #'(lambda (method)
                           ;; todo: every1 !!!
                           (every
                            #'(lambda (lst) (subclassp (first lst) (second lst)))
                            (mapcar #'(lambda (x y) (list x y))
                                    required-classes (method-specializers method))))
                     (generic-function-methods gf)))
     #'(lambda (m1 m2)
           (funcall
            (if (eq (class-of gf) *the-class-standard-gf*)
                #'std-method-more-specific-p
                #'method-more-specific-p)
            gf m1 m2 required-classes))))


(defun std-method-more-specific-p (gf method1 method2 required-classes)
    (declare (ignore gf))
    (mapc #'(lambda (spec1 spec2 arg-class)
                (unless (eq spec1 spec2)
                    (return-from std-method-more-specific-p
                        (sub-specializer-p spec1 spec2 arg-class))))
          (method-specializers method1)
          (method-specializers method2)
          required-classes)
    nil)

;;; apply-methods and compute-effective-method-function
(defun apply-methods (gf args methods)
    (funcall (compute-effective-method-function gf methods)
             args))

(defun primary-method-p (method)
    (null (method-qualifiers method)))

(defun before-method-p (method)
    (equal '(:before) (method-qualifiers method)))

(defun after-method-p (method)
    (equal '(:after) (method-qualifiers method)))

(defun around-method-p (method)
    (equal '(:around) (method-qualifiers method)))

;;; :todo: format
(defun std-compute-effective-method-function (gf methods)
    (let ((primaries (remove-if-not #'primary-method-p methods))
          (around (find-if #'around-method-p methods)))
        (when (null primaries)
            ;; bug: its bug!!!
            (error "No primary methods for the generic function ~S." gf))
        (if around
            (let ((next-emfun
                    (funcall
                     (if (eq (class-of gf) *the-class-standard-gf*)
                         #'std-compute-effective-method-function
                         #'compute-effective-method-function)
                     gf (remove around methods))))
                #'(lambda (args)
                      (funcall (method-function around) args next-emfun)))
            (let ((next-emfun (compute-primary-emfun (cdr primaries)))
                  (befores (remove-if-not #'before-method-p methods))
                  (reverse-afters
                    (reverse (remove-if-not #'after-method-p methods))))
                #'(lambda (args)
                      (dolist (before befores)
                          (funcall (method-function before) args nil))
                      (multiple-value-prog1
                          (funcall (method-function (car primaries)) args next-emfun)
                          (dolist (after reverse-afters)
                              (funcall (method-function after) args nil))))))))

;;; compute an effective method function from a list of primary methods:
(defun compute-primary-emfun (methods)
    (if (null methods)
        nil
        (let ((next-emfun (compute-primary-emfun (cdr methods))))
            #'(lambda (args)
                  (funcall (method-function (car methods)) args next-emfun)))))

;;; apply-method and compute-method-function
(defun apply-method (method args next-methods)
    (funcall (method-function method)
             args
             (if (null next-methods)
                 nil
                 (compute-effective-method-function
                  (method-generic-function method) next-methods))))

;;; :todo: compile-in-lexical-environment
(defun std-compute-method-function (method)
    (let ((form (method-body method))
          (lambda-list (method-lambda-list method))
          (mgf (method-generic-function method)))
        (compile-in-lexical-environment
         (method-environment method)
         `(lambda (args next-emfun)
              (flet ((call-next-method (&rest cnm-args)
                         (if (null next-emfun)
                             (error "No next method for the generic function ~S." ,mgf)
                             (funcall next-emfun (or cnm-args args))))
                     (next-method-p ()
                         (not (null next-emfun))))
                  (apply #'(lambda ,(kludge-arglist lambda-list)
                               ,form)
                         args))))))

;;; N.B. The function kludge-arglist is used to pave over the differences
;;; between argument keyword compatibility for regular functions versus
;;; generic functions.
(defun kludge-arglist (lambda-list)
    (if (and (member '&key lambda-list)
             (not (member '&allow-other-keys lambda-list)))
        (append lambda-list '(&allow-other-keys))
        (if (and (not (member '&rest lambda-list))
                 (not (member '&key lambda-list)))
            (append lambda-list '(&key &allow-other-keys))
            lambda-list)))

;;; Run-time environment hacking (Common Lisp ain't got 'em).
(defun top-level-environment ()
    nil) ; Bogus top level lexical environment

;;; fuck he need?
(defvar compile-methods nil)      ; by default, run everything interpreted
(defvar *compile-methods* nil)      ; by default, run everything interpreted

;;; see above
(defun compile-in-lexical-environment (env lambda-expr)
    (declare (ignore env))
    ;;(print lambda-expr)
    (if compile-methods
        (compile nil lambda-expr)
        (eval `(function ,lambda-expr))))

;;; EOF
