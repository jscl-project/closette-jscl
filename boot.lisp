;;; -*- mode:lisp; coding:utf-8 -*-


;;;(in-package #:amop)


;;;
;;; Bootstrap
;;;

(/debug "loading & build standard hierarhy!")

(defun boot-stage-1 ()
    (forget-all-classes)
    (forget-all-generic-functions)

    ;; 1. Figure out standard-class's slots.
    (setq *the-slots-of-standard-class*
          (mapcar (lambda (slotd)
                      (make-effective-slot-definition
                       :name (car slotd)
                       :initargs (let ((a (getf (cdr slotd) ':initarg)))
                                     (if a (list a) ()))
                       :initform (getf (cdr slotd) ':initform)
                       :initfunction  (let ((a (getf (cdr slotd) ':initform)))
                                          (if a (lambda () (eval a)) nil))
                       :allocation ':instance))
                  (nth 3 *the-defclass-standard-class*)))


    ;; 2. Create the standard-class metaobject by hand.
    (setq *the-class-standard-class*
          (allocate-std-instance
           :class 'tba
           :slots (make-array (length *the-slots-of-standard-class*)
                              :initial-element *secret-unbound-value*)))


    ;; 3. Install standard-class's (circular) class-of link.


    (setf (std-instance-class *the-class-standard-class*) *the-class-standard-class*)


    ;; 4. Fill in standard-class's class-slots.
    (setf-class-slots *the-class-standard-class* *the-slots-of-standard-class*)
    ;; (Skeleton built; it's now okay to call make-instance-standard-class.)

    ;; 5. Hand build the class t so that it has no direct superclasses.
    (setf-find-class 't
        (let ((class (std-allocate-instance *the-class-standard-class*)))
            (setf-class-name class 't)
            (setf-class-direct-subclasses class ())
            (setf-class-direct-superclasses class ())
            (setf-class-direct-methods class ())
            (setf-class-direct-slots class ())
            (setf-class-precedence-list class ())
            (setf-class-slots class ())
            class))

    ;; 6. Create the other superclass of standard-class (i.e., standard-object).
    (ensure-class 'standard-object
                  :direct-superclasses (list (!find-class 't))
                  :direct-slots '())

    (setq *the-class-standard-class*
          (ensure-class 'standard-class
                        :direct-superclasses nil
                        ;;(canonicalize-direct-superclasses '())
                        :direct-slots
                        (LIST
                         (LIST :NAME (QUOTE NAME) :INITARGS (QUOTE (:NAME)))
                         (LIST :NAME (QUOTE DIRECT-SUPERCLASSES) :INITARGS (QUOTE (:DIRECT-SUPERCLASSES)))
                         (LIST :NAME (QUOTE DIRECT-SLOTS))
                         (LIST :NAME (QUOTE CLASS-PRECEDENCE-LIST))
                         (LIST :NAME (QUOTE EFFECTIVE-SLOTS))
                         (LIST :NAME (QUOTE DIRECT-SUBCLASSES) :INITFORM (QUOTE NIL) :INITFUNCTION (FUNCTION (LAMBDA NIL NIL)))
                         (LIST :NAME (QUOTE DIRECT-METHODS) :INITFORM (QUOTE NIL) :INITFUNCTION (FUNCTION (LAMBDA NIL NIL))))
                        ))

    ;; 8. Replace all (3) existing pointers to the skeleton with real one.
    (setf (std-instance-class  (!find-class 't))
          *the-class-standard-class*)
    (setf (std-instance-class (!find-class 'standard-object))
          *the-class-standard-class*)
    (setf (std-instance-class *the-class-standard-class*)
          *the-class-standard-class*)
    )

;;;;;;;;;


;; (Clear sailing from here on in).

;; 9. Define the other built-in classes.

#|
#+jscl
(def!class symbol (t) ())

#+jscl
(defclass sequence (t) ())

#+jscl
(defclass array (t) ())

#+jscl
(defclass number (t) ())

#+jscl
(defclass character (t) ())

#+jscl
(defclass function (t) ())

#+jscl
(defclass hash-table (t) ())

#+jscl
(defclass package (t) ())

#+jscl
(defclass stream (t) ())

#+jscl
(defclass list (sequence) ())

#+jscl
(defclass null (symbol list) ())

#+jscl
(defclass cons (list) ())

#+jscl
(defclass vector (array sequence) ())

#+jscl
(defclass string (vector) ())

#+jscl
(defclass integer (number) ())

#+jscl
(defclass float (number) ())

|#


(defun boot-stage-2 ()
    (ensure-class 'symbol
                  :direct-superclasses '()
                  :direct-slots   '())

    (ensure-class 'sequence
                  :direct-superclasses '()
                  :direct-slots   '())


    (ensure-class 'array
                  :direct-superclasses '()
                  :direct-slots   '())


    (ensure-class 'number
                  :direct-superclasses '()
                  :direct-slots   '())


    (ensure-class 'character
                  :direct-superclasses '()
                  :direct-slots   '())


    (ensure-class 'function
                  :direct-superclasses '()
                  :direct-slots   '())

    #+nil
    (ensure-class 'hashtable
                  :direct-superclasses '()
                  :direct-slots   '())


    (ensure-class 'package
                  :direct-superclasses '()
                  :direct-slots   '())


    (ensure-class 'stream
                  :direct-superclasses '()
                  :direct-slots   '())


    (ensure-class 'list
                  :direct-superclasses (LIST (!FIND-CLASS (QUOTE sequence)))
                  :direct-slots   '())



    (ensure-class 'null
                  :direct-superclasses (LIST (!FIND-CLASS (QUOTE SYMBOL)) (!FIND-CLASS (QUOTE LIST)))
                  :direct-slots   '())


    (ensure-class 'cons :direct-superclasses (LIST (!FIND-CLASS (QUOTE list))) :direct-slots   '())


    (ensure-class 'vector
                  :direct-superclasses (LIST (!FIND-CLASS (QUOTE array)) (!FIND-CLASS (QUOTE sequence)))
                  :direct-slots   '())


    (ensure-class 'string
                  :direct-superclasses (LIST (!FIND-CLASS (QUOTE vector)))
                  :direct-slots   '())


    (ensure-class 'integer
                  :direct-superclasses (LIST (!FIND-CLASS (QUOTE number)))
                  :direct-slots   '())


    (ensure-class 'float
                  :direct-superclasses (LIST (!FIND-CLASS (QUOTE number)))
                  :direct-slots   '())



    ;; 10. Define the other standard metaobject classes.
    (setq *the-class-standard-gf*
          (ensure-class 'standard-generic-function
                        :direct-superclasses '()
                        :direct-slots (LIST
                                       (LIST :NAME (QUOTE NAME) :INITARGS (QUOTE (:NAME)))
                                       (LIST :NAME (QUOTE LAMBDA-LIST) :INITARGS (QUOTE (:LAMBDA-LIST)))
                                       (LIST :NAME (QUOTE METHODS)
                                             :INITFORM (QUOTE NIL)
                                             :INITFUNCTION (FUNCTION (LAMBDA NIL NIL)))
                                       (LIST :NAME (QUOTE METHOD-CLASS)
                                             :INITARGS (QUOTE (:METHOD-CLASS)))
                                       (LIST :NAME (QUOTE DISCRIMINATING-FUNCTION))
                                       (LIST :NAME (QUOTE CLASSES-TO-EMF-TABLE)
                                             :INITFORM (QUOTE (MAKE-HASH-TABLE :TEST (FUNCTION EQUAL)))
                                             :INITFUNCTION (FUNCTION (LAMBDA NIL (MAKE-HASH-TABLE :TEST (FUNCTION EQUAL))))))))


    (setq *the-class-standard-method*
          (ensure-class 'standard-method
                        :direct-superclasses '()
                        :direct-slots (LIST
                                       (LIST :NAME (QUOTE LAMBDA-LIST) :INITARGS (QUOTE (:LAMBDA-LIST)))
                                       (LIST :NAME (QUOTE QUALIFIERS) :INITARGS (QUOTE (:QUALIFIERS)))
                                       (LIST :NAME (QUOTE SPECIALIZERS) :INITARGS (QUOTE (:SPECIALIZERS)))
                                       (LIST :NAME (QUOTE BODY) :INITARGS (QUOTE (:BODY)))
                                       (LIST :NAME (QUOTE GENERIC-FUNCTION)
                                             :INITFORM (QUOTE NIL)
                                             :INITFUNCTION (FUNCTION (LAMBDA NIL NIL)))
                                       (LIST :NAME (QUOTE FUNCTION)))))

    )


(boot-stage-1)
(boot-stage-2)


#+nil (defmacro defclass (name direct-superclasses direct-slots &rest options)
          `(progn
               (amop::ensure-class ',name
                                   :direct-superclasses
                                   ,(amop::canonicalize-direct-superclasses direct-superclasses)
                                   :direct-slots
                                   ,(amop::canonicalize-direct-slots direct-slots)
                                   ,@(amop::canonicalize-defclass-options options))
               ',name))


#+nil (defmacro defgeneric (function-name lambda-list &rest options)
          `(prog1 ',function-name
               (amop::ensure-generic-function
                ',function-name
                :lambda-list ,(amop::canonicalize-defgeneric-ll lambda-list)
                ,@(amop::canonicalize-defgeneric-options options))))


#+nil (defmacro defmethod (&rest args)
          (multiple-value-bind (function-name qualifiers lambda-list specializers body)
              (amop::parse-defmethod args)
              `(prog1 ',function-name
                   (amop::ensure-method (amop::find-generic-function ',function-name)
                                        :lambda-list ,(amop::canonicalize-defgeneric-ll lambda-list)
                                        :qualifiers ,(amop::canonicalize-defgeneric-ll qualifiers)
                                        :specializers ,(amop::canonicalize-specializers specializers)
                                        :body ',body))))



;; Voila! The class hierarchy is in place.
;; (It's now okay to define generic functions and methods.)

;;; EOF