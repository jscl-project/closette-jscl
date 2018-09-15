;;; -*- mode:lisp; coding:utf-8 -*-


;;;(in-package #:amop)


;;;
;;; Bootstrap
;;;

(forget-all-classes)
(forget-all-generic-functions)
;; How to create the class hierarchy in 10 easy steps:



#|
;; 1. Figure out standard-class's slots.
(setf *the-slots-of-standard-class*
      (mapcar #'(lambda (slotd)
                    (make-effective-slot-definition
                     :name (car slotd)
                     :initargs
                     (let ((a (getf (cdr slotd) ':initarg)))
                         (if a (list a) ()))
                     :initform (getf (cdr slotd) ':initform)
                     :initfunction
                     (let ((a (getf (cdr slotd) ':initform)))
                         (if a #'(lambda () (eval a)) nil))
                     :allocation ':instance))
              (nth 3 *the-defclass-standard-class*)))

;; 2. Create the standard-class metaobject by hand.
(setf *the-class-standard-class*
      (allocate-std-instance
       :class 'tba
       :slots (make-array (length *the-slots-of-standard-class*)
                          :initial-element *secret-unbound-value*)))

;; 3. Install standard-class's (circular) class-of link.
;;;(setf (std-instance-class the-class-standard-class) the-class-standard-class)


(setf (std-instance-class *the-class-standard-class*) *the-class-standard-class*)
;; (It's now okay to use class-... accessor).

;; 4. Fill in standard-class's class-slots.
(setf (class-slots *the-class-standard-class*) *the-slots-of-standard-class*)
;; (Skeleton built; it's now okay to call make-instance-standard-class.)

;; 5. Hand build the class t so that it has no direct superclasses.
(setf (find-class 't)
      (let ((class (std-allocate-instance *the-class-standard-class*)))
          (setf (class-name class) 't)
          (setf (class-direct-subclasses class) ())
          (setf (class-direct-superclasses class) ())
          (setf (class-direct-methods class) ())
          (setf (class-direct-slots class) ())
          (setf (class-precedence-list class) ())
          (setf (class-slots class) ())
          class))
;; (It's now okay to define subclasses of t.)

;; 6. Create the other superclass of standard-class (i.e., standard-object).

#+jscl (defclass standard-object (t) ())


;; 7. Define the full-blown version of standard-class.


(setf *the-class-standard-class* (eval *the-defclass-standard-class*))



;; 8. Replace all (3) existing pointers to the skeleton with real one.


(setf (std-instance-class (find-class 't))
      *the-class-standard-class*)
(setf (std-instance-class (find-class 'standard-object))
      *the-class-standard-class*)
(setf (std-instance-class *the-class-standard-class*)
      *the-class-standard-class*)

|#


;;;;;;;;;
(defun stage1 ()
    ;; 1. Figure out standard-class's slots.
    ;;(print 'stage1)
    (setf *the-slots-of-standard-class*
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

    ;;(print 'allocate-standard)
    ;; 2. Create the standard-class metaobject by hand.
    (setf *the-class-standard-class*
          (allocate-std-instance
           :class 'tba
           :slots (make-array (length *the-slots-of-standard-class*)
                              :initial-element *secret-unbound-value*)))


    ;; 3. Install standard-class's (circular) class-of link.
;;;(setf (std-instance-class the-class-standard-class) the-class-standard-class)


    (setf (std-instance-class *the-class-standard-class*) *the-class-standard-class*)
    ;; (It's now okay to use class-... accessor).

    ;;(print 'step3)

    ;; 4. Fill in standard-class's class-slots.
    (setf-class-slots *the-class-standard-class* *the-slots-of-standard-class*)
    ;; (Skeleton built; it's now okay to call make-instance-standard-class.)

    ;;(print 'step4)

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

    ;;(print 'step5)
    ;; (It's now okay to define subclasses of t.)

    ;; 6. Create the other superclass of standard-class (i.e., standard-object).


    ;;;(ensure-class 'standard-object :direct-superclasses ,(canonicalize-direct-superclasses '(t)) :direct-slots   '())

    ;;(progn
    (ensure-class 'standard-object
                  :direct-superclasses (list (find-class 't))
                  :direct-slots '())
    ;;    t)

    ;;(print 'step6)

    ;; 7. Define the full-blown version of standard-class.

    #|
    (defparameter *the-defclass-standard-class*  ;standard-class's defclass form
    '(!defclass standard-class ()
    ((name :initarg :name)              ; :accessor class-name
    (direct-superclasses               ; :accessor class-direct-superclasses
    :initarg :direct-superclasses)
    (direct-slots)                     ; :accessor class-direct-slots
    (class-precedence-list)            ; :accessor class-precedence-list
    (effective-slots)                  ; :accessor class-slots
    (direct-subclasses :initform ())   ; :accessor class-direct-subclasses
    (direct-methods :initform ()))))   ; :accessor class-direct-methods
    |#


    #|
    (ensure-class 'standard-class
    :direct-superclasses
    ,(canonicalize-direct-superclasses '())
    :direct-slots
    ,(canonicalize-direct-slots

    '((name :initarg :name)              ; :accessor class-name
    (direct-superclasses               ; :accessor class-direct-superclasses
    :initarg :direct-superclasses)
    (direct-slots)                     ; :accessor class-direct-slots
    (class-precedence-list)            ; :accessor class-precedence-list
    (effective-slots)                  ; :accessor class-slots
    (direct-subclasses :initform ())   ; :accessor class-direct-subclasses
    (direct-methods :initform ()))))
    |#

    ;;(setf *the-class-standard-class* (eval *the-defclass-standard-class*))

    #+nil (setf *the-class-standard-class*
                (ensure-class 'standard-class
                              :direct-superclasses
                              (canonicalize-direct-superclasses '())
                              :direct-slots
                              `,(canonicalize-direct-slots
                                 '((name :initarg :name)              ; :accessor class-name
                                   (direct-superclasses               ; :accessor class-direct-superclasses
                                    :initarg :direct-superclasses)
                                   (direct-slots)                     ; :accessor class-direct-slots
                                   (class-precedence-list)            ; :accessor class-precedence-list
                                   (effective-slots)                  ; :accessor class-slots
                                   (direct-subclasses :initform ())   ; :accessor class-direct-subclasses
                                   (direct-methods :initform ())))))


    (setf *the-class-standard-class*
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

    ;;(print 'step7)

    ;; 8. Replace all (3) existing pointers to the skeleton with real one.


    (setf (std-instance-class  (find-class 't))
          *the-class-standard-class*)
    (setf (std-instance-class (find-class 'standard-object))
          *the-class-standard-class*)
    (setf (std-instance-class *the-class-standard-class*)
          *the-class-standard-class*)

    ;;(print 'done)
    )


#-jscl
(stage1)

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

#-jscl
(ensure-class 'symbol
              :direct-superclasses '()
              :direct-slots   '())

#-jscl
(ensure-class 'sequence
              :direct-superclasses '()
              :direct-slots   '())

#-jscl
(ensure-class 'array
              :direct-superclasses '()
              :direct-slots   '())

#-jscl
(ensure-class 'number
              :direct-superclasses '()
              :direct-slots   '())

#-jscl
(ensure-class 'character
              :direct-superclasses '()
              :direct-slots   '())

#-jscl
(ensure-class 'function
              :direct-superclasses '()
              :direct-slots   '())

#+nil
(ensure-class 'hashtable
              :direct-superclasses '()
              :direct-slots   '())

#-jscl
(ensure-class 'package
              :direct-superclasses '()
              :direct-slots   '())

#-jscl
(ensure-class 'stream
              :direct-superclasses '()
              :direct-slots   '())

#-jscl
(ensure-class 'list
              :direct-superclasses (LIST (FIND-CLASS (QUOTE sequence)))
              :direct-slots   '())


#-jscl
(ensure-class 'null
              :direct-superclasses (LIST (FIND-CLASS (QUOTE SYMBOL)) (FIND-CLASS (QUOTE LIST)))
              :direct-slots   '())

#-jscl
(ensure-class 'cons :direct-superclasses (LIST (FIND-CLASS (QUOTE list))) :direct-slots   '())

#-jscl
(ensure-class 'vector
              :direct-superclasses (LIST (FIND-CLASS (QUOTE array)) (FIND-CLASS (QUOTE sequence)))
              :direct-slots   '())

#-jscl
(ensure-class 'string
              :direct-superclasses (LIST (FIND-CLASS (QUOTE vector)))
              :direct-slots   '())

#-jscl
(ensure-class 'integer
              :direct-superclasses (LIST (FIND-CLASS (QUOTE number)))
              :direct-slots   '())

#-jscl
(ensure-class 'float
              :direct-superclasses (LIST (FIND-CLASS (QUOTE number)))
              :direct-slots   '())



;; 10. Define the other standard metaobject classes.


;;; note: eval
#-jscl
;;(setf *the-class-standard-gf* (eval *the-defclass-standard-generic-function*))
(setf *the-class-standard-gf*
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


;;; note: eval
#-jscl
;;(setf *the-class-standard-method* (eval *the-defclass-standard-method*))
(setf *the-class-standard-method*
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

#-jscl
(defclass mops () ((name)))


;;; EOF
