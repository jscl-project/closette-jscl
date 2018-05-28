;;; -*- mode:lisp; coding:utf-8 -*-


(in-package #:clos)


(defmacro dts-def ()
    `(progn
         (defvar *debug-ts/start* 0)
         (defvar *debug-ts/end*  0)))

(defmacro dts-start ()
    `(setq *debug-ts/start* (#j:Date:now)))

(defmacro dts-log (note)
    `(#j:console:log (jscl::lisp-to-js ,note)
                     (/ (- (#j:Date:now) *debug-ts/start*) 1000)))

(dts-def)

;;;
;;; Bootstrap
;;;

(forget-all-classes)
(forget-all-generic-functions)
;; How to create the class hierarchy in 10 easy steps:

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

(dts-start)

(defclass standard-object (t) ())

(dts-log "defclass standard-object")

;; 7. Define the full-blown version of standard-class.

(dts-start)

(setf *the-class-standard-class* (eval *the-defclass-standard-class*))

(dts-log "eval defclass standard class")


;; 8. Replace all (3) existing pointers to the skeleton with real one.

(dts-start)

(setf (std-instance-class (find-class 't))
      *the-class-standard-class*)
(setf (std-instance-class (find-class 'standard-object))
      *the-class-standard-class*)
(setf (std-instance-class *the-class-standard-class*)
      *the-class-standard-class*)

(dts-log "Step 8")

;; (Clear sailing from here on in).

;; 9. Define the other built-in classes.

(dts-start)

(defclass symbol (t) ())
(defclass sequence (t) ())
(defclass array (t) ())
(defclass number (t) ())
(defclass character (t) ())
(defclass function (t) ())
(defclass hash-table (t) ())
(defclass package (t) ())
(defclass stream (t) ())
(defclass list (sequence) ())
(defclass null (symbol list) ())
(defclass cons (list) ())
(defclass vector (array sequence) ())
(defclass string (vector) ())
(defclass integer (number) ())
(defclass float (number) ())

(dts-log "Step 9")


;; 10. Define the other standard metaobject classes.

(dts-start)

(setf *the-class-standard-gf* (eval *the-defclass-standard-generic-function*))

(setf *the-class-standard-method* (eval *the-defclass-standard-method*))

(dts-log "Step 10")

;; Voila! The class hierarchy is in place.
;; (It's now okay to define generic functions and methods.)


;;; EOF
