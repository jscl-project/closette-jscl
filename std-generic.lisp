;;; -*- mode:lisp; coding:utf-8 -*-

(/debug "loading std-generic")

;;; (in-package :amop)

;;;
;;; Generic function metaobjects and standard-generic-function
;;;

(defparameter *the-defclass-standard-generic-function*
  '(!defclass standard-generic-function ()
    ((name :initarg :name)      ; :accessor generic-function-name
     (lambda-list               ; :accessor generic-function-lambda-list
      :initarg :lambda-list)
     (methods :initform ())     ; :accessor generic-function-methods
     (method-class              ; :accessor generic-function-method-class
      :initarg :method-class)
     (discriminating-function)  ; :accessor generic-function-
                                        ;    -discriminating-function
     (classes-to-emf-table      ; :accessor classes-to-emf-table
      :initform (make-hash-table :test #'equal)))))



(defvar *the-class-standard-gf*) ;standard-generic-function's class metaobject


;;; generic-function-name
(defun generic-function-name (gf)
    (!slot-value gf 'name))


(defun setf-generic-function-name (gf new-value)
    (setf-slot-value gf 'name new-value))



;;; generic-function-lambda-list
(defun generic-function-lambda-list (gf)
    (!slot-value gf 'lambda-list))

(defun setf-generic-function-lambda-list (gf new-value)
    (setf-slot-value gf 'lambda-list new-value))



;;; generic-function-methods
(defun generic-function-methods (gf)
    (!slot-value gf 'methods))

(defun setf-generic-function-methods (gf new-value)
    (setf-slot-value gf 'methods new-value))

;;; for push method (generic-function-method gf))
#+nil
(defun (setf generic-function-methods) (new-value gf)
    (setf-slot-value gf 'methods new-value))

;;; note: last remove setf

(defun (setf generic-function-methods) (new-value gf)
    (setf (slot-value gf 'methods) new-value))


(defun push-generic-function-methods (new-value gf)
    (let ((lst (!slot-value gf 'methods)))
        (push new-value lst)
        (setf-slot-value gf 'methods lst)))


;;; generic-function-discriminating-function
(defun generic-function-discriminating-function (gf)
    (!slot-value gf 'discriminating-function))


(defun setf-generic-function-discriminating-function (gf new-value)
    (setf-slot-value gf 'discriminating-function new-value))



;;; generic-function-method-class
(defun generic-function-method-class (gf)
    (!slot-value gf 'method-class))


(defun setf-generic-function-method-class (gf new-value)
    (setf-slot-value gf 'method-class new-value))


;;; Internal accessor for effective method function table

(defun classes-to-emf-table (gf)
    ;;(break "class-emf-table")
    (!slot-value gf 'classes-to-emf-table))


(defun setf-classes-to-emf-table (gf new-value)
    (setf-slot-value gf 'classes-to-emf-table new-value))



;;;
;;; Method metaobjects and standard-method
;;;

(defparameter *the-defclass-standard-method*
  '(!defclass standard-method ()
    ((lambda-list :initarg :lambda-list)     ; :accessor method-lambda-list
     (qualifiers :initarg :qualifiers)       ; :accessor method-qualifiers
     (specializers :initarg :specializers)   ; :accessor method-specializers
     (body :initarg :body)                   ; :accessor method-body
     (generic-function :initform nil)        ; :accessor method-generic-function
     (function))))                           ; :accessor method-function


(defvar *the-class-standard-method*)    ;standard-method's class metaobject


;;; method-lambda-list
(defun method-lambda-list (method)
    (!slot-value method 'lambda-list))


(defun setf-method-lambda-list (method new-value)
    (setf-slot-value method 'lambda-list new-value))


;;; method-qualifiers
(defun !method-qualifiers (method)
    (!slot-value method 'qualifiers))


(defun setf-method-qualifiers (method new-value)
    (setf-slot-value method 'qualifiers new-value))


;;; method-specializers
(defun !method-specializers (method)
    (!slot-value method 'specializers))


(defun setf-method-specializers (method new-value)
    (setf-slot-value method 'specializers new-value))



;;; method-body
(defun method-body (method)
    (!slot-value method 'body))


(defun setf-method-body (method new-value)
    (setf-slot-value method 'body new-value))


;;; method-generic-function
(defun method-generic-function (method)
    (!slot-value method 'generic-function))


(defun setf-method-generic-function (method new-value)
    (setf-slot-value method 'generic-function new-value))


;;; method-function
(defun method-function (method)
    (!slot-value method 'function))


(defun setf-method-function (method new-value)
    (setf-slot-value method 'function new-value))

;;; note: last
(defun (setf method-function) (new-value method)
    (setf-slot-value method 'function new-value))



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
             `(!find-class ',(cadr option))))
      (:method-class
       (list ':method-class
             `(!find-class ',(cadr option))))
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


(defun setf-find-generic-function (symbol new-value)
    (setf (gethash symbol *generic-function-table*) new-value))



#+nil
(defun forget-all-generic-functions ()
    (setq *generic-function-table* (make-hash-table :test #'eq))
    (values))


;;; ensure-generic-function
(defun !ensure-generic-function (function-name &rest all-keys)
    (if (find-generic-function function-name nil)
        ;; todo: ?
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
            ;;(setf (find-generic-function function-name) gf)
            (setf-find-generic-function function-name gf)
            gf)))

;;; finalize-generic-function

;;; N.B. Same basic idea as finalize-inheritance.  Takes care of recomputing
;;; and storing the discriminating function, and clearing the effective method
;;; function table.

(defun finalize-generic-function (gf)
    #+nil(setf (generic-function-discriminating-function gf)
               (funcall (if (eq (class-of gf) *the-class-standard-gf*)
                            #'std-compute-discriminating-function
                            #'compute-discriminating-function)
                        gf))
    ;;(break "finalize")

    (setf-generic-function-discriminating-function
     gf
     (funcall (if (eq (!class-of gf) *the-class-standard-gf*)
                  #'std-compute-discriminating-function
                  #'compute-discriminating-function)
              gf))
    (let* (;;(fake0 (break "fake0"))
           (fname (generic-function-name gf))
           ;;(fake1 (break "fake1"))
           (sfname (setf-function-symbol fname)))
        #-jscl (jscl::!fset sfname (generic-function-discriminating-function gf))
        #+jscl (jscl::fset sfname (generic-function-discriminating-function gf))
        ;;(break "let*")
        (if (and (consp fname) (equal (car fname) 'setf))
            (make-setf-pair (cadr fname) sfname))
        ;;(setf (classes-to-emf-table gf) (make-hash-table :test #'eq))
        ;;(break "setf-classes")
        (setf-classes-to-emf-table gf (make-hash-table :test #'eq))
        (values)))


;;; make-instance-standard-generic-function creates and initializes an
;;; instance of standard-generic-function without falling into method lookup.
;;; However, it cannot be called until standard-generic-function exists.

(defun make-instance-standard-generic-function (generic-function-class &rest all-keys)
    (declare (ignore generic-function-class))
    (let ((name (get-keyword-from all-keys :name))
          (lambda-list (get-keyword-from all-keys :lambda-list))
          (method-class (get-keyword-from all-keys :method-class))
          (gf (std-allocate-instance *the-class-standard-gf*)))

        ;;(setf (generic-function-name gf) name)
        ;;(setf (generic-function-lambda-list gf) lambda-list)
        ;;(setf (generic-function-methods gf) ())
        ;;(setf (generic-function-method-class gf) method-class)

        (setf-generic-function-name gf name)
        (setf-generic-function-lambda-list gf lambda-list)
        (setf-generic-function-methods gf ())
        (setf-generic-function-method-class gf method-class)


        ;;(setf (classes-to-emf-table gf) (make-hash-table :test #'equal))
        (finalize-generic-function gf)
        gf))


;;; macro

#+nil
(defmacro def!generic (function-name lambda-list &rest options)
    `(prog1 ',function-name
         (!ensure-generic-function
          ',function-name
          :lambda-list ,(canonicalize-defgeneric-ll lambda-list)
          ,@(canonicalize-defgeneric-options options))))




;;; EOF
