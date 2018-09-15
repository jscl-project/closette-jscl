;;; -*- mode:lisp; coding:utf-8 -*-

;;; debug tools
;;; From closette-test.lisp

(in-package :clos.bin)

(defun subclasses* (class)
    (clos::remove-duplicates
     (cons class
           (clos::mapappend #'subclasses*
                            (clos::class-direct-subclasses class)))))

;;; (export '(subclasses))
(defun subclasses (class &optional names)
    (let ((result (remove class (subclasses* class))))
        (if names (mapcar #'clos::class-name result)
            result)))

;;; (export '(in-order-p))
(defun in-order-p (c1 c2)
    (flet ((in-order-at-subclass-p (sub)
               (let ((cpl (clos::class-precedence-list sub)))
                   (not (null (member c2 (cdr (member c1 cpl))))))))
        (or (eq c1 c2)
            (every #'in-order-at-subclass-p
                   (clos::intersection (subclasses* c1)
                                       (subclasses* c2))))))

;;; display defclass

;;; (export '(display-defclass))

(defun display-defclass (class-name)
    (printer-defclass (generate-defclass (find-class class-name)))
    (values))


(defun printer-defclass (expr)
    (print* 'defclass (first expr))
    (print* " " (second expr))
    (dolist (it (car (nthcdr 2 expr)))
        (print* "  " it)))


(defun generate-defclass (class)
    `(,(class-name class)
      ,(mapcar #'class-name (cdr (clos::class-precedence-list class)))
      ,(mapcar #'(lambda (slot)
                     (generate-inherited-slot-specification class slot))
               (clos::class-slots class))))

(defun generate-slot-specification (slot)
    `(,(clos::slot-definition-name slot)
      ,@(when (clos::slot-definition-initfunction slot)
            `(:initform ,(clos::slot-definition-initform slot)))
      ,@(when (clos::slot-definition-initargs slot)
            (clos::mapappend #'(lambda (initarg) `(:initarg ,initarg))
                             (clos::slot-definition-initargs slot)))
      ,@(unless (eq (clos::slot-definition-allocation slot) ':instance)
            `(:allocation ,(clos::slot-definition-allocation slot)))
      ,@(when (clos::slot-definition-readers slot)
            (clos::mapappend #'(lambda (reader) `(:reader ,reader))
                             (clos::slot-definition-readers slot)))
      ,@(when (clos::slot-definition-writers slot)
            (clos::mapappend #'(lambda (writer) `(:writer ,writer))
                             (clos::slot-definition-writers slot)))))

(defun generate-inherited-slot-specification (class slot)
    (let* ((source-class
             (find-if #'(lambda (superclass)
                            (find (clos::slot-definition-name slot)
                                  (clos::class-direct-slots superclass)
                                  :key #'clos::slot-definition-name))
                      (clos::class-precedence-list class)))
           (generated-slot-spec
             (generate-slot-specification slot)))
        (if (eq source-class class)
            generated-slot-spec
            (append generated-slot-spec
                    `(:inherited-from ,(class-name source-class))))))

;;; display generate-defgeneric
(defun generate-defgeneric (gf)
    `(defgeneric ,(clos::generic-function-name gf)
         ,(clos::generic-function-lambda-list gf)))

;;; (export '(display-defgeneric))
(defun display-defgeneric (arg)
    (let ((gf (if (symbolp arg) (clos::find-generic-function arg) arg)))
        (print (generate-defgeneric gf))
        (terpri)
        (values)))


;;; display-generic-function
(defun generate-defmethod (method &key show-body)
    `(defmethod ,(clos::generic-function-name (clos::method-generic-function method))
         ,@(clos::method-qualifiers method)
         ,(generate-specialized-arglist method)
         ,@(when show-body (list (clos::method-body method)))))

(defun generate-specialized-arglist (method)
    (let* ((specializers (clos::method-specializers method))
           (lambda-list (clos::method-lambda-list method))
           (number-required (length specializers)))
        (append (mapcar #'(lambda (arg class)
                              (if (eq class (find-class 't))
                                  arg
                                  `(,arg ,(class-name class))))
                        (subseq lambda-list 0 number-required)
                        specializers)
                (subseq lambda-list number-required))))


(defun generate-specialized-arglist-1 (specializers lambda-list)
    (let ((number-required (length specializers)))
        (append (mapcar #'(lambda (arg class)
                              (if (eq class (find-class 't))
                                  arg
                                  `(,arg ,(class-name class))))
                        (subseq lambda-list 0 number-required)
                        specializers)
                (subseq lambda-list number-required))))

;;; (export '(display-generic-function))
(defun display-generic-function (gf-name &key show-body)
    (display-defgeneric gf-name)
    (dolist (method (clos::generic-function-methods (clos::find-generic-function gf-name)))
        (print (generate-defmethod method :show-body show-body)))
    (values))


;;; all-generic
(defun all-generic-functions (&key names)
    (let ((result))
        (setq result
              (clos::remove-duplicates
               (clos::mapappend #'class-direct-generic-functions
                                (subclasses* (find-class 't)))))
        (if names
            (mapcar #'clos::generic-function-name result)
            result)))


(defun class-direct-generic-functions (class)
    (clos::remove-duplicates
     (mapcar #'clos::method-generic-function
             (clos::class-direct-methods class))))


;;; relevant generic function
(defun reader-method-p (method)
    (let ((specializers (clos::method-specializers method)))
        (and (= (length specializers) 1)
             (member (clos::generic-function-name (clos::method-generic-function method))
                     (clos::mapappend #'clos::slot-definition-readers
                                      (clos::class-direct-slots (car specializers)))
                     :test #'equal))))

(defun writer-method-p (method)
    (let ((specializers (clos::method-specializers method)))
        (and (= (length specializers) 2)
             (member (clos::generic-function-name (clos::method-generic-function method))
                     (clos::mapappend #'clos::slot-definition-writers
                                      (clos::class-direct-slots (cadr specializers)))
                     :test #'equal))))

(defun relevant-generic-functions (class ceiling &key elide-accessors-p (names nil))
    (let ((fclass (if (symbolp class) (find-class class) class))
          (fceiling (if (symbolp ceiling) (find-class ceiling) ceiling))
          (result))
        (setq result
              (clos::remove-duplicates
               (mapcar #'clos::method-generic-function
                       (remove-if
                        #'(lambda (m)
                              (and elide-accessors-p
                                   (or (reader-method-p m)
                                       (writer-method-p m))))
                        (clos::mapappend #'clos::class-direct-methods
                                         (clos::set-difference (clos::class-precedence-list fclass)
                                                               (clos::class-precedence-list fceiling)))))))
        (if names
            (mapcar #'clos::generic-function-name result)
            result)))

;;; EOF
