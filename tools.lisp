;;; -*- mode:lisp; coding:utf-8 -*-

;;; debug tools
;;; From closette-test.lisp

(in-package :cl-user)

(defun subclasses* (class)
    (closette::remove-duplicates
     (cons class
           (closette::mapappend #'subclasses*
                                (closette::class-direct-subclasses class)))))

(export '(subclasses))
(defun subclasses (class &optional names)
    (let ((result (remove class (subclasses* class))))
        (if names (mapcar #'closette::class-name result)
            result)))

(export '(in-order-p))
(defun in-order-p (c1 c2)
    (flet ((in-order-at-subclass-p (sub)
               (let ((cpl (closette::class-precedence-list sub)))
                   (not (null (member c2 (cdr (member c1 cpl))))))))
        (or (eq c1 c2)
            (every #'in-order-at-subclass-p
                   (closette::intersection (subclasses* c1)
                                           (subclasses* c2))))))

;;; display defclass

(export '(display-defclass))

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
      ,(mapcar #'class-name (cdr (closette::class-precedence-list class)))
      ,(mapcar #'(lambda (slot)
                     (generate-inherited-slot-specification class slot))
               (closette::class-slots class))))

(defun generate-slot-specification (slot)
    `(,(closette::slot-definition-name slot)
      ,@(when (closette::slot-definition-initfunction slot)
            `(:initform ,(closette::slot-definition-initform slot)))
      ,@(when (closette::slot-definition-initargs slot)
            (closette::mapappend #'(lambda (initarg) `(:initarg ,initarg))
                                 (closette::slot-definition-initargs slot)))
      ,@(unless (eq (closette::slot-definition-allocation slot) ':instance)
            `(:allocation ,(closette::slot-definition-allocation slot)))
      ,@(when (closette::slot-definition-readers slot)
            (closette::mapappend #'(lambda (reader) `(:reader ,reader))
                                 (closette::slot-definition-readers slot)))
      ,@(when (closette::slot-definition-writers slot)
            (closette::mapappend #'(lambda (writer) `(:writer ,writer))
                                 (closette::slot-definition-writers slot)))))

(defun generate-inherited-slot-specification (class slot)
    (let* ((source-class
             (find-if #'(lambda (superclass)
                            (find (closette::slot-definition-name slot)
                                  (closette::class-direct-slots superclass)
                                  :key #'closette::slot-definition-name))
                      (closette::class-precedence-list class)))
           (generated-slot-spec
             (generate-slot-specification slot)))
        (if (eq source-class class)
            generated-slot-spec
            (append generated-slot-spec
                    `(:inherited-from ,(class-name source-class))))))



;;; display generate-defgeneric
(defun generate-defgeneric (gf)
    `(defgeneric ,(closette::generic-function-name gf)
         ,(closette::generic-function-lambda-list gf)))

(export '(display-defgeneric))
(defun display-defgeneric (arg)
    (let ((gf (if (symbolp arg) (closette::find-generic-function arg) arg)))
        (print (generate-defgeneric gf))
        (terpri)
        (values)))


;;; display-generic-function
(defun generate-defmethod (method &key show-body)
    `(defmethod ,(closette::generic-function-name (closette::method-generic-function method))
         ,@(closette::method-qualifiers method)
         ,(generate-specialized-arglist method)
         ,@(when show-body (list (closette::method-body method)))))

(defun generate-specialized-arglist (method)
    (let* ((specializers (closette::method-specializers method))
           (lambda-list (closette::method-lambda-list method))
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

(export '(display-generic-function))
(defun display-generic-function (gf-name &key show-body)
    (display-defgeneric gf-name)
    (dolist (method (closette::generic-function-methods (closette::find-generic-function gf-name)))
        (print (generate-defmethod method :show-body show-body)))
    (values))


;;; all-generic
(defun all-generic-functions (&key names)
    (let ((result))
        (setq result
              (closette::remove-duplicates
               (closette::mapappend #'class-direct-generic-functions
                                    (subclasses* (find-class 't)))))
        (if names
            (mapcar #'closette::generic-function-name result)
            result)))


(defun class-direct-generic-functions (class)
    (closette::remove-duplicates
     (mapcar #'closette::method-generic-function
             (closette::class-direct-methods class))))


;;; relevant generic function
(defun reader-method-p (method)
    (let ((specializers (closette::method-specializers method)))
        (and (= (length specializers) 1)
             (member (closette::generic-function-name (closette::method-generic-function method))
                     (closette::mapappend #'closette::slot-definition-readers
                                          (closette::class-direct-slots (car specializers)))
                     :test #'equal))))

(defun writer-method-p (method)
    (let ((specializers (closette::method-specializers method)))
        (and (= (length specializers) 2)
             (member (closette::generic-function-name (closette::method-generic-function method))
                     (closette::mapappend #'closette::slot-definition-writers
                                          (closette::class-direct-slots (cadr specializers)))
                     :test #'equal))))

(defun relevant-generic-functions (class ceiling &key elide-accessors-p (names nil))
    (let ((fclass (if (symbolp class) (find-class class) class))
          (fceiling (if (symbolp ceiling) (find-class ceiling) ceiling))
          (result))
        (setq result
              (closette::remove-duplicates
               (mapcar #'closette::method-generic-function
                       (remove-if
                        #'(lambda (m)
                              (and elide-accessors-p
                                   (or (reader-method-p m)
                                       (writer-method-p m))))
                        (closette::mapappend #'closette::class-direct-methods
                                             (closette::set-difference (closette::class-precedence-list fclass)
                                                                       (closette::class-precedence-list fceiling)))))))
        (if names
            (mapcar #'closette::generic-function-name result)
            result)))

;;; EOF
