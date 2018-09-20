;;; -*- mode:lisp; coding:utf-8 -*-

;;(in-package :amop)


(push :mop *features*)

#+nil
(defmacro defclass (name direct-superclasses direct-slots &rest options)
    `(progn
         (ensure-class ',name
                       :direct-superclasses
                       ,(canonicalize-direct-superclasses direct-superclasses)
                       :direct-slots
                       ,(canonicalize-direct-slots direct-slots)
                       ,@(canonicalize-defclass-options options))
         ',name))


(defmacro defclass (name direct-superclasses direct-slots &rest options)
    ;;`(progn
    `(ensure-class ',name
                   :direct-superclasses
                   ,(canonicalize-direct-superclasses direct-superclasses)
                   :direct-slots
                   ,(canonicalize-direct-slots direct-slots)
                   ,@(canonicalize-defclass-options options)))
;;',name))


#+nil
(defmacro defgeneric (function-name lambda-list &rest options)
    `(prog1 ',function-name
         (!ensure-generic-function
          ',function-name
          :lambda-list ,(canonicalize-defgeneric-ll lambda-list)
          ,@(canonicalize-defgeneric-options options))))

(defmacro defgeneric (function-name lambda-list &rest options)
    ;;`(prog1 ',function-name
    `(!ensure-generic-function
      ',function-name
      :lambda-list ,(canonicalize-defgeneric-ll lambda-list)
      ,@(canonicalize-defgeneric-options options)))



(defmacro defmethod (&rest args)
    (multiple-value-bind (function-name qualifiers lambda-list specializers body)
        (parse-defmethod args)
        ;;`(prog1 ',function-name
        `(ensure-method (find-generic-function ',function-name)
                        :lambda-list ,(canonicalize-defgeneric-ll lambda-list)
                        :qualifiers ,(canonicalize-defgeneric-ll qualifiers)
                        :specializers ,(canonicalize-specializers specializers)
                        :body ',body)))


(defmacro with-slots ((&rest slots) instance-name &body forms)
    (let ((instance (gensym)))
        `(let ((,instance ,instance-name))
             (symbol-macrolet
                 ,(loop for slot-name in slots
                        collect (if (symbolp slot-name)
                                    `(,slot-name
                                      (!slot-value ,instance ,slot-name))
                                    `(,(first slot-name)
                                      (!slot-value ,instance ,(second slot-name)))))
                 ,@forms))))


(defmacro with-accessors ((&rest Readers) instance-name &body forms)
    (let ((instance (gensym)))
        `(let ((,instance ,instance-name))
             (symbol-macrolet
                 ,(loop for (var reader) in Readers
                        collect `(,var (,reader ,instance)))
                 ,@forms))))



(jscl::fset 'class-of (fdefinition '!class-of))
(jscl::fset 'class-name (fdefinition '!class-name))
(jscl::fset 'find-class (fdefinition '!find-class))
(jscl::fset 'slot-value (fdefinition '!slot-value))
(jscl::fset 'slot-boundp (fdefinition '!slot-boundp))
(jscl::fset 'slot-makunbound  (fdefinition '!slot-makunbound))
(jscl::fset 'slot-exists-p  (fdefinition '!slot-exists-p))
(jscl::fset 'slot-exists-p  (fdefinition '!slot-exists-p))



(jscl::fset 'ensure-generic-function (fdefinition '!ensure-generic-function))
(jscl::fset 'find-method (fdefinition '!find-method))
(jscl::fset 'add-method (fdefinition '!add-method))
(jscl::fset 'remove-method (fdefinition '!remove-method))


(jscl::fset 'method-qualifiers (fdefinition '!method-qualifiers))
(jscl::fset 'method-specializers (fdefinition '!method-specializers))





;;; EOF
