;;;
;;; Utilities
;;;


(in-package :cl-user)

(defmacro setf* (var &rest expr)
    `(progn
         (if (setf ,var ,@expr) t nil)))

;;; (safe (find-class ...))
;;;
(defmacro safe (&rest body) `(prog1 t ,@body))



(in-package :closette)


(defvar *logtrace* nil)

(defun logtrace (&optional mode)
    (if mode
        (if (eq mode 'on) (setq *logtrace* t)
            (if (eq mode 'off) (setq *logtrace* nil) 'unknow-trace-mode))
        *logtrace*))


(defun log-trace (note class)
    (when *logtrace*
        (#j:console:log
         (if (symbolp note) (symbol-name note))
         (print-object*  class)))
    (values))


(defun print-obj* (head things)
    (princ '<)
    (princ head)
    (princ " ")
    (dolist (it things)
        (princ it)
        (princ " "))
    (princ '>)
    (terpri))


(defun print-object* (std &optional stream)
    (cond ((std-instance-p std)
           (let* ((class (std-instance-class std))
                  (slots (std-instance-slots std))
                  (str-class (print-object-class-name std))
                  (str-slots (print-object-slots slots)))
               (if stream
                   (progn
                       (print-obj* str-class str-slots)
                       (values))
                   (format nil "~a ~a" str-class str-slots))))))

(defun print-object-slots (slot)
    (unless (arrayp slot)
        (error "its not slot"))
    (let* ((size (length slot))
           (place)
           (result))
        (dotimes (idx size)
            (setq place (aref slot idx))
            (cond ((std-instance-p place)
                   (push (print-object-class-name place) result))
                  ((consp place)
                   (push "(..)" result))
                  ((arrayp place)
                   (push "#(..)" result))
                  ((or (numberp place)
                       (stringp place)
                       (symbolp place))
                   (push place result))
                  (t push '@ result)))
        (reverse result)))

(defun print-object-class-name (place)
    (format nil "{~a ~a}" (class-name (class-of place))
            (class-name (class-of (class-of place))) ))

;;; keywords trace
(defun key-trace (fn keys)
    (when *logtrace
        (let ((fname (if (symbolp fn) (symbol-name fn) fn))
              (result))
            (setq result (mapcar (lambda (x) (if (symbolp x) (symbol-name x) t)) keys))
            (#j:console:log "Keys " fname (format nil "~a" result))))
    (values))



;;; kludge for (defun (setf name) ...) syntax
(defun setf-function-symbol (spec)
    (if (consp spec)
        (let ((print-name (write-to-string spec)))
            (intern print-name
                    (symbol-package (cadr spec))))
        spec))

(defmacro defun* (name forms &rest body)
    (cond ((symbolp name)
           `(defun ,name ,forms ,@body))
          ((and (consp name) (eq (car name) 'setf))
           (let ((sfn (setf-function-symbol name)))
               `(progn
                    (defun ,sfn ,forms ,@body)
                    (defsetf ,(cadr name) ,sfn))))
          (t (error "defun* ~a unknow function specifier" name))))


;;; kludge for defsetf generic accessor
(defun make-setf-pair (access-fn update-fn)
    (PUSH (CONS access-fn
                (LAMBDA (&REST arguments)
                    (DESTRUCTURING-BIND (&REST ARGS) arguments
                        (LET ((G!NEW (GENSYM))
                              (G!ARGS (MAPCAR (LAMBDA (S)(GENSYM)) ARGS)))
                            (VALUES
                             G!ARGS
                             ARGS
                             (LIST G!NEW)
                             (CONS update-fn
                                   (APPEND G!ARGS (LIST G!NEW)))
                             (CONS access-fn  G!ARGS))))))
          JSCL::*SETF-EXPANDERS*)
    (values))


;;; (setf (cadr lst) something)
(defun* (setf cadr) (lst value) (rplaca (cdr lst) value))


;;; kludge to rotatef
(defmacro psetf (&rest assignments)
    (when assignments
        (when (null (cdr assignments))
            (error "PSETF odd number of arguments"))
        (let ((value (gensym)))
            `(let ((,value ,(cadr assignments)))
                 (psetf ,@(cddr assignments))
                 (setf ,(car assignments) ,value)
                 nil))))

;;; dumb rotatef
(defmacro rotatef (&rest places)
    (when places
        (let ((args '()))
            (dolist (x places)
                (push x args)
                (push x args))
            (push (first places) args)
            (setf args (nreverse args))
            (setf (car args) 'psetf)
            `(progn ,args 'nil))))



(defmacro print-unreadable-object((object stream &key type identity) &body body)
    `(let ((.stream. ,stream)
           (.object. ,object))
         (format .stream. "#<")
         ,(when type
              '(format .stream. "~S" (type-of .object.)))
         ,(when (and type (or body identity))
              '(format .stream. " "))
         ,@body
         ,(when (and identity body)
              '(format .stream. " "))
         (format .stream. ">")
         nil))

;;;

(defun get-keyword-from (args key &optional default)
    (let ((val (getf args key)))
        (if val val default)))

(defun get-properties (plist indicator)
    (do ((it  plist (cddr it)))
        ((null it)
         (values nil nil nil))
        (when (member (car it) indicator)
            (return (values (first it) (second it) it)))))



;;; push-on-end is like push except it uses the other end:
(defmacro push-on-end (value location)
    `(setf ,location (nconc ,location (list ,value))))

;;; (setf getf*) is like (setf getf) except that it always changes the list,
;;;              which must be non-nil.
;;; defsetf getf*

#|
(defun setf-getf* (plist key new-value)
    (block body
        (do ((x plist (cddr x)))
            ((null x))
            (when (eq (car x) key)
                (setf (car (cdr x)) new-value)
                (return-from body new-value)))
        (push-on-end key plist)
        (push-on-end new-value plist)
        new-value))

(defsetf getf* setf-getf*)
|#

(defun* (setf getf*) (plist key new-value)
    (block body
        (do ((x plist (cddr x)))
            ((null x))
            (when (eq (car x) key)
                (setf (car (cdr x)) new-value)
                (return-from body new-value)))
        (push-on-end key plist)
        (push-on-end new-value plist)
        new-value))



;;; mapappend is like mapcar except that the results are appended together:
(defun mapappend (fun &rest args)
    (if (some #'null args)
        ()
        (append (apply fun (mapcar #'car args))
                (apply #'mapappend fun (mapcar #'cdr args)))))

;;; mapplist is mapcar for property lists:
(defun mapplist (fun x)
    (if (null x)
        ()
        (cons (funcall fun (car x) (cadr x))
              (mapplist fun (cddr x)))))


;;; set-difference
(defun set-difference (list1 list2 &key key (test #'eq))
    (cond (list2
           (let ((result '()))
               (dolist (it list1)
                   (when (not (member it list2 :key key :test test))
                       (push it result)))
               result))
          (t list1)))

;;; union
(defun union (list1 list2 &key key (test #'eq))
    (cond ((and list1 list2)
           (let ((result list2))
               (dolist (it list1)
                   (when (not (member it list2 :key key :test test))
                       (push it result)))
               result))
          (list1)
          (list2)))



;;; intersection
(defun intersection (list1 list2 &key key (test #'eql))
    (let ((result '()))
        (dolist (it list1)
            (when (member it list2 :key key :test test )
                (push it result)))
        result))


;;; remove-duplicates bug:
(defun remove-duplicates (sequence &key from-end (test 'eql) test-not (key 'identity) (start 0) end)
    (when (or (not (eql start 0))
              end)
        (setq sequence (subseq sequence start end)))
    (if from-end
        (do* ((result (cons nil nil))
              (tail result)
              (i sequence (cdr i)))
             ((null i)
              (cdr result))
            (unless (member (car i) (cdr result) :test test :key key)
                (setf (cdr tail) (cons (car i) nil)
                      tail (cdr tail))))
        (do* ((result (cons nil nil))
              (tail result)
              (i sequence (cdr i)))
             ((null i)
              (cdr result))
            (unless (member (car i) (cdr i) :test test :key key)
                (setf (cdr tail) (cons (car i) nil)
                      tail (cdr tail))))))

;;;
(defun find-if-not (predicate sequence &key key)
    (find-if (complement predicate) sequence :key key))


;;; sort
(defun sort1 (seq fn key)
    (cond ((endp seq) '())
          (t (do* ((ipos seq (cdr ipos))
                   (imin ipos ipos))
                  ((null ipos) seq)
                 (do ((i (cdr ipos) (cdr i)))
                     ((null i))
                     (when (funcall fn (funcall key (car i)) (funcall key (car imin)))
                         (setf imin i)))
                 (when (not (eq imin ipos))
                     (let ((old-ipos (car ipos))
                           (old-imin (car imin)))
                         (setf (car ipos) old-imin
                               (car imin) old-ipos)))))))

(defun sort (seq fn &key key)
    (unless key (setf key 'identity))
    (sort1 seq fn  key))


;;; every with more sequences
(defun every1 (predicate first-seq &rest more-sequences)
    (apply #'map nil (lambda (&rest seqs)
                         (when (not (apply predicate seqs))
                             (return-from every1 nil)))
           first-seq more-sequences)
    t)


;;; EOF
