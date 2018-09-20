;;;
;;; Utilities
;;;


;;;(in-package :amop)

;;; print*
(defun print* (&rest args)
    (dolist (it args)
        (princ it) (princ " "))
    (terpri))


;;; print unreadable
(defun print-obj* (head things)
    (princ '<)
    (princ head)
    (princ " ")
    (dolist (it things)
        (princ it)
        (princ " "))
    (princ '>)
    (terpri))


;;;
(defun print-object* (std &key stream)
    (cond ((std-instance-p std)
           (let* ((class (std-instance-class std))
                  (slots (std-instance-slots std))
                  (str-class (print-object-class-name std))
                  (str-slots (print-object-slots slots)))
               (if stream
                   (progn
                       (print-obj* str-class str-slots)
                       (values))
                   (format nil "~a ~a" str-class str-slots))))
          (t (if stream
                 (progn
                     (print std) (values))
                 (format nil "~a" std)
                 )) ))


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
                  (t (push '@ result))))
        (reverse result)))

(defun print-object-class-name (place)
    (format nil "{~a ~a}" (class-name (class-of place))
            (class-name (class-of (class-of place))) ))


;;; print support
(defun mop-object-slots (slot)
    (unless (arrayp slot)
        (error "its not slot"))
    (let* ((size (length slot))
           (place)
           (result))
        (dotimes (idx size)
            (setq place (aref slot idx))
            (cond ((std-instance-p place)
                   (push (mop-object-class-name place) result))
                  ((consp place)
                   (push "(..) " result))
                  ((arrayp place)
                   (push "#(..) " result))
                  ((or (numberp place)
                       (symbolp place))
                   (push (concat (string place) " ") result))
                  ((stringp place)
                   (push (concat place " ") result))
                  (t (push "@ " result))))
        (apply 'concat (reverse result))))

(defun mop-object-class-name (place)
    ;;(#j:console:log :obj-class-name place)
    (concat (string (!class-name (!class-of place)))
            " "
            (string (!class-name (!class-of (!class-of place)))) ))

(defun mop-object-printer (form stream)
    (let ((res (case (!class-name (!class-of form))
                 (standard-class
                  (concat "#<standard-class " (string (!class-name form)) ">"))
                 (standard-generic-function
                  (concat "#<standard-generic-function " (string (generic-function-name form)) ">"))
                 (standard-method
                  (concat "#<standard-method " (string (generic-function-name (method-generic-function form)))
                          (write-to-string (generate-specialized-arglist form)) ">"))
                 (otherwise
                  (concat "#<instance " (string (!class-name (!class-of form))) " "
                          (mop-object-slots (std-instance-slots form)) ">")))))
        #+nil(simple-format stream "~a" res)
        (simple-format stream res)))


;;; hash-table printer
(defun hash-table-object-printer (form stream)
    (let* ((hashfn (cadr form))
           (fname (oget hashfn "fname"))
           (tail-pos (position #\- fname))
           (testfn (subseq fname 0 tail-pos))
           (res))
        (setq res (concat "#<hash-table :test " (string-downcase testfn) ">"))
        (simple-format stream res)))


;;; structure printer
(defun structure-object-printer (form stream)
    (let ((res))
        (setq res (concat "#<structure " (string-downcase (string (car form))) ">"))
        (simple-format stream res)))




;;; kludge for (defun (setf name) ...) syntax
(defun setf-function-symbol (spec)
    (if (consp spec)
        (let ((print-name (write-to-string spec)))
            (intern print-name
                    (symbol-package (cadr spec))))
        spec))


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



(defmacro !print-unreadable-object((object stream &key type identity) &body body)
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


;;; push-on-end is like push except it uses the other end:
(defmacro push-on-end (value location)
    `(setf ,location (nconc ,location (list ,value))))

;;; (setf getf*) is like (setf getf) except that it always changes the list,
;;;              which must be non-nil.

(defun (setf getf*) (new-value plist key)
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

;;; find-if-not
(defun !find-if-not (predicate sequence &key key)
    (find-if (complement predicate) sequence :key key))


;;; sort
;;; https://en.wikipedia.org/wiki/Selection_sort
(defun sort1 (lst fn &key (key 'identity))
    (if (endp lst) '()
        (block selection-sort
            (let* ((j lst)
                   (imin j))
                (while t
                    (when (null j)
                        (return lst))
                    (tagbody
                       (let ((i (cdr j)))
                           (while t
                               (if (null i) (return nil))
                               (if (funcall fn
                                            (funcall key (car i))
                                            (funcall key (car imin)))
                                   (setq imin i))
                               (setq i (cdr i))))
                       (when (not (eq imin j))
                           (let ((swap-j (car j))
                                 (swap-imin (car imin)))
                               (setf (car j) swap-imin (car imin) swap-j))))
                    (setq j (cdr j) imin j)))) ))


;;; every with more sequences
(defun every1 (predicate first-seq &rest more-sequences)
    (apply #'map nil (lambda (&rest seqs)
                         (when (not (apply predicate seqs))
                             (return-from every1 nil)))
           first-seq more-sequences)
    t)


;;; EOF
