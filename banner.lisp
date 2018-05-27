;;; -*- mode:lisp; coding:utf-8 -*-


(in-package :clos.bin)

(defvar *clos-alive-time* (#j:Date:now))

;;; *clos-created*
;;; *clos-date-arg*
(export '(cl-user::lores-timestamp))
(lores-timestamp clos)

(defvar *clos-established* (apply 'jscl::make-new #j:Date *clos-date-arg*))

(defun compute-alive ()
    (let* ((ss0 (floor (/ (- (#j:Date:now) *clos-alive-time*) 1000)))
           (hms (/ ss0 3600))
           (hh (floor hms))
           (rmm  (* (- hms  hh) 60))
           (mm (floor rmm))
           (ss (#j:Math:ceil (* (- rmm mm) 60))))
        (values hh mm ss)))

(defun compute-exists (date-arg)
    (let* ((dt (apply #'jscl::make-new #j:Date date-arg))
           (ss0 (floor (/ (- (#j:Date:now) ((jscl::oget dt "getTime"))) 1000)))
           (hms (/ ss0 3600))
           (hh (floor hms))
           (rmm  (* (- hms  hh) 60))
           (mm (floor rmm))
           (ss (#j:Math:ceil (* (- rmm mm) 60))))
        (format nil "~a.h ~a.m ~a.s" hh mm ss)))


(defun make-lisp-info ()
    (format nil "~a ~a ~a"
            (lisp-implementation-type)
            (lisp-implementation-version)
            (jscl::compilation-notice)))


(defun date/component (obj key)
    (funcall ((jscl::oget obj key "bind") obj)))


(defun make-compile-date ()
    (let ((dt (jscl::make-new #j:Date)))
        (jscl::concat ""
                      (date/component dt "getFullYear") "/"
                      (1+ (date/component dt "getMonth")) "/"
                      (date/component dt "getDate") " "
                      (date/component dt "getHours") ":"
                      (date/component dt "getMinutes") ":"
                      (date/component dt "getSeconds") " ")))


#+nil (defparameter clos-compile-info
        (list
         "The JSCL port CLOSETTE system. Was compile the Lores from Moren environment"
         (format nil "~a" (make-lisp-info))
         (format nil "CLOS beta.02.0 built on ~a" (make-compile-date))))


(defparameter *clos-compile-info*
  (list
   "The JSCL port CLOSETTE system. CLOS version: beta.02.0"
   (format nil "This instance was compiled with the Lores from Moren environment at ~a" *clos-created*)
   (format nil "and exists ~a" (compute-exists *clos-date-arg*))))



(defun banner-info (banner)
    (let ((height (length banner))
          (width)
          (wl))

        (setq width
              (apply 'min (dolist (it banner wl)
                              (push (length it) wl))))
        (values height width) ))

(defun banner-from-lib (number)
    (let* ((num (if number number (random (length *banlib*))))
           (banner (copy-list (nth num *banlib*))))
        (multiple-value-bind (height width) (banner-info banner)
            (list height width banner))))



(defun printer (info)
    (#j:jqconsole:Write info "jqconsole-output" nil)
    (terpri))


;;; comile info
(defun compile-info (&optional number)
    (dolist (it (glue-compile (banner-from-lib number)))
        (printer it))
    (terpri)
    (terpri)
    (values))


#+nil (defparameter clos-compile-info
        (list
         "The JSCL port CLOSETTE system. Was compile the Lores from Moren environment"
         (format nil "~a" (make-lisp-info))
         (format nil "CLOS beta.02.0 built on ~a" (make-compile-date))))


(defun glue-compile (ban-elt &optional (glue *clos-compile-info*))
    (let ((glue-line '(0 1 2))
          (pos 0)
          (pad))
        (DESTRUCTURING-BIND (height width banner) ban-elt
            (cond ((>= height 6) (setq glue-line '(1 3 4)))
                  ((= height 5) (setq glue-line '(1 2 3)))
                  ((= height 4) (setq glue-line '(0 2 3))))
            (setq pad (make-string 3))
            (dotimes (idx (length glue))
                (setq pos (nth idx glue-line))
                (setf (nth pos banner)
                      (jscl::concat (nth pos banner)
                                    pad
                                    (nth idx glue))))
            banner)))


;;; alive info

(export '(alive))

(defun alive (&optional number)
    (terpri)
    (dolist (it (glue-alive (banner-from-lib number)))
        (printer it))
    (terpri)
    (terpri)
    (values))

(defun glue-alive (banner)
    (glue-compile banner
                  (list
                   (multiple-value-bind (hh mm ss) (compute-alive)
                       (format nil "Alive time ~a.h ~a.m ~a.s" hh mm ss))
                   (format nil "Classes ~a " (class-alive-info))
                   (format nil "Generic functions ~a" (generic-alive-info)))))

(defun class-alive-info ()
    (hash-table-count clos::*class-table*))

(defun generic-alive-info ()
    (hash-table-count clos::*generic-function-table*))


(in-package :cl-user)

(dotimes (i 6) (terpri))
(clos.bin::compile-info)


;;; EOF
