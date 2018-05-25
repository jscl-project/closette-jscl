;;; -*- mode:lisp; coding:utf-8 -*-

(defpackage :clos.banner
  (:use :cl))


(in-package :clos.banner)

(defparameter *ban*
  '(
    " "
    "   ____   _        ___    ____ "
    "  / ___| | |      / _ \\  / ___| "
    " | |     | |     | | | | \\___ \\"
    " | |___  | |___  | |_| |  ___) | "
    "  \\____| |_____|  \\___/  |____/"
    " "
    " "
    ))



(defun draw (&optional (banner *ban*))
    (dolist (it banner)
        (#j:jqconsole:Write it "jqconsole-output" nil)
        (terpri)))

(dotimes (i 6) (terpri))
(draw)

(in-package :cl-user)

;;; EOF
