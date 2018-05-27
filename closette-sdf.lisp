;;; -*- mode:lisp; coding:utf-8 -*-

;;; System CLOSETTE definition file
;;; for compilation with LORES:QLOAD
;;;

(lores:defsys :closette
    :path "git/jscl-closette"
    :components ((:file "package")
                 (:file "utils")
                 (:file "fonts")
                 (:file "banner")
                 (:file "closette")
                 (:file "closette-boot")
                 (:file "closette-final")
                 (:file "tools")))

;;; eof
