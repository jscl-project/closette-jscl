# closette
Closette - a toy Common Lisp Object System (CLOS) described in The Art of the Metaobject Protocol (AMOP)

Modified from original for JSCL.

Compilation and running was done in Moren environment. 

## Test generic

```lisp

(defgeneric mctest (x))
(defmethod mctest :around ((x integer))
  (format t "(:around integer)")
  (call-next-method))
(defmethod mctest :around ((x number))
  (format t "(:around number)")
  (call-next-method))
(defmethod mctest :before ((x number))
  (format t "(:before number)"))
(defmethod mctest  ((x number))
  (format t "(primary number)")
  (1+ (call-next-method)))
(defmethod mctest :after ((x number))
  (format t "(:after number)"))
(defmethod mctest :before ((x t))
  (format t "(:before t)"))
(defmethod mctest  ((x t))
  (format t "(primary t)")
  100)
(defmethod mctest :after ((x t))
  (format t "(:after t)"))

(mctest 1)
;; =>
(:around integer)(:around number)(:before number)(:before t)(primary number)(primary t)(:after t)(:after number)101

```


