(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

; e.g.

(let ((n 0))
  (while (< n 10) (print (incf n))))

(defmacro until (test &body body)
  `(while (not ,test) ,@body))


(let ((n 0))
  (until (= n 10) (print (incf n))))
