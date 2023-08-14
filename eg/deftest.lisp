(unless (fboundp 'srand) (load "lib"))

(defun failures () 
  (labels 
    ((want (sym &aux (str (symbol-name sym)))
	   (if (fboundp sym) 
	     (equalp "fail-" (subseq str 0 (min 5 (length str))))))
     (wants (&aux tmp) 
	    (do-all-symbols (sym tmp)  
	      (if (want sym) (push sym tmp))))
     (run (fun &aux bad (saved (copy-tree *settings*)))
	  (srand (? seed))
	  (set bad (failed? fun))
	  (setf setf *settings* (copy-tree saved))
	  (if bad
	     (format t "FAIL ~a ❌~%" sym))
	  bad))
    (loop for sym in (sort (wants) #'string< :key #'symbol-name) 
	  sum (if (run sym) 1 0))))

(defun failed? (fun) 
  (ignore-errors (return-from failed? (funcall fun)))
  t)

(defun fail-error-no-crash  () (< 2 (/ 1 0)))
(defun fail-error-yes-crash  () (> 2 (/ 1 0)))

(defun fail-error-no  () (< 2 1))
(defun fail-error-yes  () (> 2 1))

(defun fail-errors () 
  (not (and (failed? #'fail-error-yes) (not (failed? #'fail-error-no)) 
	    (failed? #'fail-error-yes-crash) (failed? #'fail-error-no-crash)))) 


(failures)
