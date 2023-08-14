(let ((seed 1234567891))
  (defun srand (&optional (n 1234567891)) (setf seed n))

  (defun rand (&optional (n 1))
    (setf seed (mod (* 16807.0d0 seed) 2147483647.0d0))
    (* n (- 1.0d0 (/ seed 2147483647.0d0))))

  (defun rint (&optional (n 1) &aux (base 10000000000.0))
    (floor (* n (/ (rand base) base)))))

(defun args () 
  #+clisp ext:*args*  
  #+sbcl sb-ext:*posix-argv*)

(defun stop (&optional (status 0)) 
  #+clisp (ext:exit status) 
  #+sbcl  (sb-ext:exit :code status))


