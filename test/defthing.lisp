(load "macros"); test-defthings.lisp

(things 
  (defstruct person name age salary)
  (defstruct team  commander crew)) 

(defun make-team (who)
  (let ((persons (loop for (name yob role) in who collect 
		      (make-person name yob role))))
(%make-team :commander (first persons) :crew (rest persons))))

(defun make-person (name yob role)
(%make-person :name name :salary (role->salary role) :age  (- (this-year) yob)))
;-------------------------------------------------------------------------------
(defun role->salary (role)
  (cdr (assoc role '((commander . 30054) (walker . 18622 ) (pilot . 17147)))))
  
(defun this-year ()
  (sixth (multiple-value-list (get-decoded-time))))
;--------------------------------------------------------------------------------------
(let ((team (make-team '((neil 1930 commander) (buzz 1930 walker) (mike 1930 pilot)))))
  (oo team commander name))

; ==> NEIL
