(load "macros") ; test-has.lisp

(let (count)
  (dolist (x '(aa aa aa aa bb bb cc)) (incf (has x count)))
  (print count))
    
; => ((CC . 1) (BB . 2) (AA . 4))
