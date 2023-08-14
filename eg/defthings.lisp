#|
## Defstruct

### Less is More
One of my design heroes is Deiter Rams, the german 
|#

; (unless (fboundp 'aif) (load "defmacro"))
; 
; (things
;   (defstruct num (n 0) (at 0) (txt "") (mu 0) (m2 0) (lo 1E32) (hi -1E32) (heaven 1))
;   (defstruct sym (n 0) (at 0) (txt "") (most 0) mode seen)
; 
; (defun make-num(&optional init &key (at 0) (txt " ")) 
;   (%make-name :at at :txt txt  
;               :heaven (if (eql #\- (char txt (1- (length txt)))) 0 1)))
; 
(defun rand (&optional (n 1))
  (setf *seed* (mod (* 16807.0d0 *seed*) 2147483647.0d0))
  (* n (- 1.0d0 (/ *seed* 2147483647.0d0))))

(defun rint (&optional (n 1) &aux (base 10000000000.0))
  (floor (* n (/ (rand base) base))))

(defstruct tri a b c)

(defmethod is ((i tri) ((s string)))
  (is i (read-from-string s nil nil)))
(defmethod is ((i tri) ((n number)))
  (with-slots (a c) i
    (assert (<= a n c) nil "out of range") 
    n)
(defmethod is ((i tri) ((n null)))
  (with-slots (a b c)  i
    (let ((c1 (- b a)/(- c a)))
      (is i (+ a  (* (- c b) (+ c1 (- (rand) c1) (sqrt (rand)))))))))

(defmethod mid ((i tri)) 
  (with-slots (a b c)  i
  (/ (+ a (* 4 b) c) 3))
(defmethod div ((i tri)) 
  (with-slots (a b c)  i
    (/ (+ (expt a 2) (expt b 2)  (expt c 2) (- (* a b) (* a c) (* b c))) 18)))

(defvar *settings*
  '((about "cutr"
           ("cutr: to understand 'it',  cut 'it' up, then seek patterns in"
            "the pieces. E.g. here we use cuts for multi- objective,"
            "semi- supervised, rule-based explanation."
            "(c) Tim Menzies <timm@ieee.org>, BSD-2 license"
            ""))
    (bins      "initial number of bins"     16)
    (bootstrap "bootstraps"                 256)
    (cliffs    "nonparametric small delta"  .147)
    (cohen     "parametric small delta"     .35)
    (file      "read data file"             "../data/auto93.csv")
    (go        "start up action"            help)
    (help      "show help"                  nil)
    (seed      "random number seed"         1234567891)
    (min       "min size"                   .5)
    (rest      "exapansion best to rest"    3)
    (top       "top items to explore"       10)
    (want      "optimization goal"          plan)))

(defmacro ? (x) `(caddr (assoc ',x *settings*)))
