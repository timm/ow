#|
# Defmacro

!!! summary  "TL;DR"
    As shown by the examples here,
    LISP's
    macro system makes it trivial to extend the language.

    Like any other power tool, macros need to
    be used with care.
    [Google's LISP style guide](https://google.github.io/styleguide/lispguide.xml?showone=Macros#Macros)
    cautions that  macros should  be used sparingly.
    For example, in   1000 lines of my own LISP code, there might only
    be 30 (ish) lines of macros.  But even if I don't write macros all
    the time, the key here is that, with LISP, the door is always open
    to creating new and powerful and succinct abstractions.

On this page:

|Macro |:|  Notes |
|----------------------------------------------:|:|:-------|
|[`aif`](#anaphoric-if)                         |:| (anaphoric if) for accessing a conditional without having to recompute it|
|[`o`](#nested-slot-accessors)                 |:| easy  access to nested slots|
|[`defthing, defthings`](#saner-simpler-objects)|:| fixes drawbacks with defstruct and OO in LISP  |
|[`has`](#symbol-counts)                        |:| simplifying  symbol counting (for key sizes of 50 or less)|
|[`with-csv`](#csv-reader)                      |:| easy processing of csv files |

[TOC]

## Attack of the Walrus

Do you think you don't need the flexibility of macros?
OK, then lets take a look at what happens in languages _without_ that  flexibility.

Who remembers the bitter feud
over
the walrus operator (`:=`) in Python3? 
That operator 
allows assignments as part of expression evaluation. 
That way, if you need the result of a conditional, you do not have to run
that test again. For example, in Python, without walrus:

     x := someBigLongCalculation()
     if x: handle(x)

But with walrus:

     if x := someBigLongCalculation(): handle(x)

All in all it is a pretty minor addition to Python.
Even so, the walrus operator was hotly debated and
there were some very nasty social media posts
about the way the issue was decided.
The discussion got so toxic that the leader of the Python community,
Guido van Rossum, https://hub.packtpub.com/why-guido-van-rossum-quit/[quit the Python project].
"Now that (walrus) is done, I don't ever want to have to fight so hard for a 
(change)  and find that so many people despise my decisions.", he said.

## Anaphoric If

To a LISPer, that whole debate about the walrus operator is just insane.
If you want the walrus, it can be added with just two lines of code.   
|#
(defmacro aif (test this &optional that)
  `(let ((it ,test))            
     (if it ,this ,that)))  
#|
If the above, note that:

1. `Defmacro` returns a list that replaces the original list (and LISP interprets that new list as code).
2. In that code, the \` backtick defines a toggle environment where symbols are not evaluated...
3. Unless proceeded by a `,` comma. Backticks lets us mix in names passed into the macro
   (in this case, the actual code of the condition `test` as well as what to do in the `this` and
   `that branch`).

(Also, not shown above, the idiom `,@list` means create the list and lay it out flat.)

The above `defmacro`
lets us trap the results of `test`  into `it`, then use `it` later.

    (aif (big-long-calculation)
      (foo it))

Note that this change can be made to your local
LISP without having to lobby some central committee. No drama.
And if you don't like the `aif` macro? Fine, just don't use it.

## Macro Basics

Most things are LISP are lists, even the code.
Macros are functions (called at load time) that return lists which the LISP interprets as code.
So macros are code that rewrites code.

Macros are  not so much "coded" so much as they are "drawn". For example, the above `aif` definition,
  the last line shows the code that is desired.
For another example of "drawing a macro", suppose someone had been nice enough to define a `while` macro for you:

     (defmacro while (test &body body)
       `(do ()
            ((not ,test))
          ,@body))
      
     ; e.g. print numbers 1,2,3... 10
     (let ((n 0))
       (while (< n 10) (print (incf n))))

Then you could imagine an `until` macro that was just a `not while`-- which you could draw up as a new macro like
this:

     (defmacro until (test &body body)
       `(while (not ,test) ,@body))
        
     ; e.g. print numbers 1,2,3... 10
     (let ((n 0))
       (until (= n 10) (print (incf n))))

LISP makes extensive use of macros. For example, here's the expansion of
a seemingly simple `dotimes` call.  Note that `dotimes` expands into a set of goto statements
(and, by the way,  the funny symbols (e.g. `#:LOOP-2860`) are variables created to handle some processing in the code). 

     (pprint (macroexpand '(dotimes (i 10) (print i)))) 
        
     ; ==>
      (BLOCK NIL
       (LET ((I 0))
         (TAGBODY #:LOOP-2860       
               (IF (>= I 10) 
                   (GO #:END-2861))  
               (PRINT I) 
               (PSETQ I (1+ I)) 
               (GO #:LOOP-2860) 
            #:END-2861
               (RETURN-FROM NIL (PROGN NIL)))))

Here's a more interesting example.
For PYTHON programmers, I'll say the following is like using a context manager
    for reaching a file. That is to say, when reading files, the `with-open-file` this  macro ensures:

1. The file is open before any reading starts (see the initial call to `open`);
2. No find streams
    are left open and dangling, even if there is a code crash (see the use of `unwind-protect`); 
3. When terminating, or cleaning up after a crash, the last 2 lines of the expansion
    keeps  shouting at the file stream until it closes. Which is exactly what we want to happen.

Here we go:

    (pprint (macroexpand '(with-open-file (s f) (print (read s)))))
       
    ; ==>
    (LET ((S (OPEN F))) (DECLARE (SYSTEM::READ-ONLY S)) 
     (UNWIND-PROTECT                                   
       (MULTIPLE-VALUE-PROG1 (PROGN (PRINT (READ S)))
          (WHEN S (CLOSE S)))                         
            (WHEN S (CLOSE S :ABORT T))))

The end of this page has some other tutorial material
on macros (e.g. traps for macro newbies).
And if you need more details that that, 
go see the 
[LISP cookbook on macros](https://lispcookbook.github.io/cl-cookbook/macros.html)
which has some really good tutorial material.


## Nested Slot Accessors

Consider  
nested accesses to a field inside a struct;  e.g. the `streetNum` of the `address` of
the `home` of the `manager` of the `company`. In standard LISP, that could be done with:

    (slot-value 
       (slot-value 
          (slot-value 
             (slot-value *company 'manager) 'home) 'address) 'streetNum)

That's a little verbose, right? So lets fix that with a macro.
This is a recursive macro (which is a little tricky) that works front to back over a list of slots. 
The first slot becomes the inner most accessors and accessors to the other slots are wrapped around it.
|#
(defmacro o (struct slot &rest slots) 
   (if slots
     `(o (slot-value ,struct ',slot) ,@slots)  ; case one: we have to recurs
     `(slot-value ,struct ',slot)))  ; case two: no slots left, so just do an access.
#|
With this macro, the above  example becomes something much more palatable.

    (o *company* manager home address streetNum)

One common idiom is to slip in a print statement to view the contents of a struct.
The following `oo` macro handles that (and note that it returns the struct so you can slip it in, get the print, and still
				 carry on processing the struct).
|#
(defmacro oo (struct slot &rest slots)
  `(progn (print (o ,struct ,slot ,@slots)) 
          ,struct))
#|

## Saner, Simpler,  Objects

Like many people,  I have... issues... with the CLOS object system. 
It can be so verbose to (e.g.) define and new class, or specialize the initialization of  a new instance.
Worse, the functions that (e.g.) access the slot names of an instance vary from implementation to implementatin.

Hence I wrote `defthing` that adds a constructor to `defstruct` as well as  method `slots-of` that lists
all the slots of a thing.
|#
(defmacro defthing (it &rest has) 
  (labels ((make (x) (intern (format nil "%MAKE-~a" x))) 
           (name (x) (if (consp x) (car x) x))) 
    `(progn (defstruct (,it (:constructor ,(make it))) ,@has)
            (defmethod slots-of ((_ ,it)) ',(mapcar #'name has)))))
#|
Then, just cause it was so easy to do, I wrote `things` which turns
a list of `defstruct`s into  `defthings`:
|#
(defmacro things (&rest defstructs)
  `(progn ,@(loop for (defstruct . slots) in defstructs collect `(defthing ,@slots))))
#|
This allows for simpler instance management. In the following, a set of structs are converted
to things (using `(things defstructs)`). 
Then we see (for example) the  `make-team` constructor 
 looking up our team's salary and age before calling the constructor primitive constructor `%make-team`. 


;include test/defthing.lisp

## Symbol Counts

`has` is a macro for self creating items in a symbol table.
When counting less than 50 symbols, 
this code runs as fast as hash tables, and is simpler to use.
|#
(defmacro has (x lst &optional (init 0))      
  `(cdr (or (assoc ,x ,lst :test #'equal)
            (car (setf ,lst (cons (cons ,x ,init) ,lst))))))
#|

For example:

;include test/has.lisp
    
Two nice features of this code are that:

* It is
self-initializing-- from the `init` argument.
* What we do with the counts can be controlled
by some wrapper function. For example, in the above
example, we used `incf` to increase the counts (and we could
have also used `decf` to reduce the counts).

## CSV Reader
`call-with-csv` applies a function `fun` to each line of csv `file`
(and before that call,
the lines are split on commas and leading and training white space is removed). 
|#
(defun call-with-csv (file fun)
  (labels ((trim (s) (string-trim `(#\Space #\Tab #\Newline) s))
           (split (s &optional (sep #\,) (here 0))
                  (let* ((there (position sep s :start here))
                         (word  (trim (subseq s here there))))
                    (labels ((tail () (if there (split s sep (1+ there)))))
                      (if (equal word "") (tail) (cons word (tail)))))))
    (with-open-file (s file) 
      (loop (funcall fun (split (or (read-line s nil) (return))))))))
#|
No, I won't explain this code since the plan here is simplify its use, with a macro.
The `with-csv` macro demonstrates two useful
macro tricks; 

* Macros can define a return variable (see the `out` variable, below).
* It is useful to code up everything you want as a function (e.g. `call-with-csv`, then add the `defmacro` as a final layer);
|#
(defmacro with-csv ((line file &optional out) &body body)
  `(progn (call-with-csv ,file #'(lambda (,line) ,@body)) 
          ,out))
#|
Here is `with-csv` in operation. It sums the number of cells in all lines of a  csv file.

;include test/with-csv.lisp

## Newbie Mistakes 

### Repeated Processing

Here's a classic newb errors: _repeated processing_. 
The following macro looks fine _but_ it includes the `x` expression
twice. So what ever `x` does, it does it twice. 

     (defmacro square-1 (x)
        `(* ,x ,x))

This could be a very bad thing, depending on 
how slow is `x` to compute, or if  `x` has global side-effects such that calling it twice gives
different answers each time.

### Variable Capture

We could try to fix this, and if we do that wrong then we get to another newb error: 
_variable capture_. In this next macro, we run `x` only once and capture its output in `z`. 
Then we square
`z`. All right? Nope!

     (defmacro square-2 (x)
        `(let ((z ,x))
           (* z z))))

The problem here is that `x` can be arbitrary code which if it includes a `z` variable,
could mean that that code gets confused by the other `z` (and which point, it is anyone's guess 
							      what happens next).

To fix that problem, we need a variable name that is guaranteed never to appear anywhere
else in the source code. This is something that the LISP built-in function `gensym` can  offer.
(so the variables with the funny syntax like `#:G2856` are made by `gensym`).

     (defmacro square (x)
       (let ((z (gensym)))
         `(let ((,z ,x))
            (* ,z ,z))))
         
     (print (macroexpand  '(square 2)))   
     (print (square 2))
        
     ; ==>
     (LET ((#:G2856 2)) (* #:G2856 #:G2856))  ; <== This is what "(square 2)" expands into.
     4                                        ; <== This is the result of running "(square 2)".

## A Little Fun
Not that I use the following, but its so much fun, I just got to share.

Here is an ultra-cool anaphoric lambda macro
which binds the function itself to the anaphor `self`, allowing it to recurs:

    (defmacro alambda (parms &body body)
       `(labels ((self ,parms ,@body))
          #'self))
    
     (alambda (n) ; factorial lambda 
       (if (= n 0)  
         1
         (* n (self (1- n))))) 

You know you have really  caught the macro bug if this example gets you thinking "is all of OO just 10 lines of LISP macros?". 
Exercise for the reader! (But, btw, I've tried it and it gets surprisingly tricky surprisingly quickly).

## "Don't like it"

Say you don't like the code I've got here. No drama.[^GRA95]
We don't need
to go all walrus about it. Just delete my code and do whatever it is you
wanted to do.  And send me a link to that revised code-- I'd really enjoy seeing how
you organize things. Share and enjoy!


[^DIJ72]: Edger W. Dijkstra (1972), The Humble Programmer (EWD 340) (ACM Turing Award lecture).
[^GRA95]: Paul Graham (1995), ANSI Common Lisp.  Prentice-Hall |#
