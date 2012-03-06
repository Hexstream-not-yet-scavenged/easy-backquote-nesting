(in-package #:easy-backquote-nesting)

;;;; I started writing a "proper" easy-backquote-nesting.lisp but it's
;;;; taking too long and I'm tired and I feel the pressure to release
;;;; this thing so for now I'll just cheat and skip many steps and get
;;;; right to the raw matter of my technique itself, without any
;;;; context whatsoever. The intended audience is experienced lispers
;;;; who already know what backquote nesting is and why you'd want to
;;;; use it.

;; Here's the macro we want to write. It's not a really great example
;; for what I want to illustrate because there isn't a lot of variety
;; but I'm in a bit of a hurry and it should be "good enough" (feeling
;; depressed yet?).
(defmacro define-multidef-macro (name global-args local-args &body body)
  (let ((local-args-var (if (consp local-args)
			    (first local-args)
			    local-args))
	(local-args-args (if (consp local-args)
			     (second local-args))))
    (let ((to-collect (gensym "TO-COLLECT"))
	  (collect (gensym "COLLECT")))
      `(defmacro ,name (,global-args &body ,local-args-var)
	 (macrolet ((,local-args-var (,to-collect)
		      `(let (,',collect)
			 (dolist (,',local-args-var ,',local-args-var)
			   (destructuring-bind ,',(or local-args-args local-args-var)
			       ,',local-args-var 
			     (push ,,to-collect ,',collect)))
			 (nreverse ,',collect))))
	   ,@body)))))

;; Here's an example of how you might use this macro.
;; We won't use the global-args feature for simplicity.
(define-multidef-macro check-types () (args (place type &optional type-string))
  `(progn ,@(args `(check-type ,place ,type ,@(if type-string (list type-string))))))

;; Here's the macroexpansion of the preceding definition:
#+nil
(DEFMACRO CHECK-TYPES (() &BODY ARGS)
  (MACROLET ((ARGS (#:TO-COLLECT992)
	       `(LET (#:COLLECT993)
		  (DOLIST (ARGS ARGS)
		    (DESTRUCTURING-BIND
			  (PLACE TYPE &OPTIONAL TYPE-STRING)
			ARGS
		      (PUSH ,#:TO-COLLECT992 #:COLLECT993)))
		  (NREVERSE #:COLLECT993))))
    `(PROGN
       ,@(ARGS
	  `(CHECK-TYPE ,PLACE ,TYPE
		       ,@(IF TYPE-STRING
			     (LIST TYPE-STRING)))))))

;; So, this defined a CHECK-TYPES macro that you can then use,
;; for example, like this:
(defun foo (my-string my-number my-symbol)
  (check-types ()
    (my-string string)
    (my-number number)
    (my-symbol symbol))
  (list my-string my-number my-symbol))


;; Okay. So that's the end product. Now, we'll attempt to write the
;; DEFINE-MULTIDEF-MACRO macro step-by-step.

;; In this abridged version, I'll just go ahead and assume we have a
;; perfectly clear idea of what we want the define-multidef-macro to
;; do and even how it's implemented, with the caveat that we don't
;; know the right magical voodoo incantations of nested backquotes to
;; write exactly.

;; So. We're an experienced macro writer so we come up with almost 100% of the solution in an instant:
#+nil
(defmacro define-multidef-macro (name global-args local-args &body body)
  (let ((local-args-var (if (consp local-args)
			    (first local-args)
			    local-args))
	(local-args-args (if (consp local-args)
			     (second local-args))))
    (let ((to-collect (gensym "TO-COLLECT"))
	  (collect (gensym "COLLECT")))
      `(defmacro ,name (,global-args &body ,local-args-var)
	 (macrolet ((,local-args-var (,to-collect)
		      `(let (??collect)
			 (dolist (??local-args-var ??local-args-var)
			   (destructuring-bind ??(or local-args-args local-args-var)
			       ??local-args-var 
			     (push ??to-collect ??collect)))
			 (nreverse ??collect))))
	   ,@body)))))

;; I just made up this "?" notation to make it easier to explain, I
;; don't normally use these but maybe it might make it easier for you
;; to get started with this technique. Note that I don't bother with
;; "push" and "nreverse" in this case because they're completely
;; quoted and that case is intuitive.

;; The only thing that's stumping us big time is how to properly
;; unquote things. I put ?'s at the tricky places where we know we
;; have to unquote somehow, but we're not sure how.

;; Each "?" must be replaced by "" or "'" or "," or ",@" or ",'" or ",@'".

;; The RIGHTMOST "?" is associated with the OUTERMOST backquote.
;; We'll start with the outermost backquote level first.

;; Notice the potential for ambiguity: what if we write "," at a place where there's "??"?
;; Of course the parser can't distinguish between " ," and ", ".
;; Is the "," associated with the left or right "?"?
;; TRICKY ANSWER: It's associated with the LEFT one (", "). Weird, eh?
;; But what if we want to associate it with the one on the right?
;; Then we'll put ",'" in front (it may look contradictory: "unquote quote??").
;; This has the effect of "using up" a level of backquoting
;; without otherwise changing anything.

;; The first step is: COMPLETELY IGNORE THE NESTED BACKQUOTE. Here I
;; write the nested backquote, but we'll just pretend it doesn't exist
;; for now. We're thinking solely in terms of the outermost defmacro
;; and backquote at this point, just like for "normal" macros.
;; So we come up with this:
#+nil
(defmacro define-multidef-macro (name global-args local-args &body body)
  (let ((local-args-var (if (consp local-args)
			    (first local-args)
			    local-args))
	(local-args-args (if (consp local-args)
			     (second local-args))))
    (let ((to-collect (gensym "TO-COLLECT")) ; I'd normally use WITH-UNIQUE-NAMES here.
	  (collect (gensym "COLLECT")))
      `(defmacro ,name (,global-args &body ,local-args-var)
	 (macrolet ((,local-args-var (,to-collect)
		      `(let (?,collect)
			 (dolist (?,local-args-var ?,local-args-var)
			   (destructuring-bind ?,(or local-args-args local-args-var)
			     ?,local-args-var 
			     (push ?,to-collect ?,collect)))
			 (nreverse ?,collect))))
	   ,@body)))))

;; Now we're ready to deal with the innermost backquote/leftmost
;; unquotes. At this point we can conveniently ignore the outermost
;; context/rightmost unquotes that we just wrote.

#+nil
(defmacro define-multidef-macro (name global-args local-args &body body)
  (let ((local-args-var (if (consp local-args)
			    (first local-args)
			    local-args))
	(local-args-args (if (consp local-args)
			     (second local-args))))
    (let ((to-collect (gensym "TO-COLLECT")) ; I'd normally use WITH-UNIQUE-NAMES here.
	  (collect (gensym "COLLECT")))
      `(defmacro ,name (,global-args &body ,local-args-var)
	 (macrolet ((,local-args-var (,to-collect)
		      `(let (,',collect)
			 (dolist (,',local-args-var ,',local-args-var)
			   (destructuring-bind ,',(or local-args-args local-args-var)
			     ,',local-args-var 
			     (push ,,to-collect ,',collect)))
			 (nreverse ,',collect))))
	   ,@body)))))
;; And now you see why this is a bad example ;P
;; It's ,', almost everywhere.


;; I have to say, even with this technique, I can't quite read the
;; classic definition of ONCE-ONLY (I'm beginning to think some kind
;; of special technique was used to painfully craft it). I don't even
;; know for sure that it still works for levels of backquote nesting
;; beyond 2, but I suspect it ought to. Maybe it works in theory but
;; doesn't work in practice? Would be funny because the technique I
;; just exposed sort of doesn't work in theory (it doesn't give you a
;; mental model of how things actually work) but it works in practice.

;; With this, I hope the reign of terror of nested backquotes is over.
