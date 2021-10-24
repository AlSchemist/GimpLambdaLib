; https://srfi.schemers.org/srfi-2/
; SRFI: Scheme Requests for Implementation https://srfi.schemers.org/
; AND-LET*: an AND with local bindings, a guarded LET* special form
; by Oleg Kiselyov
; Section 1: Oleg Kiselyov	1999 SRFI-2 and-let*
; Section 2: Aubrey Jaffer	2003 Implementation as macro in SLIB 3b6-1
; Section 3: AlSchemist		2021 Multiline comments and pretty print
; 2021/09/27: 	GimpλLib 1.0 by AlSchemist for Gimp 2.10.28 TinyScheme Script-Fu
;				and-let* as a TinyScheme macro
; Line: 324 define: 1 comment: 291 = 89.8% blank: 13

;_______________________________	Section 1 begin by Oleg Kiselyov	1999

; 		A special form and-let*
;	           Validation code
;
; AND-LET* (formerly known as LAND*) is an AND with local bindings,
; a guarded LET* special form.
; It evaluates a sequence of forms one after another till the first one 
; that yields #f; the non-#f result of a form can be bound to a fresh variable and
; used in the subsequent forms.
;
; It is defined in SRFI-2 <http://srfi.schemers.org/srfi-2/>
;
; Motivation:
; When an ordinary AND is formed of _proper_ boolean expressions: (AND E1 E2 ...)
;
; the expression E2, if it gets to be evaluated, knows that E1 has returned non-#f.
; Moreover, E2 knows exactly what the result of E1 was - #t 
; - so E2 can use this knowledge to its advantage.
; If E1 however is an _extended_ boolean expression, E2 can no longer tell
; which particular non-#f value was returned by E1. 
; Chances are it took a lot of work to evaluate E1, and the produced result
; (a number, a vector, a string, etc) may be of value to E2.
; Alas, the AND form merely checks that the result is not an #f, and throws it away.
; If E2 needs it, it has to recompute the value again.
; This proposed AND-LET* special form lets constituent expressions get hold
; of the results of already evaluated expressions, without re-doing their work.
;
; Syntax: (AND-LET* (CLAWS) BODY)
;
; where CLAWS is a list of expressions or bindings: CLAWS ::= '() | (cons CLAW CLAWS)
; Every element of the CLAWS list, a CLAW, must be one of the following:
;	(VARIABLE EXPRESSION) or (EXPRESSION) or BOUND-VARIABLE
; These CLAWS are evaluated in the strict left-to-right order.
; For each CLAW, the EXPRESSION part is evaluated first (or BOUND-VARIABLE is looked up).
;
; If the result is #f, AND-LET* immediately returns #f,
; thus disregarding the rest of the CLAWS and the BODY.
; If the EXPRESSION evaluates to not-#f, and the CLAW is of the form
;	(VARIABLE EXPRESSION)
; the EXPRESSION's value is bound to a freshly made VARIABLE.
; => The VARIABLE is available for _the rest_ of the CLAWS, and the BODY.
;
; Thus AND-LET* is a sort of cross-breed between LET* and AND.
;
; Denotation semantics:
;
; Eval[ (AND-LET* (CLAW1 ...) BODY), Env] =
;	EvalClaw[ CLAW1, Env ] andalso 
;		Eval[ (AND-LET* ( ...) BODY), ExtClawEnv[ CLAW1, Env]]
;
; Eval[ (AND-LET* (CLAW) ), Env] = EvalClaw[ CLAW, Env ]
; Eval[ (AND-LET* () FORM1 ...), Env] = Eval[ (BEGIN FORM1 ...), Env ]
; Eval[ (AND-LET* () ), Env] = #t
;
; EvalClaw[ BOUND-VARIABLE, Env ] = Eval[ BOUND-VARIABLE, Env ]
; EvalClaw[ (EXPRESSION), Env ] = Eval[ EXPRESSION, Env ]
; EvalClaw[ (VARIABLE EXPRESSION), Env ] = Eval[ EXPRESSION, Env ]
;
; ExtClawEnv[ BOUND-VARIABLE, Env ] = Env
; ExtClawEnv[ (EXPRESSION), Env ] = EnvAfterEval[ EXPRESSION, Env ]
; ExtClawEnv[ (VARIABLE EXPRESSION), Env ] = 
;	ExtendEnv[ EnvAfterEval[ EXPRESSION, Env ],
;		   VARIABLE boundto Eval[ EXPRESSION, Env ]]
;
; If AND-LET* is implemented as a macro, it converts a AND-LET* expression
; into a "tree" of AND and LET expressions. For example,
#|
(AND-LET* ((my-list (compute-list)) ((not (null? my-list))))
  	(do-something my-list)
)
 |#;->	is transformed into
#| AlSchemist: the interest of the first and is negotiable
(and	(let	(	(my-list (compute-list)))
			(and	my-list
					(not (null? my-list))
					(begin (do-something my-list))
)		)	)
 |#

; Sample applications:
#| The following piece of code (from my treap package)  
(let	((new-root (node:dispatch-on-key root key ...)))
	(if new-root (set! root new-root))
)
 |# ;-> could be elegantly re-written as
#|
(and-let* ((new-root (node:dispatch-on-key root key ...)))
	(set! root new-root)
)
 |#

; A very common application of and-let* is looking up a value associated 
; with a given key in an assoc list, returning #f in case of a look-up failure:
#| Standard implementation
(define (look-up key alist)
	(let ((found-assoc (assq key alist)))
		(and found-assoc (cdr found-assoc))
)	)
 |#;->	A more elegant solution
#|
(define (look-up key alist)
	(cdr (or (assq key alist) '(#f . #f)))
)
 |#; An implementation which is just as graceful as the latter
#|   and just as efficient as the former:
(define (look-up key alist)
	(and-let* ((found-assoc (assq key alist))) (cdr found-assoc))
)
 |#; AlSchemist replaced x with found-assoc to more easily compare the solutions

#| Generalized cond:
(or
	(and-let* (bindings-cond1) body1)
	(and-let* (bindings-cond2) body2)
	(begin else-clause)
)
 |#
; Unlike => (cond's send), AND-LET* applies beyond cond.
; AND-LET* can also be used to generalize cond,
; as => is limited to sending of a single value; 
; AND-LET* allows as many bindings as necessary (which are performed in sequence)
#|
(or
	(and-let* ((c (read-char)) ((not (eof-object? c))))
		(string-set! some-str i c) (inc! i)
	)
	(begin (do-process-eof))
)
 |#

; Another concept AND-LET* is reminiscent of is programming with guards:
; an AND-LET* form can be considered a sequence of _guarded_ expressions.
; In a regular program, forms may produce results, bind them to variables
; and let other forms use these results.
; AND-LET* differs in that it checks to make sure that every produced result 
; "makes sense" (that is, not an #f).
; The first "failure" triggers the guard and aborts the rest of the
; sequence (which presumably would not make any sense to execute anyway).
;
; $Id: vland.scm,v 2.2 2004/07/08 20:19:39 oleg Exp oleg $
;--- Test cases

#| No claws
(and-let* () 1)		(and-let* () 1 2)		(and-let* () )
 |#;->		 1					   2					#t
#| must-be-a-syntax-error
(and-let* #f #t)	(and-let* #f)
 |#;->	Error: reverse: argument 1 must be: pair or '()

#| One claw, no body
(let ((x #f)) (and-let* (x)))	(let ((x 1)) (and-let* (x)))
 |#;->	 #f								 1
#|
(let ((x 1)) (and-let* ( ((+ x 1)) )))	(and-let* ((x #f)) )	(and-let* ((x 1)) )
 |#;->						   2					  #f					  1
#| must-be-a-syntax-error
(and-let* #f #t)	(and-let* #f)
 |#;->	Error: car: argument 1 must be: pair 

#| two claws, no body
(and-let* ( (#f) (x 1)) )	(and-let* ( (2) (x 1)) )	(and-let* ( (x 1) (2)) )
 |#;->		 #f								   1						   2
#|
(and-let* ( (x 1) x) )		(and-let* ( (x 1) (x)) )
 |#;->		   1						   1
#| must-be-a-syntax-error
(and-let* #f #t)	(and-let* #f)
 |#;->	Error: car: argument 1 must be: pair

#| two claws, body
(let ((x #f)) (and-let* (x) x))	(let ((x "")) (and-let* (x) x))
 |#;->	 #f								 ""
#|
(let ((x "")) (and-let* (x)  ))	(let ((x 1)) (and-let* (x) (+ x 1)))
 |#;->	 ""														2
#|
(let ((x #f)) (and-let* (x) (+ x 1))) (let ((x 1)) (and-let* (((positive? x))) (+ x 1)))
 |#;->	 #f																			2
#|
(let ((x 1)) (and-let* (((positive? x))) ))
(let ((x 0)) (and-let* (((positive? x))) (+ x 1)))
(let ((x 1)) (and-let* (((positive? x)) (x (+ x 1))) (+ x 1)))
(let ((x 1)) (and-let* (((positive? x)) (x (+ x 1)) (x (+ x 1))) (+ x 1)))
 |#;->							 #t			  #f		  3			  4
#|
(let ((x 1)) (and-let* (x ((positive? x))) (+ x 1)))
 |#;->											2
#|
(let ((x 1)) (and-let* ( ((begin x)) ((positive? x))) (+ x 1)))
 |#;->													   2
#|
(let ((x 0)) (and-let* (x ((positive? x))) (+ x 1)))
(let ((x #f)) (and-let* (x ((positive? x))) (+ x 1)))
(let ((x 1)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y)))
(let ((x 0)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y)))
(let ((x #f)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y)))
 |#;->								#f			;  ^guarded from dividing by zero
#|
(let ((x 3)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y)))
 |#;->	1,5.0
#| Copyright (C) Oleg Kiselyov (1999). All Rights Reserved.
Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 |#
;_______________________________	Section 2 begin by Aubrey Jaffer	2003
; https://people.csail.mit.edu/jaffer/SLIB.html
;;"srfi-2.scm": Guarded LET* special form
;Copyright (C) 2003 Aubrey Jaffer
;
;Permission to copy this software, to modify it, to redistribute it,
;to distribute modified versions, and to use it for any purpose is
;granted, subject to the following restrictions and understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warranty or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

(define-macro (and-let* claws . body)
	(define (andin claw ans)
		(if	(and (pair? ans) (eq? 'and (car ans)))
			`(and ,claw ,@(cdr ans))
			`(and ,claw ,ans)
	)	)
	(do	((claws (reverse claws) (cdr claws))
			(ans	(cond	((null? body) '(and))
						((null? (cdr body)) (car body))
						(else (cons 'begin body))
					)
					(let	((claw (car claws)))
						(cond	((symbol? claw) (andin claw ans))
							((and (pair? claw) (null? (cdr claw)))
								(andin (car claw) ans)
							)
							(else `(let (,claw) ,(andin (car claw) ans)))
		)	)		)	)
		((null? claws) ans)
)	)
;_______________________________	Section 3 begin by AlSchemist 2021

; The following is free TinyScheme open source copyrighted AlSchemist

#| Perf: TinyScheme macro expansion must be optimized in 
; C:\Program Files\GIMP 2\lib\gimp\2.0\plug-ins\script-fu\script-fu.exe
(let*	(	(alist    '(	(range all) (hue 0)(saturation -0.40)(lightness 0)
							(range red) (hue 0)(saturation 0)	 (lightness 0)
			)			)
			(nbrRun	10000)(timeEnd 0)(timeStart (gettimeofday))
		)
	(define (look-up1 key alist) ; Oleg Kiselyov	1999
		(let ((found-assoc (assq key alist)))
			(and found-assoc (cdr found-assoc))
	)	)
	(define (look-up2 key alist) ; Oleg Kiselyov	1999
		(cdr (or (assq key alist) '(#f . #f)))
	) ; AlSchemist 2002:                   ^ returned value if key does not exist
	(define (look-up3 key alist) ; Oleg Kiselyov	1999
		(and-let* ((found-assoc (assq key alist))) (cdr found-assoc))
	)
	(and	(equal? (look-up1 'saturation alist) (look-up2 'saturation alist)
					(look-up3 'saturation alist)
			) ;		^(-0,4.0)
			(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
;				(look-up1 'saturation alist)
				(look-up2 'saturation alist) ; "A more elegant solution": the fatest
;				(look-up3 'saturation alist)
			)	(time-stat timeStart timeEnd nbrRun)
)	)
 |#;->		  1 s 652 ms 819 µs for 10K runs. 		165 µs by run look-up2 cdr or
;			  1 s 780 ms 799 µs for 10K runs. 		178 µs by run look-up1 and cdr
;		3 mn 21 s 718 ms 830 µs for 10K runs. 20 ms 171 µs by run look-up3 as macro
#| 
Redistribution and use in source forms, with or without modification,
are permitted, free of charge, provided that the following conditions are met:
1. Redistributions of source code must retain:
   - the copyright notices,
   - the original URL provided by AlSchemist:
     https://github.com/AlSchemist/GimpLambdaLib
   - this list of conditions and the following disclaimer.
2. The name of the authors may not be used to endorse or promote products
   derived from this software without specific prior written permission.

3. This source code cannot be sell.

Unless a specific copyright, credits or URL is specified,
this free open source code is Copyright (C) AlSchemist (2021).
All Rights Reserved.

This software is provided by the authors "as is" and any express or
implied warranties, including, but not limited to, the implied warranties
of merchantability and fitness for a particular purpose are disclaimed.

In no event shall the authors be liable for any direct, indirect,
incidental, special, exemplary, or consequential damages 
(including, but not limited to, procurement of substitute goods or services;
loss of use, data, or profits; or business interruption)
however caused and on any theory of liability, whether in contract, 
strict liability, or tort (including negligence or otherwise)
arising in any way out of the use of this software,
even if advised of the possibility of such damage.

Usage to load again: Gimp 2.10.28 menu "Filters" > "Script-Fu" > "Console"
(load "C:/Users/YourUserName/AppData/Roaming/GIMP/2.10/scripts/2srfi-002-and-let.scm")
 |# (macro? and-let*)