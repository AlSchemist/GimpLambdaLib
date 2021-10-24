; http://srfi.schemers.org/srfi-8/srfi-8.html
; SRFI: Scheme Requests for Implementation https://srfi.schemers.org/
;;"1srfi-008.scm": 1999/08/30: receive: Binding to multiple values
; Section 1: John D. Stone	1999	SRFI 8 receive
;			 AlSchemist		2021	TinyScheme integration for Gimp 2.10.28 Script-Fu
;									Add multiline comments, examples and pretty print
; Section 2: Ashley, Dybvig 1994	call-with-values values mv? simplified by AlSchemist
; Section 3: AlSchemist		2021	mv-values for show. call/cc-values
;  							Does not redefine call-with-current-continuation
; 2021/09/27: 	GimpλLib 1.0 by AlSchemist for Gimp 2.10.28 TinyScheme Script-Fu
;				receive as a TinyScheme macro
; Line: 219 define: 5 comment: 176 = 80.3% blank: 8

;_______________________________	Section 1 begin by John David Stone 1999

(macro (receive lstMacroCall)
	(let*	(	(formals	(cadr	lstMacroCall))
				(expression	(caddr	lstMacroCall))
				(body		(cadddr	lstMacroCall))
			)
	   `(call-with-values (lambda () ,expression)
			(lambda ,formals ,body)
)	)	)
#| Alternative version with define-macro
(define-macro (receive formals expression body)
	   `(call-with-values (lambda () ,expression)
			(lambda ,formals ,body)
)	)
 |#
#| Copyright (C) John David Stone (1999). All Rights Reserved.
Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 |#
;_______________________________	Section 2 begin by Ashley and Dybvig 1994
; "An Efficient Implementation of Multiple Return Values in Scheme"
; by J. Michael Ashley, R. Kent Dybvig
; Proceedings of the 1994 ACM Conference on LISP and
; Functional Programming, 140–149, 1994. Copyright (c) 1994 ACM.

; AlSchemist: How a function could return two values instead only one?
#| Return the input lst splitted in two sublists prefixed by symbol mv
(define (split lst)
	(if (or (null? lst) (null? (cdr lst)))
		(values lst '())
		(call-with-values
			(lambda () (split (cddr lst))) ; producer
			(lambda (odds evens) (values (cons (car lst) odds) (cons (cadr lst) evens))
)	)	)	)
(split '(a b c d e f))
 |#;->	(mv-values (a c e) (b d f))
; AlSchemist: in fact the function continues to return only one list with the two values

#| AlSchemist: we can present the two values on each line by value
(show (split '(a b c d e f)))
(call-with-values (lambda()(split '(a b c d e f))) 
	(lambda(lstLeft lstRight) (show lstLeft "\n" lstRight))
)
 |#;->	(a c e)
;		(b d f)
#|	Split the input list in two sublists and insert lstMiddle between them
(let*	(	(lstIn '(a b c d e f)) (lstMiddle '(D E F)))
	(if #t ; #t: to test call-with-values. Otherwise #f: to test receive
		(call-with-values (lambda()(split lstIn)) ; produce 2 sublists
			(lambda(lstLeft lstRight)(append lstLeft lstMiddle lstRight)) ; consume
		)
		(receive (lstLeft lstRight) (split lstIn) ; produce 2 sublists
			(append lstLeft lstMiddle lstRight) ; consume the two sublists
)	)	)
 |#;->	(a c e D E F b d f) ; receive or call-with-vales have the same result

#| Perf: the above split must be defined
(let*	(	(lstIn '(a b c d e f)) (lstMiddle '(D E F))
			(nbrRun	10000)(timeEnd 0)(timeStart (gettimeofday))
		)
	(let loop ((idx 0))
		(if (>= idx nbrRun) (set! timeEnd (gettimeofday))
			(begin	
				(call-with-values (lambda()(split lstIn)) ; produce 2 sublists
					(lambda(lstLeft lstRight)(append lstLeft lstMiddle lstRight)) ; consume
				)
;				(receive (lstLeft lstRight) (split lstIn) ; produce 2 sublists
;					(append lstLeft lstMiddle lstRight) ; consume the two sublists
;				)
				(loop (+ idx 1))
	)	)	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->	 7 s 968 ms 217 µs for 10K runs. 000 ms 796 µs by run call-with-values
;		23 s 936 ms 951 µs for 10K runs. 002 ms 393 µs by run receive

#| Does lst begin with the special flag mv-values?
(mv? '(mv-values first last))	(mv? nil)
 |#;->	#t							 #f
(define (mv? lst) ; was magic?(lambda(x)(and (pair? x)(eq? (car x) magic))))
	(and (pair? lst) (eq? (car lst) 'mv-values))
) ;                                 ^was magic, a doted pair (multiple . values)
#| Chapter "3.1 Rewriting values and call-with-values" page 2
(values)			(values 'e)		(values 'e1 'e2 'e3 'dot 'dot 'dot)
 |#;->	(mv-values)			 e		(mv-values e1 e2 e3 dot dot dot)
#|
(+ (values 2) 4)	(if (values #t) 1 2)	(begin (values 1 2 3) 4)
 |#;->	6							1							  4
; AlSchemist added the dot making args optional that is to say a list
(define (values . args)
		(if (and (not (null? args )) (null? (cdr args )))
			(car args )
			(cons 'mv-values args) ; cons the special flag magic that begins the list
)		)
;  R5RS: (call-with-values producer consumer)
#| R5RS: return the second of two values
(call-with-values (lambda () (values 4 5)) (lambda (a b) b))
 |#;->	5
#| AlSchemist: not the easiest way to generate minus one ;-)
(call-with-values * -)
 |#;->	-1

#| Chapter "2 Multiple Return Values" page 1
(call-with-values (lambda () (values 1 2)) +)
 |#;->	3
#| "values itself serves as the producer."
(call-with-values values (lambda args args))
 |#;->	()
#|
(call-with-values (lambda () 4) (lambda (x) x))
 |#;->	4
(define (call-with-values producerNoArg consumer)
		(let ((prodResult (producerNoArg)))
			(if (mv? prodResult)
				(apply consumer (cdr prodResult)) ; skip the special flag 'mv-values
				(consumer prodResult)
)	)	)
#| AlSchemist replaces call/cc with call/cc-values preserving the redefinition of primitive
(call-with-values 
	(lambda () (call/cc-values (lambda (k ) (k 2 3)))) ; producer
	(lambda (x y) (list x y)) ; consumer
)
 |#;->	(2 3)
(define (call/cc-values procArityOne)
	(call-with-current-continuation ; AlSchemist did not overload it
		(lambda (k)
			(procArityOne (lambda args (k (apply values args ))
)	)	)	)	)
#| How to know if call-with-current-continuation is the original primitive not overloaded?
(procedure? call-with-current-continuation) (procedure? car)
 |#;->	#t									 			#t
#| It is not the GimpλLib that redefined call-with-current-continuation but script-fu.init
(closure? call-with-current-continuation)	(closure? car)
 |#;->	#t	; redefined								  #f ; car is the original primitive
#| The content of the redefinition:
(get-closure-code call-with-current-continuation)
 |#;->	(lambda (func) (old-c/cc (lambda (continuation) (func (let ((current-ws *active-windings*)) (lambda (x) (set-active-windings! current-ws) (continuation x)))))))
#| Annex A Variants of split used for performance comparisons: continuation-passing style
(define (split-cps ls values)
	(if (or (null? ls) (null? (cdr ls)))
		(values ls '())
		(split-cps
			(cddr ls)
			(lambda (odds evens) (values (cons (car ls) odds) (cons (cadr ls) evens))
)	)	)	)
(split-cps '(a b c d e f ) values)
 |#;->	(mv-values (a c e) (b d f))
#| Classical procedural where the first sublist is embedded inside the second sublist
(define (split-cons lst)
	(if (or (null? lst) (null? (cdr lst)))
		(cons lst nil)
		(let ((pair (split-cons (cddr lst))))
			(cons (cons (car lst) (car pair)) (cons (cadr lst) (cdr pair)))
)	)	)
(split-cons '(a b c d e f))
 |#;->	((a c e) b d f)
;_______________________________	Section 3 begin by AlSchemist 2021
;
; The following is free TinyScheme open source copyrighted AlSchemist

#| Display a multiple values list line by line for each item except the header 'mv-values
(show '(mv-values line1 line2))
 |#;->	line1
;		line2
(define (mv-show lst)
	(cond	(	(not (mv? lst)) lst)
			(	(= (length lst) 3) (show (second lst) "\n" (third lst)))
			(else	(let loop ((lst (cdr lst))) ; skip the special flag 'mv-values
						(if (pair? lst)
							(if (not-pair? (cdr lst)) (show (car lst)) ; last item
								(begin (show (car lst)) (loop (cdr lst)))
)	)	)	)	)	)
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
(load "C:/Users/YourUserName/AppData/Roaming/GIMP/2.10/scripts/1srfi-008-receive.scm")
 |# (closure? call-with-values)