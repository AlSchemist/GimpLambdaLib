;;"comlist.scm" Implementation of COMMON LISP list functions for Scheme
; https://people.csail.mit.edu/jaffer/SLIB.html
; Copyright (C) 1991, 1993, 1995, 2001, 2003 Aubrey Jaffer. SLIB 3b6-1
; Copyright (C) 2000 Colin Walters
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

;;; Some of these functions may be already defined in your Scheme.
;;; Comment out those definitions for functions which are already defined.
;;;; LIST FUNCTIONS FROM COMMON LISP

;;; Some tail-recursive optimizations made by Colin Walters <walters@cis.ohio-state.edu>
;;; AGJ restored order July 2001.

; 2021/09/27: 	GimpλLib 1.0 by AlSchemist for Gimp 2.10.28 TinyScheme Script-Fu
;				Add multiline comments, examples and pretty print by AlSchemist
; Line: 451 define: 27 comment: 238 = 52.7% blank: 9
#|
(comlist:make-list 3 'Lae)
 |#;->	(Lae Lae Lae)
;;;@ From: hugh@ear.mit.edu (Hugh Secker-Walker)
#| (define comlist:make-list make-list)
(define (comlist:make-list k . init)
	(set! init (if (pair? init) (car init)))
	(do ((k (+ -1 k) (+ -1 k)) (result '() (cons init result))) ((negative? k) result)
)	)
 |#
#| Perf:
(let*	((nbrRun 1000)(timeEnd 0)(timeStart (gettimeofday)))
	(let loop ((idx 0))
		(if (>= idx nbrRun) (time-stat timeStart (gettimeofday) nbrRun)
			(begin
;				(vector->list (make-vector 3 'Lae))
				(make-list 3 'Lae)
;				(comlist:make-list 3 'Lae)
				(loop (+ idx 1))
)	)	)	)
 |#;->		218 ms 246 µs for 1K runs. 000 ms 218 µs by run vector->list make-vector
;			323 ms 001 µs for 1K runs. 000 ms 323 µs by run make-list
;		9 s 358 ms 846 µs for 1K runs. 009 ms 358 µs by run comlist:make-list

#|
(comlist:copy-list '(a b c))
 |#;->	(a b c)
(define (comlist:copy-list lst) (append lst '()))
#|

(comlist:adjoin 'Lae '(ti tia))
 |#;->	(Lae ti tia)
(define (comlist:adjoin obj lst) (if (memv obj lst) lst (cons obj lst)))
#|
(comlist:union '(Lae) '(ti tia))
 |#;->	(Lae ti tia)
(define comlist:union
	(letrec	((onion	(lambda	(lst1 lst2)
						(if	(null? lst1)
							lst2
							(onion (cdr lst1) (comlist:adjoin (car lst1) lst2))))))
		(lambda	(lst1 lst2)
			(cond	((null? lst1) lst2)
				((null? lst2) lst1)
				((null? (cdr lst1)) (comlist:adjoin (car lst1) lst2))
				((null? (cdr lst2)) (comlist:adjoin (car lst2) lst1))
				((< (length lst2) (length lst1)) (onion (reverse lst2) lst1))
				(else (onion (reverse lst1) lst2))
)	)	)	)
#|
(comlist:intersection '(Lae ti) '(ti tia))
 |#;->	(ti)
(define (comlist:intersection lst1 lst2)
	(if	(null? lst2)
		lst2
		(let build-intersection	((lst1 lst1) (result '()))
			(cond	((null? lst1) (reverse result))
				((memv (car lst1) lst2)
					(build-intersection (cdr lst1) (cons (car lst1) result)))
				(else (build-intersection (cdr lst1) result))
)	)	)	)
#|
(comlist:set-difference '(Lae ti) '(ti tia))
 |#;->	(Lae)
(define (comlist:set-difference lst1 lst2)
	(if	(null? lst2)
		lst1
		(let build-difference	((lst1 lst1) (result '()))
			(cond	((null? lst1) (reverse result))
				((memv (car lst1) lst2) (build-difference (cdr lst1) result))
				(else (build-difference (cdr lst1) (cons (car lst1) result)))
)	)	)	)
#|
(comlist:subset? '(ti) '(ti tia))
 |#;->	#t
(define (comlist:subset? lst1 lst2)
	(or	(eq? lst1 lst2)
		(let loop	((lst1 lst1))
			(or (null? lst1) (and (memv (car lst1) lst2) (loop (cdr lst1))))
)	)	)
#|
(comlist:position 'ti '(Lae ti tia))
 |#;->	1
(define (comlist:position obj lst)
	(define pos
		(lambda	(n lst)
			(cond	((null? lst) #f)
				((eqv? obj (car lst)) n)
				(else (pos (+ 1 n) (cdr lst))))))
	(pos 0 lst)
)
#|
(comlist:reduce-init (lambda(init n)(if (> n 3)(cons n init) init)) nil '(1 2 3 4 5))
 |#;->	(5 4)
(define (comlist:reduce-init pred? init lst)
	(if (null? lst) init (comlist:reduce-init pred? (pred? init (car lst)) (cdr lst)))
)
#|
(comlist:reduce (lambda(n1 n2)(+ n1 n2)) '(1 2 3 4 5))
 |#;->	15
(define (comlist:reduce pred? lst)
	(cond	((null? lst) lst)
		((null? (cdr lst)) (car lst))
		(else (comlist:reduce-init pred? (car lst) (cdr lst)))
)	)
#|
(comlist:some odd? '(1 2 3 4 5))
 |#;->	#t
(define (comlist:some pred lst . rest)
	(cond	((null? rest)
				(let mapf	((lst lst))
					(and (not (null? lst)) (or (pred (car lst)) (mapf (cdr lst))))))
		(else
			(let mapf	((lst lst) (rest rest))
				(and	(not (null? lst))
						(or	(apply pred (car lst) (map car rest))
							(mapf (cdr lst) (map cdr rest))
)	)	)	)	)		)
#|
(comlist:every odd? '(1 3 5))
 |#;->	#t
(define (comlist:every pred lst . rest)
	(cond	((null? rest)
				(let mapf	((lst lst))
					(or (null? lst) (and (pred (car lst)) (mapf (cdr lst))))))
		(else
			(let mapf	((lst lst) (rest rest))
				(or	(null? lst)
					(and	(apply pred (car lst) (map car rest))
							(mapf (cdr lst) (map cdr rest))
)	)	)	)	)	)
#|
(comlist:notany (lambda(n)(> n 10)) '(1 2 3 4 5))
 |#;->	#t
(define (comlist:notany pred . ls) (not (apply comlist:some pred ls)))
#|
(comlist:notevery even? '(1 3 5))
 |#;->	#t
(define (comlist:notevery pred . ls) (not (apply comlist:every pred ls)))
#|
((comlist:list-of?? integer?) '(1 2 3 4 5))
 |#;->	#t
(define (comlist:list-of?? predicate . bound)
	(define (errout) (apply error 'comlist:list-of?? predicate bound))
	(case	(length bound)
		((0) (lambda (obj) (and (list? obj) (comlist:every predicate obj))))
		((1)
			(set! bound (car bound))
			(cond	((negative? bound)
						(set! bound (- bound))
						(lambda	(obj)
							(and	(list? obj)
									(<= bound (length obj))
									(comlist:every predicate obj)
					)	)	)
				(else
					(lambda	(obj)
						(and	(list? obj)
								(<= (length obj) bound)
								(comlist:every predicate obj)
		)	)	)	)	)
		((2)
			(let	((low (car bound)) (high (cadr bound)))
				(cond	((or (negative? low) (negative? high)) (errout))
					((< high low) (set! high (car bound)) (set! low (cadr bound))))
				(lambda	(obj)
					(and	(list? obj)
							(<= low (length obj) high)
							(comlist:every predicate obj)
		)	)	)	)
		(else (errout))
)	)
#|
(comlist:find-if integer? '(a b 5 d))
 |#;->	5
(define (comlist:find-if pred? lst)
	(cond	((null? lst) #f)
		((pred? (car lst)) (car lst))
		(else (comlist:find-if pred? (cdr lst)))
)	)
#|
(comlist:member-if integer? '(a b 5 d))
 |#;->	(5 d)
(define (comlist:member-if pred? lst)
	(cond	((null? lst) #f)
		((pred? (car lst)) lst)
		(else (comlist:member-if pred? (cdr lst)))
)	)
#|
(comlist:remove 5 '(a b 5 d))
 |#;->	(a b d)
(define (comlist:remove obj lst)
	(define head (list '*head*))
	(let remove	((lst lst) (tail head))
		(cond	((null? lst))
			((eqv? obj (car lst)) (remove (cdr lst) tail))
			(else (set-cdr! tail (list (car lst))) (remove (cdr lst) (cdr tail)))))
	(cdr head)
)
#|
(comlist:remove-if integer? '(a b 5 d))
 |#;->	(a b d)
(define (comlist:remove-if pred? lst)
	(let remove-if	((lst lst) (result '()))
		(cond	((null? lst) (reverse result))
			((pred? (car lst)) (remove-if (cdr lst) result))
			(else (remove-if (cdr lst) (cons (car lst) result)))
)	)	)
#|
(comlist:remove-if-not symbol? '(a b 5 d))
 |#;->	(a b d)
(define (comlist:remove-if-not pred? lst)
	(let remove-if-not	((lst lst) (result '()))
		(cond	((null? lst) (reverse result))
			((pred? (car lst)) (remove-if-not (cdr lst) (cons (car lst) result)))
			(else (remove-if-not (cdr lst) result))
)	)	)
;;;@ From: hugh@ear.mit.edu (Hugh Secker-Walker)
#|
(comlist:nreverse '(R a c e C a r))
 |#;->	(r a C e c a R)
#| (define comlist:nreverse reverse)
(define (comlist:nreverse rev-it)
	(cond	((null? rev-it) rev-it)
		((not (list? rev-it)) (error "comlist:nreverse: Not a list in arg1" rev-it))
		(else
			(do	(	(reved '() rev-it)
					(rev-cdr (cdr rev-it) (cdr rev-cdr))
					(rev-it rev-it rev-cdr)
				)
				((begin (set-cdr! rev-it reved) (null? rev-cdr)) rev-it)
)	)	)	)
 |#
#| Perf:
(let*	((nbrRun 1000)(timeEnd 0)(timeStart (gettimeofday)))
	(let loop ((idx 0))
		(if (>= idx nbrRun) (time-stat timeStart (gettimeofday) nbrRun)
			(begin	
;				(reverse			'(R a c e C a r))
				(comlist:nreverse	'(R a c e C a r))
				(loop (+ idx 1))
)	)	)	)
 |#;->		030 ms 746 µs for 1K runs. 000 ms 030 µs by run reverse
;		9 s 968 ms 249 µs for 1K runs. 009 ms 968 µs by run comlist:nreverse

#|
(comlist:last '(a b c d e) 2)
 |#;->	(d e)
(define (comlist:last lst n) (comlist:nthcdr (- (length lst) n) lst))
#| Perf:
(let*	((nbrRun 1000)(timeEnd 0)(timeStart (gettimeofday)))
	(let loop ((idx 0))
		(if (>= idx nbrRun) (time-stat timeStart (gettimeofday) nbrRun)
			(begin	
				(comlist:last '(a b c d e) 2)
;				(take-right	  '(a b c d e) 2)		; 1srfi-001-list.scm
				(loop (+ idx 1))
)	)	)	)
 |#;->	187 ms 046 µs for 1K runs. 000 ms 187 µs by run comlist:last
;		327 ms 731 µs for 1K runs. 000 ms 327 µs by run take-right

#|
(comlist:butlast '(a b c d e) 2)
 |#;->	(a b c)
(define comlist:butlast drop-right)
;(define (comlist:butlast lst n) (comlist:butnthcdr (- (length lst) n) lst))
#| Perf:
(let*	((nbrRun 1000)(timeEnd 0)(timeStart (gettimeofday)))
	(let loop ((idx 0))
		(if (>= idx nbrRun) (time-stat timeStart (gettimeofday) nbrRun)
			(begin	
				(drop-right 	 '(a b c d e) 2)	; 1srfi-001-list.scm
;				(comlist:butlast '(a b c d e) 2)
				(loop (+ idx 1))
)	)	)	)
 |#;->		 340 ms 238 µs for 1K runs. 000 ms 340 µs by run drop-right
;		10 s 109 ms 442 µs for 1K runs. 010 ms 109 µs by run comlist:butlast
#|
(comlist:nthcdr 2 '(a b c d e))
 |#;->	(c d e)
(define (comlist:nthcdr n lst) (if (zero? n) lst (comlist:nthcdr (+ -1 n) (cdr lst))))
#|
(comlist:butnthcdr 2 '(a b c d e))
 |#;->	(a b)
(define (comlist:butnthcdr k lst)
	(cond	((negative? k) lst)
		((or (zero? k) (null? lst)) '())
		(else
			(let	((ans (list (car lst))))
				(do	(	(lst (cdr lst) (cdr lst))
						(tail ans (cdr tail))
						(k (+ -2 k) (+ -1 k))
					)
					((or (negative? k) (null? lst)) ans)
					(set-cdr! tail (list (car lst)))
)	)	)	)	)
#|
(comlist:butnth 2 '(a b c d e))	(comlist:butnth 1 '(a b c d e))
 |#;->	(a b d e)				(a c d e)
(define (comlist:butnth k lst)
	(cond	((negative? k) lst)
		((null? lst) lst)
		((zero? k) (cdr lst))
		(else
			(let	((ans (list (car lst))))
				(do	((lst (cdr lst) (cdr lst))
						(tail ans (cdr tail))
						(k (+ -2 k) (+ -1 k)))
					((or (negative? k) (null? lst))
						(cond ((not (null? lst)) (set-cdr! tail (cdr lst))))
						ans)
					(set-cdr! tail (list (car lst)))
)	)	)	)	)

;;;; CONDITIONALS

;;;@ Checks to see if a list has any duplicate MEMBERs.
#|
(comlist:has-duplicates? '(Lae ti ti a))
 |#;->	#t
(define (comlist:has-duplicates? lst)
	(cond	((null? lst) #f)
		((member (car lst) (cdr lst)) #t)
		(else (comlist:has-duplicates? (cdr lst)))
)	)

;;;@ remove duplicates of MEMBERs of a list
#|
(comlist:remove-duplicates '(Lae ti ti a))
 |#;->	(Lae ti a)
(define comlist:remove-duplicates
	(letrec	((rem-dup
					(lambda	(lst nlst)
						(cond	((null? lst) (reverse nlst))
							((member (car lst) nlst) (rem-dup (cdr lst) nlst))
							(else (rem-dup (cdr lst) (cons (car lst) nlst)))))))
		(lambda (lst) (rem-dup lst '()))
)	)
#|
(comlist:list* 'L 'a 'e '(t i t i a))
 |#;->	(L a e t i t i a)
(define comlist:list*
	(letrec	((list*1
					(lambda	(obj)
						(if	(null? (cdr obj))
							(car obj)
							(cons (car obj) (list*1 (cdr obj)))
			))		)	)
		(lambda (obj1 . obj2) (if (null? obj2) obj1 (cons obj1 (list*1 obj2))))
)	)
#| Perf:
(let*	((nbrRun 10000)(timeEnd 0)(timeStart (gettimeofday)))
	(let loop ((idx 0))
		(if (>= idx nbrRun) (time-stat timeStart (gettimeofday) nbrRun)
			(begin	
				(comlist:list* 'L 'a 'e '(t i t i a))
;				(cons* 'L 'a 'e '(t i t i a))
				(loop (+ idx 1))
)	)	)	)
 |#;->	1 s 507 ms 181 µs for 10K runs. 000 ms 150 µs by run
;		1 s 578 ms 130 µs for 10K runs. 000 ms 157 µs by run
#|
(comlist:delete 'ti '(Lae ti ti a))
 |#;->	(Lae a)
(define (comlist:delete obj lst)
	(let delete	((lst lst))
		(cond	((null? lst) '())
			((equal? obj (car lst)) (delete (cdr lst)))
			(else (set-cdr! lst (delete (cdr lst))) lst)
)	)	)
#|
(comlist:delete-if (lambda(symb)(eq? symb 'ti)) '(Lae ti ti a))
 |#;->	(Lae a)
(define (comlist:delete-if pred lst)
	(let delete-if	((lst lst))
		(cond	((null? lst) '())
			((pred (car lst)) (delete-if (cdr lst)))
			(else (set-cdr! lst (delete-if (cdr lst))) lst)
)	)	)
#|
(comlist:delete-if-not (lambda(symb)(not (eq? symb 'ti))) '(Lae ti ti a))
 |#;->	(Lae a)
(define (comlist:delete-if-not pred lst)
	(let delete-if	((lst lst))
		(cond	((null? lst) '())
			((not (pred (car lst))) (delete-if (cdr lst)))
			(else (set-cdr! lst (delete-if (cdr lst))) lst)
)	)	)
;_______________________________	Section 2 begin by AlSchemist 2021
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
(load "C:/Users/YourUserName/AppData/Roaming/GIMP/2.10/scripts/2slib-comlist.scm")
 |# (closure? comlist:has-duplicates?)