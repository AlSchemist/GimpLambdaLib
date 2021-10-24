; https://srfi.schemers.org/srfi-1/srfi-1.html
; SRFI: Scheme Requests for Implementation https://srfi.schemers.org/
; Section 1: SRFI-1 List

;;; Copyright (c) 1998, 1999 by Olin Shivers. You may do as you please with
;;; this code as long as you do not remove this copyright notice or
;;; hold me liable for its use. Please send bug reports to shivers@ai.mit.edu.
;;;     -Olin

; Section 2: list utilities by Oleg Kiselyov
; $Id: util.scm,v 2.6 2004/07/08 19:51:57 oleg Exp oleg $

; Section 3: list utilities by AlSchemist
; 2021/09/27: 	GimpλLib 1.0 by AlSchemist for Gimp 2.10.28 TinyScheme Script-Fu
;				Add multiline comments, examples and pretty print
; Line: 1319 define: 81 comment: 613 = 46.4% blank: 29

;_______________________________	Section 1 begin by Olin Shivers 1999
;;; Constructors
;;; Occasionally useful as a value to be passed to a fold or other
;;; higher-order procedure.
(define (xcons d a) (cons a d))

;;; Make a list of length LEN.
#|
(make-list 3)		(make-list 3 'ab)
 |#;->	(#f #f #f)	(ab ab ab)
(define (make-list len . maybe-elt)
	(check-arg (lambda (n) (and (integer? n) (>= n 0))) len 'make-list)
	(let	(	(elt(cond	((null? maybe-elt) #f) ; Default value
							((null? (cdr maybe-elt)) (car maybe-elt))
							(else (error "0100. make-list: too many arguments"
										len maybe-elt
			)	)	)		)	  )
;		(do (	(i len (- i 1))					; original
;				(ans '() (cons elt ans))
;			)
;			((<= i 0) ans)
;		)
		(vector->list (make-vector len elt))	; script-fu-compat.init
)	)
#| Perf:
(let*	((nbrRun 1000)(timeEnd 0)(timeStart (gettimeofday)))
	(let loop ((idx 0))
		(if (>= idx nbrRun) (time-stat timeStart (gettimeofday) nbrRun)
			(begin
;				(vector->list (make-vector 3 'Lae)) 
				(make-list 3 'Lae) ; do becomes loop cons
;				(make-list 3 'Lae) ; original macro do
				(loop (+ idx 1))
)	)	)	)
 |#;->		218 ms 246 µs for 1K runs. 000 ms 218 µs by run vector->list make-vector
 ;			234 ms 400 µs for 1K runs. 000 ms 234 µs by run do becomes loop cons
;		9 s 109 ms 012 µs for 1K runs. 009 ms 109 µs by run original macro do

;;; Make a list of length LEN. Elt i is (PROC i) for 0 <= i < LEN.
#|
(list-tabulate 3 (lambda(idx)(string (integer->char (+ idx 97)))))
 |#;->	("a" "b" "c")
(define (list-tabulate len proc)
	(check-arg (lambda (n) (and (integer? n) (>= n 0))) len 'list-tabulate)
	(check-arg procedure? proc 'list-tabulate)
	(let loop ((i (- len 1)) (ans '()))
		(if (< i 0) ans
			(loop (- i 1) (cons (proc i) ans))
)	)	)
#| Perf:
(let*	((nbrRun 1000)(timeEnd 0)(timeStart (gettimeofday)))
	(let loop ((idx 0))
		(if (>= idx nbrRun) (time-stat timeStart (gettimeofday) nbrRun)
			(begin
				(list-tabulate 3 (lambda(idx)(string (integer->char (+ idx 97))))) ; original
				(loop (+ idx 1))
)	)	)	)
 |#;->		703 ms 187 µs for 1K runs. 000 ms 703 µs by run do becomes let loop cons
;			890 ms 601 µs for 1K runs. 000 ms 890 µs by run do becomes let loop vector-set!
;		9 s 796 ms 407 µs for 1K runs. 009 ms 796 µs by run original macro do

;;; (cons* a1 a2 ... an) = (cons a1 (cons a2 (cons ... an)))
;;; (cons* a1) = a1	(cons* a1 a2 ...) = (cons a1 (cons* a2 ...))
;;;
;;; (cons first (unfold not-pair? car cdr rest values))
#|
(cons* "a" "b" (list "c"))
 |#;->	("a" "b" "c")
(define (cons* first . rest)
	(let recur ((x first) (rest rest))
		(if (pair? rest)
			(cons x (recur (car rest) (cdr rest)))
			x
)	)	)
;;; (unfold not-pair? car cdr lis values)
#| Duplicate lis
(list-copy '(h e l l o))
 |#;->	(h e l l o)
(define (list-copy lis)
	(let recur ((lis lis))
		(if (pair? lis)
			(cons (car lis) (recur (cdr lis)))
			lis
)	)	)
;;; IOTA count [start step]	(start start+step ... start+(count-1)*step)
#|
(iota 5)			(iota 5 5 2)
 |#;->	(0 1 2 3 4)	(5 7 9 11 13)
(define (iota count . maybe-start+step)
	(check-arg integer? count 'iota)
	(if (< count 0) (error "0110. iota: negative step count" count))
	(let*	(	(start (if (pair? maybe-start+step)(car maybe-start+step) 0))
				(step (if (= (length maybe-start+step) 2)(cadr maybe-start+step) 1))
			)
		(check-arg number? start 'iota)(check-arg number? step 'iota)
		(let loop ((n 0) (r '()))
			(if (= n count)
				(reverse r)
				(loop (+ 1 n)
					(cons (+ start (* n step)) r)
)	)	)	)	)
#| Caution: a circular list does not have any limit.
(let* ((lstCircle (circular-list 'a '(b c))))
	(do	(	(idx 0 (inc idx)))
		(	(>= idx 7)) ; retrieve the seven first values
			(show idx ": item: " (list-ref lstCircle idx))
)	)
 |#;->	0: item: a	1: item: (b c)	2: item: a	3: item: (b c)
;		4: item: a	5: item: (b c)	6: item: a	()
#|
(map + '(3 1 4 1) (circular-list 1 0))
 |#;->	   (4 1 5 1)
(define (circular-list val1 . vals)
	(let ((ans (cons val1 vals)))
		(set-cdr! (last-pair ans) ans)
		ans
)	)
;;; <proper-list> ::= ()			; Empty proper list
;;;		  |   (cons <x> <proper-list>)	; Proper-list pair
;;; Note that this definition rules out circular lists -- and this
;;; function is required to detect this case and return false.
#|
(proper-list? nil)(proper-list? '(a b c))(proper-list? (circular-list 'a '(b c)))
 |#;->	       #t				 #t						#f
(define (proper-list? x)
	(let lp ((x x) (lag x))
		(if (pair? x)
			(let ((x (cdr x)))
				(if (pair? x)
					(let	(	(x	(cdr x))
								(lag (cdr lag))
							)
						(and (not (eq? x lag)) (lp x lag))
					)
					(null? x)
			)	)
			(null? x)
)	)	)
;;; A dotted list is a finite list (possibly of length 0) terminated
;;; by a non-nil value. Any non-cons, non-nil value (e.g., "foo" or 5)
;;; is a dotted list of length 0.
;;;
;;; <dotted-list> ::= <non-nil,non-pair>	; Empty dotted list
;;;               |   (cons <x> <dotted-list>)	; Proper-list pair
#|
(dotted-list? '(a b c))(dotted-list? '(key . value))
 |#;->	         #f					      #t
(define (dotted-list? x)
	(let lp ((x x) (lag x))
		(if (pair? x)
			(let ((x (cdr x)))
				(if (pair? x)
					(let	((x	(cdr x))
						(lag (cdr lag)))
						(and (not (eq? x lag)) (lp x lag))
					)
					(not (null? x))
			)	)
			(not (null? x))
)	)	)
#|
(circular-list? (circular-list 'a '(b c)))(circular-list? '(a b c))
 |#;->	          #t											#f
(define (circular-list? x)
	(let lp ((x x) (lag x))
		(and (pair? x)
			(let ((x (cdr x)))
				(and (pair? x)
					(let	(	(x   (cdr x))
								(lag (cdr lag))
							)
						(or (eq? x lag) (lp x lag))

)	)	)	)	)	)
#|
(not-pair? nil)(not-pair? '(first))
 |#;->		#t				#f
(define (not-pair? x) (not (pair? x)))	; Inline me.

;;; This is a legal definition which is fast and sloppy:
;;;     (define null-list? not-pair?)
;;; but we'll provide a more careful one:
(define (null-list? l)
	(cond ((pair? l) #f)
		  ((null? l) #t)
		  (else (error "1320. null-list?: argument out of domain" l))
)	)
#|
(length+ '(a b c))(length+ (circular-list 'a '(b c)))
 |#;->	    3				#f
(define (length+ x)			; Returns #f if X is circular.
	(let lp ((x x) (lag x) (len 0))
		(if (pair? x)
			(let	(	(x (cdr x))
						(len (+ len 1))
					)
				(if (pair? x)
					(let 	((x	(cdr x))
								(lag (cdr lag))
								(len (+ len 1))
							)
						(and (not (eq? x lag)) (lp x lag len))
					)
					len
			)	)
			len
)	)	)
#| Make list of pairs key value from the list of keys and the list of values
(zip '(a b c) '(1 2 3))
 |#;->	((a 1) (b 2) (c 3))
(define (zip list1 . more-lists) (apply map list list1 more-lists))

;;;;;;;;;;;;;
;;; Selectors
(define first  car)
(define second cadr)
(define third  caddr)
(define fourth cadddr)
(define (fifth   x) (car    (cddddr x)))
(define (sixth   x) (cadr   (cddddr x)))
(define (seventh x) (caddr  (cddddr x)))
(define (eighth  x) (cadddr (cddddr x)))
(define (ninth   x) (car  (cddddr (cddddr x))))
(define (tenth   x) (cadr (cddddr (cddddr x))))
#|
(show (car+cdr '(left middle right)))
 |#;->	left
;		(middle right)
(define (car+cdr pair) (values (car pair) (cdr pair)))

#| Return the first k elements of lis
(take '(a b c d e) 3)		(take '(a) 2)
 |#;->	(a b c)				(a)
; AlSchemist added (or (null? lis)) if k > (length lis)
(define (take lis k)
	(let recur ((lis lis) (k (check-arg integer? k 'take)))
		(if (or (zero? k) (null? lis)) '() (cons (car lis) (recur (cdr lis) (- k 1))))
)	)
#| Perf:
(let*	(	(lstInt (iota 1000))
			(nbrRun	1000)(timeEnd 0)(timeStart (gettimeofday))
		)
	(let loop ((idx 0))
		(if (>= idx nbrRun) (time-stat timeStart (gettimeofday) nbrRun)
			(begin	
				(list-head lstInt 50)
;				(take lstInt 50)
				(loop (+ idx 1))
)	)	)	)
 |#;->	3 s 500 ms 011 µs for 1K runs. 003 ms 500 µs by run list-head
;		3 s 781 ms 248 µs for 1K runs. 003 ms 781 µs by run take

#| Drop 3 first items keeping the rest
(drop '(a b c d e) 3)	(drop '(a) 2)
 |#;->	(d e)			()
(define (drop lis k)
	(let iter ((lis lis) (k (check-arg integer? k 'drop)))
		(if (or (zero? k) (null? lis)) lis (iter (cdr lis) (- k 1)))
)	)
#| Perf:
(let*	(	(lstInt (iota 1000))
			(nbrRun	500)(timeEnd 0)(timeStart (gettimeofday))
		)
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
;		(drop lstInt 500)
		(list-tail lstInt 500)
	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->	6 s 546 ms 923 µs for 500 runs. 13 ms  93 µs by run list-tail
;		6 s 578 ms 169 µs for 500 runs. 13 ms 156 µs by run drop
; line 242 C:\Program Files\GIMP 2\share\gimp\2.0\scripts\script-fu.init

#|
(let* ((lst '(a b c d e))) (take! lst 3))
 |#;->	(a b c)
(define (take! lis k)
	(check-arg integer? k 'take!)
	(if (zero? k) '()
		(begin (set-cdr! (drop lis (- k 1)) '()) lis)
)	)
;;; TAKE-RIGHT and DROP-RIGHT work by getting two pointers into the list,
;;; off by K, then chasing down the list until the lead pointer falls off
;;; the end.
#|
(take-right '(a b c d e) 3)
 |#;->	(c d e)
(define (take-right lis k)
	(check-arg integer? k 'take-right)
	(let lp ((lag lis) (lead (drop lis k)))
		(if (pair? lead) (lp (cdr lag) (cdr lead)) lag)
)	)
#| Perf:
(duration '(list-from-tail	'(a b c d e) 3) 10000)
(duration '(take-right		'(a b c d e) 3) 10000)
 |#;->	    953 ms 145 µs for 10K runs.  95 µs by run list-from-tail
 ;		1 s 843 ms 753 µs for 10K runs. 184 µs by run take-right
#|
(drop-right '(a b c d e) 3)
 |#;->	(a b)
(define (drop-right lis k)
	(check-arg integer? k 'drop-right)
	(let recur ((lag lis) (lead (drop lis k)))
		(if (pair? lead) (cons (car lag) (recur (cdr lag) (cdr lead))) '())
)	)
;;; In this function, LEAD is actually K+1 ahead of LAG. 
;;; This lets us stop LAG one step early, in time to smash its cdr to ().
#|
(let* ((lst '(a b c d e))) (drop-right! lst 3))
 |#;->	(a b)
(define (drop-right! lis k)
	(check-arg integer? k 'drop-right!)
	(let ((lead (drop lis k)))
		(if (pair? lead)
			(let lp ((lag lis) (lead (cdr lead)))
				(if (pair? lead)
					(lp (cdr lag) (cdr lead))
					(begin (set-cdr! lag '()) lis)))
			'()	; Special case dropping everything -- no cons to side-effect.
)	)	)
#|
(show (split-at '(a b c g h i j k) 3))
 |#;->			 (a b c)				; the first three items
;					   (g h i j k)		; the rest
#| Insert lstMiddle after the third item of lstIn
(let*	(	(lstIn '(a b c g h i j k)) (lstMiddle '(D E F)) (idxSplitter 3))
	(append (take lstIn idxSplitter) lstMiddle (drop lstIn idxSplitter))
)
 |#;->	(a b c D E F g h i j k)
#| call-with-values calls with 2 functions: a multiple values producer, their consumer
(let*	(	(lstIn '(a b c g h i j k)) (lstMiddle '(D E F)) (idxSplitter 3))
	(call-with-values (lambda()(split-at lstIn idxSplitter)) ; produce two sublists
		(lambda(lstLeft lstRight)(append lstLeft lstMiddle lstRight)) ; consume them
	);	 ^the consumer combines the multiple values into a single value like a list
)
 |#;->	(a b c D E F g h i j k)
(define (split-at x k)
	(check-arg integer? k 'split-at)
	(let recur ((lis x) (k k))
		(if (zero? k)
			(values '() lis)
			(call-with-values (lambda () (recur (cdr lis) (- k 1))) ; receive producer
				(lambda (prefix suffix)	; receive consumer
					(values (cons (car lis) prefix) suffix)
)	)	)	)	)
#| Perf
(let*	(	(lstIn '(a b c g h i j k)) (lstMiddle '(D E F)) (idxSplitter 3)
			(nbrRun	10000)(timeEnd 0)(timeStart (gettimeofday))
		)
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
		(append (take lstIn idxSplitter) lstMiddle (drop lstIn idxSplitter))
	)	(time-stat timeStart timeEnd nbrRun)
)
(let*	(	(lstIn '(a b c g h i j k)) (lstMiddle '(D E F)) (idxSplitter 3)
			(nbrRun	10000)(timeEnd 0)(timeStart (gettimeofday))
		)
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
		(call-with-values (lambda()(split-at lstIn idxSplitter)) ; produce 2 sublists
			(lambda(lstLeft lstRight)(append lstLeft lstMiddle lstRight)) ; consume
		)
	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->	2 s 874 ms 527 µs for 10K runs. 287 µs by run take and drop
;		5 s 187 ms   2 µs for 10K runs. 518 µs by run call-with-values
#|
(let*	(	(lstIn '(a b c g h i j k)) (lstMiddle '(D E F)) (idxSplitter 3))
	(call-with-values (lambda()(split-at! lstIn idxSplitter))
		(lambda(lstLeft lstRight)(append lstLeft lstMiddle lstRight))
)	)
 |#;->	(a b c D E F g h i j k)
(define (split-at! x k)
	(check-arg integer? k 'split-at!)
	(if (zero? k)
		(values '() x)
		(let* ((prev (drop x (- k 1))) (suffix (cdr prev)))
			(set-cdr! prev '())
			(values x suffix)
)	)	)
#|
(last-pair '(first middle last))
 |#;->	(last)
(define (last-pair lis)
	(check-arg pair? lis 'last-pair)
	(let lp ((lis lis)) (let ((tail (cdr lis))) (if (pair? tail) (lp tail) lis))
)	)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unzippers -- 1 through 5
#|
(unzip1 '((key1 val1)(key2 val2)(key3 val3)))
 |#;->	(key1 key2 key3)
(define (unzip1 lis) (map car lis))
#| AlSchemist: receive macro should be too slow. It's replaced with call-with-values
(show (unzip2 '((key1 val1)(key2 val2)(key3 val3))))
 |#;->	(key1 key2 key3)
;		(val1 val2 val3)
(define (unzip2 lis)
	(let recur ((lis lis))
		(if (null-list? lis)
			(values lis lis)
			(let ((elt (car lis)))
				(call-with-values (lambda () (recur (cdr lis))) ; was receive
					(lambda (a b) (values (cons (car elt) a) (cons (cadr elt) b)))
)	)	)	)	)
#|
(show (unzip3 '((left1 mid1 right1)(left2 mid2 right2)(left3 mid3 right3))))
 |#;->	(left1 left2 left3)
;		(mid1 mid2 mid3)
;		(right1 right2 right3)
(define (unzip3 lis)
	(let recur ((lis lis))
		(if (null-list? lis)
			(values lis lis lis)
			(let ((elt (car lis)))
				(call-with-values (lambda () (recur (cdr lis))) ; was receive
					(lambda (a b c)	(values	(cons (car elt) a)
											(cons (cadr elt) b)
											(cons (caddr elt) c)
)	)	)	)	)	)				)
#|
(show (unzip4 '((a 1 A -1)(b 2 B -2)(c 3 C -3))))
 |#;->	(a b c)
;		(1 2 3)
;		(A B C)
;		(-1 -2 -3)
(define (unzip4 lis)
	(let recur ((lis lis))
		(if (null-list? lis)
			(values lis lis lis lis)
			(let ((elt (car lis)))
				(call-with-values (lambda () (recur (cdr lis))) ; was receive
					(lambda (a b c d)	(values
											(cons (car elt) a)
											(cons (cadr elt) b)
											(cons (caddr elt) c)
											(cons (cadddr elt) d)
)	)	)	)	)	)					)
#|
(show (unzip5 '((a 1 A -1 Monday)(b 2 B -2 Tuesday)(c 3 C -3 Wednesday))))
 |#;->	(a b c)
;		(1 2 3)
;		(A B C)
;		(-1 -2 -3)
;		(Monday Tuesday Wednesday)
(define (unzip5 lis)
	(let recur ((lis lis))
		(if (null-list? lis)
			(values lis lis lis lis lis)
			(let ((elt (car lis)))
				(call-with-values (lambda () (recur (cdr lis))) ; was receive
					(lambda (a b c d e)	(values	(cons (car elt) a)
												(cons (cadr elt) b)
												(cons (caddr elt) c)
												(cons (cadddr elt) d)
												(cons (car (cddddr elt)) e)
)	)	)	)	)	)					)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(append-reverse '(left middle) '(right))
 |#;->	(middle left right)
(define (append-reverse rev-head tail) (fold cons tail rev-head))
#|
(append-reverse '(left middle) '(right))
 |#;->	(middle left right)
(define (append-reverse! rev-head tail)
	(let lp ((rev-head rev-head) (tail tail))
		(if (null-list? rev-head)
			tail
			(let ((next-rev (cdr rev-head)))
				(set-cdr! rev-head tail)
				(lp next-rev rev-head)
)	)	)	)
#|
(concatenate '((a b) (c) (d e f)))
 |#;->	(a b c d e f)
(define (concatenate  lists) (reduce-right append  '() lists))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fold/map internal utilities
;;; These little internal utilities are used by the general
;;; fold & mapper funs for the n-ary cases . It'd be nice if they got inlined.
;;; One the other hand, the n-ary cases are painfully inefficient as it is.
;;; An aggressive implementation should simply re-write these functions
;;; for raw efficiency; I have written them for as much clarity, portability,
;;; and simplicity as can be achieved.
;;;
;;; I use the dreaded call/cc to do local aborts. A good compiler could
;;; handle this with extreme efficiency. An implementation that provides
;;; a one-shot, non-persistent continuation grabber could help the compiler
;;; out by using that in place of the call/cc's in these routines.
;;;
;;; These functions have funky definitions that are precisely tuned to
;;; the needs of the fold/map procs -- for example, to minimize the number
;;; of times the argument lists need to be examined.

;;; Return (map cdr lists).
;;; However, if any element of LISTS is empty, just abort and return '().
#|
(%cdrs '((a b c) (d e) (f)))	(%cdrs '((a b c) (d e) () (f)))
 |#;->	((b c) (e) ())			()
(define (%cdrs lists)
	(call-with-current-continuation
		(lambda (abort)
			(let recur ((lists lists))
				(if (pair? lists)
					(let ((lis (car lists)))
						(if (null-list? lis)
							(abort '())
							(cons (cdr lis) (recur (cdr lists)))
					)	)
					nil
)	)	)	)	)
#|
(%cars+ '((a b c) (d e) (f)) '(last element))
 |#;->	(a d f (last element))
(define (%cars+ lists last-elt) ; (append! (map car lists) (list last-elt))
	(let recur ((lists lists))
		(if (pair? lists) (cons (caar lists) (recur (cdr lists))) (list last-elt))
)	)
;;; LISTS is a (not very long) non-empty list of lists.
;;; Return two lists: the cars & the cdrs of the lists.
;;; However, if any of the lists is empty, just abort and return [() ()].
#|
(show (%cars+cdrs '((a b c) (d e) (f))))	(show (%cars+cdrs '((a b c) () (f))))
 |#;->	(a d f)								()
;		((b c) (e) ())						()
(define (%cars+cdrs lists)
	(call-with-current-continuation
		(lambda (abort)
			(let recur ((lists lists))
				(if (pair? lists) ; AlSchemist: all call-with-values were receive
					(call-with-values (lambda () (car+cdr lists))
						(lambda (list other-lists)
							(if (null-list? list)
								(abort '() '())
								(call-with-values (lambda () (car+cdr list))
									(lambda (a d)
										(call-with-values (lambda () (recur other-lists))
											(lambda (cars cdrs)	
												(values (cons a cars) (cons d cdrs))
					)	)	)	)	)	)	)
					(values '() '())
)	)	)	)	)
;;; Like %CARS+CDRS, but we pass in a final elt tacked onto the end of the
;;; cars list. What a hack.
#|
(show (%cars+cdrs+ '((a b c) (d e) (f)) '(cars final)))
 |#;->	(a d f (cars final))
;		((b c) (e) ())
(define (%cars+cdrs+ lists cars-final)
	(call-with-current-continuation
		(lambda (abort)
			(let recur ((lists lists))
				(if (pair? lists) ; AlSchemist: all call-with-values were receive
					(call-with-values (lambda () (car+cdr lists))
						(lambda (list other-lists)
							(if (null-list? list)
								(abort '() '())
								(call-with-values (lambda () (car+cdr list))
									(lambda (a d)
										(call-with-values (lambda () (recur other-lists))
											(lambda (cars cdrs)	
												(values (cons a cars) (cons d cdrs))
					)	)	)	)	)	)	)
					(values (list cars-final) '())
)	)	)	)	)
;;; Like %CARS+CDRS, but blow up if any list is empty.
#|
(show (%cars+cdrs/no-test '((a b c) (d e)))) (show (%cars+cdrs/no-test nil))
 |#;->	(a d)									()
;		((b c) (e))								()
(define (%cars+cdrs/no-test lists)
	(let recur ((lists lists))
		(if (pair? lists)
			(receive
				(list other-lists)
				(car+cdr lists)
				(receive
					(a d)
					(car+cdr list)
					(receive
						(cars cdrs)
						(recur other-lists)
						(values (cons a cars) (cons d cdrs)))))
			(values '() '())
)	)	)
;;;;;;;;;
;;; count
#|
(count (lambda(n)(> n 3)) '(1 2 3 4 5 6 7))
 |#;->	4
#|
(count (lambda(n)(> n 3)) '(1 2 3 4) '((1 a)(2 b)(3 c)(0 d)))
 |#;->	1
(define (count pred list1 . lists)
	(check-arg procedure? pred 'count)
	(if (pair? lists)
		(let lp ((list1 list1) (lists lists) (i 0))
			(if (null-list? list1) i
				(call-with-values (lambda () (%cars+cdrs lists))
					(lambda (as ds)
						(if (null? as) i
							(lp (cdr list1) ds 
								(if (apply pred (car list1) as) (+ i 1) i)
		)	)	)	)	)	)
		(let lp ((lis list1) (i 0))
			(if (null-list? lis) i (lp (cdr lis) (if (pred (car lis)) (+ i 1) i)))
)	)	)

;;; fold/unfold
#| (append-reverse '(left middle) '(right))
(unfold-right null-list? car cdr '(left middle) '(right))
 |#;->	(middle left right)
#|
(unfold-right null? car cdr '(R a c e C a r) '(in a mirror))
 |#;->	(r a C e c a R in a mirror)
(define (unfold-right pNull? firstSeed gNext seed . maybe-tail)
	(check-arg procedure? pNull?	'unfold-right)
	(check-arg procedure? firstSeed 'unfold-right)
	(check-arg procedure? gNext		'unfold-right)
	(let lp ((seed seed) (ans (if (pair? maybe-tail)(car maybe-tail) nil)))
		(if (pNull? seed) ans (lp (gNext seed) (cons (firstSeed seed) ans)))
)	)
#|
(unfold null? (lambda(seed) (symbol->string (car seed))) cdr '(R a c e C a r) 
	(lambda(seed) '(": as string" ))
)
 |#;->	("R" "a" "c" "e" "C" "a" "r" ": as string")
 #| List of squares: 1^2 ... 10^2
(unfold (lambda (x) (> x 10)) (lambda (x) (* x x)) (lambda (x) (+ x 1)) 1)
 |#;->	(1 4 9 16 25 36 49 64 81 100)
(define (unfold pNull? firstSeed gNext seed . maybe-tail-gen)
	(check-arg procedure? pNull?	'unfold)
	(check-arg procedure? firstSeed 'unfold)
	(check-arg procedure? gNext		'unfold)
	(if (pair? maybe-tail-gen)
		(let ((tail-gen (car maybe-tail-gen)))
			(if (pair? (cdr maybe-tail-gen))
				(apply error "unfold: too many arguments" maybe-tail-gen)
				(let recur ((seed seed))
					(if (pNull? seed) (tail-gen seed)
						(cons (firstSeed seed) (recur (gNext seed)))
		)	)	)	)
		(let recur ((seed seed))
			(if (pNull? seed) '()
				(cons (firstSeed seed) (recur (gNext seed)))
)	)	)	)
#|
(fold cons nil '(a b c))
 |#;->	(c b a)
(define (fold kons knil lis1 . lists)
	(check-arg procedure? kons 'fold)
	(if (pair? lists)
		(let lp ((lists (cons lis1 lists)) (ans knil)) ; N-ary case
			(receive
				(cars+ans cdrs)
				(%cars+cdrs+ lists ans)
				(if (null? cars+ans) ans ; Done.
					(lp cdrs (apply kons cars+ans))
		)	)	)
		(let lp ((lis lis1) (ans knil)) ; Fast path
			(if (null-list? lis) ans (lp (cdr lis) (kons (car lis) ans))
)	)	)	)
#|
(fold-right cons nil '(a b c))
 |#;->	(a b c)
(define (fold-right kons knil lis1 . lists)
	(check-arg procedure? kons 'fold-right)
	(if (pair? lists)
		(let recur ((lists (cons lis1 lists))) ; N-ary case
			(let ((cdrs (%cdrs lists)))
				(if (null? cdrs) knil (apply kons (%cars+ lists (recur cdrs))))))
		(let recur ((lis lis1)) ; Fast path
			(if (null-list? lis)
				knil
				(let ((head (car lis))) (kons head (recur (cdr lis))))
)	)	)	)
;;; REDUCE and REDUCE-RIGHT only use RIDENTITY in the empty-list case.
;;; These cannot meaningfully be n-ary.
#|
(reduce max 0 '(2 4 1 7))
 |#;->	7
(define (reduce f ridentity lis)
	(check-arg procedure? f 'reduce)
	(if (null-list? lis) ridentity (fold f (car lis) (cdr lis)))
)
#|
(reduce-right append '() '((a b c)(d e f)))
 |#;->	(a b c d e f)
(define (reduce-right f ridentity lis)
	(check-arg procedure? f 'reduce-right)
	(if (null-list? lis)
		ridentity
		(let recur ((head (car lis)) (lis (cdr lis)))
			(if (pair? lis) (f head (recur (car lis) (cdr lis))) head)
)	)	)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mappers:
#|
(append-map (lambda (x)(list x (string->symbol(string-upcase(symbol->string x))))) '(a b c))
 |#;->	(a A b B c C)
#|
(append-map (lambda (x)(list x (string-upcase x))) '("a" "b" "c"))
 |#;->	("a" "A" "b" "B" "c" "C")
(define (append-map f lis1 . lists)
	(really-append-map 'append-map  append  f lis1 lists)
)
(define (really-append-map who appender f lis1 lists)
	(check-arg procedure? f who)
	(if (pair? lists)
		(call-with-values (lambda () (%cars+cdrs (cons lis1 lists)))
			(lambda (cars cdrs)
				(if (null? cars)
					'()
					(let recur ((cars cars) (cdrs cdrs))
						(let ((vals (apply f cars)))
							(receive
								(cars2 cdrs2)
								(%cars+cdrs cdrs)
								(if (null? cars2)
									vals
									(appender vals (recur cars2 cdrs2))
		)	)	)	)	)	)	)
		(if (null-list? lis1)
			'()
			(let recur ((elt (car lis1)) (rest (cdr lis1)))
				(let ((vals (f elt)))
					(if (null-list? rest)
						vals
						(appender vals (recur (car rest) (cdr rest)))
)	)	)	)	)	)
#| Perf: append
(let*	(	; add specific init
			(nbrRun	10000)(timeEnd 0)(timeStart (gettimeofday))
		)
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
		(append-map (lambda (x)(list x (string-upcase x))) '("a" "b" "c"))
	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->	4 s               for 10K runs. 445 µs by run srfi
;		7 s 733 ms 845 µs for 10K runs. 773 µs by run apply append map below
#|
(define (append-map f lis1 . lists)
	(if (pair? lists)
		(apply append (map f (append lis1 lists)))
		(apply append (map f lis1))
)	)
 |#

#|
(pair-for-each display '(1 2 3))
 |#;->	(1 2 3)(2 3)(3)()
#|
(pair-for-each show '(1 2 3) '(a b c d))
 |#;->	(1 2 3)(a b c d)
;		(2 3)(b c d)
;		(3)(c d)
;		()
(define (pair-for-each proc lis1 . lists)
	(check-arg procedure? proc 'pair-for-each)
	(if (pair? lists)
		(let lp ((lists (cons lis1 lists)))
			(let ((tails (%cdrs lists)))
				(if (pair? tails) (begin (apply proc lists) (lp tails)))))
		(let lp ((lis lis1))				;; Fast path.
			(if (not (null-list? lis))
				(let ((tail (cdr lis))) 	; Grab the cdr now,
					(proc lis) (lp tail)	; in case PROC SET-CDR!s LIS.
)	)	)	)	)
;;; We stop when LIS1 runs out, not when any list runs out.
#|
(let* ((lstIn '(1 2 3))) (map! inc lstIn))
(let* ((lstIn '(1 2 3))) (map! inc lstIn '(a b c)))	
 |#;->	(2 3 4)			(2 2 3)
(define (map! f lis1 . lists)
	(check-arg procedure? f map!)
	(if (pair? lists)
		(let lp ((lis1 lis1) (lists lists))
			(if (not (null-list? lis1))
				(receive
					(heads tails)
					(%cars+cdrs/no-test lists)
					(set-car! lis1 (apply f (car lis1) heads))
					(lp (cdr lis1) tails)
		)	)	)
		;; Fast path.
		(pair-for-each (lambda (pair) (set-car! pair (f (car pair)))) lis1))
	lis1
)
;;; Map F across L, and save up all the non-false results.
#|
(let* ((lstIn '(1 2 3))) (filter-map inc lstIn))
(let* ((lstIn '(1 2 3))) (filter-map inc lstIn '(a b c)))	
 |#;->	(2 3 4)			  (2 3 4)
(define (filter-map f lis1 . lists)
	(check-arg procedure? f 'filter-map)
	(if (pair? lists)
		(let recur ((lists (cons lis1 lists)))
			(receive
				(cars cdrs)
				(%cars+cdrs lists)
				(if (pair? cars)
					(cond ((apply f cars) => (lambda (x) (cons x (recur cdrs))))
						  (else (recur cdrs)))
					'())))
		(let recur ((lis lis1))
			(if (null-list? lis)
				lis
				(let ((tail (recur (cdr lis))))
					(cond ((f (car lis)) => (lambda (x) (cons x tail))) (else tail))
)	)	)	)	)
;;; Map F across lists, guaranteeing to go left-to-right.
;;; NOTE: Some implementations of R5RS MAP are compliant with this spec;
;;; in which case this procedure may simply be defined as a synonym for MAP.
#|
(map-in-order inc '(1 2 3))	(map-in-order inc '(1 2 3) '(a b c))
 |#;->	(2 3 4)				(2 3 4)
(define (map-in-order f lis1 . lists)
	(check-arg procedure? f 'map-in-order)
	(if (pair? lists)
		(let recur ((lists (cons lis1 lists)))
			(receive
				(cars cdrs)
				(%cars+cdrs lists)
				(if (pair? cars)
					(let ((x (apply f cars))) ; Do head first,
						(cons x (recur cdrs)) ; then tail.
					) '()
		)	)	)
		(let recur ((lis lis1))
			(if (null-list? lis)
				lis
				(let ((tail (cdr lis))
					(x (f (car lis))))		; Do head first,
					(cons x (recur tail))	; then tail.
)	)	)	)	)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILTER, REMOVE, PARTITION and their destructive counterparts do not
;;; disorder the elements of their argument.

;; This FILTER shares the longest tail of L that has no deleted elements.
;; If Scheme had multi-continuation calls, they could be made more efficient.
#| Filter from the list lis each empty string
(filter (lambda (s)(positive?(string-length s))) '("" "func" "prm" ""))
 |#;->	("func" "prm")

#|
(filter (lambda (n) (zero? (remainder n 2))) '(0 7 8 8 43 -4))
 |#;->	(0 8 8 -4)
(define (filter pred lis) ; Sleazing with EQ? makes this one faster.
	(check-arg procedure? pred 'filter)
	(let recur ((lis lis))
		(if (null-list? lis) ; Use NOT-PAIR? to handle dotted lists.
			lis
			(let ((head (car lis)) (tail (cdr lis)))
				(if (pred head)
					(let ((new-tail (recur tail))) ; Replicate the RECUR call so
						(if (eq? tail new-tail) lis (cons head new-tail)))
					(recur tail) ; this one can be a tail call.
)	)	)	)	)
#| Perf:
(let*	((nbrRun 10000)(timeEnd 0)(timeStart (gettimeofday)))
	(let loop ((idx 0))
		(if (>= idx nbrRun) (time-stat timeStart (gettimeofday) nbrRun)
			(begin	
				(filter (lambda (s)(positive?(string-length s))) '("" "func" "prm" ""))
				(loop (+ idx 1))
)	)	)	)
 |#;->	2 s 983 ms 877 µs for 10K runs. 000 ms 298 µs by run
;;; This implementation of FILTER!
;;; - doesn't cons, and uses no stack;
;;; - is careful not to do redundant SET-CDR! writes, as writes to memory are
;;;   usually expensive on modern machines, and can be extremely expensive on
;;;   modern Schemes (e.g., ones that have generational GC's).
;;; It just zips down contiguous runs of in and out elts in LIS doing the
;;; minimal number of SET-CDR!s to splice the tail of one run of ins to the
;;; beginning of the next.
#| Filter from the list lis each empty string
(let* ((lst '("" "func" "prm" ""))) (filter! (lambda (s)(positive?(string-length s))) lst))
 |#;->	("func" "prm")
#|
(let* ((lst '(0 7 8 8 43 -4))) (filter! (lambda (n) (zero? (remainder n 2))) lst))
 |#;->	(0 8 8 -4)
(define (filter! pred lis)
	(check-arg procedure? pred 'filter!)
	(let lp ((ans lis))
		(cond ((null-list? ans) ans) ; Scan looking for
			  ((not (pred (car ans))) (lp (cdr ans))) ; first cons of result.
			  (else
				(letrec
;; SCAN-IN: (CDR PREV) = LIS and (CAR PREV) satisfies PRED.
;; Scan over a contiguous segment of the list that satisfies PRED.
					(	(scan-in(lambda (prev lis)
										(if (pair? lis)
											(if (pred (car lis))
												(scan-in lis (cdr lis))
												(scan-out prev (cdr lis))
						)		)		)	)
;; SCAN-OUT: (CAR PREV) satisfies PRED. Scan over a contiguous segment of the list
;; that *doesn't* satisfy PRED. When the segment ends, patch in a link from PREV
;; to the start of the next good segment, and jump to SCAN-IN.        
						(scan-out
							(lambda (prev lis)
								(let lp ((lis lis))
									(if (pair? lis)
										(if (pred (car lis))
											(begin	(set-cdr! prev lis)
													(scan-in lis (cdr lis))
											)
											(lp (cdr lis))
										)
										(set-cdr! prev lis)
					)	)	)	)	)
				   (scan-in ans (cdr ans))
				   ans ; ANS is the eventual answer.
)	)	)	)	)
#| Perf:
(let*	(	(lst '("" "func" "prm" ""))
			(nbrRun 10000)(timeEnd 0)(timeStart (gettimeofday))
		)
	(let loop ((idx 0))
		(if (>= idx nbrRun) (time-stat timeStart (gettimeofday) nbrRun)
			(begin	
				(filter! (lambda (s)(positive?(string-length s))) lst)
				(loop (+ idx 1))
)	)	)	)
 |#;->	3 s 800 ms 469 µs for 10K runs. 000 ms 380 µs by run
;;; Answers share common tail with LIS where possible;
;;; the technique is slightly subtle.
#|
(show (partition (lambda (n) (< n 3)) '(1 2 3 4 5)))
 |#;->	(1 2)
;		(3 4 5)
(define (partition pred lis)
	(check-arg procedure? pred 'partition)
	(let recur ((lis lis))
		(if (null-list? lis) (values lis lis)
			(let ((elt (car lis)) (tail (cdr lis)))
				(call-with-values (lambda () (recur tail)) ; was receive
					(lambda (in out)
						(if (pred elt)
							(values (if (pair? out) (cons elt in) lis) out)
							(values in (if (pair? in) (cons elt out) lis))
)	)	)	)	)	)	)
;;; This implementation of PARTITION!
;;; - doesn't cons, and uses no stack;
;;; - is careful not to do redundant SET-CDR! writes, as writes to memory are
;;;   usually expensive on modern machines, and can be extremely expensive on
;;;   modern Schemes (e.g., ones that have generational GC's).
;;; It just zips down contiguous runs of in and out elts in LIS doing the
;;; minimal number of SET-CDR!s to splice these runs together into the result
;;; lists.

(define (partition! pred lis)
	(check-arg procedure? pred 'partition!)
	(if (null-list? lis) (values lis lis)
		;; This pair of loops zips down contiguous in & out runs of the list,
		;; splicing the runs together.
		(letrec ((scan-in ;;   invariant SCAN-IN:  (cdr in-prev)  = LIS.
					 (lambda (in-prev out-prev lis)
						 (let lp ((in-prev in-prev) (lis lis))
							 (if (pair? lis)
								 (if (pred (car lis))
									 (lp lis (cdr lis))
									 (begin
										 (set-cdr! out-prev lis)
										 (scan-out in-prev lis (cdr lis))))
								 (set-cdr! out-prev lis))))) ; Done.
				 (scan-out ;;   invariant SCAN-OUT: (cdr out-prev) = LIS.
					 (lambda (in-prev out-prev lis)
						 (let lp ((out-prev out-prev) (lis lis))
							 (if (pair? lis)
								 (if (pred (car lis))
									 (begin
										 (set-cdr! in-prev lis)
										 (scan-in lis out-prev (cdr lis)))
									 (lp lis (cdr lis)))
								 (set-cdr! in-prev lis)))))) ; Done.
			(if (pred (car lis)) ;; Crank up the scan&splice loops.
				;; LIS begins in-list. Search for out-list's first pair.
				(let lp ((prev-l lis) (l (cdr lis)))
					(cond ((not (pair? l)) (values lis l))
						  ((pred (car l)) (lp l (cdr l)))
						  (else (scan-out prev-l l (cdr l)) (values lis l)))) ; Done.
				;; LIS begins out-list. Search for in-list's first pair.
				(let lp ((prev-l lis) (l (cdr lis)))
					(cond ((not (pair? l)) (values l lis))
						  ((pred (car l)) (scan-in l prev-l (cdr l)) (values l lis))
						  (else (lp l (cdr l)))                       ;^Done.
)	)	)	)	)	)
;;; Inline us, please.
#|
(remove  (lambda(symb)(eq? symb 'c)) '(a b c d))
(remove! (lambda(symb)(eq? symb 'c)) '(a b c d))
 |#;->	(a b d)
(define (remove  pred l) (filter  (lambda (x) (not (pred x))) l))
(define (remove! pred l) (filter! (lambda (x) (not (pred x))) l))
;;; Here's the taxonomy for the DELETE/ASSOC/MEMBER functions.
;;; (I don't actually think these are the world's most important
;;; functions -- the procedural FILTER/REMOVE/FIND/FIND-TAIL variants
;;; are far more general.)
;;;
;;; Function			Action
;;; ---------------------------------------------------------------------------
;;; remove pred lis		Delete by general predicate
;;; delete x lis [=]		Delete by element comparison
;;;
;;; find pred lis		Search by general predicate
;;; find-tail pred lis		Search by general predicate
;;; member x lis [=]		Search by element comparison
;;;
;;; assoc key lis [=]		Search alist by key comparison
;;; alist-delete key alist [=]	Alist-delete by key comparison

#| Return the #1 element of list that satisfies predicate pred; #f if no element does
(find even? '(3 1 4 1 5 9))
 |#;->	4
(define (find pred lst) (cond ((find-tail pred lst) => car) (else #f)))
#| Return the #1 pair of list whose car satisfies pred. If no pair does, return #f
(find-tail even? '(3 1 37 -8 -5 0 0))	(find-tail even? '(3 1 37 -5))
 |#;->	(-8 -5 0 0)						#f
(define (find-tail pred lst)
	(check-arg procedure? pred 'find-tail)
	(let lp ((lst lst))
		(and (not (null-list? lst)) (if (pred (car lst)) lst (lp (cdr lst))))
)	)
#| Longest initial prefix of list whose elements all satisfy the predicate pred.
(take-while even? '(2 18 3 10 22 9))
(take-while! even? '(2 18 3 10 22 9))
 |#;->	(2 18)
(define (take-while pred lis)
	(check-arg procedure? pred 'take-while)
	(let recur ((lis lis))
		(if (null-list? lis) '()
			(let ((x (car lis))) (if (pred x) (cons x (recur (cdr lis))) '()))
)	)	)
(define (take-while! pred lis)
	(check-arg procedure? pred 'take-while!)
	(if (or (null-list? lis) (not (pred (car lis)))) '()
		(begin
			(let lp ((prev lis) (rest (cdr lis)))
				(if (pair? rest)
					(let ((x (car rest)))
						(if (pred x) (lp rest (cdr rest)) (set-cdr! prev '())))))
			lis
)	)	)
#| Drops the longest initial prefix of list whose elements all satisfy predicate pred
(drop-while even? '(2 18 3 10 22 9))
 |#;->	(3 10 22 9)
; returns the rest of the list
(define (drop-while pred lis)
	(check-arg procedure? pred 'drop-while)
	(let lp ((lis lis))
		(if (null-list? lis) '() (if (pred (car lis)) (lp (cdr lis)) lis))
)	)
#| splits into longest initial prefix satisfying pred, and the remaining tail
(show (span  even? '(2 18 3 10 22 9)))	(show (span! even? '(2 18 3 10 22 9)))
 |#;->	(2 18)
;		(3 10 22 9)
(define (span pred lis)
	(check-arg procedure? pred 'span)
	(let recur ((lis lis))
		(if (null-list? lis)
			(values '() '())
			(let ((x (car lis)))
				(if (pred x)
					(call-with-values (lambda () (recur (cdr lis)))
						(lambda (prefix suffix)
							(values (cons x prefix) suffix)
					)	)
					(values '() lis)
)	)	)	)	)
(define (span! pred lis)
	(check-arg procedure? pred 'span!)
	(if (or (null-list? lis) (not (pred (car lis))))
		(values '() lis)
		(let ((suffix
				  (let lp ((prev lis) (rest (cdr lis)))
					  (if (null-list? rest)
						  rest
						  (let ((x (car rest)))
							  (if (pred x)
								  (lp rest (cdr rest))
								  (begin (set-cdr! prev '()) rest)))))))
			(values lis suffix)
)	)	)
#| breaks the list at the first element satisfying pred
(show (break  even? '(3 1 4 1 5 9)))	(show (break! even? '(3 1 4 1 5 9)))
 |#;->	(3 1)
;		(4 1 5 9)
(define (break  pred lis) (span  (lambda (x) (not (pred x))) lis))
(define (break! pred lis) (span! (lambda (x) (not (pred x))) lis))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#| Return #t if the predicate returns #t on any application
(any integer? '(a 3 b 2.7))(any integer? '(a 3.1 b 2.7))
 |#;->	#t					#f
 #|
(any <	'(3 1 4 1 5)
		'(2 7 1 8 2)
)
 |#;->	   #t
#| 
(any <	'(3 1 4 1 5)
		'(2 0 1 0 2)
)
  |#;->	#f ; all elements of the first list are superior to their corresponding item
(define (any pred lis1 . lists)
	(check-arg procedure? pred 'any)
	(if (pair? lists) ; AlSchemist: all call-with-values were receive
		(call-with-values (lambda () (%cars+cdrs (cons lis1 lists)))
			(lambda (heads tails)
				(and (pair? heads)
					(let lp ((heads heads) (tails tails))
						(call-with-values (lambda () (%cars+cdrs tails))
							(lambda (next-heads next-tails)
								(if (pair? next-heads)
									(or (apply pred heads) (lp next-heads next-tails))
									(apply pred heads)
		)	)	)	)	)	)	)
		(and (not (null-list? lis1))
			(let lp ((head (car lis1)) (tail (cdr lis1)))
				(if (null-list? tail)
					(pred head)
					(or (pred head) (lp (car tail) (cdr tail)))
)	)	)	)	)
#| Return #t if the predicate returns true on every application
(every integer? '(a 3 b 2.7))(every integer? '(a 3.1 b 2.7))(every integer? '(1 2 3))
 |#;->	#f					  #f							 #t
(define (every pred lis1 . lists)
	(check-arg procedure? pred 'every)
	(if (pair? lists)
		(call-with-values (lambda () (%cars+cdrs (cons lis1 lists)))
			(lambda (heads tails)
				(or (not (pair? heads))
					(let lp ((heads heads) (tails tails))
						(call-with-values (lambda () (%cars+cdrs tails))
							(lambda (next-heads next-tails)
								(if (pair? next-heads)
									(and (apply pred heads) (lp next-heads next-tails))
									(apply pred heads)
		)	)	)	)	)	)	)
		(or (null-list? lis1)
			(let lp ((head (car lis1)) (tail (cdr lis1)))
;				(show "every: lis1: " lis1 " head: " head " tail: " tail)
				(if (null-list? tail)
					(pred head)
					(and (pred head) (lp (car tail) (cdr tail)))
)	)	)	)	)
#| Return the index of the leftmost element that satisfies pred.
(list-index (lambda(x)(= x 4)) '(3 1 4 1 5 9))
(list-index even? '(3 1 4 1 5 9)) (list-index < '(3 1 4 1 5 9 2 5 6) '(2 7 1 8 2))
 |#;->					2							1
#|
(list-index = '(3 1 4 1 5 9 2 5 6) '(2 7 1 8 2))
 |#;->	#f 
(define (list-index pred lis1 . lists)
	(check-arg procedure? pred 'list-index)
	(if (pair? lists)
		(let lp ((lists (cons lis1 lists)) (n 0))
			(receive
				(heads tails)
				(%cars+cdrs lists)
				(and (pair? heads) (if (apply pred heads) n (lp tails (+ n 1))))))
		(let lp ((lis lis1) (n 0))
			(and (not (null-list? lis)) (if (pred (car lis)) n (lp (cdr lis) (+ n 1)))
)	)	)	)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;_______________________________	Section 1 end

; tools powered by AlSchemist 2021 for the section 1

;_______________________________	Section 2 begin by Oleg Kiselyov 2004
																
; http://okmij.org/ftp/README.html by Oleg Kiselyov. mij: made in Japan
; The following is free open source copyrighted Oleg Kiselyov
; All multiline comments and pretty prints are adapted by AlSchemist
; http://okmij.org/ftp/Scheme/util.html#list-util
#| inserts a VAL between elements of the list SRC-L
(list-intersperse '(a b c) '=)
 |#;->	(a = b = c)
; -- procedure+: list-intersperse SRC-L ELEM
; inserts ELEM between elements of the SRC-L, returning a freshly allocated list (cells, that is)
(define (list-intersperse src-l elem)
	(if (null? src-l) src-l
		(let loop ((l (cdr src-l)) (dest (cons (car src-l) '())))
			(if (null? l) (reverse dest)
				(loop (cdr l) (cons (car l) (cons elem dest)))
)	)	)	) 
;_______________________________	Section 3 begin by AlSchemist 2021

; The following is free TinyScheme open source copyrighted AlSchemist

#| list of nbrItem first items. Equivalent to take
(list-head '(a b c d e f) 2)
 |#;->	(a b)
(define (list-head lstin nbrItem)
	(let*	((lstOut nil))
		(let loop ((idxNbr 0) (lstObj lstin))
			(if (>= idxNbr nbrItem) (reverse lstOut)
				(if (pair? lstObj)
					(begin
						(set! lstOut (cons (car lstObj) lstOut))
						(loop (+ idxNbr 1) (cdr lstObj))
					)
					(reverse lstOut)
)	)	)	)	)
#| Perf:
(let*	((nbrRun 10000)(timeEnd 0)(timeStart (gettimeofday)))
	(let loop ((idx 0))
		(if (>= idx nbrRun) (time-stat timeStart (gettimeofday) nbrRun)
			(begin	
				(list-head '(a b c d e f) 2)
;				(take '(a b c d e f) 2)
				(loop (+ idx 1))
)	)	)	)
 |#;->	1 s 718 ms 270 µs for 10K runs. 000 ms 171 µs by run
;		1 s 984 ms 426 µs for 10K runs. 000 ms 198 µs by run

; line 242 C:\Program Files\GIMP 2\share\gimp\2.0\scripts\script-fu.init
#| From 4th item until the end of the list
(list-tail '(a b c d e f) 4)
 |#;->	(e f)
#| 4 items from the tail of the list
(list-from-tail '(a b c d e f) 4)
 |#;->	(c d e f)
(define (list-from-tail lst nbr) (list-tail lst (- (length lst) nbr)))

;-
#| list of values that match key in each pair
(pair-vals '((range all) (hue 0)(saturation -0.40)(lightness 0)
    		 (range red) (hue 0)(saturation 0)	  (lightness 0)) nil 'range)
 |#;->	(all red)
#| list of values that match key in each pair
(pair-vals '((range all) (hue 0)(saturation -0.40)(lightness 0)
    		 (range red) (hue 0)(saturation 0)	  (lightness 0)) nil)
 |#;->	(all 0 -0,4.0 0 red 0 0 0)
(define (pair-vals lstPair lstOut . maybe-key)
	(let* ((isKey? (pair? maybe-key)))
		(if (not(pair? lstPair)) (reverse lstOut)
			(if (eq? isKey? #t)
				(pair-vals (cdr lstPair) 
					(if (eqv? (car maybe-key) (caar lstPair)) (cons (cadar lstPair) lstOut) lstOut)
					(car maybe-key)
				)
				(pair-vals (cdr lstPair)(cons (cadar lstPair) lstOut))
)	)	)	)
#| value in pair at idx
(pair-val '((saturation -0.40000000000000008)) 0)
 |# ;-> -0,4.0
(define (pair-val lstPair idx) (cadr (list-ref lstPair idx)))
(define (pair-key lstPair idx) (car  (list-ref lstPair idx)))

#| Return the number of arguments of the given closure that is to say a defined proc
(arity void) 	(arity arity)	(arity car)		(arity string-take)	(arity string-fold)
 |#;->	0				1				#f				2					3
(define (arity proc)
	(if (closure? proc)
		(let* ((lstPrm (cadr (get-closure-code proc))))
			(if (list? lstPrm) (length lstPrm) (length+ lstPrm))
		) #f ; primitive procedures or macros are not taken in consideration
)	)
#| Perf:
(let*	((nbrRun 10000)(timeEnd 0)(timeStart (gettimeofday)))
	(let loop ((idx 0))
		(if (>= idx nbrRun) (set! timeEnd (gettimeofday))
			(begin	
				(arity string-take) ; mandatory arguments
;				(arity string-fold) ; optional final parameter
				(loop (+ idx 1))
	)	)	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->		759 ms 633 µs for 10K runs. 000 ms 075 µs by run (arity string-take)
;		2 s 224 ms 358 µs for 10K runs. 000 ms 222 µs by run (arity string-fold)
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
(load "C:/Users/YourUserName/AppData/Roaming/GIMP/2.10/scripts/1srfi-001-list.scm")
 |# (closure? check-arg)