; https://srfi.schemers.org/srfi-32/srfi-32.html
; Scheme Requests For Implementation (srfi) https://srfi.schemers.org/
;;; The SRFI-32 sort package -- quick sort			-*- Scheme -*-
;;; Copyright (c) 1998 by Olin Shivers.
;;; This code is open-source.

; 2021/09/27: 	GimpλLib 1.0 by AlSchemist for Gimp 2.10.28 TinyScheme Script-Fu
;				Subset for TinyScheme adapted by AlSchemist
; all "<" or "elt<" to compare items have been replaced with "cmp"
; Line: 370 define: 18 comment: 169 = 45.6% blank: 3

; Extract from sort.scm
#|
(list-sort '(9 7 1) <)
 |#;->	(1 7 9)
(define (list-sort lst cmp)			; Sort lists by converting to
	(let ((vect (list->vector lst)))		; a vector and sorting that.
		(heap-sort! vect cmp)
		(vector->list vect)
)	)
#| extract from sortp.scm
(list-sorted? '(1 7 9) <) (list-sorted? '(9 7 1) <)
 |#;->	     #t                        #f
(define (list-sorted? lst cmp)
	(or (not (pair? lst))
		(let lp ((prev (car lst)) (tail (cdr lst)))
			(or (not (pair? tail))
				(let ((next (car tail)))
					(and (not (cmp next prev))
					(lp next (cdr tail)))
)	)	)	)	)
#|
(vector-sorted? #(1 7 9) <) (vector-sorted? #(9 7 1) <)
 |#;->	         #t                          #f
(define (vector-sorted? v cmp . maybe-start+end)
	(let-vector-start+end 'vector-sorted? v maybe-start+end
		(lambda(start end)
			(or (>= start end) ; Empty range
				(let lp ((i (+ start 1)) (vi-1 (vector-ref v start)))
					(or (>= i end)
						(let ((vi (vector-ref v i)))
							(and (not (cmp vi vi-1))
							(lp (+ i 1) vi))
)	)	)	)	)	)	)
; Extract from vqsort.scm
#| 
(quick-sort #(3 2 1) <)(quick-sort #(3 2 1) < 0 2)(quick-sort #(3 2 1) < 1)
 |#;->	     #(1 2 3)               #(2 3)                     #(1 2)
(define (quick-sort v cmp . maybe-start+end)
	(let-vector-start+end 'quick-sort v maybe-start+end
		(lambda(start end)
			(let ((ans (vector-copy v start end)))
				(%quick-sort! ans cmp 0 (- end start))
				ans
)	)	)	)
#| 
(let* ((vect #(9 7 1))) (quick-sort! vect <))
 |#;->	#(1 7 9)
(define (quick-sort! v cmp . maybe-start+end)
	(let-vector-start+end 'quick-sort! v maybe-start+end
		(lambda(start end) (%quick-sort! v cmp start end))
)	)
(define (vector-copy v . maybe-start+end) ; powered by Olin Shivers 1998
	(let-vector-start+end 'vector-copy v maybe-start+end
		(lambda(start end)
			(let*	(	(len (- end start))
						(ans (make-vector len))
					)
				(do (	(i (- len 1) (- i 1))
						(j (- end 1) (- j 1))
					)
					((< i 0) ans)
					(vector-set! ans i (vector-ref v j))
)	)	)	)	)
;;; This is not too well thought out, for a general utility.
(define (vector-copy! target src . maybe-start+end)
	(let-vector-start+end 'vector-copy src maybe-start+end
		(lambda(start end)
			(let ((len (- end start)))
				(do ((i (- len 1) (- i 1))
					(j (- end 1) (- j 1)))
					((< i 0))
					(vector-set! target i (vector-ref src j))
	)	)	)	)
	target ; AlSchemist 2021
)
#| moved out from %quick-sort! replacing receive values producer consumer
(median 7 9 1 <) (median 20 10 1 <)
 |#;->	 7                   10 
(define (median v1 v2 v3 cmp) ; powered by AlSchemist 2021
;	(show "median: " v1 " " v2 " " v3)
	(let*	( 							(little v2) 	 (big v1))
		(if (cmp v1 v2) (begin (set! 	 little	v1) (set! big v2)))
		(if (cmp big v3) big
			(if (cmp little v3) v3 little)
)	)	)
#| original body of median defined as lambda by Olin Shivers inside %quick-sort!:
	(receive (little big) ; vars
		(if (cmp v1 v2) (values v1 v2) (values v2 v1)) ; mv-exp producer
		(if (cmp big v3) big ; body consumer
			(if (cmp little v3) v3 little))
	)
 |#
; test and perf:
#| Sort a vector containing a list #((numkey1 char1 string1) (numkeyN charN stringN))
(let*	(	(nbrMax 10)
			(vRef (make-vector nbrMax 0)) ; unsorted vector of reference
			(vSor (make-vector nbrMax 0)) ; sorted vector
			(nbrRun	1000)(timeEnd 0)(timeStart 0)
		)
	(do	((idx 0 (inc idx)))
		((>= idx nbrMax))
		(vector-set! vRef idx 
			(list (- nbrMax idx) (integer->char (- 122 idx)) ; 10->1 z->q descending
				(atom->string(integer->char (+ idx 65))))    ; A->J
		)
;		(vector-set! vRef idx 
;			(list idx (integer->char (- 122 idx))			 ; 1->10 z->q ascending
;				(atom->string(integer->char (+ idx 65)))     ; A-J
;		)	)
	)
	(show "vRef: " vRef)
	(set! timeStart (gettimeofday))
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
;		(set! vSor (quick-sort  vRef (lambda (lst1 lst2) (< (car lst1) (car lst2))))) ; S1
		(set! vSor (insert-sort vRef (lambda (lst1 lst2) (< (car lst1) (car lst2))))) ; S2
;		(set! vSor (heap-sort   vRef (lambda (lst1 lst2) (< (car lst1) (car lst2))))) ; S3
	)
	(time-stat timeStart timeEnd nbrRun)
	(show "vSor: " vSor)
)
 |#;->
; vRef: #((10 z A) (9 y B) (8 x C) (7 w D) (6 v E) (5 u F) (4 t G) (3 s H) (2 r I) (1 q J))
; S1: 17 s  31 ms  39 µs for 1K runs. 17 ms  31 µs by run
; S2: 11 s 734 ms 413 µs for 1K runs. 11 ms 734 µs by run
; S3: 18 s 609 ms 372 µs for 1K runs. 18 ms 609 µs by run
; vSor: #((1 q J) (2 r I) (3 s H) (4 t G) (5 u F) (6 v E) (7 w D) (8 x C) (9 y B) (10 z A))
(define (%quick-sort! v cmp start end)
	(let recur ((l start) (r end))	; Sort the range [l,r).
;		(show "Sort the range [" l "," r "]")
		(if (< 5 (- r l))
			;; Choose the median of V[l], V[r], and V[middle] for the pivot.
			(let*	(	(pivot (median	(vector-ref v l)
										(vector-ref v (quotient (+ l r) 2))
										(vector-ref v (- r 1)) cmp)
					)	)
;				(show " pivot: " pivot)
				(let loop ((i l) (j (- r 1)))
					(let	(	(i	(let scan ((i i)) (if (cmp (vector-ref v i) pivot) (scan (+ i 1)) i)))
								(j	(let scan ((j j)) (if (cmp pivot (vector-ref v j)) (scan (- j 1)) j)))
							)
						(if (< i j)
							(let ((tmp (vector-ref v j)))
								(vector-set! v j (vector-ref v i))	; Swap V[I]
								(vector-set! v i tmp)		;  and V[J].
								(loop (+ i 1) (- j 1))
							)
							(begin (recur l i) (recur (+ j 1) r))
			)	)	)	)
			(%insert-sort! v cmp l r)
	)	)
	v ; AlSchemist 2021
)
; extract from visort.scm
#|
(insert-sort #(3 2 1) <)
 |#;->	#(1 2 3)
;;; %insert-sort! is also called from vqsort.scm's quick-sort function.
(define (insert-sort v cmp . maybe-start+end)
	(let-vector-start+end 'insert-sort v maybe-start+end
		(lambda(start end)
			(let ((ans (vector-copy v start end)))
				(%insert-sort! ans cmp 0 (- end start))
				ans
)	)	)	)
(define (insert-sort! v cmp . maybe-start+end)
	(let-vector-start+end 'insert-sort! v maybe-start+end
		(lambda(start end) (%insert-sort! v cmp start end))
)	)
(define (%insert-sort! v cmp start end)
	(do ((i (+ 1 start) (+ i 1)))	; Invariant: [start,i) is sorted.
		((>= i end))
		(let ((val (vector-ref v i)))
			(vector-set! v
				(let lp ((j i))		; J is the location of the
					(if (<= j start) start	; "hole" as it bubbles down.
						(let* (	(j-1 (- j 1))
								(vj-1 (vector-ref v j-1)))
							(cond 	(	(cmp val vj-1)
										(vector-set! v j vj-1)
										(lp j-1)
									)
									(else j)
				)	)	)	)
				val
	)	)	)
	v ; AlSchemist 2021
)
; extract from vhsort.scm
(define (heap-sort! v cmp . maybe-start+end)
	(let-vector-start+end 'heap-sort! v maybe-start+end
		(lambda(start end) (really-heap-sort! v cmp start end))
)	)
(define (heap-sort v cmp . maybe-start+end)
	(let-vector-start+end 'heap-sort v maybe-start+end
		(lambda(start end)
			(let	((ans (vector-copy v start end)))
				(really-heap-sort! ans cmp 0 (- end start))
				ans
)	)	)	)
;;; If a heap structure is embedded into a vector at indices [start,end), then:
;;;   1. The two children of index k are start + 2*(k-start) + 1 = k*2-start+1
;;;                                  and start + 2*(k-start) + 2 = k*2-start+2.
;;;
;;;   2. The first index of a leaf node in the range [start,end) is
;;;          first-leaf = floor[(start+end)/2]
;;;      (You can deduce this from fact #1 above.) 
;;;      Any index before FIRST-LEAF is an internal node.
;; Vector V contains a heap at indices [START,END).
;; The heap is in heap order in the range (I,END)
;; -- i.e., every element in this range is >= its children.
;; Bubble HEAP[I] down into the heap to impose heap order on the range [I,END).
(define (restore-heap! v cmp start end i)
	(let*	(	(vi (vector-ref v i))
				(first-leaf (quotient (+ start end) 2)) ; Can fixnum overflow.
				(final-k 
					(let lp ((k i))
						(if (>= k first-leaf) k ; Leaf, so done.
							(let*	(	(k*2-start (+ k (- k start))) ; Don't overflow.
										(child1 (+ 1 k*2-start))
										(child2 (+ 2 k*2-start))
										(child1-val (vector-ref v child1))
										(max-child		child1) ; receive vars
										(max-child-val	child1-val)
									)
								(if (< child2 end) ; mv-exp producer
									(let ((child2-val (vector-ref v child2)))
										(if (not (cmp child2-val child1-val))
											(begin	(set! max-child child2)
													(set! max-child-val child2-val)
									)	)	)
								) ; remove receive values prod/consu by AlSchemist
								(cond	((cmp vi max-child-val) ; body consumer
											(vector-set! v k max-child-val)
											(lp max-child)
										)
										(else k)
				)	)	)	)	)
			) ; Done.
		(vector-set! v final-k vi)
)	)
(define (really-heap-sort! v cmp start end)
	;; Put the unsorted subvector V[start,end) into heap order.
	(let ((first-leaf (quotient (+ start end) 2)))  ; Can fixnum overflow.
		(do ((i (- first-leaf 1) (- i 1)))
			((< i 0))
			(restore-heap! v cmp start end i)
	)	)
	(do ((i (- end 1) (- i 1)))
		((<= i 0))
		(let ((top (vector-ref v start)))
			(vector-set! v start (vector-ref v i))
			(vector-set! v i top)
			(restore-heap! v cmp start i start)
)	)	)
;;; Vector binary search. The vector must be sorted. Extract from vbinsearch.scm
;;; Olin Shivers 98/11.
#| Returns the index of the matching element otherwise #f
(vector-binary-search '#((1 . one) (3 . three) (4 . four) (25 . twenty-five)) < car 4)
(vector-binary-search '#((1   one) (3   three) (4   four) (25   twenty-five)) < car 4)
 |#;->	2                                      ; ^match at index 2                   ^key
(define (vector-binary-search v cmp elt->key key . maybe-start+end)
	(let-vector-start+end 'vector-binary-search v maybe-start+end
		(lambda(start end)
			(let lp ((left start) (right end))
				(and (< left right)
					(let* 	(	(m (quotient (+ left right) 2))
								(elt (vector-ref v m))
								(elt-key (elt->key elt))
							)
						(cond	((cmp key elt-key) (lp left m))
								((cmp elt-key key) (lp (+ m 1) right))
								(else m)
)	)	)	)	)	)	)
;;; This code is Copyright (c) 1998 by Olin Shivers.
;;; The terms are: You may do as you please with this code, as long as you 
;;; do not delete this notice or hold me responsible for any outcome related to its use.
; The software is provided "as is", without warranty of any kind, express or
; implied, including but not limited to the warranties of merchantability, 
; fitness for a particular purpose and noninfringement.
; In no event shall the authors or copyright holders be liable for any claim, 
; damages or other liability, whether in an action of contract, tort or
; otherwise, arising from, out of or in connection with the software or
; the use or other dealings in the software.
;_______________________________	Section 1 end

; tools powered by AlSchemist 2021 for the section 1
#| Handle defaulting & checking the optional START/END
(let-vector-start+end 'quick-sort! #(1 2 3 4) nil (lambda(start end) (show "start: " start " end: " end)))
 |#;->	                                       start: 0 end: 4
#|
(let-vector-start+end 'quick-sort! #(1 2 3 4) '(1) (lambda(start end) (show "start: " start " end: " end)))
 |#;->	                                       start: 1 end: 4
#|
(let-vector-start+end 'quick-sort! #(1 2 3 4) '(1 3) (lambda(start end) (show "start: " start " end: " end)))
 |#;->	                                       start: 1 end: 3
 #|
(let-vector-start+end 'quick-sort! #(1 2 3 4) '(Z *pi*) (lambda(start end) nil))
(let-vector-start+end 'quick-sort! #(1 2 3 4) '(-1 3)	(lambda(start end) nil))
(let-vector-start+end 'quick-sort! #(1 2 3 4) '( 3 1)	(lambda(start end) nil))
(let-vector-start+end 'quick-sort! #(1 2 3 4) '( 0 5)	(lambda(start end) nil))
 |#;->	Error: 3260. quick-sort! start & end must be integer 
 ;		Error: 3261. quick-sort! start must be >= 0 
 ;		Error: 3262. quick-sort! start must be < 1
 ;		Error: 3263. quick-sort! end must be <= 4
(define (let-vector-start+end caller str lstOpt proc) ; powered by AlSchemist 2021
	(if (pair? lstOpt)
		(let*	(	(start	(car lstOpt))
					(len	(vector-length str))
					(end	(if (= (length lstOpt) 2) (cadr lstOpt) len))
				)
			(cond	((or (not (integer? start))(not (integer? end)))
						(error "3260." caller 'start '& 'end 'must 'be 'integer)
					)
					((< start 0)  (error "3261." caller 'start 'must 'be '>= 0))
					((> start end)(error "3262." caller 'start 'must 'be '<  end))
					((> end len)  (error "3263." caller 'end   'must 'be '<= len))
					(else (proc start end))
		)	)
		(proc 0 (vector-length str))		
)	)
#| Perf
(let*	(	(nbrRun	2000)(timeEnd 0)(timeStart (gettimeofday))
		)
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
		(let-vector-start+end 'test1 #(1 2 3 4) nil (lambda(start end) nil))
	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->	78 ms 158 µs for 2K runs. 39 µs by run
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
(load "C:/Users/YourUserName/AppData/Roaming/GIMP/2.10/scripts/2srfi-032-sort.scm")
 |# (closure? median)