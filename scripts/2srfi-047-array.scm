; https://srfi.schemers.org/srfi-47/
; SRFI: Scheme Requests for Implementation https://srfi.schemers.org/

; Section 1: Aubrey Jaffer  2006	SRFI-047 Array SLIB 3b6-1
;			 AlSchemist		2021	TinyScheme integration for Gimp 2.10.28 Script-Fu
;									Add multiline comments, examples and pretty print
; Section 2: AlSchemist      2021	Redefinition of equal? becomes array=?
; 2021/09/27: GimpλLib 1.0 by AlSchemist for Gimp 2.10.28 TinyScheme Script-Fu
; Line: 648 define: 24 comment: 307 = 47.3% blank: 14

;_______________________________	Section 1 begin by Aubrey Jaffer 2006
; https://people.csail.mit.edu/jaffer/SLIB.html
; http://cvs.savannah.gnu.org/viewvc/*checkout*/slib/slib/array.scm?content-type=text%2Fvnd.viewcvs-markup&revision=HEAD
;;;;"array.scm" Arrays for Scheme
; Copyright (C) 2001, 2003, 2005, 2006 Aubrey Jaffer
;
; Permission to copy this software, to modify it, to redistribute it,
; to distribute modified versions, and to use it for any purpose is
; granted, subject to the following restrictions and understandings.
; 
; 1.  Any copy made of this software must include this copyright notice in full.
; 
; 2.  I have made no warranty or representation that the operation of
; this software will be error-free, and I am under no obligation to
; provide any services, by way of maintenance, update, or otherwise.
; 
; 3.  In conjunction with products arising from the use of this
; material, there shall be no use of my name in any advertising,
; promotional, or sales literature without prior written consent in
; each case.

;;@code{(require 'array)} or @code{(require 'srfi-63)}
;;@ftindex array

;(require 'record) ; srfi-9
;(require 'multiarg-apply)
#| Create the type array as a record
(show array:rtd)
 |#;->	record type array (dimensions scales offset store)
(define array:rtd
	(make-record-type "array" '(dimensions scales offset store)
)	)	;                              dim scales list, exact integer, data store
#|
(define matrix (list->array 2 '#() '((1 2 3) (4 5 6))))
(show matrix)
 |#;->	0: 1 2 3
;		1: 4 5 6
#|
(array:dimensions matrix)
 |#;->	(2 3)
(define array:dimensions
	(let*	(	(dimensions (record-accessor array:rtd 'dimensions)))
		(lambda	(array)
			(cond	; check strict-array first : AlSchemist 2021
				((strict-array? array) (dimensions array))
				((vector? array) (list (vector-length array)))
				((string? array) (list (string-length array)))
				(else #f)
)	)	)	)
#|
(array:scales matrix)
 |#;->	(3 1)
(define array:scales
	(let*	((scales (record-accessor array:rtd 'scales)))
		(lambda	(obj)
			(cond	((strict-array? obj) (scales obj))
				((or (string? obj) (vector? obj)) '(1))
				(else #f)
)	)	)	)
#| Internal linear vector to store the data of the array
(array:store matrix)
 |#;->	#(1 2 3 4 5 6)
(define array:store
	(let*	((store (record-accessor array:rtd 'store)))
		(lambda	(obj)
			(cond	((strict-array? obj) (store obj))
				((or (string? obj) (vector? obj)) obj)
				(else #f)
)	)	)	)
#|
(array:offset matrix)
 |#;->	0
(define array:offset
	(let*	((offset (record-accessor array:rtd 'offset)))
		(lambda	(obj)
			(cond	((strict-array? obj) (offset obj))
				((or (string? obj) (vector? obj)) 0)
				(else #f)
)	)	)	)
; Internal builder for make-array defining the 4 tags of a generated array
(define array:construct
	(record-constructor array:rtd '(dimensions scales offset store))
)
#| Returns #t if the obj is an array, and #f if not.
(array? matrix)
 |#;->	#t
;; Arrays are not disjoint from other Scheme types such as string and vector
;;Vectors and possibly strings also satisfy array?.
(define array? (lambda (obj) (or (strict-array? obj) (string? obj) (vector? obj))))

#| Recursively compares the contents of arrays
(array=? (make-array (A:fixN32b 4) 5 3)(make-array (A:fixN32b 4) 5 3))
 |#;->	#t
#|
(array=? (make-array '#(foo) 3 3) (make-array '#(foo) 3 3))
 |#;->	#t
#|
(array=? "abc" "abc")	(array=? (make-vector 5 'a)(make-vector 5 'a))
 |#;->	#t				#t
; Was a redefinition of equal. AlSchemist changed to array=?
;; Returns #t if obj1 and obj2 have the same rank and dimensions and the
;; corresponding elements of obj1 and obj2 are equal?.
(define (array=? obj1 obj2)
	(cond
		((and (strict-array? obj1) (strict-array? obj2))
			(and	(equal? (array:dimensions obj1) (array:dimensions obj2))
					(letrec	(	(rascan
									(lambda	(dims idxs)	; arRay scanner
										(if	(null? dims)
											(equal?
												(apply array-ref obj1 idxs)
												(apply array-ref obj2 idxs))
											(do	((res	#t
														(rascan
															(cdr dims)
															(cons idx idxs)))
													(idx (+ -1 (car dims)) (+ -1 idx)))
												((or (not res) (negative? idx)) res)
							)	)	)	)	)
						(rascan (reverse (array:dimensions obj1)) '())
		)	)		)
		((and (string? obj1) (string? obj2)) (string=? obj1 obj2))
		((and (vector? obj1) (vector? obj2))
			(and	(equal? (vector-length obj1) (vector-length obj2))
					(do	((idx (+ -1 (vector-length obj1)) (+ -1 idx)))
						((or	(negative? idx)
								(not	(equal?
											(vector-ref obj1 idx)
											(vector-ref obj2 idx)
						 )		)		)
							(negative? idx)
		)	)	)	)
		(else #f)
)	)
#| Returns the number of dimensions. 0 if not an array
(array-rank matrix)
 |#;->	2
(define (array-rank obj) (if (array? obj) (length (array:dimensions obj)) 0))
#| Returns a list of dimensions. Alias transforming separator ":" to "-"
(array-dimensions matrix)
 |#;->	(2 3)
(define array-dimensions array:dimensions)

;; Creates and returns an array of type prototype of given dimensions
;; and filled with elements from prototype.
;; prototype must be an array, vector, or string.
;; The implementation-dependent type of the returned array
;; will be the same as the type of prototype; 
;; except if that would be a vector or string with rank not equal to one,
;; in which case some variety of array will be returned.
;;
;; If the prototype has no elements, then the initial contents of the returned
;; array are unspecified.
;; Otherwise, the returned array is filled with the element at the origin of prototype.
#| Return a prototypical uniform-array enclosing the optional argument
(show (make-array '#(foo) 2 3))
 |#;->	0: foo foo foo
 ;		1: foo foo foo
 ; If the uniform-array type is supported by the implementation, then it is returned; 
 ; defaulting to the next larger precision type; resorting finally to vector.
(define (make-array prototype . dimensions)
	(define prot (array:store prototype))
	(define pdims (array:dimensions prototype))
	(define onedim? (eqv? 1 (length dimensions)))
	(define tcnt (apply * dimensions))
	(let	((initializer
					(if	(zero? (apply * pdims))
						'()
						(list (apply array-ref prototype (map (lambda (x) 0) pdims)))
			))		)
		(cond	((and onedim? (string? prot))
					(apply make-string (car dimensions) initializer))
			((and onedim? (vector? prot))
				(apply make-vector (car dimensions) initializer))
			(else
				(let	((store	(if	(string? prot)
									(apply make-string tcnt initializer)
									(apply make-vector tcnt initializer)
						))		)
					(define (loop dims scales)
						(if	(null? dims)
							(array:construct dimensions (cdr scales) 0 store)
							(loop (cdr dims) (cons (* (car dims) (car scales)) scales))
					)	)
					(loop (reverse dimensions) '(1))
)	)	)	)	)
;; alias for make-array.
(define create-array make-array)

;; Create shared subarrays of other arrays.
;; The mapper is a function that translates coordinates in the new array 
;; into coordinates in the old array. A mapper must be linear. 
;; Its range must stay within the bounds of the old array,
;; but it can be otherwise arbitrary.
#| From an array 8*8 of 64 booleans, create the array of the big diagonal
(define chessboard (make-array '#(#f) 8 8))
(define big-diagonal (make-shared-array chessboard (lambda (row) (list row row)) 8))
(array-set! big-diagonal 'pawn 3)
(show chessboard)
 |#;->	0: #f #f #f   #f #f #f #f #f
;		1: #f #f #f   #f #f #f #f #f
;		2: #f #f #f   #f #f #f #f #f
;		3: #f #f #f pawn #f #f #f #f
;		4: #f #f #f   #f #f #f #f #f
;		5: #f #f #f   #f #f #f #f #f
;		6: #f #f #f   #f #f #f #f #f
;		7: #f #f #f   #f #f #f #f #f
#|
(define center (make-shared-array chessboard (lambda(ro co)(list(+ 3 ro)(+ 3 co))) 2 2))
(array-set! center 'e4 0 0)
(show chessboard)
 |#;->	0: #f #f #f #f #f #f #f #f
;		1: #f #f #f #f #f #f #f #f
;		2: #f #f #f #f #f #f #f #f
;		3: #f #f #f e4 #f #f #f #f
;		4: #f #f #f #f #f #f #f #f
;		5: #f #f #f #f #f #f #f #f
;		6: #f #f #f #f #f #f #f #f
;		7: #f #f #f #f #f #f #f #f
; The mapper is a function that translates coordinates in the new array 
; into coordinates in the old array. 
(define (make-shared-array array mapper . dimensions)
	(define odl (array:scales array))
	(define rank (length dimensions))
	(define shape
		(map (lambda (dim) (if (list? dim) dim (list 0 (+ -1 dim)))) dimensions)
	)
	(do	((idx (+ -1 rank) (+ -1 idx))
			(uvt	(if	(zero? rank) '()
						(append (cdr (vector->list (make-vector rank 0))) '(1))
					)
					(append (cdr uvt) '(0))
			)
			(uvts '() (cons uvt uvts)))
		((negative? idx)
			(let	((ker0 (apply + (map * odl (apply mapper uvt)))))
				(array:construct
					(map (lambda (dim) (+ 1 (- (cadr dim) (car dim)))) shape)
					(map	(lambda	(uvt)
								(- (apply + (map * odl (apply mapper uvt))) ker0)
							)
							uvts)
					(apply	+ (array:offset array)
							(map * odl (apply mapper (map car shape)))
					)
					(array:store array)
)	)	)	)	)
;; Returns an array of rank and type consisting of all the elements, 
;; in row-major order, of lst.
;; lst must be a rank-nested list consisting of all the elements,
;; in row-major order, of the array to be created.
;; When rank is 0, lst is the lone array element; not necessarily a list.
#|
(define square (list->array 2 '#() '((1 2) (3 4))))
(show square)
 |#;->	0: 1 2
;		1: 3 4
;; @result{} #2A((1 2) (3 4))
#|            ^  this specific header of array is displayed by Common Lisp (CL)
(list->array 0 '#() 3)
 |#;->	#(#((rtd) "array" () (dimensions scales offset store) 5 #<CLOSURE> #<CLOSURE>) () () 0 #(3))
;; @result{} #0A 3
(define (list->array rank proto lst)
	(define dimensions
		(do	((shp '() (cons (length row) shp))
				(row lst (car lst))
				(rnk (+ -1 rank) (+ -1 rnk)))
			((negative? rnk) (reverse shp))
	)	)
	(let	((nra (apply make-array proto dimensions)))
		(define (l2ra dims idxs row)
			(cond	((null? dims) (apply array-set! nra row (reverse idxs)))
				((if	(not (eqv? (car dims) (length row)))
						(error 'list->array 'non-rectangular 'array dims dimensions))
					(do	((idx 0 (+ 1 idx)) (row row (cdr row)))
						((>= idx (car dims)))
						(l2ra (cdr dims) (cons idx idxs) (car row))
		)	)	)	)
		(l2ra dimensions '() lst)
		nra
)	)
;;Returns a rank-nested list consisting of all the elements, in row-major order, of ra.  
;; In the case of a rank-0 array, @0 returns the single element.
#|
(array->list matrix)
 |#;->	((1 2 3) (4 5 6))
#| Common Lisp:   (array->list	 #2A((ho ho ho) (ho oh oh)))
(array->list (list->array 2 '#()   '((ho ho ho) (ho oh oh))))
 |#;->								((ho ho ho) (ho oh oh))
;;						  @result{} ((ho ho ho) (ho oh oh))
#| Common Lisp:		(array->list #0A ho)
(array->list (list->array 1 '#()   '(ho)))
|#;->								(ho)
;;						   @result{} ho
(define (array->list ra)
	(define (ra2l dims idxs) ; arRay to list
		(if	(null? dims)
			(apply array-ref ra (reverse idxs))
			(do	((lst '() (cons (ra2l (cdr dims) (cons idx idxs)) lst))
					(idx (+ -1 (car dims)) (+ -1 idx)))
				((negative? idx) lst)
	)	)	)
	(ra2l (array:dimensions ra) '())
)
;; Returns an array of type prototype consisting of all the elements, in row-major
;; order, of vect.  In the case of a rank-0 array, vect has a single element.
#| Linear initialization of an array of dimension 2 through a given vector 
(show (vector->array #(1 2 3 4) #() 2 2))
 |#;->	0: 1 2
;		1: 3 4
;;                @result{} #2A((1 2) (3 4))
#|
(show (vector->array '#(3) '#()))
 |#;->				  () () 0 #(3)
;; Common Lisp:   @result{} #0A 3
;; vect is a vector of length = to the product of exact nonnegative integers dimensions
(define (vector->array vect prototype . dimensions)
	(define vdx (vector-length vect))
	(if	(not (eqv? vdx (apply * dimensions)))
		(error 'vector->array vdx '<> (cons '* dimensions))
	)
	(let	((ra (apply make-array prototype dimensions)))
		(define (v2ra dims idxs) ; vector to arRay
			(cond	((null? dims)
						(set! vdx (+ -1 vdx))
						(apply array-set! ra (vector-ref vect vdx) (reverse idxs))
					)
				(else
					(do	((idx (+ -1 (car dims)) (+ -1 idx)))
						((negative? idx) vect)
						(v2ra (cdr dims) (cons idx idxs))
		)	)	)	)
		(v2ra dimensions '())
		ra)
)
;; Returns a new vector consisting of all the elements of ra in row-major order.
#| In Common Lisp: (array->vector #2A ((1 2)( 3 4)))
(array->vector   (list->array 2 '#() '((1 2) (3 4))))
 |#;->						#(1 2 3 4)
;;                @result{} #(1 2 3 4)
#| In Common Lisp: (array->vector #0A ho)
(define ar1 (list->array 1 '#() '(ho)))
(array->vector ar1) (array? ar1) (vector? ar1)(strict-array? ar1)(record-instance? ar1)
 |#;->		 #(ho)			#t				#t				#f						#f
;; @result{} #(ho)
; Convert the given array to a new vector
(define (array->vector ra)
	(define dims (array:dimensions ra))
	(let*	((vdx (apply * dims)) (vect (make-vector vdx)))
		(define (ra2v dims idxs) ; arRay to vector
			(if	(null? dims)
				(let	((val (apply array-ref ra (reverse idxs))))
					(set! vdx (+ -1 vdx))
					(vector-set! vect vdx val))
				(do	((idx (+ -1 (car dims)) (+ -1 idx)))
					((negative? idx) vect)
					(ra2v (cdr dims) (cons idx idxs))
		)	)	)
		(ra2v dims '())
		vect)
)
#| Returns #t if its arguments would be acceptable to array-ref.
(array:in-bounds? square '(0 1))	(array:in-bounds? square '(0 2))
 |#;->						 #t									 #f
(define (array:in-bounds? array indices)
	(do	((bnds (array:dimensions array) (cdr bnds)) (idxs indices (cdr idxs)))
		((or	(null? bnds) (null? idxs)
				(not (integer? (car idxs)))
				(not (< -1 (car idxs) (car bnds))))
			(and (null? bnds) (null? idxs))
)	)	)
#| The "-" syntax accepts optional indices while the ":" syntax accepts a list
(array-in-bounds? square 0 1)	(array-in-bounds? square 0 2)
 |#;->					  #t							  #f
(define (array-in-bounds? array . indices) (array:in-bounds? array indices))

#| Returns the (index1, index2, ...) element of array
(array-ref square 0 0)(array-ref square 0 1)(array-ref square 1 0)(array-ref square 1 1)
 |#;->			  1						2					  3						4
(define (array-ref array . indices)
	(define store (array:store array))
	(or (array:in-bounds? array indices) (error 'array-ref 'bad-indices indices))
	((if (string? store) string-ref vector-ref)
		store
		(apply + (array:offset array) (map * (array:scales array) indices))
)	)
;; Stores obj in the (index1, index2, ...) element of array
;; The return value is the result of vector-set! or string-set!
#|
(define matrix (vector->array #(1 2 3 4) #() 2 2))
(array-set! matrix 42 1 1)
(show matrix)
 |#;->	0: 1  2
;		1: 3 42
(define (array-set! array obj . indices)
	(define store (array:store array))
	(or (array:in-bounds? array indices) (error 'array-set! 'bad-indices indices))
	((if (string? store) string-set! vector-set!)
		store
		(apply + (array:offset array) (map * (array:scales array) indices))
		obj
)	)
;; Return a prototypical uniform-array enclosing the optional argument
;; (which must be of the correct type).
;; If the uniform-array type is supported by the implementation, then it is returned;
;; defaulting to the next larger precision type; resorting finally to vector.
#| Generate a vector of the verified initial default value
((make-prototype-checker 'A:bool boolean? vector) #t)
 |#;->	#(#t)
(define (make-prototype-checker name pred? creator)
	(lambda	args
		(case	(length args)
			((1)
				(if	(pred? (car args))
					(creator (car args))
					(error name 'incompatible 'type (car args))
			)	)
			((0) (creator))
			(else (error name 'wrong 'number 'of 'args args))
)	)	)
#|
((integer-bytes?? 1) 255)	((integer-bytes?? 1) 256)	((integer-bytes?? 2) 65536)
 |#;->				  #t						  #f							#f
(define (integer-bytes?? n)
	(lambda	(obj)
		(and	(integer? obj)
				(exact? obj)
				(or (negative? n) (not (negative? obj)))
				(do	((num obj (quotient num 256)) (n (+ -1 (abs n)) (+ -1 n)))
					((or (zero? num) (negative? n)) (zero? num))
)	)	)		)
;; Returns an inexact 128-bit flonum complex uniform-array prototype.
(define A:floC128b (make-prototype-checker 'A:floC128b complex? vector))
;; Returns an inexact 64-bit flonum complex uniform-array prototype.
(define A:floC64b (make-prototype-checker 'A:floC64b complex? vector))
;; Returns an inexact 32-bit flonum complex uniform-array prototype.
(define A:floC32b (make-prototype-checker 'A:floC32b complex? vector))
;; Returns an inexact 16-bit flonum complex uniform-array prototype.
(define A:floC16b (make-prototype-checker 'A:floC16b complex? vector))

;; Returns an inexact 128-bit flonum real uniform-array prototype.
(define A:floR128b (make-prototype-checker 'A:floR128b real? vector))
;; Returns an inexact 64-bit flonum real uniform-array prototype.
(define A:floR64b (make-prototype-checker 'A:floR64b real? vector))
;; Returns an inexact 32-bit flonum real uniform-array prototype.
(define A:floR32b (make-prototype-checker 'A:floR32b real? vector))
;; Returns an inexact 16-bit flonum real uniform-array prototype.
(define A:floR16b (make-prototype-checker 'A:floR16b real? vector))

;; Returns an exact 128-bit decimal flonum rational uniform-array prototype.
(define A:floR128d (make-prototype-checker 'A:floR128d real? vector))
;; Returns an exact 64-bit decimal flonum rational uniform-array prototype.
(define A:floR64d (make-prototype-checker 'A:floR64d real? vector))
;; Returns an exact 32-bit decimal flonum rational uniform-array prototype.
(define A:floR32d (make-prototype-checker 'A:floR32d real? vector))

;; Returns an exact binary fixnum uniform-array prototype with at least:
;; 64-bit of precision.
(define A:fixZ64b (make-prototype-checker 'A:fixZ64b (integer-bytes?? -8) vector))
;; 32-bit of precision.
(define A:fixZ32b (make-prototype-checker 'A:fixZ32b (integer-bytes?? -4) vector))
;; 16-bit of precision.
(define A:fixZ16b (make-prototype-checker 'A:fixZ16b (integer-bytes?? -2) vector))
;; 8-bit of precision.
(define A:fixZ8b  (make-prototype-checker 'A:fixZ8b (integer-bytes?? -1) vector))

;; Returns an exact non-negative binary fixnum uniform-array proto with at least:
;; 64-bit of precision.
(define A:fixN64b (make-prototype-checker 'A:fixN64b (integer-bytes?? 8) vector))
;; 32-bit of precision.
(define A:fixN32b (make-prototype-checker 'A:fixN32b (integer-bytes?? 4) vector))
;; 16-bit of precision.
(define A:fixN16b (make-prototype-checker 'A:fixN16b (integer-bytes?? 2) vector))
;; 8-bit of precision.
(define A:fixN8b (make-prototype-checker 'A:fixN8b (integer-bytes?? 1) vector))

#| Returns a boolean uniform-array prototype.
(A:bool #t)		(A:bool *pi*)
 |#;->	#(#t)	Error:  --  A:bool incompatible type 3,141592654.0 
(define A:bool (make-prototype-checker 'A:bool boolean? vector))
;_______________________________	Section 2 begin by AlSchemist 2021
 
; The following is free TinyScheme open source copyrighted AlSchemist
#| disjoint array predicate. In TinyScheme, use strict-array? instead of array?
(define matrix (list->array 2 '#() '((1 2 3) (4 5 6))))
(strict-array? matrix)	(vector? matrix)	(array? matrix)	(record? matrix)
 |#;->	#t				 #t					 #t				 #f
(define (strict-array? array)((record-predicate array:rtd) array))
#| Align previous rows replacing the last space with nbrSpace spaces 
(let* ((vRowStr (make-vector 2 "a b")))(vector-spaced! vRowStr 0 2) vRowStr)
 |#;->	              #("a  b" "a b")
(define (vector-spaced! vRowStr indRow nbrSpace)
	(do ((idxRow indRow (dec idxRow)))((< idxRow 0))
		(let*	((strPrv (vector-ref vRowStr idxRow))
				 (iSpa (string-index-right-okmij strPrv #\space))
				 (sSpa (make-string nbrSpace #\space))
				)
			(vector-set! vRowStr idxRow (string-replace strPrv sSpa iSpa (inc iSpa)))
)	)	)
#| Linear initialization of a square array of dimension two
(define square (vector->array (list->vector (iota 4 1)) #() 2 2))
(array->list square)
(show square)
 |#;->	((1 2) (3 4))
;		0: 1 2
;		1: 3 4
#| Be sure that the first parameter of iota 12 = 2 * 6 the multiplication of dims
(define rectangle (vector->array (list->vector (iota 12 1)) #() 2 6))
(array->list rectangle)
(show rectangle)
(array2-display rectangle 2 2)
 |#;->	((1 2 3 4 5 6) (7 8 9 10 11 12))
;		0: 1 2 3  4  5  6
;		1: 7 8 9 10 11 12
(define (array2-display array nbrRow nbrCol)
	(let*	(	(vRowStr (make-vector nbrRow "")) (vectArr (array:store array))
				(lenCol	 0) (idxArr 0) (rowLast (dec nbrRow))(colLast (dec nbrCol))
			)
		(do ((indCol 0 (inc indCol))) ((= indCol nbrCol))
			(set! lenCol 0)	(set! idxArr indCol)
			(do (	(indRow 0 (inc indRow))) ((= indRow nbrRow))
				(let*	(	(strRow	(if (zero? indCol)
										(string-append (number->string indRow) ":")
										(vector-ref vRowStr indRow)
							)		)
							(strObj	(obj->string (vector-ref vectArr idxArr)))
							(lenObj	(string-length strObj))
						)
					(if (and (> lenObj lenCol)(> lenCol 0))
						(vector-spaced! vRowStr (dec indRow) (inc(- lenObj lenCol)))
						(if (< lenObj lenCol)
							(set! strObj 
								(string-append 
									(make-string (- lenCol lenObj) #\space) strObj
					)	)	)	)
					(vector-set! vRowStr indRow (string-append strRow " " strObj))
					(if (> lenObj lenCol) (set! lenCol lenObj))
					(set! idxArr (+ idxArr nbrCol))
		)	)	)
		(do (	(indRow 0 (inc indRow))) ((= indRow nbrRow))
			(display (vector-ref vRowStr indRow))
			(if (< indRow rowLast)(newline))
)	)	)
#| Linear initialization of an array of dimension three: 3 * 3 * 3 = (expt 3 3) = 27
(define cube (vector->array (list->vector (iota (expt 3 3) 1)) #() 3 3 3))
(array->list cube)
(show cube)
(array3-display cube 3 3 3)
 |#;->	(((1 2 3) (4 5 6) (7 8 9)) ((10 11 12) (13 14 15) (16 17 18)) ((19 20 21) (22 23 24) (25 26 27)))
;		0:  1  2  3   4  5  6   7  8  9
;		1: 10 11 12  13 14 15  16 17 18
;		2: 19 20 21  22 23 24  25 26 27
(define (array3-display array nbrRow nbrCol nbrIdz)
	(let*	(	(vRowStr (make-vector nbrRow "")) (vectArr (array:store array))
				(lenCol	0) (idxArr 0) (nbrRowCol (* nbrRow nbrCol))
				(rowLast (- nbrRow 1))(colLast (- nbrCol 1)) (idzLast (- nbrIdz 1))
			)
		(do ((indCol 0 (+ indCol 1))) ((= indCol nbrCol))
			(do ((indZ 0 (+ indZ 1))) ((= indZ nbrIdz))
				(set! lenCol 0)	(set! idxArr (+	indZ (* indCol nbrCol)))
				(do (	(indRow 0 (+ indRow 1))) ((= indRow nbrRow))
					(let*	(	(strRow	(if (zero? (+ indCol indZ))
											(string-append (number->string indRow) ":")
											(vector-ref vRowStr indRow)
								)	)
								(strObj	(obj->string (vector-ref vectArr idxArr)))
								(lenObj	(string-length strObj))
							)
						(if (and (> lenObj lenCol)(> lenCol 0))
							(vector-spaced! vRowStr (- indRow 1) (+(- lenObj lenCol) 1))
							(if (< lenObj lenCol)
								(set! strObj (string-pad strObj lenCol))
						)	)
						(if (and (= indZ 0)(> indCol 0))
							(vector-set! vRowStr indRow (string-append strRow "  " strObj))
							(vector-set! vRowStr indRow (string-append strRow " "  strObj))
						)
						(if (> lenObj lenCol) (set! lenCol lenObj)) ; max column width
						(set! idxArr (+ idxArr nbrRowCol))
		)	)	)	)
		(do (	(indRow 0 (+ indRow 1))) ((= indRow nbrRow))
			(display (vector-ref vRowStr indRow))
			(if (< indRow rowLast)(newline))
)	)	)
#| Prefer the generic and shorter "show" instead of the dedicated alias
(define cube (vector->array (list->vector (iota 27 27 -1)) #() 3 3 3))
(show cube)
 |#;->	0: 27 26 25  24 23 22  21 20 19
;		1: 18 17 16  15 14 13  12 11 10
;		2:  9  8  7   6  5  4   3  2  1
(define (array-display array)
	(check-arg strict-array? array 'show)
	(let*	(	(dims	(array:dimensions array)))
		(case (length dims)
			((2)	(array2-display array (car dims) (cadr dims)))
			((3)	(array3-display array (car dims) (cadr dims) (caddr dims)))
			(else	(record-instance-display array))
)	)	)
#| Perf:
(let*	(	(cube (vector->array (list->vector (iota (expt 3 3) 1)) #() 3 3 3))
			(nbrRun	10)(timeEnd 0)(timeStart (gettimeofday))
		)
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
		(show cube)	
	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->	    1 s 234 ms 378 µs for 10 runs. 123 ms 437 µs by run
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
(load "C:/Users/YourUserName/AppData/Roaming/GIMP/2.10/scripts/2srfi-047-array.scm")
 |# (closure? make-array)