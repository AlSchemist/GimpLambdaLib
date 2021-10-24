; https://srfi.schemers.org/srfi-66/srfi-66.html
; SRFI: Scheme Requests for Implementation https://srfi.schemers.org/

; Section 1: Michael Sperber	2005	SRFI 66 Octet Vectors
;			 AlSchemist			2021	TinyScheme migration for Gimp 2.10.28 Script-Fu
;										Add multiline comments, examples and pretty print
; Section 2: AlSchemist       	2021	TinyScheme integration for Gimp 2.10.28 Script-Fu
; 2021/09/27: GimpλLib 1.0 by AlSchemist for Gimp 2.10.28 TinyScheme Script-Fu
; Line: 197 define: 14 comment: 100 = 50.7% blank: 4

;_______________________________	Section 1 begin by Michael Sperber 2006
; 
#| Returns a newly allocated octet vector of k elements initialized to the fill octet
(define u8vect5 (make-u8vector 5 0))	(make-u8vector 0 *pi*)
(show u8vect5)
 |#;->	#(0 0 0 0 0)					Error: 3000. make-u8vector: ... 0 failed. 

(define (make-u8vector nbrElt fill)
	(check-arg 	(lambda(nbrElt) (and(integer? nbrElt)(positive? nbrElt))) nbrElt
		'make-u8vector ; AlSchemist 2021: more precise indication of the caller
	)
	(check-arg octet? fill 'make-u8vector)
	(really-make-u8vector (make-vector nbrElt fill))
)
#|
(define u8vect5 (list->u8vector (iota 5)))
(show u8vect5)			(list->u8vector '(0 -1))
 |#;->	#(0 1 2 3 4)	Error: 3000. list->u8vector: ... -1 failed. 
(define (list->u8vector octets)
	(map (lambda(octet)(check-arg octet? octet 'list->u8vector)) octets) ; AlSchemist
	(really-make-u8vector (apply vector octets))
)
#| returns a newly allocated list of the elements of u8vector in the same order
(u8vector->list u8vect5)
 |#;->	(0 1 2 3 4)
(define (u8vector->list u8vector) (vector->list (u8vector-elements u8vector)))
#| Returns a newly allocated octet vector whose elements contain the given octets
(show (u8vector 5 128 255))		(u8vector 5 128 256)
 |#;->	#(5 128 255				Error: 3000. u8vector: ... 256 failed. 
(define (u8vector . octets)
	(map (lambda(octet)(check-arg octet? octet 'u8vector)) octets) ; AlSchemist 2021
	(list->u8vector octets)
)
#| Returns the number of elements in u8vector as an exact integer
(u8vector-length u8vect5)
 |#;->	5
(define (u8vector-length u8vector)
	(check-arg u8vector? u8vector 'u8vector-length) ; AlSchemist 2021
	(vector-length (u8vector-elements u8vector))
)
#| Returns the contents of element k of u8vector
(u8vector-ref u8vect5 4)
 |#;->	4
(define (u8vector-ref u8vector k)
	(check-arg u8vector? u8vector 'u8vector-ref) ; AlSchemist 2021
	(check-arg 	(lambda(k)	(and(integer? k)(positive? k)
								(< k (u8vector-length u8vector)
				)			)	) k 'u8vector-ref
	)
	(vector-ref (u8vector-elements u8vector) k)
)
#| Stores octet in element k of u8vector
(u8vector-set! u8vect5 4 255)
 |#;->	#(0 1 2 3 255)
(define (u8vector-set! u8vector k octet)
	(check-arg octet? octet 'u8vector-set!)
	(check-arg u8vector? u8vector 'u8vector-set!) ; AlSchemist 2021
	(check-arg 	(lambda(k)	(and(integer? k)(positive? k)
								(< k (u8vector-length u8vector)
				)			)	) k 'u8vector-set!
	)
	(vector-set! (u8vector-elements u8vector) k octet)
)
#| Copies data from octet vector source to octet vector target
(define u8vectTarget (make-u8vector 5 0))
(show u8vectTarget)
(show (u8vector-copy! u8vect5 0 u8vectTarget 0 5))
 |#;->	u8vectTarget#(0 0 0 0 0)
;                   #(0 1 2 3 255)
(define (u8vector-copy! source source-start target target-start count)
	(check-arg u8vector? source 'u8vector-copy!) ; AlSchemist 2021
	(check-arg u8vector? target 'u8vector-copy!) ; AlSchemist 2021
	(if	(>= source-start target-start)
		(do	((i 0 (+ i 1)))
			((= i count))
			(u8vector-set! target (+ target-start i)
				(u8vector-ref source (+ source-start i))))
		(do	((i (- count 1) (- i 1)))
			((= i -1))
			(u8vector-set! target (+ target-start i)
				(u8vector-ref source (+ source-start i))
	)	)	)
	target ; AlSchemist 2021
)
#| Returns a newly allocated copy of octet vector u8vector
(show (u8vector-copy u8vect5))
 |#;->	#(0 1 2 3 255)
(define (u8vector-copy u8vector)
	(check-arg u8vector? u8vector 'u8vector-copy) ; AlSchemist 2021
	(let*	((size (u8vector-length u8vector)) (copy (make-u8vector size 0)))
		(u8vector-copy! u8vector 0 copy 0 size)
)	)
#| Compares u8vector-1 and u8vector-2 returning value consistent with vector ordering
(u8vector=? u8vect5 (u8vector-copy u8vect5))
 |#;->	#t
(define (u8vector=? u8vector1 u8vector2)
	(check-arg u8vector? u8vector1 'u8vector-compare) ; AlSchemist 2021
	(check-arg u8vector? u8vector2 'u8vector-compare)
	(let	((size (u8vector-length u8vector1)))
		(and	(= size (u8vector-length u8vector2))
				(let loop	((idx 0))
					(or	(>= idx size)
						(and	(=	(u8vector-ref u8vector1 idx)
									(u8vector-ref u8vector2 idx)
								)
								(loop (+ 1 idx))
)	)	)		)	)	)
#|
(u8vector-compare (make-u8vector 5 0) u8vect5)
 |#;->	-1
(define (u8vector-compare u8vector1 u8vector2)
	(check-arg u8vector? u8vector1 'u8vector-compare) ; AlSchemist 2021
	(check-arg u8vector? u8vector2 'u8vector-compare)
	(let	(	(length-1 (u8vector-length u8vector1))
				(length-2 (u8vector-length u8vector2)))
		(cond	((< length-1 length-2) -1)
			((> length-1 length-2) 1)
			(else
				(let loop	((i 0))
					(if	(= i length-1)
						0
						(let	((elt-1 (u8vector-ref u8vector1 i))
									(elt-2 (u8vector-ref u8vector2 i)))
							(cond	((< elt-1 elt-2) -1)
								((> elt-1 elt-2) 1)
								(else (loop (+ i 1)))
)	)	)	)	)	)	)	)
#| Copyright (C) Michael Sperber (2005). All Rights Reserved.
Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 |#
;(define-record-type
;	:u8vector
;	(really-make-u8vector elements)
;	u8vector?
;	(elements u8vector-elements)
;)
;_______________________________	Section 2 begin by AlSchemist 2021
 
; The following is free TinyScheme open source copyrighted AlSchemist
#|
(octet? 255)	(octet? 256)
 |#;->	 #t				 #f
(define (octet? thing)
	(and (integer? thing) (exact? thing) (>= thing 0) (<= thing 255))
)
#| Create the type array:rtd as a record
(show :u8vector)
 |#;->	record type :u8vector (elements)
(define :u8vector (make-record-type ':u8vector '(elements)))
(define really-make-u8vector (record-constructor :u8vector '(elements)))
(define (u8vector? u8vect) ((record-predicate :u8vector) u8vect))
(define (u8vector-elements u8vect)((record-accessor :u8vector 'elements) u8vect))
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
(load "C:/Users/YourUserName/AppData/Roaming/GIMP/2.10/scripts/2srfi-066-octet-vector.scm")
 |# (closure? make-u8vector)