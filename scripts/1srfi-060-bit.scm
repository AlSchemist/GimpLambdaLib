; https://srfi.schemers.org/srfi-60/
; SRFI: Scheme Requests for Implementation https://srfi.schemers.org/

; Section 1: Aubrey Jaffer		2005	SRFI 60 Bit access and operations for integers
;			 AlSchemist			2021	TinyScheme migration for Gimp 2.10.28 Script-Fu
;										Add multiline comments, examples and pretty print
; 2021/09/27: GimpλLib 1.0 by AlSchemist for Gimp 2.10.28 TinyScheme Script-Fu
; Line: 351 define: 18 comment: 137 = 39.0% blank: 6

;_______________________________	Section 1 begin by Aubrey Jaffer 2005
; https://people.csail.mit.edu/jaffer/SLIB.html
; http://cvs.savannah.gnu.org/viewvc/*checkout*/slib/slib/logical.scm?content-type=text%2Fvnd.viewcvs-markup&revision=HEAD
;;;; "logical.scm", bit access and operations for integers for Scheme
;;; Copyright (C) 1991, 1993, 2001, 2003, 2005 Aubrey Jaffer SLIB 3b6-1
;
;Permission to copy this software, to modify it, to redistribute it,
;to distribute modified versions, and to use it for any purpose is
;granted, subject to the following restrictions and understandings.
;
;1.  Any copy made of this software must include this copyright notice in full.
;
;2.  I have made no warranty or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in each case.

(define logical:boole-xor
 '#(#(0   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15)
    #(1   0  3  2  5  4  7  6  9  8 11 10 13 12 15 14)
    #(2   3  0  1  6  7  4  5 10 11  8  9 14 15 12 13)
    #(3   2  1  0  7  6  5  4 11 10  9  8 15 14 13 12)
    #(4   5  6  7  0  1  2  3 12 13 14 15  8  9 10 11)
    #(5   4  7  6  1  0  3  2 13 12 15 14  9  8 11 10)
    #(6   7  4  5  2  3  0  1 14 15 12 13 10 11  8  9)
    #(7   6  5  4  3  2  1  0 15 14 13 12 11 10  9  8)
    #(8   9 10 11 12 13 14 15  0  1  2  3  4  5  6  7)
    #(9   8 11 10 13 12 15 14  1  0  3  2  5  4  7  6)
    #(10 11  8  9 14 15 12 13  2  3  0  1  6  7  4  5)
    #(11 10  9  8 15 14 13 12  3  2  1  0  7  6  5  4)
    #(12 13 14 15  8  9 10 11  4  5  6  7  0  1  2  3)
    #(13 12 15 14  9  8 11 10  5  4  7  6  1  0  3  2)
    #(14 15 12 13 10 11  8  9  6  7  4  5  2  3  0  1)
    #(15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0)
)  )
(define logical:boole-and
 '#(#(0 0 0 0 0 0 0 0 0 0  0  0  0  0  0  0)
    #(0 1 0 1 0 1 0 1 0 1  0  1  0  1  0  1)
    #(0 0 2 2 0 0 2 2 0 0  2  2  0  0  2  2)
    #(0 1 2 3 0 1 2 3 0 1  2  3  0  1  2  3)
    #(0 0 0 0 4 4 4 4 0 0  0  0  4  4  4  4)
    #(0 1 0 1 4 5 4 5 0 1  0  1  4  5  4  5)
    #(0 0 2 2 4 4 6 6 0 0  2  2  4  4  6  6)
    #(0 1 2 3 4 5 6 7 0 1  2  3  4  5  6  7)
    #(0 0 0 0 0 0 0 0 8 8  8  8  8  8  8  8)
    #(0 1 0 1 0 1 0 1 8 9  8  9  8  9  8  9)
    #(0 0 2 2 0 0 2 2 8 8 10 10  8  8 10 10)
    #(0 1 2 3 0 1 2 3 8 9 10 11  8  9 10 11)
    #(0 0 0 0 4 4 4 4 8 8  8  8 12 12 12 12)
    #(0 1 0 1 4 5 4 5 8 9  8  9 12 13 12 13)
    #(0 0 2 2 4 4 6 6 8 8 10 10 12 12 14 14)
    #(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
)  )

(define (logical:ash-4 x)
	(if (negative? x) (+ -1 (quotient (+ 1 x) 16)) (quotient x 16))
)
(define (logical:reduce op4 ident)
	(lambda args
		(do ((res ident (op4 res (car rgs) 1 0)) (rgs args (cdr rgs)))
			((null? rgs) res)
)	)	)
#| Returns the integer which is the bit-wise AND of the integer arguments
(number->string (logand #b1100 #b1010) 2)
 |#;->	"1000"
(define logand
	(letrec ((lgand (lambda (n2 n1 scl acc)
						(cond
							((= n1 n2) (+ acc (* scl n1)))
							((zero? n2) acc)
							((zero? n1) acc)
							(else
								(lgand (logical:ash-4 n2)
									(logical:ash-4 n1)
									(* 16 scl)
									(+ (* (vector-ref
											(vector-ref
												logical:boole-and
												(modulo n1 16))
											(modulo n2 16))
										scl)
									 acc)
			))		)	)	)	)
		(logical:reduce lgand -1)
)	)
#| Returns the integer which is the bit-wise OR of the integer arguments
(number->string (logior #b1100 #b1010) 2)
 |#;->	"1110"
(define logior
	(letrec 
		(	(lgior 
				(lambda (n2 n1 scl acc)
					(cond
						((= n1  n2) (+ acc (* scl n1)))
						((zero? n2) (+ acc (* scl n1)))
						((zero? n1) (+ acc (* scl n2)))
						(else
							(lgior (logical:ash-4 n2) (logical:ash-4 n1) (* 16 scl)
								(+	(*	(- 15	(vector-ref
													(vector-ref logical:boole-and
														(- 15 (modulo n1 16)))
													(- 15 (modulo n2 16))
										)		)
										scl
									)
								 acc
		)	)	)	)	)	)	)
		(logical:reduce lgior 0)
)	)
#| Returns the integer which is the bit-wise XOR of the integer arguments
(number->string (logxor #b1100 #b1010) 2)
 |#;->	"110"
(define logxor
	(letrec ((lgxor (lambda (n2 n1 scl acc)
						(cond ((= n1 n2) acc)
							((zero? n2) (+ acc (* scl n1)))
							((zero? n1) (+ acc (* scl n2)))
							(else
							 (lgxor (logical:ash-4 n2)
									(logical:ash-4 n1)
									(* 16 scl)
									(+ (* (vector-ref
											  (vector-ref logical:boole-xor (modulo n1 16))
											  (modulo n2 16))
										  scl)
									   acc
			))		)	)	) )		)
		(logical:reduce lgxor 0)
)	)
#| Returns the integer which is the one's-complement of the integer argument
(number->string (lognot #b10000000) 2)	(number->string (lognot #b0) 2)
 |#;->	"-10000001"						"-1"
(define (lognot n) (- -1 n))
#|
(logtest #b0100 #b0111)	(logtest #b0100 #b1011)
 |#;->	 #t						 #f
(define (logtest n1 n2) (not (zero? (logand n1 n2))))
#|
(logbit? 0 #b1101)	(logbit? 1 #b1101)
 |#;->	        #t				   #f
(define (logbit? index n) (logtest (expt 2 index) n))
#| Returns an integer the same as from except in the indexth bit, 
;  which is 1 if bit is #t and 0 if bit is #f
(number->string (copy-bit 0 0 #t) 2)	
 |#;->	"1"
(define (copy-bit index to bool)
  (if bool
      (logior to (arithmetic-shift 1 index))
      (logand to (lognot (arithmetic-shift 1 index)))))
#| Returns an integer composed of some bits from integer n0 and some from integer n1
(bitwise-if 3 #b0101 #b1010)
 |#;->	9 ; A bit of the result is taken from n0 
; if the corresponding bit of integer mask is 1 and from n1 if that bit of mask is 0
(define (bitwise-if mask n0 n1) (logior (logand mask n0) (logand (lognot mask) n1)))
#| Returns the integer composed of the start (inclusive) through end (exclusive) bits of n
(number->string (bit-field #b1101101010 0 4) 2)
 |#;->	"1010"
(define (bit-field n start end)
	(logand (lognot (ash -1 (- end start))) (arithmetic-shift n (- start)))
)
#| Returns an integer the same as to except possibly in the start (inclusive) 
;  through end (exclusive) bits, which are the same as those of from.
;  The 0-th bit of from becomes the startth bit of the result
(number->string (copy-bit-field #b1101101010 0 0 4) 2)
 |#;->	"1101100000"
(define (copy-bit-field to from start end)
	(bitwise-if
		(arithmetic-shift (lognot (ash -1 (- end start))) start)
		(arithmetic-shift from start)
		to
)	)
#| Returns bit-field from start to end cyclically permuted by count bits towards high-order
(number->string (rotate-bit-field #b0100 3 0 4) 2)
 |#;->	"10"
(define (rotate-bit-field n count start end)
	(define width (- end start))
	(set! count (modulo count width))
	(let ((mask (lognot (ash -1 width))))
		(define zn (logand mask (arithmetic-shift n (- start))))
		(logior
			(arithmetic-shift
				(logior
					(logand mask (arithmetic-shift zn count))
					(arithmetic-shift zn (- count width)))
				start)
			(logand (lognot (ash mask start)) n)
)	)	)
#| Shift left
(number->string (ash #b1 3) 2)
 |#;->	"1000"
(define (arithmetic-shift n count)
	(if (negative? count)
		(let ((k (expt 2 (- count))))
			(if (negative? n) (+ -1 (quotient (+ 1 n) k)) (quotient n k)))
		(* (expt 2 count) n)
)	)
#| Returns the number of bits neccessary to represent n
(integer-length #b10101010)	(integer-length #b1111111111111111111111111111111)
 |#;->	8					31 ;            2147483647
(define integer-length
	(letrec	((intlen	(lambda	(n tot)
							(case	n
								((0 -1) (+ 0 tot))
								((1 -2) (+ 1 tot))
								((2 3 -3 -4) (+ 2 tot))
								((4 5 6 7 -5 -6 -7 -8) (+ 3 tot))
								(else (intlen (logical:ash-4 n) (+ 4 tot)))
			))			)	)
		(lambda (n) (intlen n 0))
)	)
#|
(bitwise-bit-count #b10101010)	(bitwise-bit-count 0)	(bitwise-bit-count #b1)
 |#;->				 4							   0						 1
(define bitwise-bit-count
	(letrec	((logcnt
					(lambda	(n tot)
						(if	(zero? n)
							tot
							(logcnt	(quotient n 16)
									(+	(vector-ref '#(0 1 1 2 1 2 2 3 1 2 2 3 2 3 3 4)
											(modulo n 16)
										)
										tot
			))		)	)	)		)
		(lambda	(n)
			(cond	((negative? n) (lognot (logcnt (lognot n) 0)))
				((positive? n) (logcnt n 0))
				(else 0)
)	)	)	)
#| Returns the number of bits in integer n
(logcount #b10101010)	(logcount 0)	(logcount -2)	(logcount #b1)
 |#;->	4				0				1				1
(define (logcount n)
	(cond	((negative? n) (bitwise-bit-count (lognot n)))
			(else (bitwise-bit-count n))
)	)
#| Returns the number of factors of two of integer n
(log2-binary-factors (* 32768 32768))
 |#;->	30 ;		  1073741824
(define (log2-binary-factors n) (+ -1 (integer-length (logand n (- n)))))
#| Returns n with the order of bits start to end reversed
(number->string (bit-reverse 2 #b101))
 |#;->	"2"
(define (bit-reverse k n)
	(do	((m (if (negative? n) (lognot n) n) (arithmetic-shift m -1))
			(k (+ -1 k) (+ -1 k))
			(rvs 0 (logior (arithmetic-shift rvs 1) (logand 1 m))))
		((negative? k) (if (negative? n) (lognot rvs) rvs))
)	)
#|
(number->string (reverse-bit-field #xa7 0 8) 16)
 |#;->	"e5"
(define (reverse-bit-field n start end)
	(define width (- end start))
	(let	((mask (lognot (ash -1 width))))
		(define zn (logand mask (arithmetic-shift n (- start))))
		(logior
			(arithmetic-shift (bit-reverse width zn) start)
			(logand (lognot (ash mask start)) n)
)	)	)
#| list of len booleans corresponding to each bit of the non-negative integer k
(integer->list #b101)
 |#;->	(#t #f #t)
(define (integer->list k . len)
	(if (negative? k) (error 'integer->list 'unexpected 'negative k))
	(if	(null? len)
		(do ((k k (arithmetic-shift k -1)) (lst '() (cons (odd? k) lst))) ((<= k 0) lst))
		(do	(	(idx (+ -1 (car len)) (+ -1 idx))
				(k k (arithmetic-shift k -1))
				(lst '() (cons (odd? k) lst))
			)
			((negative? idx) lst)
)	)	)
#| integer formed from the booleans in the list list
(list->integer '(#t #f #t))	(booleans->integer #t #f #t)
 |#;->	5										5
(define (list->integer bools)
	(do ((bs bools (cdr bs)) (acc 0 (+ acc acc (if (car bs) 1 0)))) ((null? bs) acc))
)
(define (booleans->integer . bools) (list->integer bools))

;;;;@ SRFI-60 aliases
(define ash arithmetic-shift)
(define bitwise-ior logior)
(define bitwise-xor logxor)
(define bitwise-and logand)
(define bitwise-not lognot)
(define bit-count logcount)
(define bit-set?   logbit?)
(define any-bits-set? logtest)
(define first-set-bit log2-binary-factors)
(define bitwise-merge bitwise-if)
; (provide 'srfi-60) ; in comment by AlSchemist 2021

;;; Legacy
;;(define (logical:rotate k count len) (rotate-bit-field k count 0 len))
;;(define (logical:ones deg) (lognot (ash -1 deg)))
;;(define integer-expt expt)		; legacy name
#|
Copyright (C) Aubrey Jaffer (2004, 2005). All Rights Reserved.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 |#
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
(load "C:/Users/YourUserName/AppData/Roaming/GIMP/2.10/scripts/1srfi-060-bit.scm")
 |# (closure? bitwise-bit-count)