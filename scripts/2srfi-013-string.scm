; https://srfi.schemers.org/srfi-13/srfi-13.scm
; SRFI: Scheme Requests for Implementation https://srfi.schemers.org/
; Section 1: Olin Shivers	2000	SRFI 13 string
;			 AlSchemist		2021	TinyScheme integration for Gimp 2.10.28 Script-Fu
;									Add multiline comments, examples and pretty print
; 2021/09/27: 	GimpλLib 1.0 by AlSchemist for Gimp 2.10.28 TinyScheme Script-Fu
;				Perf: overwrite string-downcase string-upcase from script-fu-compat.init
;				Don't integrate SRFI 13 append! too dangerous for TinyScheme
;				Remove append! based concatenate! pair-fold-right pair-fold append-map!
; Line: 2814 define: 110 comment: 1357 = 48.2% blank: 61

;;; SRFI 13 string library reference implementation		-*- Scheme -*-
;;; Olin Shivers 7/2000
;;; Copyright (c) 1988-1994 Massachusetts Institute of Technology.
;;; Copyright (c) 1998, 1999, 2000 Olin Shivers. All rights reserved.
;;;   The details of the copyrights appear at the end of the first section.
;;;   Short summary: BSD-style open source.


; Section 2: a subset of SRFI-13 functions: String utilities by Oleg Kiselyov 2004
; $Id: srfi-13-local.scm,v 1.2 2004/07/08 20:24:53 oleg Exp oleg $
; $Id: util.scm,v 2.6 2004/07/08 19:51:57 oleg Exp oleg $

; Section 3: Daniel M. Sunday's string-contains-maker by Ken Dickey 1991

; Section 4: string utilities by AlSchemist

;_______________________________	Section 1 begin by Olin Shivers 2000

#|
(substring-spec-ok? "abcdef" 0 6)
 |#;->	#t
(define (substring-spec-ok? s start end)
	(and (string? s)
		(integer? start)(exact? start)	(<= 0 start)
		(integer? end)	(exact? end)	(<= start end)
		(<= end (string-length s))
)	)
#|
(check-substring-spec 'testOk "abcdef" 0 6)
 |#;->	#t
#|
(check-substring-spec 'testKo "abcdef" 0 7)
 |#;->	Error: 1300. Illegal substring spec. in proc "abcdef" from 0 to 7 
(define (check-substring-spec proc s start end)
	(if (substring-spec-ok? s start end) #t ; AlSchemist adds #t
		(error "1300. Illegal substring spec. in" proc s 'from start 'to end)
)	)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; substring/shared S START [END]
#|
(substring/shared "begin" 0 3)
 |#;->	"beg"
(define (substring/shared s start . maybe-end)
	(check-arg string? s 'substring/shared)
	(let*	(	(slen	(string-length s))
				(end	(if (pair? maybe-end) (car maybe-end) slen))
			)
		(check-arg (lambda (idx) (and (integer? idx) (exact? idx) (<= 0 idx)))
			start 'substring/shared
		)
		(check-arg (lambda (idx) (and (integer? idx) (exact? idx)
									(<= start idx) (<= idx slen)))
			end 'substring/shared
		)
		(%substring/shared s start end)
)	)
;;; Split out so that other routines in this library can avoid arg-parsing
;;; overhead for END parameter.
#| The difference with the above function is that there is not optional parameter
(%substring/shared "begin" 0 3)
 |#;->	"beg"
(define (%substring/shared s start end)
	(if (and (zero? start) (= end (string-length s))) s (substring s start end))
)
#|
(string-copy "begin end" 0 5)
 |#;->	"begin"
(define (string-copy s . maybe-start+end)
	(let-string-start+end 'string-copy s maybe-start+end
		(lambda (start end) (substring s start end))
)	)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic iterators and other higher-order abstractions
;;; (string-fold kons knil s [start end])
;;; (string-fold-right kons knil s [start end])
;;; (string-unfold       p f g seed [base make-final])
;;; (string-unfold-right p f g seed [base make-final])
;;; You want compiler support for high-level transforms on fold and unfold ops.
;;; You'd at least like a lot of inlining for clients of these procedures.
;;; Don't hold your breath.
#| Map the kons proc across string s from left to right
(string-fold (lambda (char lst) (cons (atom->string char) lst)) nil "Level")
 |#;->	("l" "e" "v" "e" "L")
#| Count the number of lower-case chars in string s
(string-fold (lambda (ch count)(if (char-lower-case? ch)(inc count) count)) 0 "Racecar")
 |#;->	6
#| Double every backslash in string s:
(let*	( 	(strIn		"C:\\tool")
 			(lenStrOut	(string-fold	(lambda(ch count)
											(+ count (if (char=? ch #\\) 2 1))
										)	0 strIn
			)			)
			(strOut		(make-string lenStrOut))
		)
	(string-fold	(lambda(ch idx)
						(let*	(	(idx	(if (not (char=? ch #\\)) idx
												(begin	(string-set! strOut idx ch)
														(inc idx)
								)	)		)	)
							(string-set! strOut idx ch)
							(inc idx)
						)
					)	0 strIn
	)
	(show "lenStrin : " (string-length strIn) " strIn : " (dblQuote strIn))
	(show "lenStrOut: " lenStrOut             " strOut: " (dblQuote strOut))
)
 |#;->	lenStrin : 7 strIn : "C:\tool"
;		lenStrOut: 8 strOut: "C:\\tool"
(define (string-fold kons knil s . maybe-start+end)
	(check-arg procedure? kons 'string-fold)
	(let-string-start+end 'string-fold s maybe-start+end
		(lambda (start end)
			(let lp ((v knil) (i start))
				(if (< i end) (lp (kons (string-ref s i) v) (+ i 1)) v)
)	)	)	)
#| Perf: Count the number of tab in the string
(let*	(	(nbrRun	10000)(timeStart (gettimeofday)))
	(let loop ((idx 0))
		(if (>= idx nbrRun) (time-stat timeStart (gettimeofday) nbrRun)
			(begin	
				(string-fold (lambda (ch cnt)(if (char=? ch #\tab)(inc cnt) cnt))
					0 "Lae\ti\tia"
				)
				(loop (+ idx 1))
)	)	)	)
 |#;->	8 s 796 ms 439 µs for 10K runs. 000 ms 879 µs by run
#| Catamorphism maps proc kons across string s from right to left
(string-fold-right (lambda (ch lst) (cons (atom->string ch) lst)) nil "Level")
 |#;->	("L" "e" "v" "e" "l")
(define (string-fold-right kons knil s . maybe-start+end)
	(check-arg procedure? kons 'string-fold-right)
	(let-string-start+end 'string-fold-right s maybe-start+end
		(lambda (start end)
			(let lp ((objOut knil)(i (- end 1)))
				(if (>= i start)
					(lp (kons (string-ref s i) objOut)(- i 1)) objOut
)	)	)	)	)
;;; (string-unfold p f g seed [base make-final])
;;; This is the fundamental constructor for strings. 
;;; - P aka pNull? tells us when to stop 
;;;   -- when it returns true when applied to one of these seed values.
;;; - F aka first->char maps each seed value to the corresponding character 
;;;   in the result string: (f lst) = ((compose char-function car) lst)
;;;   These chars are assembled into the string in a left-to-right order.
;;; - G aka gNext is used to generate a series of "seed" values from the initial seed:
;;;     SEED, (G SEED), (G^2 SEED), (G^3 SEED), ...: (gNext lst) could be (cdr lst)
;;; - BASE is the optional initial/leftmost portion of the constructed string;
;;;   it defaults to the empty string "".
;;; - MAKE-FINAL is applied to the terminal seed value 
;;;	  (on which P returns true) 
;;;   to produce the final/rightmost portion of the constructed string.
; AlSchemist 2021: Applying make-final to the terminal seed value does not help.
;     Because the terminal seed value could be often nil.
;;;   It defaults to (LAMBDA (X) "").
;     That is to say: (make-final nil) returns ""
;     AlSchemist suggests to keep the initial context of seeds for make-final.
;;; In other words, the following (simple, inefficient) definition holds:
#|
(string-unfold null? car cdr '(#\a #\b #\c) "" (lambda(lst) ""))
 |#;->	"abc"                              ; ^baseLeft       ^make-finalRight
#| Specification of string-unfold before the final implementation:
(define (string-unfold	pNull? first->char gNext seeds
						baseLeft make-finalRight)
	(string-append baseLeft
		(let recur ((lstSeed seeds)) ; AlSchemist 2021: keep initial seeds context
			(if (pNull? lstSeed) (make-finalRight seeds) ; for make-finalRight
				(string-append (string (first->char lstSeed))
					(recur (gNext lstSeed))
)	)	)	)	)
 |#
;;; STRING-UNFOLD is a fairly powerful constructor -- you can use it to
;;; reverse a string, copy a string, convert a list to a string, read
;;; a port into a string, and so forth. Examples:
;(define (string . chars) (string-unfold null? car cdr chars)) ; string builder
#| (list->string             '(#\a #\b #\c))
(string-unfold null? car cdr '(#\a #\b #\c))
 |#;->	"abc"
;;; A problem with the following simple formulation is that it pushes one
;;; stack frame for every char in the result string 
;;; -- an issue if you are using it to read a 100kchar string. 
;;; So we don't use it -- but I include it to give a clear, 
;;; straightforward description of what the function does.
#| Structuration as a loop before the final implementation:
(define (string-unfold p f g seed base make-final)
	(let	(	(ans
					(let recur ((seed seed) (i (string-length base)))
						(if (p seed)
							(let*	(	(final (make-final seed))
										(ans (make-string (+ i (string-length final))))
									)
								(string-copy! ans i final)
								ans
							)

							(let*	(	(c (f seed))
										(s (recur (g seed) (+ i 1)))
									)
								(string-set! s i c)
								s
			)	)	)	)	)
		(string-copy! ans 0 base)
		ans
)	)
 |#
;;; The strategy is to allocate a series of chunks into which we stash the
;;; chars as we generate them. Chunk size goes up in powers of two starting
;;; with 40 and levelling out at 4k, i.e.
;;;     40 40 80 160 320 640 1280 2560 4096 4096 4096 4096 4096...
;;; This should work pretty well for short strings, 1-line (80 char) strings,
;;; and longer ones. When done, we allocate an answer string and copy the
;;; chars over from the chunk buffers.
#| Map (symb->char (car lstOfSymb)) over the list of symbols, producing a string
(string-unfold null? (compose car string->list symbol->string car) cdr '(a b c))
(string-unfold null? (compose3 (lambda(str)(car (string->list str))) symbol->string car) cdr '(a b c))
 |#;->	"abc" ; that is to say: (dblQuote '(a b c))

#| string-tabulate applies first->char to the size = 26 indexes from seed 0
(string-unfold 	(lambda (idx) (>= idx 26)) 
				(lambda (idx)(integer->char (+ idx 97))) inc 0 "Alphabet: "
)
 |#;->	"Alphabet: abcdefghijklmnopqrstuvwxyz"

#| (string-map char-upcase "abc")
(string-unfold null? (compose char-upcase car) cdr (string->list "abc")
 	"Uppercase: " ; left base
	(lambda (lst)(string-append            " was: " (list->string lst)))
)
 |#;->	"Uppercase: ABC was: abc"
(define (string-unfold pNull? first->char gNext seeds . base+make-final)
;    				   ^	  ^			  ^	        ^was seed: AlSchemist 2021
	(check-arg procedure? pNull?		'string-unfold) ; pNull?      was p
	(check-arg procedure? first->char	'string-unfold) ; first->char was f
	(check-arg procedure? gNext			'string-unfold) ; gNext       was g
	(let-maybe-base+make-final 'string-unfold base+make-final
		(lambda (base make-final)
			(let lp	(	(chunks '())		; Previously filled chunks
						(nchars 0)			; Number of chars in CHUNKS
						(chunk (make-string 40)) ; Current chunk into which we write
						(chunk-len 40)
						(i 0)				; Number of chars written into CHUNK
						(seed seeds)
					) ;           ^AlSchemist 2021: keep initial seeds context
				(let lp2 ((i i) (seed seed))
					(if (not (pNull? seed))
						(let ((c (first->char seed))
							(seed (gNext seed)))
							(if (< i chunk-len)
								(begin	(string-set! chunk i c)
										(lp2 (+ i 1) seed)
								)

								(let*	(	(nchars2 (+ chunk-len nchars))
											(chunk-len2 (min 4096 nchars2))
											(new-chunk (make-string chunk-len2))
										)
									(string-set! new-chunk 0 c)
									(lp (cons chunk chunks) (+ nchars chunk-len)
										new-chunk chunk-len2 1 seed
						)	)	)	)

						;; We're done. Make the answer string & install the bits.
						(let*	(	(final (make-final seeds)) 
									; AlSchemist 2021       ^add "s"
									(flen (string-length final))
									(base-len (string-length base))
									(j (+ base-len nchars i))
									(ans (make-string (+ j flen)))
								)
							(%string-copy! ans j final 0 flen)	; Install FINAL.
							(let ((j (- j i)))
								(%string-copy! ans j chunk 0 i)		; Install CHUNK[0,I).
								(let lp ((j j) (chunks chunks))		; Install CHUNKS.
									(if (pair? chunks)
										(let*	(	(chunk  (car chunks))
													(chunks (cdr chunks))
													(chunk-len (string-length chunk))
													(j (- j chunk-len))
												)
											(%string-copy! ans j chunk 0 chunk-len)
											(lp j chunks)
							)	)	)	)
							(%string-copy! ans 0 base 0 base-len)	; Install BASE.
							ans
)	)	)	)	)	)	)
#| Perf
(let*	(	(nbrRun	100)(timeEnd 0)(timeStart (gettimeofday)))
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
		(string-unfold null? (compose car string->list symbol->string car) cdr '(a b c))
	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->	1 s 061 ms 983 µs for 100 runs. 010 ms 619 µs by run define
#| From right to left:
(string-unfold-right null? car cdr (string->list "RaceCar"))
 |#;->	"raCecaR"
(define (string-unfold-right pNull? first->char gNext seeds . base+make-final)
;   AlSchemist 2021 			 ^		^			^		  ^was seed
	(check-arg procedure? pNull?		'string-unfold) ; pNull?      was p
	(check-arg procedure? first->char	'string-unfold) ; first->char was f
	(check-arg procedure? gNext			'string-unfold) ; gNext       was g
	(let-maybe-base+make-final 'string-unfold base+make-final
		(lambda (base make-final)
			(let lp	(	(chunks '())		; Previously filled chunks
						(nchars 0)			; Number of chars in CHUNKS
						(chunk (make-string 40))	; Current chunk into which we write
						(chunk-len 40)
						(i 40)				; Number of chars available in CHUNK
						(seed seeds)
					) ;           ^AlSchemist 2021: keep initial seeds context
				(let lp2 ((i i) (seed seed))	; Fill up CHUNK from right
					(if (not (pNull? seed))		; to left.
						(let ((c (first->char seed))
							(seed (gNext seed)))
							(if (> i 0)
								(let ((i (- i 1)))
									(string-set! chunk i c)
									(lp2 i seed)
								)
								(let* ((nchars2 (+ chunk-len nchars))
									(chunk-len2 (min 4096 nchars2))
									(new-chunk (make-string chunk-len2))
									(i (- chunk-len2 1)))
									(string-set! new-chunk i c)
									(lp (cons chunk chunks) (+ nchars chunk-len)
										new-chunk chunk-len2 i seed
						)	)	)	)

						;; We're done. Make the answer string & install the bits.
						(let*	(	(final (make-final seeds))
									; AlSchemist 2021       ^add "s"
									(flen (string-length final))
									(base-len (string-length base))
									(chunk-used (- chunk-len i))
									(j (+ base-len nchars chunk-used))
									(ans (make-string (+ j flen)))
								)
							(%string-copy! ans 0 final 0 flen)	; Install FINAL.
							(%string-copy! ans flen chunk i chunk-len); Install CHUNK[I,).
							(let lp	(	(j (+ flen chunk-used))		; Install CHUNKS.
										(chunks chunks)
									)
								(if (pair? chunks)
									(let*	(	(chunk  (car chunks))
												(chunks (cdr chunks))
												(chunk-len (string-length chunk))
											)
										(%string-copy! ans j chunk 0 chunk-len)
										(lp (+ j chunk-len) chunks)
									)
									(%string-copy! ans j base 0 base-len) ; Install BASE.
							)	)
							ans
)	)	)	)	)	)	)
#| Count the number of digits in the string
(let* ((count 0))
	(string-for-each	(lambda(ch)	(if (char-set-contains? char-set:digit ch)
										(set! count (inc count))
						)			) "1. e4 e5 2. Nf3"
	) count
)
 |#;->	5
(define (string-for-each proc s . maybe-start+end)
	(check-arg procedure? proc 'string-for-each)
	(let-string-start+end 'string-for-each s maybe-start+end
		(lambda (start end)
			(let lp ((i start))
				(if (< i end)
					(begin (proc (string-ref s i)) (lp (+ i 1))
)	)	)	)	)	)
#|
(string-every char-numeric? "0123")
 |#;->	#t
(define (string-every criterion s . maybe-start+end)
	(let-string-start+end 'string-every s maybe-start+end
		(lambda (start end)
			(cond
				((char? criterion)
					(let lp ((i start))
						(or (>= i end)
							(and (char=? criterion (string-ref s i)) (lp (+ i 1)))
				)	)	)
				((char-set? criterion)
					(let lp ((i start))
						(or (>= i end)
							(and (char-set-contains? criterion (string-ref s i))
								(lp (+ i 1))
				)	)	)	)
				((procedure? criterion)
					(or (= start end)
						(let lp ((i start))
							(let ((c (string-ref s i)) (i1 (+ i 1)))
						   		(if (= i1 end) (criterion c)
									(and (criterion c) (lp i1))
				)	)	)	)	)
				(else
					(error "1310. string-every: 2nd param is neither char-set, char, or predicate." criterion)
)	)	)	)	)
#|
(string-any char-numeric? "1.b2-b4")
 |#;->	#t
(define (string-any criterion s . maybe-start+end)
	(let-string-start+end 'string-any s maybe-start+end
		(lambda (start end)
			(cond
				((char? criterion)
					(let lp ((i start))
						(and (< i end)
							(or (char=? criterion (string-ref s i)) (lp (+ i 1))))))
				((char-set? criterion)
					(let lp ((i start))
						(and (< i end)
							(or (char-set-contains? criterion (string-ref s i))
								(lp (+ i 1))))))
				((procedure? criterion)
					(and (< start end)
						(let lp ((i start))
							(let ((c (string-ref s i)) (i1 (+ i 1)))
								(if (= i1 end)
									(criterion c)
									(or (criterion c) (lp i1)))))))
				(else
					(error "1311. string-any: 2nd param is neither char-set, char, or predicate." criterion)
)	)	)	)	)
#| Build a new string of length len applying (proc i) to generate the char at index idx
(string-tabulate (lambda (idx)(integer->char (+ idx 97))) 26)
(string-tabulate (lambda (idx)(integer->char (+ idx 48))) 10)
 |#;->	"abcdefghijklmnopqrstuvwxyz"			 "0123456789"
(define (string-tabulate proc len)
	(check-arg procedure? proc 'string-tabulate)
	(check-arg (lambda (val) (and (integer? val) (exact? val) (<= 0 val)))
				len 'string-tabulate
	)
	(let ((s (make-string len)))
		(do ((i (- len 1) (- i 1))) ((< i 0)) (string-set! s i (proc i)))
		s
)	)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; string-prefix-length[-ci] s1 s2 [start1 end1 start2 end2]
;;; Find the length of the common prefix/suffix.
;;; It is not required that the two substrings passed be of equal length.
;;; This was microcode in MIT Scheme -- a very tightly bummed primitive.
;;; %STRING-PREFIX-LENGTH is the core routine of all string-comparisons,
;;; so should be as tense as possible.
(define (%string-prefix-length s1 start1 end1 s2 start2 end2)
	(let* ((delta (min (- end1 start1) (- end2 start2))) (end1 (+ start1 delta)))
		(if (and (eq? s1 s2) (= start1 start2))
			delta
			(let lp ((i start1) (j start2))
				(if (or (>= i end1) (not (char=? (string-ref s1 i) (string-ref s2 j))))
					(- i start1)
					(lp (+ i 1) (+ j 1))
)	)	)	)	)
;;; string-suffix-length[-ci] s1 s2 [start1 end1 start2 end2]
(define (%string-suffix-length s1 start1 end1 s2 start2 end2)
	(let* ((delta (min (- end1 start1) (- end2 start2))) (start1 (- end1 delta)))
		(if (and (eq? s1 s2) (= end1 end2))
			delta
			(let lp ((i (- end1 1)) (j (- end2 1)))
				(if (or (< i start1) (not (char=? (string-ref s1 i) (string-ref s2 j))))
					(- (- end1 i) 1)
					(lp (- i 1) (- j 1))
)	)	)	)	)
(define (%string-prefix-length-ci s1 start1 end1 s2 start2 end2)
	(let* ((delta (min (- end1 start1) (- end2 start2))) (end1 (+ start1 delta)))
		(if (and (eq? s1 s2) (= start1 start2))
			delta
			(let lp ((i start1) (j start2))
				(if (or (>= i end1) (not (char-ci=? (string-ref s1 i) (string-ref s2 j))))
					(- i start1)
					(lp (+ i 1) (+ j 1))
)	)	)	)	)
(define (%string-suffix-length-ci s1 start1 end1 s2 start2 end2)
	(let* ((delta (min (- end1 start1) (- end2 start2))) (start1 (- end1 delta)))
		(if (and (eq? s1 s2) (= end1 end2))
			delta
			(let lp ((i (- end1 1)) (j (- end2 1)))
				(if (or (< i start1)
						(not (char-ci=? (string-ref s1 i) (string-ref s2 j))))
					(- (- end1 i) 1)
					(lp (- i 1) (- j 1))
)	)	)	)	)
#|
(string-prefix-length "prefix common" "prefix suffix")
 |#;->	7           ;  1234567         1234567
(define (string-prefix-length s1 s2 . maybe-starts+ends)
	(let-string-start+end2 'string-prefix-length s1 s2 maybe-starts+ends
		(lambda (start1 end1 start2 end2)
			(%string-prefix-length s1 start1 end1 s2 start2 end2)
)	)	)
#|
(string-suffix-length "common: suffix" "prefix: suffix")
 |#;->	8            ;  	 12345678         12345678
(define (string-suffix-length s1 s2 . maybe-starts+ends)
	(let-string-start+end2 'string-suffix-length s1 s2 maybe-starts+ends
		(lambda (start1 end1 start2 end2)
			(%string-suffix-length s1 start1 end1 s2 start2 end2)
)	)	)
#|
(string-prefix-length-ci "prefix common" "preFIX suffix")
 |#;->	7              ;  1234567         1234567
(define (string-prefix-length-ci s1 s2 . maybe-starts+ends)
	(let-string-start+end2 'string-prefix-length-ci s1 s2 maybe-starts+ends
		(lambda (start1 end1 start2 end2)
			(%string-prefix-length-ci s1 start1 end1 s2 start2 end2)
)	)	)
#|
(string-suffix-length-ci "common: suffix" "prefix: suffix")
 |#;->	8            ;		    12345678         12345678
(define (string-suffix-length-ci s1 s2 . maybe-starts+ends)
	(let-string-start+end2 'string-suffix-length-ci s1 s2 maybe-starts+ends
		(lambda (start1 end1 start2 end2)
			(%string-suffix-length-ci s1 start1 end1 s2 start2 end2)
)	)	)
;;; These are all simple derivatives of the previous counting funs.
#| Does the second string begin by the first string?
(string-prefix? "prefix" "prefix common")
 |#;->	#t
(define (string-prefix? s1 s2 . maybe-starts+ends)
	(let-string-start+end2 'string-prefix? s1 s2 maybe-starts+ends
		(lambda (start1 end1 start2 end2)
			(%string-prefix? s1 start1 end1 s2 start2 end2)
)	)	)
#| Does the second string end with the first string?
(string-suffix? "suffix" "common suffix")
 |#;->	#t
(define (string-suffix? s1 s2 . maybe-starts+ends)
	(let-string-start+end2 'string-suffix? s1 s2 maybe-starts+ends
		(lambda (start1 end1 start2 end2)
			(%string-suffix? s1 start1 end1 s2 start2 end2)
)	)	)
#| Does the second string begin by the first string case insensitive?
(string-prefix-ci? "prefix" "preFIX common")
 |#;->	#t	
(define (string-prefix-ci? s1 s2 . maybe-starts+ends)
	(let-string-start+end2 'string-prefix-ci? s1 s2 maybe-starts+ends
		(lambda (start1 end1 start2 end2)
			(%string-prefix-ci? s1 start1 end1 s2 start2 end2)
)	)	)
#| Does the second string end with the first string case insensitive?
(string-suffix-ci? "suffix" "common sufFIX")
 |#;->	#t	
(define (string-suffix-ci? s1 s2 . maybe-starts+ends)
	(let-string-start+end2 'string-suffix-ci? s1 s2 maybe-starts+ends
		(lambda (start1 end1 start2 end2)
			(%string-suffix-ci? s1 start1 end1 s2 start2 end2)
)	)	)
;;; Here are the internal routines that do the real work.

(define (%string-prefix? s1 start1 end1 s2 start2 end2)
	(let ((len1 (- end1 start1)))
		(and (<= len1 (- end2 start2))
			 (= (%string-prefix-length s1 start1 end1 s2 start2 end2) len1)
)	)	)
(define (%string-suffix? s1 start1 end1 s2 start2 end2)
	(let ((len1 (- end1 start1)))
		(and (<= len1 (- end2 start2))
			 (= len1 (%string-suffix-length s1 start1 end1 s2 start2 end2))
)	)	)
(define (%string-prefix-ci? s1 start1 end1 s2 start2 end2)
	(let ((len1 (- end1 start1)))
		(and (<= len1 (- end2 start2))
			 (= len1 (%string-prefix-length-ci s1 start1 end1 s2 start2 end2))
)	)	)
(define (%string-suffix-ci? s1 start1 end1 s2 start2 end2)
	(let ((len1 (- end1 start1)))
		(and (<= len1 (- end2 start2))
			 (= len1 (%string-suffix-length-ci s1 start1 end1 s2 start2 end2))
)	)	)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hash
;;; Compute (c + 37 c + 37^2 c + ...) modulo BOUND, with sleaze thrown in
;;; to keep the intermediate values small. (We do the calculation with just
;;; enough bits to represent BOUND, masking off high bits at each step in
;;; calculation. If this screws up any important properties of the hash
;;; function I'd like to hear about it. -Olin)
;;;
;;; If you keep BOUND small enough, the intermediate calculations will 
;;; always be fixnums. How small is dependent on the underlying Scheme system; 
;;; we use a default BOUND of 2^22 = 4194304, which should hack it in
;;; Schemes that give you at least 29 signed bits for fixnums. The core 
;;; calculation that you don't want to overflow is, worst case,
;;;     (+ 65535 (* 37 (- bound 1)))
;;; where 65535 is the max character code. Choose the default BOUND to be the
;;; biggest power of two that won't cause this expression to fixnum overflow, 
;;; and everything will be copacetic.
; AlSchemist: bitwise-and is defined in 1srfi-060-bit.scm
(define (%string-hash s char->int bound start end)
	(let ((iref (lambda (s i) (char->int (string-ref s i))))
		  (mask (let lp ((i 65536)) (if (>= i bound) (- i 1) (lp (+ i i))))))
		(let lp ((i start) (ans 0))
			(if (>= i end)
				(modulo ans bound)
				(lp (+ i 1) (bitwise-and mask (+ (* 37 ans) (iref s i))))
)	)	)	)
#|
(string-hash "hash")
 |#;->	1210760
(define (string-hash s . maybe-bound+start+end)
	(let*	(	(bound	(if (pair? maybe-bound+start+end)
							(car maybe-bound+start+end)
							4194304
			)	)		)
		(check-arg (lambda (n) (and (integer? bound) (exact? bound) (<= 0 bound)))
			bound 'string-hash
		)
		(let-string-start+end 'string-hash s 
			(if (pair? maybe-bound+start+end) (cdr maybe-bound+start+end) nil)
			(lambda (start end)
				(%string-hash s char->integer bound start end)
)	)	)	)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Cutting & pasting strings
;;; string-take string nchars
#|
(string-take "First Last" 5)
 |#;->	"First"
(define (string-take s n)
	(check-arg string? s 'string-take)
	(check-arg (lambda (val) (and (integer? n) (exact? n) (<= 0 n (string-length s))))
		n 'string-take
	)
	(%substring/shared s 0 n)
)
#|
(string-take-right "First Last" 4)
 |#;->	"Last"
(define (string-take-right s n)
	(check-arg string? s 'string-take-right)
	(let ((len (string-length s)))
		(check-arg (lambda (val) (and (integer? n) (exact? n) (<= 0 n len)))
			n 'string-take-right
		)
		(%substring/shared s (- len n) len)
)	)
#|
(string-drop "First Last" 5)
 |#;->	" Last"
(define (string-drop s n)
	(check-arg string? s 'string-drop)
	(let ((len (string-length s)))
		(check-arg (lambda (val) (and (integer? n) (exact? n) (<= 0 n len)))
			n 'string-drop
		)
		(%substring/shared s n len)
)	)
#|
(string-drop-right "First Last" 4)
 |#;->	"First "
(define (string-drop-right s n)
	(check-arg string? s 'string-drop-right)
	(let ((len (string-length s)))
		(check-arg (lambda (val) (and (integer? n) (exact? n) (<= 0 n len)))
			n 'string-drop-right
		)
		(%substring/shared s 0 (- len n))
)	)
#| Trim left spaces              Trim left leading zeros from strNbr
(string-trim "  Left Right   ")	(string-trim "000123" #\0)
 |#;->	"Left Right   "			"123"
(define (string-trim s . criterion+start+end)
	(if (pair? criterion+start+end)
		(let-string-start+end 'string-trim s (cdr criterion+start+end)
			(lambda (start end)
				(cond	((string-skip s (car criterion+start+end) start end) =>
						 (lambda (idx) (%substring/shared s idx end))
						)
						(else "")
		)	)	)
		(let*	((end (string-length s)))
			(cond	((string-skip s char-set:whitespace 0 end) =>
					 (lambda (idx) (%substring/shared s idx end))
					)
					(else "")
)	)	)	)
#|
(string-trim-right "  Left Right   ")
 |#;->	"  Left Right"
(define (string-trim-right s . criterion+start+end)
	(let*	((criterion (if (pair? criterion+start+end) (car criterion+start+end)
							char-set:whitespace
			))			)
		(let-string-start+end 'string-trim-right s
			(if (pair? criterion+start+end) (cdr criterion+start+end) nil)
			(lambda (start end)
				(cond	((string-skip-right s criterion start end) =>
							(lambda (idx) (%substring/shared s start (+ 1 idx)))
						)
					(else "")
)	)	)	)	)
#|
(string-trim-both "  Left Right   ")
 |#;->	"Left Right"
(define (string-trim-both s . criterion+start+end)
	(if (pair? criterion+start+end)
		(let-string-start+end 'string-trim-both s (cdr criterion+start+end)
			(lambda (start end)
				(cond	((string-skip s (car criterion+start+end) start end) =>
							(lambda	(idx)
								(%substring/shared s idx
									(+ 1 (string-skip-right s criterion idx end))
						)	)	)
					(else "")
		)	)	)
		(let*	((end (string-length s)))
			(cond	((string-skip s char-set:whitespace 0 end) =>
						(lambda	(idx)
							(%substring/shared s idx
								(+ 1 (string-skip-right s char-set:whitespace idx end))
					)	)	)
				(else "")
)	)	)	)
#| Perf:
(let*	(	(nbrRun	10000)(timeEnd 0)(timeStart (gettimeofday)))
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
;		(string-trim "000123" #\0) ; Trim left leading zeros from strNbr
;		(string-trim		"  Left Right   ")
;		(string-trim-right	"  Left Right   ")
		(string-trim-both	"  Left Right   ")
	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->	 6 s 468 ms 209 µs for 10K runs. 000 ms 646 µs by run string-trim #\0
;		 8 s 890 ms 693 µs for 10K runs. 000 ms 889 µs by run string-trim Left
;		11 s 546 ms 419 µs for 10K runs. 001 ms 154 µs by run string-trim-right
;		18 s 546 ms 338 µs for 10K runs. 001 ms 854 µs by run string-trim-both		
#|
(string-pad "123" 5)
 |#;->	"  123"
(define (string-pad s n . char+start+end)
	(let*	((char (if (pair? char+start+end) (car char+start+end) #\space)))
		(let-string-start+end 'string-pad s 
			(if (pair? char+start+end) (cdr char+start+end) nil)
			(lambda (start end)
				(check-arg (lambda (n) (and (integer? n) (exact? n) (<= 0 n)))
					n 'string-pad
				)
				(let ((len (- end start)))
					(if (<= n len)
						(%substring/shared s (- end n) end)
						(let ((ans (make-string n char)))
							(%string-copy! ans (- n len) s start end)
							ans)
)	)	)	)	)	)
#|
(string-pad-right "123" 5)
 |#;->	"123  "
(define (string-pad-right s n . char+start+end)
	(let*	((char (if (pair? char+start+end) (car char+start+end) #\space)))
		(let-string-start+end 'string-pad-right s 
			(if (pair? char+start+end) (cdr char+start+end) nil)
			(lambda (start end)
				(check-arg (lambda (n) (and (integer? n) (exact? n) (<= 0 n)))
					n 'string-pad-right
				)
				(let ((len (- end start)))
					(if (<= n len)
						(%substring/shared s start (+ start n))
						(let ((ans (make-string n char)))
							(%string-copy! ans 0 s start end)
							ans
)	)	)	)	)	)	)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filtering strings
;;; string-delete char/char-set/pred string [start end]
;;; If the criterion is a char or char-set, we scan the string twice with
;;;   string-fold -- once to determine the length of the result string, 
;;;   and once to do the filtered copy.
;;; If the criterion is a predicate, we don't do this double-scan strategy, 
;;;   because the predicate might have side-effects or be very expensive to
;;;   compute. So we preallocate a temp buffer pessimistically, and only do
;;;   one scan over S. This is likely to be faster and more space-efficient
;;;   than consing a list.
#|
(string-delete char-set:whitespace "Delete Spaces Between Words")
 |#;->	"DeleteSpacesBetweenWords" ; char-set:whitespace needs 1srfi-014-char-set.scm
(define (string-delete criterion s . maybe-start+end)
	(let-string-start+end 'string-delete s maybe-start+end
		(lambda (start end)
			(if (procedure? criterion)
				(let*	(	(slen (- end start))
							(temp (make-string slen))
							(ans-len(string-fold
									   (lambda (c i)
										   (if (criterion c) i
											   (begin (string-set! temp i c) (+ i 1))
										)	) 0 s start end
						)	)		)
					(if (= ans-len slen) temp (substring temp 0 ans-len))
				)
				(let*	(	(cset	(cond
										((char-set? criterion) criterion)
										((char? criterion) (char-set criterion))
										(else
											(error "1315. string-delete criterion not predicate, char or char-set" criterion)
							)	)		)
							(len	(string-fold
										(lambda (c i)
											(if (char-set-contains? cset c) i (+ i 1))
										) 0 s start end
							)		)
							(ans (make-string len))
						)					   
					(string-fold
						(lambda (c i)
							(if (char-set-contains? cset c) i
								(begin (string-set! ans i c) (+ i 1))
						)	) 0 s start end
					)
					ans
)	)	)	)	)
#| Filter the string s, retaining only those characters 
; that satisfy the char/char-set/pred argument.
(string-filter char-upper-case? "KEEP UPPERCASE letters")
 |#;->	"KEEPUPPERCASE"
(define (string-filter criterion s . maybe-start+end)
	(let-string-start+end 'string-filter s maybe-start+end
		(lambda (start end)
			(if	(procedure? criterion)
				(let*	((slen (- end start))
							(temp (make-string slen))
							(ans-len(string-fold
										(lambda	(c i)
											(if	(criterion c)
												(begin (string-set! temp i c) (+ i 1))
												i
										)	) 0 s start end
						)	)		)
					(if (= ans-len slen) temp (substring temp 0 ans-len))
				)
				(let*	(	(cset
								(cond	((char-set? criterion) criterion)
										((char? criterion) (char-set criterion))
										(else
											(error "1316. string-delete criterion not predicate, char or char-set" criterion)
							)	)		)
							(len	(string-fold
										(lambda	(c i)
											(if (char-set-contains? cset c) (+ i 1) i)
										) 0 s start end
							)		)
							(ans (make-string len))
						)
					(string-fold	(lambda	(c i)
										(if	(char-set-contains? cset c)
											(begin (string-set! ans i c) (+ i 1))
											i
									)	) 0 s start end
					)
					ans
)	)	)	)	)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; String search
;;; string-index       string char/char-set/pred [start end]
#|
(string-index "zero-based index" #\x)(string-index "zero-based index" #\e 11 16)
 |#;->	15    ;0123456789012345								     14   ; ^third "e"
#|
(string-index "zero-based index" char-set:punctuation)
 |#;->	4     ;01234
(define (string-index str criterion . maybe-start+end)
	(let-string-start+end 'string-index str maybe-start+end
		(lambda (start end)
			(cond 
				(	(char? criterion)
						(let lp ((i start))
							(and (< i end)
								(if (char=? criterion (string-ref str i)) i
									(lp (+ i 1)
				)	)	)	)	)
				(	(char-set? criterion) ; require 1srfi-014.scm
						(let lp ((i start))
							(and (< i end)
								(if (char-set-contains? criterion (string-ref str i)) i
									(lp (+ i 1)
				)	)	)	)	)
				(	(procedure? criterion)
						(let lp ((i start))
							(and (< i end)
								(if (criterion (string-ref str i)) i
									(lp (+ i 1)
				)	)	)	)	)
				(else (error "1317. string-index: second param is neither char-set, char, or predicate." criterion)
)	)	)	)	)
#| Perf:
(duration '(string-index-okmij "zero-based index" #\x)		2000)
(duration '(string-index       "zero-based index" #\x)		2000)
(duration '(string-index       "zero-based index" #\x 0 16) 2000)
 |#;->	2 s 625 ms 044 µs for 2K runs. 001 ms 312 µs by run of string-index-okmij
;		2 s 765 ms 135 µs for 2K runs. 001 ms 382 µs by run of string-index
;		2 s 983 ms 845 µs for 2K runs. 001 ms 491 µs by run of string-index 0 16
#|
(string-index-right "zero-based index" #\e)
 |#;->	14          ;012345678901234
(define (string-index-right str criterion . maybe-start+end)
	(let-string-start+end 'string-index-right str maybe-start+end
		(lambda (start end)
			(cond
				((char? criterion)
					(let lp ((i (- end 1)))
						(and (>= i start)
							(if (char=? criterion (string-ref str i)) i (lp (- i 1)))
				)	)	)
				((char-set? criterion)
					(let lp ((i (- end 1)))
						(and (>= i start)
							(if (char-set-contains? criterion (string-ref str i)) i
								(lp (- i 1))
				)	)	)	)
				((procedure? criterion)
					(let lp ((i (- end 1)))
						(and (>= i start)	(if (criterion (string-ref str i)) i 
												(lp (- i 1))
				)	)	)					)
				(else
				   (error "1318. string-index-right: 2nd param is neither char-set, char, or predicate." criterion)
)	)	)	)	)
#| search for the first char that doesn't satisfy the criterion
(string-skip "a 1 b 2 c 3" (char-set-union char-set:lower-case char-set:whitespace))
 |#;->	2    ;012
(define (string-skip str criterion . maybe-start+end)
	(let-string-start+end 'string-skip str maybe-start+end
		(lambda (start end)
			(cond 
				((char? criterion)
					(let lp ((i start))
					(and (< i end)
						(if (char=? criterion (string-ref str i)) (lp (+ i 1)) i))))
				((char-set? criterion)
					(let lp ((i start))
					(and (< i end)
						(if (char-set-contains? criterion (string-ref str i))
							(lp (+ i 1))
							i))))
				((procedure? criterion)
					(let lp ((i start))
					(and (< i end) (if (criterion (string-ref str i)) (lp (+ i 1)) i))))
					(else
						(error "1320. string-skip: 2nd param is neither char-set, char, or predicate." criterion)
)	)	)	)   )
#|
(string-skip-right "a 1 b 2 c 3" char-set:digit)
 |#;->	9		   ;0123456789
(define (string-skip-right str criterion . maybe-start+end)
	(let-string-start+end 'string-skip-right str maybe-start+end
		(lambda (start end)
			(cond
				((char? criterion)
					(let lp ((i (- end 1)))
						(and (>= i start)
							(if (char=? criterion (string-ref str i)) (lp (- i 1)) i)
				)	)	)
				((char-set? criterion)
					(let lp ((i (- end 1)))
						(and (>= i start)
							(if (char-set-contains? criterion (string-ref str i))
								(lp (- i 1))
								i
				)	)	)	)
				((procedure? criterion)
					(let lp ((i (- end 1)))
						(and (>= i start)
							(if (criterion (string-ref str i)) (lp (- i 1)) i)
				)	)	)
				(else
				   (error "1321. string-skip-right: CRITERION param is neither char-set or char." criterion)
)	)	)	)	)
#|
(string-count "a 1 b 2 c 3" char-set:digit)
 |#;->	3
(define (string-count s criterion . maybe-start+end)
	(let-string-start+end 'string-count s maybe-start+end
		(lambda (start end)
			(cond
				((char? criterion)
					(do ((i start (+ i 1))
						(count 0 (if (char=? criterion (string-ref s i)) (+ count 1) count)))
						((>= i end) count)
				)	)
				((char-set? criterion)
					(do (	(i start (+ i 1))
							(count 0	(if (char-set-contains? criterion (string-ref s i))
										(+ count 1) count
							)	)		)
						(	(>= i end) count)
				)	)
				((procedure? criterion)
					(do (	(i start (+ i 1))
							(count 0 (if (criterion (string-ref s i)) (+ count 1) count)))
						(	(>= i end) count)
				)	)
				(else
					(error "1322. string-count: CRITERION param is neither char-set or char."
						criterion)
)	)	)	)	)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; string-fill! string char [start end]
#|
(let* ((strTo (string-build "12345678"))) (string-fill! strTo #\space 2 3))
 |#;->						"12 45678"
(define (string-fill! s char . maybe-start+end)
	(check-arg char? char 'string-fill!)
	(let-string-start+end 'string-fill! s maybe-start+end
		(lambda (start end)
			(do (	(i (- end 1) (- i 1)))
				(	(< i start))
				(string-set! s i char)
	)	)	)	s ; AlSchemist 2021
)
;;; string-copy! to tstart from [fstart fend]
;;; 	Guaranteed to work, even if s1 eq s2.
#|
(let* ((strTo (string-build "12345678"))) (string-copy! strTo 2 "abcd"))
 |#;->	"12abcd78"
#|
(let* ((strTo (string-build "12345678"))) (string-copy! strTo 2 "abcd" 2 4))
 |#;->	"12cd5678"
(define (string-copy! to tstart from .			maybe-fstart+fend)
	(let-string-start+end 'string-copy! from	maybe-fstart+fend
		(lambda (fstart fend)
			(check-arg integer? tstart 'string-copy!)
			(check-substring-spec 'string-copy! to tstart (+ tstart (- fend fstart)))
    		(%string-copy! to tstart from fstart fend)
)	)	)
;;; Library-internal routine
#|
(let* ((strTarget (make-string 8 #\A))) (%string-copy! strTarget 2 "1234" 0 4))
 |#;->	"AA1234AA"
(define (%string-copy! to tstart from fstart fend)
	(if (> fstart tstart)
		(do (	(i fstart (+ i 1))
				(j tstart (+ j 1))
			)
			((>= i fend))
			(string-set! to j (string-ref from i))
		)

		(do (	(i (- fend 1)                    (- i 1))
				(j (+ -1 tstart (- fend fstart)) (- j 1))
			)
			((< i fstart))
			(string-set! to j (string-ref from i))
	)	)
	to ; AlSchemist 2021
)
(define (%string-copy s) (substring s 0 (string-length s))) ; srfi-14

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Returns starting-position in STRING or #f if not true.
;;; Knuth-Morris-Pratt string searching
;;; See
;;;     "Fast pattern matching in strings"
;;;     SIAM J. Computing 6(2):323-350 1977
;;;     D. E. Knuth, J. H. Morris and V. R. Pratt
;;; also described in
;;;     "Pattern matching in strings"
;;;     Alfred V. Aho
;;;     Formal Language Theory - Perspectives and Open Problems
;;;     Ronald V. Brook (editor)
;;; This algorithm is O(m + n) where m and n are the 
;;; lengths of the pattern and string respectively
#|
(string-contains "zero-based index" "x")
 |#;->	15       ; 0123456789012345
(define (string-contains text pattern . maybe-starts+ends)
	(let-string-start+end2 'string-contains text pattern maybe-starts+ends
		(lambda (t-start t-end p-start p-end)
			(%kmp-search pattern text char=? p-start p-end t-start t-end)
)	)	)
#| Perf: short pattern
(duration '(string-contains-okmij	"zero-based index" "x")		1000)
(duration '((string-contains-maker "x")	"zero-based index")		1000)
(duration '(%kmp-search "x" "zero-based index" char=? 0 1 0 16) 1000)
(duration '(string-contains			"zero-based index" "x")		1000)
 |#;->		515 ms 636 µs for 1K runs. 000 ms 515 µs by run okmij
;			640 ms 092 µs for 1K runs. 000 ms 640 µs by run maker
;		1 s 015 ms 047 µs for 1K runs. 001 ms 015 µs by run %kmp
;		1 s 046 ms 924 µs for 1K runs. 001 ms 046 µs by run srfi
#| Perf: long pattern
(let*	(	(text (string-tabulate (lambda (idx)(integer->char (+ idx 48))) 75))
			(pattern (string-tabulate (lambda (idx)(integer->char (+ idx 96))) 27))
			(nbrRun	1000)(timeEnd 0)(timeStart (gettimeofday))
		)	; expected answer 48
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
		((string-contains-maker pattern) text)
;		(string-contains-okmij text pattern)
;		(%kmp-search pattern text char=? 0 27 0 75)
;		(string-contains text pattern)
	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->	1 s 828 ms 128 µs for 1K runs. 001 ms 828 µs by run maker
;		2 s 406 ms 290 µs for 1K runs. 002 ms 406 µs by run okmij
;		6 s 296 ms 864 µs for 1K runs. 006 ms 296 µs by run %kmp
;		6 s 343 ms 864 µs for 1K runs. 006 ms 343 µs by run srfi
;;; KMP search source[start,end) for PATTERN.
;;; Return starting index of leftmost match or #f.
#|
(%kmp-search "x" "zero-based index" char=? 0 1 0 16)
 |#;->	15
(define (%kmp-search pattern text c= p-start p-end t-start t-end)
	(let ((plen (- p-end p-start))
		  (rv (%make-kmp-restart-vector pattern c= p-start p-end)))
		(let lp ((ti t-start) (pi 0) (tj (- t-end t-start)) (pj plen))
			(if (= pi plen)
				(- ti plen)
				(and (<= pj tj)
					(if (c= (string-ref text ti) (string-ref pattern (+ p-start pi)))
						(lp (+ 1 ti) (+ 1 pi) (- tj 1) (- pj 1))
						(let ((pi (vector-ref rv pi)))
							(if (= pi -1)
								(lp (+ ti 1) 0 (- tj 1) plen)
								(lp ti pi tj (- plen pi))
)	)	)	)	)	)	)	)
;;; (%make-kmp-restart-vector pattern [c= start end]) -> integer-vector
;;; Compute the KMP restart vector RV for string PATTERN.
;;; If we have matched chars 0..i-1 of PATTERN against a search string S, and
;;; PATTERN[i] doesn't match S[k], then reset i := RV[i], and try again to
;;; match S[k].  If RV[i] = -1, then punt S[k] completely, and move on to
;;; S[k+1] and PATTERN[0] -- no possible match of PAT[0..i] contains S[k].
;;;
;;; In other words, if you have matched the first i chars of PATTERN, but
;;; the i+1'th char doesn't match, RV[i] tells you what the next-longest
;;; prefix of PATTERN is that you have matched.
;;;
;;; - C= (default CHAR=?) is used to compare characters for equality.
;;;   Pass in CHAR-CI=? for case-folded string search.
;;;
;;; - START & END restrict the pattern to the indicated substring; the
;;;   returned vector will be of length END - START. The numbers stored
;;;   in the vector will be values in the range [0,END-START) -- that is,
;;;   they are valid indices into the restart vector; 
;;;   you have to add START to them to use them as indices into PATTERN.
;;;
;;; I've split this out as a separate function in case other constant-string
;;; searchers might want to use it.
;;;
;;; E.g.:
;;;    a b d  a b x
;;; #(-1 0 0 -1 1 2)
(define (%make-kmp-restart-vector pattern . maybe-c=+start+end)
	(let*	((c=	(if (pair? maybe-c=+start+end)(car maybe-c=+start+end) char=?)))
		(check-arg procedure? c= '%make-kmp-restart-vector)
		(let-string-start+end '%make-kmp-restart-vector pattern 
			(if (pair? maybe-c=+start+end) (cdr maybe-c=+start+end) nil)
			(lambda (start end)
				(let* ((rvlen (- end start)) (rv (make-vector rvlen -1)))
					(if (> rvlen 0)
						(let ((rvlen-1 (- rvlen 1)) (c0 (string-ref pattern start)))
							(let lp1 ((i 0) (j -1) (k start))
								(if (< i rvlen-1)
									(let lp2 ((j j))
										(cond
											((= j -1)
											   (let ((i1 (+ 1 i)))
												   (if (not (c= (string-ref pattern (+ k 1)) c0))
													   (vector-set! rv i1 0))
												   (lp1 i1 0 (+ k 1))))
											((c= (string-ref pattern k)
												   (string-ref pattern (+ j start)))
											   (let* ((i1 (+ 1 i)) (j1 (+ 1 j)))
												   (vector-set! rv i1 j1)
												   (lp1 i1 j1 (+ k 1))))
											(else (lp2 (vector-ref rv j)))
					)	)	)	)	)	)
					rv
)	)	)	)	)
;;; We've matched I chars from PAT. C is the next char from the search string.
;;; Return the new I after handling C. 
;;;
;;; The pattern is (VECTOR-LENGTH RV) chars long, beginning at index PAT-START
;;; in PAT (PAT-START is usually 0). The I chars of the pattern we've matched
;;; are 
;;;     PAT[PAT-START .. PAT-START + I].
;;;
;;; It's *not* an oversight that there is no friendly error checking or
;;; defaulting of arguments. This is a low-level, inner-loop procedure
;;; that we want integrated/inlined into the point of call.
(define (%kmp-step pat rv c i c= p-start)
	(let lp ((i i))
		(if (c= c (string-ref pat (+ i p-start))) ; Match =>
			(+ i 1) ;   Done.
			(let ((i (vector-ref rv i))) ; Back up in PAT.
				(if (= i -1) 0	; Can't back up further.
					(lp i))		; Keep trying for match.
)	)	)	)
;;; Zip through S[start,end), looking for a match of PAT.
;;; Assume we've already matched the first I chars of PAT when we commence at S[start].
;;; - <0:  If we find a match *ending* at index J, return -J.
;;; - >=0: If we get to the end of the S[start,end) span without finding
;;;   a complete match, return the number of chars from PAT we'd matched
;;;   when we ran off the end.
;;;
;;; This is useful for searching *across* buffers -- 
;;; that is, when your input comes in chunks of text. 
;;; We hand-integrate the %kmp-step loop for speed.
(define (%string-kmp-partial-search pat rv s i . c=+p-start+s-start+s-end)
	(check-arg vector? rv '%string-kmp-partial-search)
	(let*	(	(c=	(if (pair? c=+p-start+s-start+s-end) (car c=+p-start+s-start+s-end)
						char=?
				)
				(p-start (if (>= (length c=+p-start+s-start+s-end) 1)
							(car c=+p-start+s-start+s-end) 0
			)	)
		(check-arg procedure? c= '%string-kmp-partial-search)
		(check-arg (lambda(n)(and (integer? n) (exact? n) (<= 0 n))) 
			p-start '%string-kmp-partial-search
		)  
		(let-string-start+end '%string-kmp-partial-search s
			(if (>= (length c=+p-start+s-start+s-end) 1) (cddr c=+p-start+s-start+s-end))
			(lambda	(s-start s-end)
				(let ((patlen (vector-length rv)))
					(check-arg
						(lambda (i) (and (integer? i) (exact? i) (<= 0 i) (< i patlen)))
						i '%string-kmp-partial-search
					)
					(let lp ((si s-start) (vi i))
						(cond	
							((= vi patlen) (- si))
							((= si s-end) vi)
							(else
								(let ((c (string-ref s si)))
									(lp (+ si 1)
										(let lp2 ((vi vi))
											(if (c= c (string-ref pat (+ vi p-start)))
												(+ vi 1)
												(let ((vi (vector-ref rv vi)))
														(if (= vi -1) 0 (lp2 vi))
)	)	)	)	)	)	)	)	)	)	)	)	)	)	)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Misc
#| Is a string empty?
(duration '(string-null? "cut each word after space separator") 100000)
 |#;->	2 s 281 ms 303 µs for 100K runs. 000 ms 022 µs by run
#| Perf:
(duration '(string-null? "") 100000)
 |#;->	2 s 249 ms 549 µs for 100K runs. 000 ms 022 µs by run
(define (string-null? s) (zero? (string-length s)))
#|
(string-reverse "Madam")
 |#;->	madaM"
(define (string-reverse s . maybe-start+end)
	(let-string-start+end 'string-reverse s maybe-start+end
		(lambda (start end)
			(let*	(	(len (- end start))
						(ans (make-string len))
					)
				(do (	(i start (+ i 1))
						(j (- len 1) (- j 1))
					)
					(	(< j 0))
					(string-set! ans j (string-ref s i))
				)
				ans
)	)	)	)
#|
(let* ((str (string-build "Race" "car"))) (string-reverse! str))
 |#;->	"racecaR"
(define (string-reverse! s . maybe-start+end)
	(let-string-start+end 'string-reverse! s maybe-start+end
		(lambda (start end)
			(do (	(i (- end 1) (- i 1))
					(j start (+ j 1))
				)
				(	(<= i j))
				(let ((ci (string-ref s i)))
					(string-set! s i (string-ref s j))
					(string-set! s j ci)
			)	)
			s ; AlSchemist 1
)	)	)
#|
(reverse-list->string '(#\M #\a #\D #\a #\m))
 |#;->	"maDaM"
(define (reverse-list->string clist)
	(let* ((len (length clist))
		(s (make-string len)))
		(do ((i (- len 1) (- i 1))   (clist clist (cdr clist)))
			((not (pair? clist)))
			(string-set! s i (car clist))
		)
		s
)	)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; STRING-CONCATENATE & STRING-CONCATENATE/SHARED are passed a list of
;;; strings, which they concatenate into a result string.
;;; STRING-CONCATENATE always allocates a fresh string; 
;;; STRING-CONCATENATE/SHARED may (or may not) return a result that 
;;; shares storage with any of its arguments. In particular, if it is applied 
;;; to a singleton list, it is permitted to return the car of that list as its value.
#| Concatenate the string list strs
(string-concatenate/shared '("0" "1" "2" "3"))
 |#;->	"0123"
(define (string-append/shared . strings) (string-concatenate/shared strings))
(define (string-concatenate/shared strings)
	(let lp ((strings strings) (nchars 0) (first #f))
		(cond
			((pair? strings)			; Scan the args, add up total
				(let*	(	(string  (car strings))	; length, remember 1st 
							(tail (cdr strings))		; non-empty string.
							(slen (string-length string))
						)
					(if (zero? slen)
						(lp tail nchars first)
						(lp tail (+ nchars slen) (or first strings))
			)	)	)
			((zero? nchars) "")

			;; Just one non-empty string! Return it.
			((= nchars (string-length (car first))) (car first))

			(else
				(let ((ans (make-string nchars)))
					(let lp ((strings first) (i 0))
						(if (pair? strings)
							(let* ((s (car strings))
								(slen (string-length s)))
								(%string-copy! ans i s 0 slen)
								(lp (cdr strings) (+ i slen))
					)	)	)
					ans
)	)	)	)	)
#| Perf:
(duration '(string-concatenate/shared '("0" "1" "2" "3")) 1000)
 |#;->	15 s 256 ms 747 µs for 1K runs. 015 ms 256 µs by run
#|
(string-concatenate '("0" "1" "2" "3"))
 |#;->	"0123"
; Alas, Scheme 48's APPLY blows up if you have many, many arguments.
(define (string-concatenate strings) (apply string-append strings))
;;; Here it is written out. I avoid using REDUCE to add up string lengths
;;; to avoid non-R5RS dependencies.
#|
(define (string-concatenate strings)
	(let*	(	(total	(do (	(strings strings (cdr strings))
								(i 0 (+ i (string-length (car strings))))
							)
							((not (pair? strings)) i)
				)		)
				(ans (make-string total))
			)
		(let lp ((i 0) (strings strings))
				(if (pair? strings)
					(let* ((s (car strings))
					(slen (string-length s)))
					(%string-copy! ans i s 0 slen)
					(lp (+ i slen) (cdr strings))
		)	)	)
		ans
)	)
 |#
#| Perf:
(let*	(	(nbrRun	1000)(timeEnd 0)(timeStart (gettimeofday))
		)
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
		(string-concatenate '("0" "1" "2" "3"))
	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->		030 ms 742 µs for 1K runs. 000 ms 030 µs by run with apply string-append
 ;     39 s 874 ms 534 µs for 1K runs. 039 ms 874 µs by run without apply
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; string-concatenate-reverse        string-list [final-string end] -> string
;;; string-concatenate-reverse/shared string-list [final-string end] -> string
#|
(string-concatenate-reverse '("M" "a" "D" "a" "m"))
 |#;->	"maDaM"
(define (string-concatenate-reverse string-list . maybe-final+end)
	(if (not-pair? maybe-final+end)
		(apply string-append (reverse string-list)) ; AlSchemist 2021
		(let*	(	(final	(car maybe-final+end)) (lenFinal 0)(end	0))
			(check-arg string? final 'string-concatenate-reverse)
			(set! lenFinal (string-length final))
			(if (= (length maybe-final+end) 2)  ; AlSchemist 2021
				(begin	(set! end (cadr maybe-final+end))
						(check-arg	(lambda (idx) 
										(and (integer? idx) (exact? idx)
											(<= 0 idx lenFinal)
									)	) end 'string-concatenate-reverse
				)		)
				(set! end lenFinal)
			)
			(let	(	(len	(let lp ((sum 0) (lis string-list))
								(if (pair? lis)
									(lp (+ sum (string-length (car lis))) (cdr lis))
									sum
					)	)	)	)
				(%finish-string-concatenate-reverse len string-list final end)
	)	)	)	)
#|
(string-concatenate-reverse/shared '("M" "a" "D" "a" "m"))
 |#;->	"maDaM"
(define (string-concatenate-reverse/shared string-list . maybe-final+end)
	(let*	(	(final	(if (pair? maybe-final+end) (car maybe-final+end) ""))
				(lenFinal 0) (end 0)  ; AlSchemist 2021
			)
		(check-arg string? final 'string-concatenate-reverse/shared)
		(set! lenFinal (string-length final))
		(if (= (length maybe-final+end) 2)  ; AlSchemist 2021
			(begin	(set! end (cadr maybe-final+end)) 
					(check-arg	(lambda (idx) (and (integer? idx) (exact? idx))
									(<= 0 idx lenFinal)
								) end 'string-concatenate-reverse/shared
				)		)
			(set! end lenFinal)
		)
		;; Add up the lengths of all the strings in STRING-LIST; 
		;; also get a pointer NZLIST into STRING-LIST 
		;; showing where the first non-zero-length string starts.
		(let lp ((len 0) (nzlist #f) (lis string-list))
			(if (pair? lis)
				(let ((slen (string-length (car lis))))
					(lp (+ len slen)
						(if (or nzlist (zero? slen)) nzlist lis)
						(cdr lis)
				)	)

				(cond	((zero? len) (substring/shared final 0 end))
						;; LEN > 0, so NZLIST is non-empty.
						((and (zero? end) (= len (string-length (car nzlist))))
					 (car nzlist))
						(else (%finish-string-concatenate-reverse len nzlist final end))
)	)	)	)	)
(define (%finish-string-concatenate-reverse len string-list final end)
	(let ((ans (make-string (+ end len))))
		(%string-copy! ans len final 0 end)
		(let lp ((i len) (lis string-list))
			(if (pair? lis)
				(let*	(	(s   (car lis))
							(lis (cdr lis))
							(slen (string-length s))
							(i (- i slen))
						)
					(%string-copy! ans i s 0 slen)
					(lp i lis)
		)	)	)
		ans
)	)
#| Perf:
(duration '(string-concatenate-reverse		  '("M" "a" "D" "a" "m")) 100)
(duration '(string-concatenate-reverse/shared '("M" "a" "D" "a" "m")) 100)
(duration '(string-concatenate-reverse		  '("M" "a" "D" "a" "m") "") 100)
 |#;->		015 ms 125 µs for 100 runs. 000 ms 151 µs by run apply
;		2 s 984 ms 391 µs for 100 runs. 029 ms 843 µs by run shared
;		3 s 265 ms 147 µs for 100 runs. 032 ms 651 µs by run string-concatenate-reverse

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; string-replace s1 s2 start1 end1 [start2 end2] -> string
;;; Replace S1[START1,END1) with S2[START2,END2).
#| Replace the first backslash with slash
(let*	(	(pathScript "C:\\Program Files\\GIMP 2\\share\\gimp\\2.0\\scripts")
			(idxBackSlash (string-index-okmij pathScript #\\))
		)
	(string-replace pathScript "/" idxBackSlash (inc idxBackSlash))
)
 |#;->	"C:/Program Files\\GIMP 2\\share\\gimp\\2.0\\scripts"
(define (string-replace s1 s2 start1 end1 . maybe-start+end)
	(check-substring-spec 'string-replace s1 start1 end1)
	(let-string-start+end 'string-replace s2 maybe-start+end
		(lambda (start2 end2)
			(let*	(	(slen1 (string-length s1))
						(sublen2 (- end2 start2))
						(alen (+ (- slen1 (- end1 start1)) sublen2))
						(ans (make-string alen))
					)
				(%string-copy! ans 0 s1 0 start1)
				(%string-copy! ans start1 s2 start2 end2)
				(%string-copy! ans (+ start1 sublen2) s1 end1 slen1)
				ans
)	)	)	)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; xsubstring s from [to start end] -> string
;;; S is a string; START and END are optional arguments that demarcate
;;; a substring of S, defaulting to 0 and the length of S (e.g., the whole
;;; string). Replicate this substring up and down index space, in both the
;;:  positive and negative directions. For example, if S = "abcdefg",
;                                                        START=3^ ^END=6
; then we have the conceptual bidirectionally-infinite string:
;;; ...  d  e  f  d  e  f  d  e  f  d  e  f  d  e  f  d  e  f  d ...
;;; ... -9 -8 -7 -6 -5 -4 -3 -2 -1  0  1  2  3  4  5  6  7  8  9 ...
;                      FROM=-2^                          ^TO=8
;;; XSUBSTRING returns the substring of this string beginning at index FROM,
;;; and ending at TO (which defaults to FROM+(END-START)).
#|
(xsubstring "abcdef" -2 8 3 6)
 |#;->	"efdefdefde"
;;; You can use XSUBSTRING in many ways:
#| Rotate a string left. Rotate a string right. Replicate a string
(xsubstring "abcdef" 2)(xsubstring "abcdef" -2) (xsubstring "abc" 0 7)
 |#;->	     "cdefab"               "efabcd"                 "abcabca"
;;; Note that 
;;;   - The FROM/TO indices give a half-open range -- the characters from
;;;     index FROM up to, but not including index TO.
;;;   - The FROM/TO indices are not in terms of the index space for string S.
;;;     They are in terms of the replicated index space of the substring
;;;     defined by S, START, and END.
;;;
;;; It is an error if START=END -- although this is allowed by special
;;; dispensation when FROM=TO.
(define (xsubstring s from .				maybe-to+start+end)
	(check-arg isInt? from 'xsubstring)
	(let-string-start+end 'xsubstring s 
		(if (pair? maybe-to+start+end) (cdr maybe-to+start+end) nil)
		(lambda (start end)
			(let*	(	(to		(if (pair?		maybe-to+start+end)
									(car		maybe-to+start+end)
									(+ from end)
						)		)
						(slen   (- end start))
						(anslen (- to  from))
					)
				(check-arg isInt? to 'xsubstring)
				(cond
					((zero? anslen) "")
					((< anslen 0)	(error "1330. xsubstring from must be < to"))
					((zero? slen)	(error "1331. Cannot replicate empty string"
											'xsubstring s from to start end
					)				)
					((= 1 slen)		; Fast path for 1-char replication.
						(make-string anslen (string-ref s start))
					)
					;; Selected text falls entirely within one span.
					((= (floor (/ from slen)) (floor (/ to slen)))
						(substring s (+ start (modulo from slen))
							(+ start (modulo to   slen))
					)	)
					;; Selected text requires multiple spans.
					(else	(let ((ans (make-string anslen)))
								(%multispan-repcopy! ans 0 s from to start end)
								ans
)	)	)	)	)	)		)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; string-xcopy! target tstart s sfrom [sto start end] -> unspecific
;;; Exactly the same as xsubstring, but the extracted text is written
;;; into the string TARGET starting at index TSTART.
;;; This operation is not defined if (EQ? TARGET S) 
;;;	-- you cannot copy a string on top of itself.
#|
(let* ((target (string-build "123456"))) (string-xcopy! target 0 "abcdef" 2))
 |#;->	"cdefab"
(define (string-xcopy! target tstart s sfrom .	maybe-sto+start+end)
	(check-arg isInt? sfrom 'string-xcopy!)
	(let-string-start+end 'string-xcopy! s 
		(if (pair? maybe-sto+start+end)	(cdr 	maybe-sto+start+end) nil)
		(lambda (start end)
			(let*	(	(sto	(if (pair? 			maybe-sto+start+end)
									(car			maybe-sto+start+end)
									(+ sfrom end)
						)		)
						(tocopy (- sto sfrom))
						(tend (+ tstart tocopy))
						(slen (- end start))
					)
				(check-arg isInt? sto 'string-xcopy!)
				(check-substring-spec 'string-xcopy! target tstart tend)
				(cond ((zero? tocopy))
					((zero? slen) (error "1340. Cannot replicate empty (sub)string"
						'string-xcopy! target tstart s sfrom sto start end)
					)
					((= 1 slen)			; Fast path for 1-char replication.
						(string-fill! target (string-ref s start) tstart tend)
					)
					;; Selected text falls entirely within one span.
					((= (floor (/ sfrom slen)) (floor (/ sto slen)))
						(%string-copy! target tstart s 
							(+ start (modulo sfrom slen))
							(+ start (modulo sto   slen))
					)	)
					;; Multi-span copy.
					(else (%multispan-repcopy! target tstart s sfrom sto start end))
)	)	)	)	)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This is the core copying loop for XSUBSTRING and STRING-XCOPY!
;;; Internal -- not exported, no careful arg checking.
(define (%multispan-repcopy! target tstart s sfrom sto start end)
	(let*	(	(slen (- end start))
				(i0 (+ start (modulo sfrom slen)))
				(total-chars (- sto sfrom))
			)
		;; Copy the partial span @ the beginning
		(%string-copy! target tstart s i0 end)

		(let*	(	(ncopied (- end i0))			; We've copied this many.
					(nleft (- total-chars ncopied))	; # chars left to copy.
					(nspans (quotient nleft slen))	; # whole spans to copy
				)
			;; Copy the whole spans in the middle.
			(do ((i (+ tstart ncopied) (+ i slen))	; Current target index.
				(nspans nspans (- nspans 1)))	; # spans to copy
				((zero? nspans)
					;; Copy the partial-span @ the end & we're done.
					(%string-copy! target i s start 
						(+ start (- total-chars (- i tstart)))
				)	)
				(%string-copy! target i s start end); Copy a whole span.
)	)	)	)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (string-join string-list [delimiter grammar]) => string
;;; Paste strings together using the delimiter string.
#| 
(string-join '("foo" "bar" "baz") ":")(string-join '("foo" "bar") ":" 'prefix)
 |#;->		   "foo:bar:baz"						 ":foo:bar"
#|
(string-join '("foo" "bar" "baz") ":" 'suffix)
 |#;->		   "foo:bar:baz:"
(define (string-join strings . delim+grammar)
	(let*	(	(delim	(if (pair? delim+grammar) (car delim+grammar) " "))
				(grammar(if (= (length delim+grammar) 2) (cadr delim+grammar) 'infix))
				(buildit(lambda (lis final)
							(let recur ((lis lis))
								(if (pair? lis)
									(cons delim ; ' for prefix grammar
										(cons (car lis) (recur (cdr lis)))
									)
									final
			)	)		)	)	)
		(cond
			((pair? strings)
				(string-concatenate
					(case grammar
						((infix strict-infix)
							(cons (car strings) (buildit (cdr strings) '())))
						((prefix)				(buildit strings '()))
						((suffix)
							(cons (car strings) (buildit (cdr strings) (list delim))))
						(else (error "1350. string-join: illegal join grammar" grammar))
			)	)	)
			((not (null? strings))
				(error "1351. string-join: STRINGS parameter not list." strings)
			)
			;; STRINGS is ()
			((eq? grammar 'strict-infix)
				(error "1352. string-join: empty list cannot be joined with STRICT-INFIX grammar."
				)
			)
			(else "") ; Special-cased for infix grammar.
)	)	)
;;; Porting & performance-tuning notes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; See the section at the beginning of this file on external dependencies.
;;;
;;; The biggest issue with respect to porting is the LET-OPTIONALS* macro.
;;; There are many, many optional arguments in this library; the complexity
;;; of parsing, defaulting & type-testing these parameters is handled with the
;;; aid of this macro. There are about 15 uses of LET-OPTIONALS*. You can
;;; rewrite the uses, port the hairy macro definition (which is implemented
;;; using a Clinger-Rees low-level explicit-renaming macro system), or port
;;; the simple, high-level definition, which is less efficient.
;;;
;;; There is a fair amount of argument checking. This is, strictly speaking,
;;; unnecessary -- the actual body of the procedures will blow up if, say, a
;;; START/END index is improper. However, the error message will not be as
;;; good as if the error were caught at the "higher level." Also, a very, very
;;; smart Scheme compiler may be able to exploit having the type checks done
;;; early, so that the actual body of the procedures can assume proper values.
;;; This isn't likely; this kind of compiler technology isn't common any 
;;; longer.
;;; 
;;; The overhead of optional-argument parsing is irritating. The optional
;;; arguments must be consed into a rest list on entry, and then parsed out.
;;; Function call should be a matter of a few register moves and a jump; it
;;; should not involve heap allocation! Your Scheme system may have a superior
;;; non-R5RS optional-argument system that can eliminate this overhead. If so,
;;; then this is a prime candidate for optimising these procedures,
;;; *especially* the many optional START/END index parameters.
;;;
;;; Note that optional arguments are also a barrier to procedure integration.
;;; If your Scheme system permits you to specify alternate entry points
;;; for a call when the number of optional arguments is known in a manner
;;; that enables inlining/integration, this can provide performance 
;;; improvements.
;;;
;;; There is enough *explicit* error checking that *all* string-index
;;; operations should *never* produce a bounds error. Period. Feel like
;;; living dangerously? *Big* performance win to be had by replacing
;;; STRING-REF's and STRING-SET!'s with unsafe equivalents in the loops. 
;;; Similarly, fixnum-specific operators can speed up the arithmetic done on 
;;; the index values in the inner loops. The only arguments that are not
;;; completely error checked are
;;;   - string lists (complete checking requires time proportional to the
;;;     length of the list)
;;;   - procedure arguments, such as char->char maps & predicates.
;;;     There is no way to check the range & domain of procedures in Scheme.
;;; Procedures that take these parameters cannot fully check their
;;; arguments. But all other types to all other procedures are fully
;;; checked.
;;;
;;; This does open up the alternate possibility of simply *removing* these 
;;; checks, and letting the safe primitives raise the errors. On a dumb
;;; Scheme system, this would provide speed (by eliminating the redundant
;;; error checks) at the cost of error-message clarity.
;;;
;;; See the comments preceding the hash function code for notes on tuning
;;; the default bound so that the code never overflows your implementation's
;;; fixnum size into bignum calculation.
;;;
;;; In an interpreted Scheme, some of these procedures, or the internal
;;; routines with % prefixes, are excellent candidates for being rewritten
;;; in C. Consider STRING-HASH, %STRING-COMPARE, the 
;;; %STRING-{SUF,PRE}FIX-LENGTH routines, STRING-COPY!, STRING-INDEX &
;;; STRING-SKIP (char-set & char cases), SUBSTRING and SUBSTRING/SHARED,
;;; %KMP-SEARCH, and %MULTISPAN-REPCOPY!.
;;;
;;; It would also be nice to have the ability to mark some of these
;;; routines as candidates for inlining/integration.
;;; 
;;; All the %-prefixed routines in this source code are written
;;; to be called internally to this library. They do *not* perform
;;; friendly error checks on the inputs; they assume everything is
;;; proper. They also do not take optional arguments. These two properties
;;; save calling overhead and enable procedure integration -- but they
;;; are not appropriate for exported routines.

;;; Copyright details
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The prefix/suffix and comparison routines in this code had (extremely
;;; distant) origins in MIT Scheme's string lib, and was substantially
;;; reworked by Olin Shivers (shivers@ai.mit.edu) 9/98. As such, it is
;;; covered by MIT Scheme's open source copyright. See below for details.
;;; 
;;; The KMP string-search code was influenced by implementations written
;;; by Stephen Bevan, Brian Dehneyer and Will Fitzgerald. However, this
;;; version was written from scratch by myself.
;;;
;;; The remainder of this code was written from scratch by myself for scsh.
;;; The scsh copyright is a BSD-style open source copyright. See below for
;;; details.
;;;     -Olin Shivers

;;; MIT Scheme copyright terms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This material was developed by the Scheme project at the Massachusetts
;;; Institute of Technology, Department of Electrical Engineering and
;;; Computer Science.  Permission to copy and modify this software, to
;;; redistribute either the original software or a modified version, and
;;; to use this software for any purpose is granted, subject to the
;;; following restrictions and understandings.
;;; 
;;; 1. Any copy made of this software must include this copyright notice
;;; in full.
;;; 
;;; 2. Users of this software agree to make their best efforts (a) to
;;; return to the MIT Scheme project any improvements or extensions that
;;; they make, so that these may be included in future releases; and (b)
;;; to inform MIT of noteworthy uses of this software.
;;; 
;;; 3. All materials developed as a consequence of the use of this
;;; software shall duly acknowledge such use, in accordance with the usual
;;; standards of acknowledging credit in academic research.
;;; 
;;; 4. MIT has made no warrantee or representation that the operation of
;;; this software will be error-free, and MIT is under no obligation to
;;; provide any services, by way of maintenance, update, or otherwise.
;;; 
;;; 5. In conjunction with products arising from the use of this material,
;;; there shall be no use of the name of the Massachusetts Institute of
;;; Technology nor of any adaptation thereof in any advertising,
;;; promotional, or sales literature without prior written consent from
;;; MIT in each case.

;;; Scsh copyright terms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; All rights reserved.
;;; 
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. The name of the authors may not be used to endorse or promote products
;;;    derived from this software without specific prior written permission.
;;; 
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
;;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;_______________________________	Section 1 end

; Tools optimized by AlSchemist 2021 for the section 1

; https://stackoverflow.com/questions/32782508/scheme-function-that-return-composition-of-functions
; CC BY-SA 4.0
#| (symbol->char 'alpha) ; convert the first letter of the symbol to char
((compose car string->list symbol->string) 'alpha)
 |#;->	#\a
(define (identity x) x)

#| Compose the given list of functions. Note the double opening parenthesis
((compose car string->list symbol->string car) '(a b c))
 |#;->	#\a
#| Version compatible with string-unfold
(string-unfold null? (compose car string->list symbol->string car) cdr '(a b c))
 |#;->	"abc"
(define (compose . procs) (reduce-right compose2 identity procs))

#|  [	Begin nested comment compose-lambda: version incompatible with string-unfold
#| 
((compose-lambda car string->list symbol->string) 'alpha)
 |#;->	#\a
#| 
(string-unfold null? (compose-lambda car string->list symbol->string car) cdr '(a b c))
 |#;->	Error: string-set!: argument 3 must be: character 
(define (compose-lambda . procs)
	(define (%compose prm)
		(if	(null? procs) prm
			(let*	((proc (car procs)))
				(set! procs (cdr procs))
				(proc (%compose prm))
	)	)	)
	(lambda(prm)(%compose prm))
)
 |#;]	End nested comment compose-lambda

#|  [	Begin nested comment compose-macro: slowest version by macro
#| Generator of the body of compose-macro
(gen-compose '(car string->list symbol->string car))
 |#;->	(car (string->list (symbol->string (car prm))))
(define (gen-compose lstProc)
		(if (pair? (cdr lstProc))
			(let*	(	(proc	(car lstProc))
					)
				`(,proc ,(gen-compose (cdr lstProc)))
			)
			`(,(car lstProc) prm)
)		)
#| The macro version costs 4 ms by call vs. 0.5 ms for compose
((compose-macro car string->list symbol->string) 'alpha)
 |#;->	#\a
(macro (compose-macro lstCall)
	(let* ((prog (gen-compose (cdr lstCall))))
		`(lambda(prm) ,prog)
)	)
 |#;]	End nested comment compose-macro

#| The following lambda version of arity 2 needs two opening parenthesis
((compose2 (lambda(str)(string-ref str 0)) symbol->string) 'alpha)
 |#;->	#\a
(define (compose2 f g)(lambda(prm)(f (g prm))))

#| The following lambda version of arity 3 needs two opening parenthesis
((compose3 car string->list symbol->string) 'alpha)
 |#;->	#\a
(define (compose3 f1 f2 f3)(lambda(prm)(f1 (f2 (f3 prm)))))

#| The no-lambda version is the fatest version and needs one opening parenthesis
(compose2-no-lambda (lambda(str)(string-ref str 0)) symbol->string 'alpha)
(arity compose2-no-lambda)
 |#;->	#\a
;		3 ;                 <--3-->
(define (compose2-no-lambda f g prm)(f (g prm)))

#| Perf:
(let*	((nbrRun 10000)(timeEnd 0)(timeStart (gettimeofday)))
	(let loop ((idx 0))
		(if (>= idx nbrRun) (time-stat timeStart (gettimeofday) nbrRun)
			(begin
				(compose2-no-lambda (lambda(str)(string-ref str 0)) symbol->string 'alpha)
;				((compose2 (lambda(str)(string-ref str 0)) symbol->string) 'alpha)
;				((compose3 car string->list symbol->string) 'alpha)
;				((compose car string->list symbol->string) 'alpha)
;				((compose-macro car string->list symbol->string) 'alpha)
				(loop (+ idx 1))
)	)	)	)
 |#;->		687 ms 496 µs for 10K runs. 000 ms 068 µs by run compose2-no-lambda
;			859 ms 400 µs for 10K runs. 000 ms 085 µs by run compose2
;		3 s 640 ms 618 µs for 10K runs. 000 ms 364 µs by run compose3
;		5 s 249 ms 511 µs for 10K runs. 000 ms 524 µs by run compose reduce-right
;	   38 s 546 ms 382 µs for 10K runs. 003 ms 854 µs by run compose-macro

; Other tools powered by AlSchemist 2021 for the section 1

; Call a proc of arity two as a function of arity one
#| Generate an intermediate lambda accepting the 2nd parameter for proc
((curry string-ref "ABC") 0)
 |#;->	#\A
#| char-set needs 1srfi-014-char-set.scm
(let* ((cs (char-set #\L #\a #\e)))
	(unfold-right end-of-char-set? (curry char-set-ref cs)
		(curry char-set-cursor-next cs) (char-set-cursor cs)
)	)
 |#;->	(#\L #\a #\e)
(define (curry proc prm1) (lambda (prm2)(proc prm1 prm2)))
#| Perf:
(let*	((nbrRun 10000)(timeEnd 0)(timeStart (gettimeofday)))
	(let loop ((idx 0))
		(if (>= idx nbrRun) (time-stat timeStart (gettimeofday) nbrRun)
			(begin	((curry string-ref "ABC") 0)
					(loop (+ idx 1))
)	)	)	)
 |#;->	593 ms 212 µs for 10K runs. 000 ms 059 µs by run

#| Generate a string in the given variable that can be modified
(let* ((str (string-build "Hello" "World!"))) (string-set! str 10 #\.))
 |#;->	"HelloWorld."
#| A litteral string is immutable meaning that it cannot be modified 
(string-set! "HelloWorld!" 10 #\.)
 |#;->	Error: string-set!: unable to alter immutable string: "HelloWorld!"
(define (string-build strFirst . maybe-strings)
	(let*	(	(lstWord	(if (pair? maybe-strings)(cons strFirst maybe-strings)
								nil
				)			)
				(strFull	(if (pair? lstWord) (string-concatenate lstWord)
								strFirst
				)			)
				(len (string-length strFull))
				(strOut (make-string len))
			)
		(%string-copy! strOut 0 strFull 0 len)
)	)
#|
(isInt? 314)	(isInt? 3.0)	(isInt? 3.14)
 |#;->	#t				#t				#f
(define (isInt? val) (and (integer? val) (exact? val)))
;-
#|
(let-string-start+end 'test1 "123" nil (lambda(start end)(show "start: " start " end: " end)))
 |#;->	start: 0 end: 3
(define (let-string-start+end caller str lstOpt proc)
	(if (pair? lstOpt)
		(let*	(	(start	(car lstOpt))
					(len	(string-length str))
					(end	(if (= (length lstOpt) 2) (cadr lstOpt) len))
				)
			(cond	((or (not (integer? start))(not (integer? end)))
						(error "1360." caller 'start '& 'end 'must 'be 'integer)
					)
					((< start 0)  (error "1361." caller 'start 'must 'be '>= 0))
					((> start end)(error "1362." caller 'start 'must 'be '<  end))
					((> end len)  (error "1363." caller 'end   'must 'be '<= len))
					(else (proc start end))
		)	)
		(proc 0 (string-length str))		
)	)
#| Perf
(let*	(	(nbrRun	2000)(timeEnd 0)(timeStart (gettimeofday))
		)
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
		(let-string-start+end 'test1 "123" nil (lambda(start end) nil))
	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->	108 ms 875 µs for 2K runs. 000 ms 054 µs by run
#|
(let-string-start+end2 'test1 "st1" "str2" nil
	(lambda(start1 end1 start2 end2)(show "start1: " start1 " end1: " end1 " start2: " start2 " end2: " end2))
)
(let-string-start+end2 'test1 "st1" "str2" '(1 3 2)
	(lambda(start1 end1 start2 end2)(show "start1: " start1 " end1: " end1 " start2: " start2 " end2: " end2))
)
 |#;->	start1: 0 end1: 3 start2: 0 end2: 4
;		start1: 1 end1: 3 start2: 2 end2: 4
(define (let-string-start+end2 caller s1 s2 lstOpt proc)
	(if (pair? lstOpt)
		(let*	(	(start1	(car lstOpt))
					(len1	(string-length s1))
					(end1	(if (>= (length lstOpt) 2) (cadr	lstOpt) len1))
					(start2	(if (>= (length lstOpt) 3) (caddr	lstOpt) 0))
					(len2	(string-length s2))
					(end2	(if (=  (length lstOpt) 4) (cadddr	lstOpt) len2))
				)
			(cond	((or (not (integer? start1))(not (integer? end1)))
						(error "1370." caller 'start1 '& 'end1 'must 'be 'integer)
					)
					((or (not (integer? start2))(not (integer? end2)))
						(error "1371." caller 'start1 '& 'end1 'must 'be 'integer)
					)
					((< start1 0)	(error "1372." caller 'start1 'must 'be '>= 0))
					((> start1 end1)(error "1373." caller 'start1 'must 'be '<  end1))
					((> end1 len1)  (error "1374." caller 'end1   'must 'be '<= len1))
					((< start2 0)	(error "1375." caller 'start2 'must 'be '>= 0))
					((> start2 end1)(error "1376." caller 'start2 'must 'be '<  end2))
					((> end2 len2)  (error "1377." caller 'end2   'must 'be '<= len2))
					(else (proc start1 end1 start2 end2))
		)	)
		(proc 0 (string-length s1) 0 (string-length s2))		
)	)
;----------------------- optional string base and procedure make-final
#| By default, base is the empty string and make-final returns also ""
(let*	(	(seeds (string->list "seeds without options ")))
	(define (test seeds . base+make-final)
		(let-maybe-base+make-final 'test1 base+make-final
			(lambda (base make-final)
				(string-append base (dblQuote seeds) (make-final seeds))
	)	)	)
	(test seeds) ; no optional parameters
)
 |#;->	"seeds without options "
#| base becomes "Left " and make-final returns "" by default 
(let*	(	(seeds (string->list "seeds Right ")))
	(define (test seeds . base+make-final)
		(let-maybe-base+make-final 'test2 base+make-final
			(lambda (base make-final)
				(string-append base (dblQuote seeds) (make-final seeds))
	)	)	)
	(test seeds "Left ") ; only the first optional parameter base
)
 |#;->	"Left seeds Right "
 #| base becomes "Left " and make-final is a lambda of list
(let*	(	(seeds (string->list "seeds Right ")))
	(define (test seeds . base+make-final)
		(let-maybe-base+make-final 'test3 base+make-final
			(lambda (base make-final)
				(string-append base (dblQuote seeds) (make-final seeds))
	)	)	)
	(test seeds "Left " (lambda(lst)(string-upcase(list->string lst))))
)
 |#;->	"Left seeds Right SEEDS RIGHT "
#| base becomes "Left " and make-final puts in uppercase the seeds
(let*	(	(seeds (string->list "seeds Right ")))
	(define (test seeds . base+make-final)
		(let-maybe-base+make-final 'test4 base+make-final
			(lambda (base make-final)
				(string-append base (dblQuote seeds) (make-final seeds))
	)	)	)
	(test seeds "Left " (compose string-upcase list->string)) ; full options
)
 |#;->	"Left seeds Right SEEDS RIGHT "
(define (let-maybe-base+make-final caller lstOpt proc)
	(if (pair? lstOpt)
		(let*	(	(base		(car lstOpt))
					(make-final	(lambda (lst) ""))
				)
			(check-arg string? base caller)
			(if (= (length lstOpt) 2) 
				(begin	(set! make-final (cadr lstOpt))
						(check-arg procedure? make-final caller)
			)	)
			(proc base make-final)	
		)
		(proc "" (lambda (lst) ""))
)	)
#| Perf1:
(let*	(	(seeds (string->list "seeds without options "))
			(nbrRun	100)(timeEnd 0)(timeStart 0)
		)
	(define (test seeds . base+make-final)
		(let-maybe-base+make-final 'test1 base+make-final
			(lambda (base make-final)
				(string-append base (dblQuote seeds) (make-final seeds))
	)	)	)
	(set! timeStart (gettimeofday))
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
		(test seeds) ; no optional parameters
	)	(time-stat timeStart timeEnd nbrRun)
)
|#;->		296 ms 939 µs for 100 runs. 002 ms 969 µs by run define
;		1 s 468 ms 266 µs for 100 runs. 14 ms 682 µs by run macro
#| Perf2: 
(let*	(	(seeds (string->list "seeds Right "))
			(nbrRun	100)(timeEnd 0)(timeStart 0)
		)
	(define (test seeds . base+make-final)
		(let-maybe-base+make-final 'test2 base+make-final
			(lambda (base make-final)
				(string-append base (dblQuote seeds) (make-final seeds))
	)	)	)
	(set! timeStart (gettimeofday))
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
		(test seeds "Left ") ; only the first optional parameter base
	)	(time-stat timeStart timeEnd nbrRun)
)
|#;->		171 ms 894 µs for 100 runs. 001 ms 718 µs by run define
;		1 s 374 ms 502 µs for 100 runs. 13 ms 745 µs by run macro
#| Perf3:
(let*	(	(seeds (string->list "seeds Right "))
			(nbrRun	100)(timeEnd 0)(timeStart 0)
		)
	(define (test seeds . base+make-final)
		(let-maybe-base+make-final 'test3 base+make-final
			(lambda (base make-final)
				(string-append base (dblQuote seeds) (make-final seeds))
	)	)	)
	(set! timeStart (gettimeofday))
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
		(test seeds "Left " (lambda(lst)(string-upcase(list->string lst))))
	)	(time-stat timeStart timeEnd nbrRun)
)
|#;->		265 ms 641 µs for 100 runs. 002 ms 656 µs by run define
;		1 s 468 ms 313 µs for 100 runs. 14 ms 683 µs by run macro
#| Perf4:
(let*	(	(seeds (string->list "seeds Right "))
			(nbrRun	100)(timeEnd 0)(timeStart 0)
		)
	(define (test seeds . base+make-final)
		(let-maybe-base+make-final 'test4 base+make-final
			(lambda (base make-final)
				(string-append base (dblQuote seeds) (make-final seeds))
	)	)	)
	(set! timeStart (gettimeofday))
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
		(test seeds "Left " (compose string-upcase list->string)) ; full options
	)	(time-stat timeStart timeEnd nbrRun)
)
|#;->		374 ms 995 µs for 100 runs. 003 ms 749 µs by run define
;		1 s 546 ms 353 µs for 100 runs. 15 ms 463 µs by run macro
;_______________________________	Section 2 begin by Oleg Kiselyov 2004
																
; http://okmij.org/ftp/README.html by Oleg Kiselyov. mij: made in Japan
; The following is free open source copyrighted Oleg Kiselyov
; All multiline comments and pretty prints are adapted by AlSchemist
#| space is the separator by default
(string-split "cut each word after space separator")
 |#;->	("cut" "each" "word" "after" "space" "separator")

#| With a string as pattern, use strbreakup defined in script-fu-compat.init line 235
(string-split "3.141592654" '(#\.)) (strbreakup "3.141592654" "1415")
 |#;->	("3" "141592654")            ("3."  "92654")

; http://okmij.org/ftp/Scheme/util.html#string-util
; $Id: util.scm,v 2.6 2004/07/08 19:51:57 oleg Exp oleg $
; -- procedure+: string-split STRING
; -- procedure+: string-split STRING '()
; -- procedure+: string-split STRING '() MAXSPLIT
;
; Returns a list of whitespace delimited words in STRING.
; If STRING is empty or contains only whitespace, then the empty list
; is returned. Leading and trailing whitespaces are trimmed.
; If MAXSPLIT is specified and positive, the resulting list will
; contain at most MAXSPLIT elements, the last of which is the string
; remaining after (MAXSPLIT - 1) splits. If MAXSPLIT is specified and
; non-positive, the empty list is returned. "In time critical
; applications it behooves you not to split into more fields than you
; really need."
;
; -- procedure+: string-split STRING CHARSET
; -- procedure+: string-split STRING CHARSET MAXSPLIT
;
; Returns a list of words delimited by the characters in CHARSET in
; STRING. CHARSET is a list of characters that are treated as delimiters.
; Leading or trailing delimeters are NOT trimmed. That is, the resulting
; list will have as many initial empty string elements as there are
; leading delimiters in STRING.
;
; If MAXSPLIT is specified and positive, the resulting list will
; contain at most MAXSPLIT elements, the last of which is the string
; remaining after (MAXSPLIT - 1) splits. If MAXSPLIT is specified and
; non-positive, the empty list is returned. "In time critical
; applications it behooves you not to split into more fields than you
; really need."
;
; This is based on the split function in Python/Perl
#|
(string-split " abc d e f  ")	(string-split " abc d e f  " '() 1)
 |# ;->       ("abc" "d" "e" "f")			  ("abc d e f  ") ;  ^ maxsplit
#|
(string-split " abc d e f  " '() 0)(string-split ":abc:d:e::f:" '(#\:))
 |# ;->                       ()              ("" "abc" "d" "e" "" "f" "")
#|
(string-split ":" '(#\:)) (string-split "root:x:0:0:Lord" '(#\:) 2)
 |# ;->     ("" "")                    ("root" "x:0:0:Lord")
#|
(string-split "/usr/local/bin:/usr/bin:/usr/ucb/bin" '(#\:))
 |# ;-> ("/usr/local/bin" "/usr/bin" "/usr/ucb/bin"
#|
(string-split "/usr/local/bin" '(#\/))
 |# ;-> ("" "usr" "local" "bin")
(define (string-split str . rest)
	(let* 	((lenStr (string-length str))) ; 2021/06/05: AlSchemist's optimisation
		(define (split-by-whitespace str maxsplit) ; maxsplit is a positive number
			(define (skip-ws i yet-to-split-count)
				(cond
					((>= i lenStr) '())
					((char-whitespace? (string-ref str i))
					(skip-ws (inc i) yet-to-split-count))
					(else (scan-beg-word (inc i) i yet-to-split-count))
			)	)
			(define (scan-beg-word i from yet-to-split-count)
				(cond
				((zero? yet-to-split-count)
					(cons (substring str from lenStr) '())
				)
				(else (scan-word i from yet-to-split-count)))
			)
			(define (scan-word i from yet-to-split-count)
				(cond
					((>= i lenStr)
						(cons (substring str from i) '())
					)
					((char-whitespace? (string-ref str i))
						(cons (substring str from i) 
						(skip-ws (inc i) (- yet-to-split-count 1)))
					)
					(else (scan-word (inc i) from yet-to-split-count))
			)	)
			(skip-ws 0 (- maxsplit 1))
		)

		; maxsplit is a positive number. str is not empty
		(define (split-by-charset str delimeters maxsplit)
			(define (scan-beg-word from yet-to-split-count)
				(cond
					((>= from lenStr) '(""))
					((zero? yet-to-split-count)
						(cons (substring str from lenStr) '())
					)
					(else (scan-word from from yet-to-split-count))
			)	)
			(define (scan-word i from yet-to-split-count)
				;(show "scan-word( i=" i " from=" from " yet-to-split-count="yet-to-split-count ")")
				(cond
					((>= i lenStr)(cons (substring str from i) '()))
	; original
	;				(	(memq (string-ref str i) delimeters)
	;					(cons (substring str from i) (scan-beg-word (inc i) (- yet-to-split-count 1)))
	;				)
	; 2021/06/05: "memq" becomes "memv": https://stackoverflow.com/questions/16299246/what-is-the-difference-between-eq-eqv-equal-and-in-scheme
	; CC BY-SA 4.0
					(	(memv (string-ref str i) delimeters)
						(cons (substring str from i) (scan-beg-word (inc i) (- yet-to-split-count 1)))
					)
	; because (eq? #\: (car '(#\:))) => #f in TinyScheme but (eqv? #\: (car '(#\:))) => #t
	 
					(else (scan-word (inc i) from yet-to-split-count))
			)	)
			;(show "split-by-charset(\"" str "\" " delimeters " " maxsplit ")")
			(scan-beg-word 0 (- maxsplit 1))
		)
			
		; resolver of overloading... if omitted, maxsplit defaults to (inc lenStr)
		(if (string-null? str) '()
			(if (null? rest) 
				(split-by-whitespace str (inc lenStr))
				(let	(	(charset 	(car rest))
							(maxsplit	(if (pair? (cdr rest)) (cadr rest) (inc lenStr)))
						)
					(cond 
						((not (positive? maxsplit)) '())
						((null? charset) (split-by-whitespace str maxsplit))
						(else (split-by-charset str charset maxsplit))
)	)	)	)	)	)
#|
(duration '(string-split "cut each word after space separator") 1000)
 |#;->	1 s 702 ms 636 µs for 1K runs. 001 ms 702 µs by run

#| C:\Program Files\GIMP\share\gimp\2.0\scripts\script-fu-compat.init
(duration '(strbreakup "cut each word after space separator" " ") 1000)
 |#;->	8 s 093 ms 769 µs for 1K runs. 008 ms 093 µs by run
;-----
; make-char-quotator QUOT-RULES
;
; Given QUOT-RULES, an assoc list of (char . string) pairs, return a quotation procedure. 
; The returned quotation procedure takes a string and returns either a string or a list of strings. 
; The quotation procedure check to see if its argument string contains any instance of a character that needs to be encoded (quoted).
; If the argument string is "clean", it is returned unchanged. 
; Otherwise, the quotation procedure will return a list of string fragments.
; The input straing will be broken at the places where the special characters occur.
; The special character will be replaced by the corresponding encoding strings.
;
; For example, to make a procedure that quotes special HTML characters, do:
#|
(begin	(define string->html-list	(make-char-quotator 
			'((#\< . "&lt;") (#\> . "&gt;") (#\& . "&amp;") (#\" . "&quot;"))
		)							)
		(string->html-list "a < b & \"string\"")
)
 |#;->	("a " "&lt;" " b " "&amp;" " " "&quot;" "string" "&quot;")

(define (make-char-quotator char-encoding)
	(let ((bad-chars (map car char-encoding)))

		; Check to see if str contains one of the characters in charset, from the position i onward.
		; If so, return that character's index. otherwise, return #f
		(define (index-cset str i charset)
			(let loop ((i i))
				(and (< i (string-length str))
					(if (memv (string-ref str i) charset) i
					(loop (inc i)))
		)	)	)

		; The body of the returned function
		(lambda (str)
			(let ((bad-pos (index-cset str 0 bad-chars)))
				(if (not bad-pos) str	; str had all good chars
					(let loop ((from 0) (to bad-pos))
						(cond
							((>= from (string-length str)) '())
							((not to)
							(cons (substring str from (string-length str)) '()))
							(else
								(let ((quoted-char
									(cdr (assv (string-ref str to) char-encoding)))
									(new-to 
									(index-cset str (inc to) bad-chars)))
									(if (< from to)
										(cons
											(substring str from to)
											(cons quoted-char (loop (inc to) new-to))
										)
										(cons quoted-char (loop (inc to) new-to))
)	)	)	)	)	)	)	)	)	)
;__________

; http://okmij.org/ftp/README.html
; A subset of SRFI-13 functions
; $Id: srfi-13-local.scm,v 1.2 2004/07/08 20:24:53 oleg Exp oleg $
#|
(let* ((str (string-build "12345678"))) (string-xcopy-okmij! str 2 "abcd" 0 4))
 |#;->	"12abcd78"
(define (string-xcopy-okmij! target tstart s sfrom sto)
	(do ((i sfrom (inc i)) (j tstart (inc j)))
		((>= i sto))
		(string-set! target j (string-ref s i))
	)
	target ; AlSchemist 2021
)
#| Perf:
(let*	(	(strTarget (string-build "12345678"))
			(nbrRun	500)(timeEnd 0)(timeStart (gettimeofday))
		)
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
		(string-xcopy-okmij! strTarget 2 "abcd" 0 4)
	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->	3 s 015 ms 645 µs for 500 runs. 006 ms 031 µs by run
#| Perf:
(let*	(	(strTarget (string-build "12345678"))
			(nbrRun	500)(timeEnd 0)(timeStart (gettimeofday))
		)
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
		(string-xcopy! strTarget 2 "abcd" 0 4)
	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->	16 s for 500 runs. 32 ms 717 µs by run
;
#| Return the index of the last occurence of a-char in str, or #f
(string-index-right-okmij "zero-based index" #\e)
 |#;->	14          	 ; 012345678901234
(define (string-index-right-okmij str a-char)
	(let loop ((pos (dec (string-length str))))
		(cond
			((negative? pos) #f) 	; whole string has been searched, in vain
			((char=? a-char (string-ref str pos)) pos)
			(else (loop (dec pos)))
)	)	)
#|
(string-contains-okmij "zero-based index" "x")
 |#;->	15       	  ; 0123456789012345            
#|
(string-contains-okmij "zero-index" "indexes") (string-contains-okmij "zero" "ZERO")
 |#;->	#f                      ; ^missing ending "es"                 #f
#|
(string-contains-okmij "zero-based index" "zero")
 |#;->	0             ; ^
; string-contains    s1 s2 -> integer or false
;     Does string s1 contain string s2?
;     Return the index in s1 where s2 occurs as a substring, or false. The
;     optional start/end indices restrict the operation to the indicated substrings.
(define (string-contains-okmij str pattern) ; We do not support the optional arguments
	(let* 	(	(pat-len (string-length pattern))
				(search-span (- (string-length str) pat-len))
				(c1 (if (zero? pat-len) #f (string-ref pattern 0)))
				(c2 (if (<= pat-len 1) #f (string-ref pattern 1)))
			)
		(cond
			((not c1) 0)           ; empty pattern, matches upfront
			((not c2) (string-index str c1)) ; one-char pattern
			(else                  ; matching a pattern of at least two chars
				(let outer	((pos 0))
					(cond
						(	(> pos search-span) #f)	; nothing was found thru the whole str
						(	(not (char=? c1 (string-ref str pos)))
							(outer (+ 1 pos))	; keep looking for the right beginning
						)
						(	(not (char=? c2 (string-ref str (+ 1 pos))))
							(outer (+ 1 pos))	; could've done pos+2 if c1 == c2....
						)
						(else                  	; two char matched: high probability
							; the rest will match too
							(let inner ((i-pat 2) (i-str (+ 2 pos)))
								(if (>= i-pat pat-len) pos ; whole pattern matched
									(if (	char=? (string-ref pattern i-pat)
											(string-ref str i-str)
										)
										(inner (+ 1 i-pat) (+ 1 i-str))
										(outer (+ 1 pos))	; mismatch after partial match
)	)	)	)	)	)	)	)	)	)					
#| Perf:
(duration '(string-contains-okmij "zero-based index" "x") 2000)
 |#;->	2 s 140 ms 149 µs for 2K runs. 001 ms 070 µs by run
;-
#|
(string-suffix-okmij? "ate" "pirate")(string-suffix-okmij? "rag" "outrage")
 |#;->		                   #t                                       #f
#|
(string-suffix-okmij? "" "any-string")(string-suffix-okmij? "any-string" "any-string")
 |#;->	              #t                                     #t
; checks to make sure that PATTERN is a prefix of STRING
(define (string-suffix-okmij? pattern str)
	(let loop ((i (dec (string-length pattern))) (j (dec (string-length str))))
		(cond	(	(negative? i) #t)
				(	(negative? j) #f)
				(	(char=? (string-ref pattern i) (string-ref str j))
					(loop (dec i) (dec j))
				)
				(else #f)
)	)	)
#| Perf
(duration '(string-suffix-okmij? "ate" "pirate") 10000)
(duration '(string-suffix?	     "ate" "pirate") 10000)
 |#;->	3 s 734 ms 380 µs for 10K runs. 000 ms 373 µs by run okmij
;       5 s 343 ms 265 µs for 10K runs. 000 ms 534 µs by run check optional args

#| string-contains-ci s1 s2 -> integer or false
; checks to make sure that PATTERN is a suffix of STRING
(string-suffix-ci-okmij? "ATE" "pirate")
 |#;->	#t
(define (string-suffix-ci-okmij? pattern str)
	(let loop ((i (dec (string-length pattern))) (j (dec (string-length str))))
		(cond	(	(negative? i) #t)
				(	(negative? j) #f)
				(	(char-ci=? (string-ref pattern i) (string-ref str j))
					(loop (dec i) (dec j))
				)
				(else #f)
)	)	)
#|
(string-downcase "Hello World!")
 |# ;-> "hello world!"
; Return a new string made of characters of the original string in the lower case
(define (string-downcase str)
	(let*	(	(len		(string-length str))
				(target-str (make-string len))
			)
		(let loop ((idx 0))
			(cond	(	(>= idx len) target-str)
					(	(string-set! target-str idx (char-downcase (string-ref str idx)))
						(loop (inc idx))
)	)	)	)		)
 #| Perf:
(duration '(string-downcase "ZERO-BASED INDEX") 2000)
 |#;->	1 s 483 ms 945 µs for 2K runs. 000 ms 741 µs by run
;		7 s 687 ms 168 µs for 2K runs. 003 ms 843 µs by run script-fu-compat.init

#|
(string-upcase "Hello World!")
 |# ;-> "HELLO WORLD!"
 
; Return a new string made of characters of the original string in the upper case
(define (string-upcase str)
	(let*	(	(len		(string-length str))
				(target-str (make-string len))
			)
		(let loop ((idx 0))
			(cond	(	(>= idx len) target-str)
					(	(string-set! target-str idx (char-upcase (string-ref str idx)))
						(loop (inc idx))
					)
)	)	)	)
;_______________________________	Section 2 end 

#| https://stackoverflow.com/questions/11509500/string-replace-in-gimp-script-fu
; CC BY-SA 4.0
(string-alter "C:\\Program Files\\GIMP 2\\share\\gimp\\2.0\\scripts" "\\" "/")
 |#;->	"C:/Program Files/GIMP 2/share/gimp/2.0/scripts"

#| Use "set!" to save the result of the replacement:
(let*	((path "C:/Users/Chess/AppData/Roaming/GIMP/2.10/scripts")) (set! path (string-alter path "/" "\\")))
 |#;->	"C:\\Users\\Chess\\AppData\\Roaming\\GIMP\\2.10\\scripts"

; Replace, in the string strWhere, the pattern strWhat with strWith
(define (string-alter strWhere strWhat strWith) ; strWhere remains unmodified
    (let*	(	(lenWhat  (string-length strWhat))
				(lenWith  (dec (string-length strWith)))
				(lenWhere (string-length strWhere))
				(result strWhere)
			)
		(let loop ((curIndex 0))		
			(if (<= (+ curIndex lenWhat) lenWhere) ; loop through the main string searching for the substring
				(begin
					(if (substring-equal? strWhat result curIndex (+ curIndex lenWhat)) ; check to see if the substring is a match
						(begin
							(set! result (string-append (substring result 0 curIndex) strWith (substring result (+ curIndex lenWhat) lenWhere))) ;create the result string
							(set! curIndex (+ curIndex lenWith)) ; current index to the end of the replacement
							(set! lenWhere (string-length result)) ; new length for lenWhere so we can accurately grab what we need
					)	)
					(loop (+ curIndex 1))
		)	)	)
		result
)    )
#| Perf:
(duration '(string-alter "C:\\Program Files\\GIMP 2\\share\\gimp\\2.0\\scripts" "\\" "/") 1000)
 |#;->	8 s 718 ms 218 µs for 1K runs. 008 ms 718 µs by run
 
;_______________________________	Section 3 begin by Ken Dickey 1991

; https://legacy.cs.indiana.edu/scheme-repository/code.string.html
; Scheme code for string manipulation
; FILE			"substr.scm"
; IMPLEMENTS	Substring search
; AUTHOR		Ken Dickey
; DATE			1991 August 6
; LAST UPDATED	AlSchemist 2021 substring-search-maker becomes string-contains-maker
; NOTES	Based on "A Very Fast Substring Search Algorithm", Daniel M. Sunday,
;		CACM v33, #8, August 1990.

;; Gambit-specific compile options
;(##declare (ieee-scheme) (standard-bindings) (lambda-lift) (block) (fixnum))

;; SUBSTRING-SEARCH-MAKER takes a string (the "pattern") and returns a function
;; which takes a string (the "target") and either returns #f or the index in
;; the target in which the pattern first occurs as a substring.
#|
((string-contains-maker "test") "This is a test string")
 |#;->	10				; 		 01234567890
#|
((string-contains-maker "test") "This is a text string")
 |#;->	#f				;  ^				 ^
;; Build the proc finder for pattern-string to be applied to target string
(define (string-contains-maker pattern-string)
	(define num-chars-in-charset 256)
	(define (build-shift-vector pattern-string)
		(let* ((pat-len (string-length pattern-string))
			   (shift-vec (make-vector num-chars-in-charset (+ pat-len 1)))
			   (max-pat-index (- pat-len 1)))
			(let loop ((index 0))
				(vector-set!
					shift-vec
					(char->integer (string-ref pattern-string index))
					(- pat-len index))
				(if (< index max-pat-index) (loop (+ index 1)) shift-vec)
	)	)	)
	(let ((shift-vec (build-shift-vector pattern-string))
		  (pat-len (string-length pattern-string)))
		(lambda (target-string)
			(let* ((tar-len (string-length target-string))
				   (max-tar-index (- tar-len 1))
				   (max-pat-index (- pat-len 1)))
				(let outer ((start-index 0))
					(if (> (+ pat-len start-index) tar-len) #f
						(let inner ((p-ind 0) (t-ind start-index))
							(cond (	(> p-ind max-pat-index) #f)
								  (	(char=?
									   (string-ref pattern-string p-ind)
									   (string-ref target-string t-ind)
									)
									(if (= p-ind max-pat-index)
									   start-index
									   (inner (+ p-ind 1) (+ t-ind 1))
								  )	)
								  ((> (+ pat-len start-index) max-tar-index) #f)
								  (else
								   (outer	(+ start-index
												(vector-ref shift-vec
													(char->integer
														(string-ref target-string
															(+ start-index pat-len)
)	)	)	)	)	)	)	)	  )	)		)	)	)	)
#| Perf: the lambda builder needs a little more time than string-index char
(duration '(string-index-okmij "zero-based index" #\x)		2000)
(duration '(string-index       "zero-based index" #\x)		2000)
(duration '((string-contains-maker "x") "zero-based index")	2000)
 |#;->		937 ms 492 µs for 2K runs. 000 ms 468 µs by run okmij
;			983 ms 887 µs for 2K runs. 000 ms 491 µs by run string-index
;		1 s 296 ms 470 µs for 2K runs. 000 ms 648 µs by run string-contains-maker
#| Perf: long string pattern
(let*	(	(text (string-tabulate (lambda (idx)(integer->char (+ idx 48))) 75))
			(pattern (string-tabulate (lambda (idx)(integer->char (+ idx 96))) 27))
			(proc-finder (lambda(x) nil))
			(nbrRun	1000)(timeEnd 0)(timeStart (gettimeofday))
		) ; expected result 48
	(set! proc-finder (string-contains-maker pattern))	; perf 1 factorize builder
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
		(proc-finder text)								; perf 1 factorize builder
;		((string-contains-maker pattern) text)			; perf 2 make at each time
;		(string-contains-okmij text pattern)			; perf 3 okmij
;		(string-contains text pattern)					; perf 4 srfi
	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->	1 s 218 ms 759 µs for 1K runs. 001 ms 218 µs by run 1 factorize builder
 ;		1 s 859 ms 383 µs for 1K runs. 001 ms 859 µs by run 2 make at each time
 ;		2 s 406 ms 337 µs for 1K runs. 002 ms 406 µs by run 3 okmij
 ;		6 s 374 ms 952 µs for 1K runs. 006 ms 374 µs by run 4 srfi

;_______________________________	Section 4 begin by AlSchemist 2021
 
; The following is free TinyScheme open source copyrighted AlSchemist
#| Convert a string to a list of strings. Don't confuse with string->list of char
(map string (string->list "abc"))
(string-fold-right (lambda (char lst) (cons (atom->string char) lst)) nil "abc")
(string->string-list "abc")
 |#;->	("a" "b" "c")
(define (string->string-list str)
	(let loop ((lisOut nil)(idx (- (string-length str) 1)))
		(if (>= idx 0)
			(loop (cons (substring str idx (+ idx 1)) lisOut)(- idx 1)) lisOut
)	)	)
#| Perf:
(let*	((nbrRun 10000)(timeEnd 0)(timeStart (gettimeofday)))
	(let loop ((idx 0))
		(if (>= idx nbrRun) (time-stat timeStart (gettimeofday) nbrRun)
			(begin	
				(string->string-list "abc")
				(loop (+ idx 1))
)	)	)	)
 |#;->	 3 s 421 ms 452 µs for 10K runs. 000 ms 342 µs by run substring
;		 4 s 906 ms 319 µs for 10K runs. 000 ms 490 µs by run string-fold-right
;		 8 s 703 ms 166 µs for 10K runs. 000 ms 870 µs by run (string (string-ref))
;		18 s 390 ms 197 µs for 10K runs. 001 ms 839 µs by run (map string (string->list "abc"))
#| Uppercase the beginning of the string and downcase the rest
(string-propercase "hello")
 |#;->	"Hello"
(define (string-propercase str)
	(let*	(	(len		(string-length str))
				(target-str (make-string len))
			)
		(let loop ((idx 0))
			(cond	(	(>= idx len)	target-str)
					(	(= idx 0)
						(string-set! target-str idx (char-upcase (string-ref str idx)))
						(loop (inc idx))
					)
					(	(string-set! target-str idx (char-downcase (string-ref str idx)))
						(loop (inc idx))
)	)	)	)		)
#| Concatenate the first string with the rest. Macro is 10 times slower than set!
(let* ((strRes "Left")) (string-append  strRes " Middle" " Right") strRes)
 |#;->	        "Left"                          ; no side effect on strRes
#|
(let* ((strRes "Left")) (string-append! strRes " Middle" " Right") strRes)
 |#;->	                     "Left Middle Right"   ; side effect on strRes
(macro (string-append! lstMacroCall) ; strRes += string1 + ... + stringN
	(let*	(	(lstArg		(cdr lstMacroCall))
				(strRes	(car lstArg))
				(prog	   `(set! ,strRes ,(cons 'string-append lstArg)))
			);	(show "\n" (car lstMacroCall) " body: " prog "\n->")
		prog
)	)
;__________

 #| Usage: during the call of the function, the parameter is evaluated
(obj->string '(a b 123 cde))(obj->string '(a b 123 cde .scm))	(obj->string 123)
 |#;->	      "(a b 123 cde)"             "(a b 123 cde .scm)"	"123"
#|
(obj->string '(a b 123 cde. prm))(obj->string '(a b 123 cde . prm))	(obj->string 'symb)
 |#;->	      "(a b 123 cde. prm)"             "(a b 123 cde . prm)"	"symb"
; Convert obj to a string
(define (obj->string obj)
	(cond	((number? obj)	(number->string obj))
			((symbol? obj)	(symbol->string obj))
			(else
				(let*	(	(portOutStr (open-output-string))
							(sts		(write obj portOutStr))
							(strRes		(get-output-string portOutStr))
						)
					(close-output-port portOutStr)
					strRes
)	)		)	)
#| Perf:
(let*	(	(nbrRun	100000)(timeEnd 0)(timeStart (gettimeofday)))
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
		(obj->string 123)
	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->	2 s 530 ms 725 µs for 100K runs. 000 ms 025 µs by run

#|
(string->obj "()")
 |#;->	()
#|
(eval (string->obj "(+ 1 3)"))
 |#;->	4
(define (string->obj strPrm)
	(let*	(	(portStr	(open-input-output-string strPrm))
				(obj		(read portStr))
			)
		(close-port portStr)
		obj
)	)
#| Perf:
(duration '(string->obj "()") 100000)
 |#;->	3 s 968 ms 272 µs for 100K runs. 000 ms 039 µs by run
;-
#| Usage: during the call of the macro, each parameter is not evaluated
(objs->string a b 123 cde)(objs->string a b 123 cde .scm)
 |#;->	    "(a b 123 cde)"          "(a b 123 cde .scm)"
#|
(objs->string (a b 123 cde . prm))(objs->string a b 123 cde . prm)
 |#;->	      "(a b 123 cde . prm)"            "(a b 123 cde . prm)"#<EOF>
; The dot syntax generates additional #<EOF> outside of the result of the macro 
#| How to eat #<EOF>?
(let* ((sts (objs->string a b 123 cde . prm))) sts)
 |#;->	"(a b 123 cde . prm)"
; Convert the pair or the list to a string
(macro	(objs->string fullCall)	; macro returning its parameters as string
 	(let*	(	(lstArg (cdr fullCall)) ; cut macro name
				(prm1	(car lstArg))	; first parameter
				(prog	(if	(pair? prm1)
							(list 'obj->string (list 'quote prm1))
							(list 'obj->string (list 'quote lstArg))
			)	)		) ;(show "\nobjs->string body: " prog "\n->")
		prog
)	)
;__________

#| Build a string with backslashed double quotes inside the generated string
(dblQuote "Hello World!")(dblQuote 'Hello)(dblQuote *pi*)
 |#;->	"\"Hello World!\""          "Hello" "3.141592654"
#| With a list of strings. The separator is " ".  dblQuote improves unbreakupstr
(dblQuote '("Hello" "World!") " ")
 |#;->	"\"Hello\" \"World!\""
#| With a list of different objects
(dblQuote (list "Hello" 'World #\! *pi* 3 14) ",")
 |#;->	"\"Hello\",\"World\",!,3.141592654,3,14"
(define (dblQuote obj . maybe-sep)	; general string builder utility
	(let*	( 	(strSep (if (pair? maybe-sep)(car maybe-sep) "")))
		(cond	((string? 	obj)	(string-append "\"" obj "\""))
				((symbol? 	obj)	(symbol->string obj))
				((integer?	obj)	(number->string obj))
				((real?		obj)	(real->string obj))
				((pair?		obj)	(if (null? (cdr obj))
										(dblQuote (car obj) strSep)
										(string-append (dblQuote (car obj) strSep) strSep (dblQuote (cdr obj) strSep))
									)
				)
				((null?		obj)	"")
				((atom?		obj)	(atom->string obj))
				(else "")
)	)	)
;__________
#|
(proc? "car")(proc? 'cdr)(proc? do)(proc? (lambda(prm) prm))(proc? (compose car cdr))
 |#;->	 #t           #t         #t         #t                       #t
(define (proc? obj)
	(if (or (procedure? obj)(macro? obj)) #t
		(let*	( 	(strObj		(if (string? obj) obj (obj->string obj)))
					(strIsProc	(string-append "(procedure? "	strObj ")"))
					(strIsMacr	(string-append "(macro? "		strObj ")"))
					(isProc		(string->obj strIsProc))
					(isMacr		(string->obj strIsMacr))
				)
			(catch #f	(or	(eval isProc (interaction-environment))
							(eval isMacr (interaction-environment))
)	)	)	)			)
#|
(string->proc "car") (string->proc "car^toon") (string->proc "define-macro")
 |#;->	<car PROCEDURE 74>              #f                     #<MACRO>
(define (string->proc strProc)
	(let*	( 	(strIsProc (string-append "(procedure? " strProc ")"))
				(isProc (string->obj strIsProc))
				(strIsMac (string-append "(macro? " strProc ")"))
				(isMacro (string->obj strIsMac))
			)
		(catch #f 
			(if (eval isProc (interaction-environment))
				(eval (cadr isProc)(interaction-environment))
					(if (eval isMacro (interaction-environment)) 
						(eval (cadr isMacro)(interaction-environment)) #f
)	)	)	)		)
#| Define the variable name at the top level and initialize it with value
(let* ((symb 'top-level-variable))
		(show "Was " symb "     defined? " (defined? symb))
		(defvar! symb 123)
		(show "Is  " symb " now defined? " (defined? symb))
)
top-level-variable
 |#;->	Was top-level-variable     defined? #f
;		Is  top-level-variable now defined? #t
;		123
(define (defvar! name value)
	(let*	( 	(strName	(cond	((symbol? name) (symbol->string name))
									((string? name)	name)
									(else (error "1390. defvar! expects symbol or string for name. Was: " name)
				)			)		)
				(strValue	(obj->string value))
				(strDefine	(string-append "(define " strName " " strValue ")"))
			)
		(eval (string->obj strDefine) (interaction-environment)
)	)	)
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
(load "C:/Users/YourUserName/AppData/Roaming/GIMP/2.10/scripts/2srfi-013-string.scm")
 |# (closure? string->proc)