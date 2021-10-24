;;;; "0ts-stdio.scm" Implementation of standard C functions for Scheme
;;; Copyright (C) 1991-1993, 1996, 1999-2001 Aubrey Jaffer and Radey Shouman.

; Section 1: Aubrey Jaffer and Radey Shouman 2001 printf		from SLIB 3b6-1
; Section 2: Aubrey Jaffer					 2010 scanf			from SLIB 3b6-1
; Section 3: Aubrey Jaffer					 2003 file->exports from SLIB 3b6-1
; Section 4: Richard O'Keefe				 200x read-line 
; Section 5: AlSchemist                       2021 TinyScheme for Gimp 2.10.28 Script-Fu
; 2021/09/27: GimpλLib 1.0 by AlSchemist for Gimp 2.10.28 TinyScheme Script-Fu
; Line: 1751 define: 75 comment: 610 = 34.8% blank: 38

;_______________________________	Section 1 begin by Jaffer & Shouman 2001
; https://people.csail.mit.edu/jaffer/SLIB.html
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

;(require 'string-case)
;(require 'multiarg-apply)
;(require-if 'compiling 'generic-write)
#|
(stdio:iprintf display "%.2f" *pi*)	(stdio:iprintf display "%.2f" 0.1)
 |#;->	3.14#t						0.10#t
(define (stdio:iprintf out format-string . args)
	(cond	((not (equal? "" format-string))
				(let	((pos -1)
							(fl (string-length format-string))
							(fc (string-ref format-string 0))
						)
					(define (advance)
						(set! pos (+ 1 pos))
						(cond	((>= pos fl) (set! fc #f))
							(else (set! fc (string-ref format-string pos)))
					)	)
					(define (must-advance)
						(set! pos (+ 1 pos))
						(cond	((>= pos fl) (incomplete))
							(else (set! fc (string-ref format-string pos)))
					)	)
					(define (end-of-format?) (>= pos fl))
	(define (incomplete)(error "0020 iprintf. Conv spec incomplete:" format-string))
	(define (wna)
		(error "0021 iprintf. wrong number of args: " (length args) format-string)
	)
					(define (out* strs)
						(if	(string? strs)
							(out strs)
							(let out-loop	((strs strs))
								(or	(null? strs)
									(and (out (car strs)) (out-loop (cdr strs)))
					)	)	)	)

	(let loop ((args args))
		(advance)
		(cond
			((end-of-format?)
			;;(or (null? args) (wna))	;Extra arguments are *not* a bug.
	  		)
			((eqv? #\\ fc) ;;Emulating C strings may not be a good idea.
				(must-advance)
				(and	(case fc
							((#\n #\N) (out #\newline))
							((#\t #\T) (out #\tab)) ; was slib:tab
							;;((#\r #\R) (out #\return))
							((#\f #\F) (out #\x0C)) ; was slib:form-feed
							((#\newline) #t)
							(else (out fc))
						)
						(loop args)
			)	)
			((eqv? #\% fc)
				(must-advance)
	(let	(	(left-adjust #f)(signed #f)	(blank #f)		(alternate-form #f)
				(leading-0s #f)	(width 0)	(precision -1)	(type-modifier #f)
				(read-format-number
					(lambda	()
						(cond	((eqv? #\* fc)
									(must-advance)
									(let ((ans (car args))) (set! args (cdr args)) ans))
							(else
								(do	((c fc fc) 
									 (accum 0 (+ (* accum 10) (string->number (string c))))
									)
									((not (char-numeric? fc)) accum)
									(must-advance)
			)	)	)	)	)	)
	(define (pad pre . strs)
		(let loop	((len (string-length pre)) (ss strs))
			(cond	((>= len width) (cons pre strs))
				((null? ss)
					(cond	(left-adjust
								(cons	pre
										(append
											strs
											(list (make-string (- width len) #\space))
							)	)		)
						(leading-0s (cons pre (cons (make-string (- width len) #\0) strs)))
						(else (cons (make-string (- width len) #\space) (cons pre strs)))
				)	)
				(else (loop (+ len (string-length (car ss))) (cdr ss)))
	)	)	)
	(define integer-convert
		(lambda	(s radix fixcase)
			(cond	((not (negative? precision))
						(set! leading-0s #f)
						(if (and (zero? precision) (eqv? 0 s)) (set! s ""))
			)		)
			(set!	s	(cond	((symbol? s) (symbol->string s))
								((number? s) (number->string s radix))
								((or (not s) (null? s)) "0")
								((string? s) s)
								(else "1")
			)			)
			(if fixcase (set! s (fixcase s)))
			(let	((pre	(cond
								((equal? "" s) "")
								((eqv? #\- (string-ref s 0))
									(set! s (substring s 1 (string-length s)))
									"-"
								)
								(signed "+")
								(blank " ")
								(alternate-form 
									(case radix ((8) "0") ((16) "0x") (else ""))
								)
								(else "")
					))		)
				(pad	pre
						(if	(< (string-length s) precision)
							(make-string (- precision (string-length s)) #\0) ""
						)
						s
	)	)	)	)

 (define (float-convert num fc)
	(define (f digs exp strip-0s)
		(let	((digs (stdio:round-string digs (+ exp precision) (and strip-0s exp))))
			(cond	
				((>= exp 0)
					(let*	(	(i0	(cond	((zero? exp) 0)
										((char=? #\0 (string-ref digs 0)) 1)
										(else 0)))
								(i1 (max 1 (+ 1 exp)))
								(idigs (substring digs i0 i1))
								(fdigs (substring digs i1 (string-length digs))))
						(cons	idigs
								(if	(and (string=? fdigs "") (not alternate-form))
									'()
									(list "." fdigs)
				)	)	)		)
				((zero? precision) (list (if alternate-form "0." "0")))
				((and strip-0s (string=? digs "") (list "0")))
				(else (list "0." (make-string (min precision (- -1 exp)) #\0) digs))
	)	)	)
	(define (e digs exp strip-0s)
		(let*	(	(digs (stdio:round-string digs (+ 1 precision) (and strip-0s 0)))
					(istrt (if (char=? #\0 (string-ref digs 0)) 1 0))
					(fdigs (substring digs (+ 1 istrt) (string-length digs)))
					(exp (if (zero? istrt) exp (- exp 1)))
				)
			(list	(substring digs istrt (+ 1 istrt))
					(if (and (string=? fdigs "") (not alternate-form)) "" ".")
					fdigs
					(if (char-upper-case? fc) "E" "e")
					(if (negative? exp) "-" "+")
					(if (< -10 exp 10) "0" "")
					(number->string (abs exp))
	)	)	)
	(define (g digs exp)
		(let	((strip-0s (not alternate-form)))
			(set! alternate-form #f)
			(cond	((<= (- 1 precision) exp precision)
						(set! precision (- precision exp))
						(f digs exp strip-0s))
				(else (set! precision (- precision 1)) (e digs exp strip-0s))
	)	)	)
	(define (k digs exp sep)
		(let*	((units	'#( "y" "z" "a" "f" "p" "n" "u" "m" ""
							"k" "M" "G" "T" "P" "E" "Z" "Y"))
					(base 8)
					(uind	(let	((i	(if	(negative? exp)
											(quotient (- exp 3) 3)
											(quotient (- exp 1) 3))))
								(and (< -1 (+ i base) (vector-length units)) i)
				)	)		)
			(cond	(uind	(set! exp (- exp (* 3 uind)))
							(set! precision (max 0 (- precision exp)))
							(append
								(f digs exp #f)
								(list sep (vector-ref units (+ uind base)))))
				(else (g digs exp))
	)	)	)

		(cond	((negative?  precision) (set! precision 6))
				((and (zero? precision) (char-ci=? fc #\g)) (set! precision 1))
		)
	(let*	((str	(cond
						((number? num)
#| Original code:
							(number->string (exact->inexact num))
 |#;->	number->string becomes real->string
							(real->string (exact->inexact num))
						)
						((string? num) num)
						((symbol? num) (symbol->string num))
						(else "???")
			))		)
	(define (format-real signed? sgn digs exp . rest)
		(if	(null? rest)
			(cons	(if (char=? #\- sgn) "-" (if signed? "+" (if blank " " "")))
					(case	fc
						((#\e #\E) (e digs exp #f))
						((#\f #\F) (f digs exp #f))
						((#\g #\G) (g digs exp))
						((#\k) (k digs exp ""))
						((#\K) (k digs exp "."))))
			(append (format-real signed? sgn digs exp) (apply format-real #t rest) '("i"))
	)	)

		(or	(stdio:parse-float str
				(lambda	(sgn digs expon . imag)
					(apply pad (apply format-real signed sgn digs expon imag))
			)	)
			(pad "???")	; float-convert
 )	) 	)
		(do	()
			((case	fc	((#\-)		(set! left-adjust #t) #f)
						((#\+)		(set! signed #t) #f)
						((#\space)	(set! blank #t) #f)
						((#\#)		(set! alternate-form #t) #f)
						((#\0)		(set! leading-0s #t) #f)
						(else #t)
			))
			(must-advance)
		)
	    (cond (left-adjust (set! leading-0s #f)))
	    (cond (signed (set! blank #f)))
	    (set! width (read-format-number))
	    (cond	((negative? width)
					(set! left-adjust #t)
					(set! width (- width))
		)		)
	    (cond ((eqv? #\. fc)
			(must-advance)
			(set! precision (read-format-number)))
		)
		;Ignore these specifiers
		(case fc ((#\l #\L #\h) (set! type-modifier fc) (must-advance)))

	    ;;At this point fc completely determines the format to use.
#| original code
	    (if (null? args)
			(if (memv (char-downcase fc) 
					'(#\c #\s #\a #\d #\i #\u #\o #\x #\b #\f #\e #\g #\k)
				)
		    	(wna)
		)	)
 |# ; AlSchemist 2021's optimization:
	    (if (null? args)
			(case fc	(	(	#\c #\s #\a #\d #\i #\u #\o #\x #\b #\f #\e #\g #\k
								#\C #\S #\A #\D #\I #\U #\O #\X #\B #\F #\E #\G #\K
							) 	(wna)
		)	)			)

	    (case fc
		;; only - is allowed between % and c
		((#\c #\C)		; C is enhancement
			(and (out (string (car args))) (loop (cdr args)))
		)
		;; only - flag, no type-modifiers
		((#\s #\S)		; S is enhancement
			(let	((s	(cond	((symbol? (car args)) (symbol->string (car args)))
							((not (car args)) "(NULL)")
							(else (car args)))))
				(cond	((not (or (negative? precision) (>= precision (string-length s))))
							(set! s (substring s 0 precision))))
				(and	(out*	(cond	((<= width (string-length s)) s)
									(left-adjust
										(list s (make-string (- width (string-length s))
													#\space
									)	)		)
									(else
										(list	(make-string
													(- width (string-length s))
													(if leading-0s #\0 #\space))
												s
						)		)	)	)
						(loop (cdr args))
		)	)	)

		;; SLIB extension
	    ((#\a #\A)		;#\a #\A are pretty-print
			(let ((os "") (pr precision))
	;(require 'generic-write)
	(generic-write
		(car args)
		(not alternate-form)
		#f
		(cond	((and left-adjust (negative? pr))
					(set! pr 0)
					(lambda (s) (set! pr (+ pr (string-length s))) (out s)))
			(left-adjust
				(lambda	(s)
					(define sl (- pr (string-length s)))
					(set!	pr
						(cond ((negative? sl) (out (substring s 0 pr)) 0) (else (out s) sl)))
					(positive? sl)))
			((negative? pr)
				(set! pr width)
				(lambda	(s)
					(set! pr (- pr (string-length s)))
					(cond	((not os) (out s))
						((negative? pr) (out os) (set! os #f) (out s))
						(else (set! os (string-append os s))))
					#t))
			(else
				(lambda	(s)
					(define sl (- pr (string-length s)))
					(cond	((negative? sl) (set! os (string-append os (substring s 0 pr))))
						(else (set! os (string-append os s))))
					(set! pr sl)
					(positive? sl)
	)	)	)	)

				(cond	((and left-adjust (negative? precision))
							(cond ((> width pr) (out (make-string (- width pr) #\space)))))
					(left-adjust
						(cond	((> width (- precision pr))
									(out (make-string (- width (- precision pr)) #\space)))))
					((not os))
					((<= width (string-length os)) (out os))
					(else	(and	(out (make-string (- width (string-length os)) #\space))
								(out os)
			)	)	)		); pretty-print
	       (loop (cdr args))
		)  
		((#\d #\D #\i #\I #\u #\U)	(and (out* (integer-convert (car args) 10 #f))
		    							(loop (cdr args))
		)							)
	    ((#\o #\O)					(and (out* (integer-convert (car args) 8 #f))
										(loop (cdr args))
		)							)
		((#\x)						(and (out* (integer-convert (car args) 16
										(if stdio:hex-upper-case? string-downcase #f)))
										(loop (cdr args))
		)							)
		((#\X)						(and (out* (integer-convert (car args) 16
										(if stdio:hex-upper-case? #f string-upcase)))
										(loop (cdr args))
		)							)
		((#\b #\B)					(and (out* (integer-convert (car args) 2 #f))
										(loop (cdr args))
		)							)
		((#\%) (and (out #\%) (loop args)))
		((#\f #\F #\e #\E #\g #\G #\k #\K)
			(and (out* (float-convert (car args) fc)) (loop (cdr args)))
		)
		(else	(cond	((end-of-format?) (incomplete))
						(else (and (out #\%) (out fc) (out #\?) (loop args)))
		)		)
			)	)	)	; eqv? #\% fc		let		case fc
							(else (and (out fc) (loop args)))
)	)	)	)	)	)
#| Print the data according to the format in the port
(fprintf (current-output-port) "%s = %.5d\n" "string" 24)
 |#;->	string = 00024
 ;		15
(define (fprintf port format . args)
	(let	((cnt 0))
		(apply	stdio:iprintf
				(lambda	(x)
					(cond	((string? x)	(set! cnt (+ (string-length x) cnt))
											(display x port) #t
							)
							(else (set! cnt (+ 1 cnt)) (display x port) #t)
				)	) format args
		) cnt
)	)
#| Display the data according to the format. Return the next col
(printf "%s = %s\n" "Hello" "World!")
 |#;->	Hello = World!
;		15 ;          ^15
#|
(printf "%s, %s %d, %d:%.2d\n" "Sunday" "July" 3 10 2)	(printf "%5.2d\n" 2)
 |#;->	Sunday, July 3, 10:02							   02
#|
(printf "%5.2f\n" *pi*)
 |#;->	 3.14
(define (printf format . args) (apply stdio:fprintf (current-output-port) format args))
#|
(let loop ((idx 10))
	(if (> idx 35) (void)
		(begin	(show (sprintf idx "%#-10a|%#10a:%-10.8a:" "12345" "ABCDEFG" "123456789"))
				(loop (+ idx 5))
)	)	)
 |#;->	"12345"   
;		"12345"   | "AB
;		"12345"   | "ABCDEFG
;		"12345"   | "ABCDEFG":123
;		"12345"   | "ABCDEFG":12345678
;		"12345"   | "ABCDEFG":12345678  :
(define (sprintf width format . args)
	(let*	(	(cnt 0)
				(s	(cond	((string? width) width)
						((number? width) (make-string width))
						((not width) (make-string 100))
						(else (error "0023 sprintf. #1 arg size unknown:" width))))
				(end (string-length s)))
		(apply	stdio:iprintf
				(lambda	(x)
					(cond	
						((string? x)
							(if	(or width (>= (- end cnt) (string-length x)))
								(let loop	((lend (min (string-length x) (- end cnt)))
											 (i 0)
											)
									(if (not (>= i lend))
										(begin	(string-set! s cnt (string-ref x i))
												(set! cnt (+ cnt 1))
												(loop lend (+ i 1))
								)	)	)
								(let	()
									(set! s (string-append (substring s 0 cnt) x))
									(set! cnt (string-length s))
									(set! end cnt)
						)	)	)
						((and width (>= cnt end)))
						(else
							(cond	((and (not width) (>= cnt end))
										(set! s (string-append s (make-string 100)))
										(set! end (string-length s))
							)		)
							(string-set! s cnt (if (char? x) x #\?))
							(set! cnt (+ cnt 1))
					)	)
					(not (and width (>= cnt end)))
				) format args)
		(cond ((string? width) cnt) ((eqv? end cnt) s) (else (substring s 0 cnt)))
)	)
#| Perf:
(let*	((nbrRun 100)(timeEnd 0)(timeStart (gettimeofday)))
	(let loop ((idx 0))
		(if (>= idx nbrRun) (set! timeEnd (gettimeofday))
			(begin	
				(sprintf 50 "%#-10a|%#10a:%-10.8a:" "12345" "ABCDEFG" "123456789")
				(loop (+ idx 1))
	)	)	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->	 6 s 718 ms 246 µs for 100 runs. 067 ms 182 µs by run do becomes let loop
 ;		14 s 968 ms 312 µs for 100 runs. 149 ms 683 µs by run original

(define stdio:fprintf fprintf)
;_______________________________	Section 1 end

; TinyScheme tools powered and optimized by AlSchemist 2021 for the section 1

;; Determine the case of digits > 9.  We assume this to be constant.
;(define stdio:hex-upper-case? (string=? "-F" (number->string -15 16)))
(define stdio:hex-upper-case? #f) ; in TinyScheme

;; Parse the output of NUMBER->STRING and pass the results to PROC.
;; PROC takes SIGN-CHARACTER DIGIT-STRING EXPONENT-INTEGER [IMAGPART]
;; SIGN-CHAR will be either #\+ or #\-. DIGIT-STRING begins with "0."
;; If STR denotes a number with imaginary part not exactly zero,
;; 3 additional elements for the imaginary part are passed.
;; If STR cannot be parsed, return #F without calling PROC.
#|
(stdio:parse-float "0.1206" (lambda(sgn digs ex)
	(display sgn) (display digs) (display " e ") (display ex) (newline))
)
 |#;->	+01206 e 0
#|
(stdio:parse-float "31.415" (lambda(sgn digs ex)
	(display sgn) (display digs) (display " e ") (display ex) (newline))
)
 |#;->	+031415 e 2
#|
(stdio:parse-float "#d-0.1206e-5+37.42i" (lambda(sgn digs ex im-sgn im-digs im-ex)
	(display sgn) (display digs) (display " e ") (display ex) (display " im: ")
	(display im-sgn) (display im-digs) (display " e ") (display im-ex) (newline))
)
 |#;->	-01206 e -5 im: +03742 e 2
; Original programmation by continuation strongly simplified by AlSchemist
(define (stdio:parse-float str proc)
	(let*	(	(n (string-length str))
				(ind	(let skipPrefix ((idx 0))
								(if	(and (< idx (- n 1)) (char=? #\# (string-ref str idx)))
									(case	(string-ref str (+ idx 1))
										((#\d #\i #\e) (skipPrefix (+ idx 2)))
										((#\.) idx)
										(else #f)
									)
									idx
				)			)	)
			)
		(define (digits) ; AlSchemist removed both index i and cont
			(let* ((idxBegin ind))
				(let loop ((idx ind))
					(if (< idx n)
						(let*	((ch (string-ref str ind)))
							(if (or (char-numeric? ch) (char=? #\# ch))
								(loop (set! ind (+ ind 1)))
					)	)	)
					(if (= idxBegin ind) "0" (substring str idxBegin ind))
		)	)	)
		(define (point) ; skip decimal point. Only update ind
			(if (and (< ind n) (char=? #\. (string-ref str ind))) (set! ind (+ ind 1)) ind)
		)
		(define (expo) ; return the exponent as a signed number
			(cond	((>= ind n) 0)
				((memv (string-ref str ind) '(#\e #\s #\f #\d #\l #\E #\S #\F #\D #\L))
					(let*	((ch (string-ref str (set! ind (+ ind 1))))) ; skip "e"
						(case ch ((#\+ #\-) (set! ind (+ ind 1)))) ; skip sign
						(if	(char=? #\- ch) (-  (string->number (digits)))
												(string->number (digits))
				)	)	)
				(else 0)
		)	)
		(define (complex sgn digs ex)
			(let* 	(	(chx (string-ref str ind))) ; forward scan of comple(x)
				(case chx ; after the first float about the comple(x)
					((#\+ #\-) ; ind is still on "+" or "-": call real with another cont
						(real	(lambda	(im-sgn im-digs im-ex)
									(if	(and(= ind (- n 1))
											(char-ci=? #\i (string-ref str ind)))
										(proc sgn digs ex im-sgn im-digs im-ex) #f
					)	)		)   )	;  ^arity 6 for complex
					((#\@) ; Polar form: No point in parsing the angle ourselves
						(let	((num (string->number str)))
							(if	num
								(stdio:parse-float
									(number->string (real-part num))
									(lambda	(sgn digs ex)
										(stdio:parse-float
											(number->string (imag-part num))
											(lambda	(im-sgn im-digs im-ex)
												(proc sgn digs ex im-sgn im-digs im-ex)
								)	)	)	) #f ; ^arity 6 for complex
					)	)	)
					(else #f)
		)	)	)
		; scan a real optionnally fowolled by a complex
		(define (real cont) ; the (cont)inuation processes the complex
			(let*	(	(ch		(string-ref str ind))
						(sgn	(case ch ((#\+ #\-) (set! ind (+ ind 1)) ch) (else #\+)))
						(idigs	(digits)) (idPt	(point)) (fdigs	(digits)) (nbrExp (expo))
						(digs	(string-append "0" idigs fdigs)) ; merge
						(ndigs	(string-length digs)) (lenLeft (string-length idigs))
					)
				(set! nbrExp (+	nbrExp lenLeft))
				(let loop	((idz 1))
					(cond	((>= idz ndigs) (set! digs "0") (set! nbrExp 1))
						((char=? #\0 (string-ref digs idz)) ; remove leading zeros
							(set! nbrExp (-	nbrExp 1)) ; reduce the exponent
							(loop (+ idz 1))
						)
						(else (set! digs (substring digs (- idz 1) ndigs)))
				)	)
				(if	(= ind n) (proc sgn digs nbrExp) ; proc's arity 3 for simple float
					(cont sgn digs nbrExp) ; call another proc of arity 6 for complex
		)	)	)
		(and ind (< ind n) (real complex))
)	)
#| Perf:
(let*	(	(sgn1 #\space) (digs1 "") (ex1 "")
			(nbrRun	200)(timeEnd 0)(timeStart (gettimeofday))
		)
	(let loop ((idx 0))
		(if (>= idx nbrRun) (set! timeEnd (gettimeofday))
			(begin	
				(stdio:parse-float "0.1206"
					(lambda(sgn digs ex) (set! sgn1 sgn) (set! digs1 digs) (set! ex1 ex))
				)
				(loop (+ idx 1))
	)	)	)	(time-stat timeStart timeEnd nbrRun) (show sgn1 digs1 " e " ex1)
)
 |#;->	    218 ms 767 µs for 200 runs. 001 ms 093 µs by run AlSchemist 2021
;		2 s 358 ms 841 µs for 200 runs. 011 ms 794 µs by run original
;		+01206 e 0

;; STR is a digit string beginning with "0." representing a floating point mantissa.
;; STRIP-0S: minimum number of digits. Strip trailing zeros otherwise #f
#| Return a digit string rounded to NDIGS digits. "" if NDIGS < 0
(stdio:round-string "0.12006" 5 #f)	(stdio:round-string "0.12005" 5 4)
 |#;->				"0.1201"							"0.120"
#|
(stdio:round-string "01" 2 #f)
 |#;->	"010"
(define (stdio:round-string str ndigs strip-0s)
	(let*	(	(n (- (string-length str) 1))
				(res(cond	((< ndigs 0) "")
						((= n ndigs) str)
						((< n ndigs)
							(let	((padlen (max 0 (- (or strip-0s ndigs) n))))
								(if	(zero? padlen) str
									(string-append str
										(make-string padlen
											(if	(char-numeric? (string-ref str n)) #\0 #\#)
			)	)	)	)	)	)	)	)
		(if (> n ndigs)
			(let*	(	(nextNdigs (+ ndigs 1))
						(dig (lambda (idx)
								(case (string-ref str idx)
									((#\1) 1)((#\2) 2)((#\3) 3)((#\4) 4)
									((#\5) 5)((#\6) 6)((#\7) 7)((#\8) 8)((#\9) 9) (else 0)
						)	)	)
						(ldig (dig nextNdigs))
					)
				(set! res (substring str 0 nextNdigs))
				(if (or	(> ldig 5)
						(and	(= ldig 5)
							(let loop	((i (+ 2 ndigs)))
								(if	(> i n) (odd? (dig ndigs))
									(if	(zero? (dig i)) (loop (+ i 1)) #t)
					)	)	)	)
					(let inc!	((i ndigs))
						(let	((d (dig i)))
							(if	(< d 9)
								(string-set! res i (string-ref (number->string (+ d 1)) 0))
								(begin	(string-set! res i #\0) (inc! (- i 1)))
		)	)	)	)	)	)
		(if	strip-0s
			(let loop	((i (- (string-length res) 1)))
				(if	(or (<= i strip-0s) (not (char=? #\0 (string-ref res i))))
					(substring res 0 (+ i 1))
					(loop (- i 1))
			)	)
			res
)	)	)
#| Perf:
(let*	(	(nbrRun	10000)(timeEnd 0)(timeStart (gettimeofday)))
	(let loop ((idx 0))
		(if (>= idx nbrRun) (set! timeEnd (gettimeofday))
			(begin	
				(stdio:round-string "0.12005" 5 4)
				(loop (+ idx 1))
	)	)	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->	3 s 984 ms 005 µs for 10K runs. 000 ms 398 µs by run (string->number (string c))
 ;															 becomes case
 ;		5 s 468 ms 299 µs for 10K runs. 000 ms 546 µs by run original

#| Perf: does a char belong to a list of char?
(let*	(	(fc #\K)
			(nbrRun	10000)(timeEnd 0)(timeStart (gettimeofday))
		)
	(let loop ((idx 0))
		(if (>= idx nbrRun) (set! timeEnd (gettimeofday))
			(begin	
				(case fc						; test 1
							((	#\c #\s #\a #\d #\i #\u #\o #\x #\b #\f #\e #\g #\k
								#\C #\S #\A #\D #\I #\U #\O #\X #\B #\F #\E #\G #\K
							))
				)
;				(if (memv	(char-downcase fc)	; test 2
;							'(#\c #\s #\a #\d #\i #\u #\o #\x #\b #\f #\e #\g #\k)
;				)	)
;				(if (memv	fc					; test 3
;						   '(	#\c #\s #\a #\d #\i #\u #\o #\x #\b #\f #\e #\g #\k
;								#\C #\S #\A #\D #\I #\U #\O #\X #\B #\F #\E #\G #\K
;							)
;				)	)
				(loop (+ idx 1))
	)	)	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->		296 ms 337 µs for 10K runs. 000 ms 029 µs by run test 1 case long list down/up
;		4 s 131 ms 190 µs for 10K runs. 000 ms 413 µs by run test 2 if char-downcase
;		7 s 952 ms 733 µs for 10K runs. 000 ms 795 µs by run test 3 if long list down/up

;_______________________________	Section 2 begin by Aubrey Jaffer 2010 SLIB 3b6-1
;;;;"scanf.scm" implemenation of formatted input
;Copyright (C) 1996, 1997, 2003, 2010 Aubrey Jaffer
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

;;; Originally jjb@isye.gatech.edu (John Bartholdi) wrote some public
;;; domain code for a subset of scanf, but it was too difficult to
;;; extend to POSIX pattern compliance.
;;; Jan 96, I rewrote the scanf functions starting from the POSIX man pages.

;(require 'string-port)
;(require 'multiarg-apply)
;(require 'rev2-procedures)
;(require 'rev4-optional-procedures)
#|
(stdio:scan-and-set "%11c" "0123456789λ" #f)	(stdio:scan-and-set " %d" " -314" #f)
 |#;->	("0123456789λ")							(-314)
#|
(stdio:scan-and-set " %[sRGBSrgb]:%d/%d/%d" "RGB:1/2/3" #f)
 |#;->	("RGB" 1 2 3)
(define (stdio:scan-and-set format-string input-port . args)
	(define setters (if (equal? '(#f) args) #f args))
	(define assigned-count 0)(define chars-scanned 0)(define items '())
	(define (return)
		(cond	((and (zero? chars-scanned) (eof-object? (peek-char input-port)))
					(peek-char input-port)
				)
			(setters assigned-count) ; number of assigned setter
			(else (reverse items))
	)	)
	(define (flush-whitespace-input)
		(set! chars-scanned (+ (stdio:flush-whitespace input-port) chars-scanned))
	)
	(define (read-input-char)
		(set! chars-scanned (+ 1 chars-scanned))
		(read-char input-port)
	)
	(define (read-string width separator?)
		(if	width
			(let*	((str (make-string width)))
				(do	((i 0 (+ 1 i)))
					((>= i width) str)
					(let*	((c (peek-char input-port)))
						(cond	((eof-object? c)	(set! str (substring str 0 i))
													(set! i width)
								)
							((separator? c)
								(set! str (if (zero? i) "" (substring str 0 i)))
								(set! i width)
							)
							(else (string-set! str i (read-input-char)))
			)	)	)	)
			(let loop	((c (peek-char input-port)) (strRes ""))
				(if (or (eof-object? c) (separator? c)) strRes
					(begin	(read-input-char)
							(loop (peek-char input-port) (string-append strRes (string c)))
	)	)	)	)	)
	(define (read-word width separator?)
		(let ((l (read-string width separator?)))
			(if (zero? (string-length l)) #f l)
	)	)
(define (scanFmtStr format-port)
	(define (add-item report-field? next-item)
		(cond	(setters
					(cond	((and report-field? (null? setters))
								(error	"0030. scanf: not enough variables for format"
										format-string))
						((not next-item) (return))
						((not report-field?) (scanFmtStr format-port))
						(else
							(let	((suc ((car setters) next-item)))
								(cond	((not (boolean? suc))
											(error "0031. scanf: setter returned non-boolean"
												suc) ; Was warning
								)		)
								(set! setters (cdr setters))
								(cond	((not suc) (return))
									((eqv? -1 report-field?) (scanFmtStr format-port))
									(else
										(set! assigned-count (+ 1 assigned-count))
										(scanFmtStr format-port)
				)	)	)	)	)	)
			((not next-item) (return))
			(report-field? (set! items (cons next-item items)) (scanFmtStr format-port))
			(else (scanFmtStr format-port))
	)	)
	(define fc (read-char format-port))
	(define (scanPercent)
		(set! fc (read-char format-port))
		(let	(	(report-field? (not (char=? #\* fc)))	(width #f))
			(define (width--) (if width (set! width (- width 1))))
			(define (read-u) (string->number (read-string width char-non-numeric?)))
			(define (read-o)
				(string->number
					(read-string width
						(lambda (c)
							(not (memv c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)))
					)	) 8
			)	)
			(define (read-x)
				(string->number
					(read-string width
						(lambda (c) (not (memv (char-downcase c)
						   '(	#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 
								#\a #\b #\c #\d #\e #\f
					)	)	)		)	 ) 16
			)	)
			(define (read-radixed-unsigned)
				(let	((c (peek-char input-port)))
					(case	c
						((#\0)
							(read-input-char)
							(width--)
							(set! c (peek-char input-port))
							(case c ((#\x #\X)	(read-input-char) (width--) 
												(read-x))
									(else (read-o))
						)	)
						(else (read-u))
			)	)	)
			(define (read-ui)
				(let*	((dot? #f)
							(mantissa
								(read-word width
									(lambda	(c)
										(not(or	(char-numeric? c)
												(cond	(dot? #f)
														((eqv? #\. c) 
															(set! dot? #t) #t
														)
														(else #f)
							)	)	)	)	)	)
							(exponent
								(cond	((not mantissa) #f)
									((and	(or (not width) (> width 1))
											(memv (peek-char input-port) '(#\E #\e))
									 )
										(read-input-char) (width--)
										(let*	
											(	(expsign
													(case	(peek-char input-port)
														((#\-)	(read-input-char)
																(width--) "-")
														((#\+)	(read-input-char)
																(width--) "+")
														(else "")
												)	)
												(expint
													(and(or (not width)
															(positive? width)
														)
														(read-word width 
															char-non-numeric?
											)	)	)	)
											(and expint (string-append "e" expsign
															expint
									)	)	)			)
									(else #f)
						)	)	)
					(and	mantissa
							(string->number (string-append "#i" (or mantissa "") 
												(or exponent "")
			)	)	)		)				)
			(define (minus?) ; AlSchemist: not continuation with regard to read-signed
				(case (peek-char input-port)
					((#\-)	(read-input-char) (width--) #t)
					((#\+)	(read-input-char) (width--) #f)
					(else #f)
			)	)
			(define (read-square-bracket allbut)
				(set! fc (read-char format-port))
				(case fc ((#\^) (set! allbut #t) (set! fc (read-char format-port))))
				(let scanloop	((scanset (list fc)))
					(set! fc (read-char format-port))
					(if (eof-object? fc) (error "0032. scanf. Unmatched [ in format"))
					(case	fc
						((#\-)
							(set! fc (peek-char format-port))
							(cond	((and (char<? (car scanset) fc)
										(not (eqv? #\] fc)))
										(set! fc (char->integer fc))
										(do	((i (char->integer (car scanset)) (+ 1 i)))
											((> i fc) (scanloop scanset))
											(set! scanset (cons (integer->char i) scanset))
									)	)
								(else (scanloop (cons #\- scanset)))))
						((#\])	(add-item report-field?
									(read-word width
										(if	allbut
											(lambda (c) (memv c scanset))
											(lambda (c) (not (memv c scanset)))
						)		)	)	)
						(else (scanloop (cons fc scanset)))
			)	)	)

			(cond ((not report-field?) (set! fc (read-char format-port))))
			(if (char-numeric? fc) 
				(begin 	(set! width 0)
					(let readWidth ()
						(set! width (+ (* 10 width) (- (char->integer fc) 48)))
						(set! fc (read-char format-port))
						(if (and (not (eof-object? fc)) (char-numeric? fc)) (readWidth))
			)	)	)
			(case fc
				((#\s #\S)	(add-item report-field? (read-word width char-whitespace?)))
				((#\d #\D)	(add-item report-field? (if (minus?) (-(read-u)) (read-u))))
				((#\n)	(if (not report-field?) (error "0033. scanf. Not saving %n??"))
						(add-item -1 chars-scanned) ;-1 is special flag.
				)
				((#\c #\C)	(if (not width) (set! width 1))
					(let*	((str (make-string width)))
						(let loop ((i 0) (ch (peek-char input-port)))
							(if (or (>= i width) (eof-object? ch))
								(add-item report-field? (substring str 0 i))
								(begin	(string-set! str i (read-input-char))
										(loop (+ 1 i)(peek-char input-port))
				)	)	)	)	)
				((#\[)		(read-square-bracket #f)) ; moved by AlSchemist for visibility
				((#\e #\E #\f #\F #\g #\G)
							(add-item report-field?	(if (minus?) (- (read-ui)) (read-ui)))
				)
				((#\u #\U)	(add-item report-field? (read-u)))
				((#\x #\X)	(add-item report-field? (read-x)))
				((#\i)		(add-item report-field? 
								(if (minus?) (- (read-radixed-unsigned))
												(read-radixed-unsigned)
				)			)	)
				((#\%)		(cond	((or width (not report-field?))
										(error "0034. scanf. %% has modifiers?")
									)
								((char=? #\% (read-input-char)) (scanFmtStr format-port))
								(else (return))
				)			)
				((#\o #\O)	(add-item report-field? (read-o)))
				((#\h #\l #\L)(read-char format-port) (scanFmtStr format-port)) ; skip
				(else (error "0035. scanf. Unknown format directive:" fc))
	)	)	)
	(if (eof-object? fc) (return)
		(case fc
			((#\%) (scanPercent))	; interpret next format
			((#\space #\tab)	(stdio:flush-whitespace format-port)
								(flush-whitespace-input)
								(scanFmtStr format-port)
			)
			(else	(if (char=? (peek-char input-port) fc) 
						(begin (read-input-char) (scanFmtStr format-port))
						(return)
)	)	)	)		)
	(if (zero? (string-length format-string)) (return)
		(if (string? input-port)
			(call-with-input-string input-port
				(lambda (str-port) (apply stdio:scan-and-set format-string str-port args))
			)
			(call-with-input-string format-string scanFmtStr)
)	)	)
#| Perf:
(let*	((nbrRun	100)(timeEnd 0)(timeStart (gettimeofday)))
	(let loop ((idx 0))
		(if (>= idx nbrRun) (set! timeEnd (gettimeofday))
			(begin	(stdio:scan-and-set " %[sRGBSrgb]:%d/%d/%d" "RGB:1/2/3" #f)
					(loop (+ idx 1))
	)	)	)		(time-stat timeStart timeEnd nbrRun)
)
 |#;->		578 ms 086 µs for 100 runs. 005 ms 780 µs by run stdio:flush-whitespace
;		2 s 155 ms 806 µs for 100 runs. 021 ms 558 µs by run minus? without continuation
;		2 s 202 ms 730 µs for 100 runs. 022 ms 027 µs by run do becomes let loop
;		5 s 905 ms 725 µs for 100 runs. 059 ms 057 µs by run read width if char-numeric
;		8 s 921 ms 626 µs for 100 runs. 089 ms 216 µs by run original

#|
(char-non-numeric? #\A)	(char-non-numeric? #\9)
 |#;->			   #t					   #f
(define (char-non-numeric? c) (if (char-numeric? c) #f #t))

;;;This implements a Scheme-oriented version of SCANF: 
#| Returns a list of objects read (rather than set!-ing values)
(scanf-read-list " %[sRGBSrgb]:%d/%d/%d" "RGB:1/2/3")
 |#;->	("RGB" 1 2 3)
(define (scanf-read-list format-string . optarg)
	(define input-port
		(cond	((null? optarg) (current-input-port))
			((not (null? (cdr optarg)))
				(error "0040. scanf-read-list. Wrong-number-of-args" optarg))
			(else (car optarg))
	)	)
	(cond	((input-port? input-port) (stdio:scan-and-set format-string input-port #f))
		((string? input-port)
			(call-with-input-string input-port
				(lambda (input-port) (stdio:scan-and-set format-string input-port #f))
		)	)
		(else (error "0041. scanf-read-list argument 2 not a port" input-port))
)	)
#| Perf:
(let*	(	(str "RGB:1/2/3")
			(nbrRun	100)(timeEnd 0)(timeStart (gettimeofday))
		)
	(let loop ((idx 0))
		(if (>= idx nbrRun) (set! timeEnd (gettimeofday))
			(let*	((lstRes (scanf-read-list "%[sRGBSrgb]:%d/%d/%d" str)))
				(if (= (length lstRes) 4)
					(let*	((coding	(car	lstRes))
							 (x			(cadr	lstRes))
							 (y			(caddr	lstRes))
							 (z			(cadddr lstRes))
							)
						;(show "coding: " coding " x: " x " y: " y " z: " z)
				)	)
				(loop (+ idx 1))
	)	)	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->		546 ms 875 µs for 100 runs. 005 ms 468 µs by run without show
;			750 ms 047 µs for 100 runs. 007 ms 500 µs by run with show
;		9 s 077 ms 664 µs for 100 runs. 090 ms 776 µs by run original with do
#|
(map %stdio:setter-procedure '(coding x y z))
 |#;->	((lambda (gensym-23) (set! coding gensym-23) #t) (lambda (gensym-24) (set! x gensym-24) #t) (lambda (gensym-25) (set! y gensym-25) #t) (lambda (gensym-26) (set! z gensym-26) #t))
(define (%stdio:setter-procedure sexp)
	(let	((v (gensym)))
		(cond	((symbol? sexp) `(lambda (,v) (set! ,sexp ,v) #t))
			((not (and (pair? sexp) (list? sexp)))
				(error "0050. scanf. Setter expression not understood" sexp))
			(else
				(case	(car sexp)
					((vector-ref) `(lambda (,v) (vector-set! ,@(cdr sexp) ,v) #t))
					((array-ref)
						`(lambda (,v) (array-set! ,(cadr sexp) ,v ,@(cddr sexp)) #t))
					((substring)
						`(lambda	(,v)
								(substring-move-left! ,v 0
									(min	(string-length ,v)
											(- ,(cadddr sexp) ,(caddr sexp)))
									,(cadr sexp)
									,(caddr sexp))
								#t))
					((list-ref) `(lambda (,v) (set-car! (list-tail ,@(cdr sexp)) ,v) #t))
					((car) `(lambda (,v) (set-car! ,@(cdr sexp) ,v) #t))
					((cdr) `(lambda (,v) (set-cdr! ,@(cdr sexp) ,v) #t))
					(else (error "0051. scanf. Setter not known" sexp))
)	)	)	)	)
(define-macro (scanf format-string . args)
   `(stdio:scan-and-set ,format-string (current-input-port)
		,@(map %stdio:setter-procedure args)
)	)
#|
(let*	(	(str "RGB:1/2/3")				; input variable
			(coding "")	(x 0) (y 0) (z 0)	; output variables
			(nbrToken (sscanf str " %[sRGBSrgb]:%d/%d/%d" coding x y z))
		)
	(show "nbrToken: " nbrToken " coding: " coding " x: " x " y: " y " z: " z)
)
 |#;->	nbrToken: 4 coding: RGB x: 1 y: 2 z: 3
(define-macro (sscanf str format-string . args)
   `(stdio:scan-and-set ,format-string ,str ,@(map %stdio:setter-procedure args))
)
#| Perf:
(let*	(	(str "RGB:1/2/3") (coding "")	(x 0) (y 0) (z 0)
			(nbrRun	100)(timeEnd 0)(timeStart (gettimeofday))
		)
	(let loop ((idx 0))
		(if (>= idx nbrRun) (set! timeEnd (gettimeofday))
			(begin	(sscanf str "%[sRGBSrgb]:%d/%d/%d" coding x y z)
					(loop (+ idx 1))
	)	)	)		(time-stat timeStart timeEnd nbrRun)
	(show "coding: " coding " x: " x " y: " y " z: " z)
)
 |#;->	 1 s 843 ms 748 µs for 100 runs. 018 ms 437 µs by run stdio:scan-and-set
;		11 s 093 ms 508 µs for 100 runs. 110 ms 935 µs by run original
;		coding: RGB x: 1 y: 2 z: 3

(define-macro (fscanf input-port format-string . args)
   `(stdio:scan-and-set ,format-string ,input-port
		,@(map %stdio:setter-procedure args)
)	)
;_______________________________	Tools for the section 2 by AlSchemist

#| The last parameter is the continuation of arity the number of tokens to be scanned
(scanf-closure "RGB:1/2/3" "%[sRGBSrgb]:%d/%d/%d" 
	(lambda(coding x y z)(show "coding: " coding " x: " x " y: " y " z: " z))
)
 |#;->	coding: RGB x: 1 y: 2 z: 3
(define (scanf-closure str format-string proc)
	(let*	(	(lstVal 
					(call-with-input-string str
						(lambda (port) (stdio:scan-and-set format-string port #f))
				)	)
				(nbrPrm	(arity proc)) ; require 1srfi-001-list.scm
			)
		(if (eq? nbrPrm #f) (error "1360: scanf-closure. proc is not a closure"))
		(if (= nbrPrm (length lstVal)) ; all parameters matche
			(eval (cons proc lstVal)(interaction-environment)) #f
)	)	)
#| Perf:
(let*	(	(str "RGB:1/2/3")
			(nbrRun	100)(timeEnd 0)(timeStart (gettimeofday))
		)
	(let loop ((idx 0))
		(if (>= idx nbrRun) (set! timeEnd (gettimeofday))
			(begin
				(scanf-closure str "%[sRGBSrgb]:%d/%d/%d" 
					(lambda(coding x y z)
						;(show "coding: " coding " x: " x " y: " y " z: " z)
				)	)
				(loop (+ idx 1))
	)	)	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->	558 ms 466 µs for 100 runs. 005 ms 584 µs by run without show
;		793 ms 800 µs for 100 runs. 007 ms 938 µs by run with show
#| Version multi-values with call-with-values does not check the arity of the consumer
(scanf-mv "RGB:1/2/3" "%[sRGBSrgb]:%d/%d/%d" 
	(lambda(coding x y z)(show "coding: " coding " x: " x " y: " y " z: " z))
)
 |#;->	coding: RGB x: 1 y: 2 z: 3
(define (scanf-mv str format-string consumer)
	(call-with-values 
		(lambda()	(cons 'mv-values
						(call-with-input-string str
							(lambda (port) (stdio:scan-and-set format-string port #f))
		)			)	)
		consumer
)	)
#| Perf:
(let*	(	(str "RGB:1/2/3")
			(nbrRun	100)(timeEnd 0)(timeStart (gettimeofday))
		)
	(let loop ((idx 0))
		(if (>= idx nbrRun) (set! timeEnd (gettimeofday))
			(begin
				(scanf-mv str "%[sRGBSrgb]:%d/%d/%d" 
					(lambda(coding x y z)
						(show "coding: " coding " x: " x " y: " y " z: " z)
				)	)
				(loop (+ idx 1))
	)	)	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->	546 ms 801 µs for 100 runs. 005 ms 468 µs by run
 ;		749 ms 489 µs for 100 runs. 007 ms 494 µs by run
;_______________________________	Tools for the section 2 by Aubrey Jaffer

; https://people.csail.mit.edu/jaffer/SLIB.html
;; "bigloo.init" Initialization for SLIB for Bigloo	-*-scheme-*-
;;; Author: Aubrey Jaffer
;;;
;;; This code is in the public domain.
(define (call-with-input-string str f)
	(let* ((insp (open-input-string str)) (res (f insp))) (close-input-port insp) res)
)
;_______________________________	Section 3 begin by Aubrey Jaffer SLIB 3b6-1
;; "bigloo.init" Initialization for SLIB for Bigloo	-*-scheme-*-
;;; Author: Aubrey Jaffer
;;;
;;; This code is in the public domain.

; Current path of file in the scope of with-load-pathname
(define *load-pathname* #f)
;@
; Returns the value returned by thunk.
; Evaluates thunk in the dynamic scope where *load-pathname* is bound to path;
; the internal variable is used for messages.
; path should be a string naming a file being read or loaded.
(define (with-load-pathname path thunk)
	(let	((swap	(lambda	(new)
						(let ((old *load-pathname*)) (set! *load-pathname* new) old)
			))		)
		(let* ((old (swap path)) (val (thunk))) (swap old) val)
)	)
;"manifest.scm" List SLIB module requires and exports.
;Copyright (C) 2003 Aubrey Jaffer
#|
(file->exports "C:/Users/Chess/AppData/Roaming/GIMP/2.10/scripts/0ts-stdio.scm")
 |#;->	(stdio:iprintf fprintf printf sprintf stdio:parse-float stdio:round-string stdio:scan-and-set scanf-read-list scanf sscanf fscanf with-load-pathname file->exports read-chars-aux read-chars read-line read-text-file file-ext file-path load-catch writeCrLf show dbgList file-write-by-line file-global)
(define (file->exports file . definers)
	(if (null? definers) (set! definers '(define macro define-macro)))
	(call-with-input-file file
		(lambda	(port)
			(define exports '())
			(define startLine? #t)
			(define (top)
				(define c (peek-char port))
				(cond	((eof-object? c))
					((char=? #\newline c) (read-line port)	(set! startLine? #t) (top))
					((char-whitespace? c) (read-char port)	(set! startLine? #f) (top))
					((char=? #\;	   c) (read-line port)	(set! startLine? #t) (top))
					(else				  (sxp (read port))	(set! startLine? #f) (top))
			)	)
			(define (sxp lst)  ; customized by AlSchemist to keep only closure & macro
				(if	(and(not (eof-object? lst)) startLine? (list? lst)
						(> (length lst) 2) (memq (car lst) definers) (pair? (cadr lst))
						(symbol? (caadr lst))
						(not(string-prefix? "%" (symbol->string (caadr lst))))
					)
					(set! exports (cons (caadr lst) exports))
			)	)
			(with-load-pathname file (lambda () (top) (reverse exports)))
)	)	)
;_______________________________	Section 4 begin by Richard O'Keefe
; https://legacy.cs.indiana.edu/scheme-repository/code.string.html
; Richard O'Keefe's generic read-string routine to port code to Scheme implementations 
; that do not support a read-string or read-line primitive (readstring.oak).
; Original file: "readstring.oak"   (c) Richard O'Keefe

#| Read a text file line by line
(read-text-file "C:/Tool/Gimp/forum/Linuxgraphic/dam/session01/action-history")
 |#
; Get the text file via portin until #\newline
; Return (proc->string listOfChars) otherwise #<EOF>. Filter #\return #\newline
(define (read-chars-aux portin proc)
	(let loop ((lstResult nil))
		(let* ((ch (read-char portin)))
			(if (eof-object? ch) 
				(if (pair? lstResult) (proc (reverse lstResult)) ch) ; EOF
				(case ch ; optimised by AlSchemist 2021
					((#\newline) (proc (reverse lstResult))) ; EOL
					((#\return)	(loop lstResult)) ; skip CR
					(else (loop (cons ch lstResult)))
)	)	)	)	)
;;; (read-chars  [input-port])
;;; is just like read-string, except that it returns its result as a
;;; list of character codes.  It exists for two reasons:
;;; (1) the portable implementation of read-string/read-line was
;;;		already building such a list internally;
;;; (2) the operation is useful in its own right.
(define (read-chars . portin)
    (read-chars-aux
		(if (pair? portin) (car portin) (current-input-port))
		(lambda (chars) chars)
)	)
;;; Many Scheme systems provide a function called (read-string [input-port]) or
;;; (read-line   [input-port])
;;; which reads a line of text (terminated by a #\Newline) from the given port
;;; (which defaults to the current input port, like all other Scheme input commands)
;;; and returns it as a string.
;;; If the end of the port is reached before a #\Newline has been read, 
;;; an end of file object is returned 
;;; (however many characters may have been read before the end of port was reached).
;;; This file was written to help me port programs between several dialects of Scheme, 
;;; and uses no operations or constants that are not defined in the standard.
(define (read-line . portin)
    (read-chars-aux	(if (pair? portin) (car portin) (current-input-port)) list->string)
)
(define read-string read-line)
;;; If you already have read-string, you can: (define read-line read-string)
;;; If you already have read-line,   you can: (define read-string read-line)
;;; If you already have read-line or read-string, you can
;(define (read-chars . portin)
;	(let ((str (read-string (if (pair? portin) (car portin) (current-input-port)))))
;		(if (eof-object? str) str (string->list str))
;)	)

;_______________________________	Section 5 begin by AlSchemist 2021

; The following is free TinyScheme open source copyrighted AlSchemist
(define (inc nbr) (+ nbr 1)) ; equivalent to succ in script-fu.init
(define (dec nbr) (- nbr 1)) ; equivalent to pred

#| Skip spaces. Return the number of spaces
(call-with-input-string "   Margin" stdio:flush-whitespace)
 |#;->	3	; 			 0123	
(define (stdio:flush-whitespace port) ; optimized version by AlSchemist
	(let loop ((ch (peek-char port)) (idx 0))
		(if (eof-object? ch) idx
			(if (char-whitespace? ch)
				(begin	(read-char port) (loop (peek-char port) (+ 1 idx))) idx
)	)	)	)
#| Perf:
(let*	(	(str "  Left Right   ")
			(nbrRun	10000)(timeEnd 0)(timeStart (gettimeofday))
		)
	(let loop ((idx 0))
		(if (>= idx nbrRun) (set! timeEnd (gettimeofday))
			(begin	
				(substring/shared str (call-with-input-string str stdio:flush-whitespace))
;				(string-trim str)
				(loop (+ idx 1))
	)	)	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->	4 s 109 ms 412 µs for 10K runs. 000 ms 410 µs by run flush-whitespace
;		8 s 765 ms 630 µs for 10K runs. 000 ms 876 µs by run string-trim

#| Read a text file line by line
(read-text-file "C:/Tool/Gimp/forum/Linuxgraphic/dam/session01/action-history")

("# GIMP action-history" "" "(history-item \"file-open-recent-01\" 0)" "(history-item \"filters-hue-saturation\" 0)" "(history-item \"filters-invert-perceptual\" 0)" "(history-item \"filters-levels\" 0)" "(history-item \"filters-brightness-contrast\" 0)" "(history-item \"filters-desaturate\" 1)" "(history-item \"plug-in-script-fu-console\" 1)" "" "# end of action-history")
 |#
(define (read-text-file filin)
	(let*	( 	(portin (open-input-file filin)))
		(if (eq? portin #f) (error "0060. read-text-file cannot open " filin))
		(let loop ((lst nil))
			(let ((str (read-line portin)))
				(if (eof-object? str) 
					(begin	(close-input-port portin) (reverse lst))
					(loop (cons str lst))
)	)	)	)	)
#| Perf:
(let*	(	(nbrRun	100)(timeEnd 0)(timeStart (gettimeofday)))
	(let loop ((idx 0))
		(if (>= idx nbrRun) (set! timeEnd (gettimeofday))
			(begin	
				(read-text-file
					"C:/Tool/Gimp/forum/Linuxgraphic/dam/session01/action-history"
				)
				(loop (+ idx 1))
	)	)	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->	3 s 343 ms 288 µs for 100 runs. 033 ms 432 µs by run
 
#| with basic read-char
(define (read-text-file filin)
	(let*	( 	(portin (open-input-file filin)))
		(if (eq? portin #f) (error "0061. read-text-file cannot open " filin))
		(let loop ((lst nil))
			(let ((ch (read-char portin)))
				(if (eof-object? ch) 
					(begin	(close-input-port portin)
							(list->string (reverse lst))
					)
					(loop (cons ch lst))
)	)	)	)	)
 |#
;-
#| Extract the file extension from a filename
(file-ext "file.scm")(file-ext "file.")(file-ext "file")
 |#;->	        "scm"                ""                ""
; Get the extension of file
(define (file-ext filename)
	(let*	( 	(len	(string-length filename))
				(idxDot	(string-index-right-okmij filename #\.))
				(strExt	"")
			)
		(if (not (eq? idxDot #f))
			(begin	(set! idxDot (+ idxDot 1))
					(if (< idxDot len)
						(set! strExt (substring filename idxDot))
		)	)		)
		strExt
)	)
#| Extract the path of the filename
(file-path "C:/Tool/Gimp/forum/Linuxgraphic/dam/session01/filters/GimpLevelsConfig.csv")
 |#;->	   "C:/Tool/Gimp/forum/Linuxgraphic/dam/session01/filters/"
(define (file-path filename)
	(let*	( 	(idxSlash	(string-index-right-okmij filename #\/))
				(strPath	"")
			)
		(if (not (eq? idxSlash #f))
			(set! strPath (substring filename 0 (+ idxSlash 1)))
		)
		strPath
)	)	
#| Extract the base of the filename
(file-base "C:/Tool/Gimp/forum/Linuxgraphic/dam/session01/filters/GimpLevelsConfig.csv")
 |#;->															 "GimpLevelsConfig"
#| Extract the base of the filename
(file-base "C:/Tool/")	(file-base "C:/Tool")
 |#;->				""				  "Tool"
(define (file-base filename)
	(let*	( 	(len		(string-length filename))
				(idxDot		(string-index-right-okmij filename #\.))
				(idxSlash	(string-index-right-okmij filename #\/))
			)
		(if (eq? idxSlash #f)
			(set! idxSlash 0) ; from the beginning
			(set! idxSlash (+ idxSlash 1)) ; after the slash
		)
		(if (>= idxSlash len) ""
			(if (eq? idxDot #f)
				(substring filename idxSlash)
				(substring filename idxSlash idxDot)
)	)	)	)
(define (file-must-exist filename msgErr)
	(if (not (file-exists? filename)) (error msgErr filename))
)
#| Load script in TinyScheme syntax and catch TinyScheme error
(load-catch #t "C:/Program Files/GIMP 2/share/gimp/2.0/scripts/script-fu-util.scm")
 |#;->	script-fu-util-image-resize-from-layerscript-fu-util-image-add-layerswith-files#<EOF>#t
; On exception, flip/flop flag isCatch to #f to better locate the error
(define (load-catch catch? filin)
	(if catch?
		(catch	(begin ; catching allows closing filOut on exception
					(show "\nload-catch exception loading:\n" filin)
					#f ; returned value on exception
				) 
				(load filin)
		)
		(load filin) ; better error msg in debug: filOut is not closed
)	)
;__________
#| Display CR+LF on the standard output that is to say on the Script-Fu console
(writeCrLf (current-output-port))
 |#;->
; #t 

; Write Windows EOL CR+LF in the file open as portOut 
(define (writeCrLf portOut)
	(write-char #\return portOut) ; Windows "\r" carriage-return
	(newline portOut)             ; Unix    "\n" line feed	
)
#| Perf: write 1000 CR+LF in the Script-Fu console
(duration '(writeCrLf (current-output-port)) 1000)
 |#;->	15 ms 714 µs for 1K runs. 15 µs by run
 
 #| Copy the following line and paste it in Script-Fu console
(show "Hello World!")
 |#;->	Hello World!
;   ^ the arrow means the result of the above call is displayed in the console:
#| function "show" with two strings as parameters:
(show "Tiny" "Scheme")
 |#;->	TinyScheme
#| With variable, strings including carriage return and expressions
(let* ((counter 1))(show "counter: " counter " condition: " (= counter 1) "\naddition: " (+ counter 3)))
;->
counter: 1 condition: #t
addition: 4
 |#
; Display variable number of strings or Lisp expressions then a carriage return
(define (show firstPrm . lstOtherPrm)
;                      ^ dot separating pair between (firstPrm prm2 prm3 ... otherPrms)
	(if (pair? lstOtherPrm)	;                                 (<--- lstOtherPrm ----->)
		(let*	((portOut (car (last lstOtherPrm))))
            (if (output-port? portOut)
				(begin
					(display firstPrm portOut) ; display in the file open as portOut
					(let loop ((lstPrm	(butlast lstOtherPrm)))
						(if (pair? lstPrm)
							(begin (display (car lstPrm) portOut)(loop (cdr lstPrm)))
					)    )
					(writeCrLf portOut)
                )
				(begin
					(%display firstPrm) ; display in stdout
					(let loop ((lstPrm lstOtherPrm))
						(if (pair? lstPrm)
							(begin (%display (car lstPrm)) (loop (cdr lstPrm)))
					)	)
					(newline)
		)	)	)
        (begin (%display firstPrm) (newline))
    )
	(string->symbol "") ; return nothing
)
#| Perf:
(duration '(let* ((counter 1))(show "counter: " counter " condition: " (= counter 1) "\naddition: " (+ counter 3))) 10000)
 |#;->	1 s for 10K runs. 168 µs by run
#|
(show char-set:whitespace)
 |#;->	7:(#\x9 .. #\xd #\x20 #\xa0)
(define (%display obj)
	(cond	((and (defined? 'char-set?) (char-set? obj)) (char-set-display obj))
			((and (defined? 'mv?) (mv? obj)) (mv-show obj))
			((and (defined? 'strict-array?) (strict-array? obj)) (array-display obj))
			((and (defined? 'record?) (record? obj)) (record-display obj))
			((and (defined? 'record-instance?) (record-instance? obj))
				(record-instance-display obj)
			)
			(else (display obj))
)	)
;__________

#| The macro version is less optimized but " counter: " is automatically generated
(let* ((counter 1))(dbgList counter " condition: " (= counter 1) "\naddition: " (+ counter 3)))
;-> same output.            ^ value of variable displayed after its generated name
counter: 1 condition: #t
addition: 4
#t
 |#

; Macro implementation allows the automatic generation of variables
(macro (dbgList fullCall)
	(let*	(	(objPrm	'())	; string or lisp expression
				(strPrm	"")		; string corresponding to the symbol of a variable
				(prog   		; generated program
					(cons 'begin ; start of the program
						(let loop ((lstArg (cdr fullCall))) ; cut the macro name in the head
							(if (pair? lstArg)
								(begin
									(set! objPrm (car lstArg))
									(if (symbol? objPrm)
										(begin
											(set! strPrm (string-append " " (symbol->string objPrm) ": "))
											(cons	(list 'display strPrm)
													(cons	(list 'display objPrm)
															(loop (cdr lstArg))
										)	)		)
										(cons (list 'display objPrm) (loop (cdr lstArg)))
								)	)
								'((newline)) ; end of the program
			)	)	)	)	) ;(displayln "\ndbgList body: ") (displayln prog) (displayln "Result ->")
 		prog
)	)
#| Perf:
(duration '(let* ((counter 1))(dbgList counter " condition: " (= counter 1) "\naddition: " (+ counter 3))) 10000)
 |#;->	796 ms 408 µs for 10K runs. 79 µs by run vs. 46 µs  for "show"

#| Debug with "(displayln...)" uncommented:
(let* ((counter 1))(dbgList counter "condition: " (= counter 1) "\naddition: " (+ counter 3)))
;-> 
dbgList body:
(begin (display counter: ) (display counter) (display  condition: ) (display (= counter 1)) (display addition: ) (display (+ counter 3)) (newline))
Result ->
counter: 1 condition: #t
addition: 4
#t
 |#
;- 
 ; Write listObj with one object by line in the text file to be created in path
(define (file-write-by-line path listObj)
	(call-with-output-file path
		(lambda (portOut)
			(map 	(lambda (item)
						(let*	((item item))
							(write item portOut)
							(writeCrLf portOut)
						)
					)
					listObj
	)	)	)
	(file-exists? path) ; return #t if the generated file exists otherwise #f
)
#| Perf: write 50 times number 0 to 9 in file "frameDuration.txt"
(let*	(	(filin "C:/Users/Chess/Pictures/webp/ZeMarmot/frameDuration.txt")
			(nbrRun 100)(timeEnd 0)(timeStart (gettimeofday))
		)
	(let loop ((idx 0))
		(if (>= idx nbrRun) (time-stat timeStart (gettimeofday) nbrRun)
			(begin	
				(file-write-by-line filin (iota 10))
				(loop (+ idx 1))
)	)	)	)
 |#;->	593 ms 753 µs for 100 runs. 005 ms 937 µs by run file-write-by-line write
;		718 ms 269 µs for 100 runs. 007 ms 182 µs by run file-write-by-line show
;-
#|
(file-global "C:\\Program Files\\GIMP 2\\share\\gimp\\2.0\\scripts\\*.init")
 |#;->	retourne la liste des fichiers .scm avec le chemin complet et backslashs doublés
#|
("C:\\Program Files\\GIMP 2\\share\\gimp\\2.0\\scripts\\plug-in-compat.init"
 "C:\\Program Files\\GIMP 2\\share\\gimp\\2.0\\scripts\\script-fu-compat.init"
 "C:\\Program Files\\GIMP 2\\share\\gimp\\2.0\\scripts\\script-fu.init")
 |#
#|
(file-global "C:/Tool/Gimp/forum/Linuxgraphic/dam/session01/filters/*.scm")
 |#;->	retourne la liste des fichiers .scm avec le chemin complet au format Unix
#| ("C:/Tool/Gimp/forum/Linuxgraphic/dam/session01/filters/GimpBrightnessContrastConfig.scm"
  "C:/Tool/Gimp/forum/Linuxgraphic/dam/session01/filters/GimpHueSaturationConfig.scm"
  "C:/Tool/Gimp/forum/Linuxgraphic/dam/session01/filters/GimpLevelsConfig.scm")
 |#
(define (file-global strPath)
    (let*   (   (lstFile (file-glob strPath 1))
            )
        (if (zero? (car lstFile))
            (begin ; Linux to Windows path translation
                (set! lstFile (file-glob (string-alter strPath "/" "\\") 1))
                (if (zero? (car lstFile)) 
                    nil ; no files match the pattern
                    ; Windows to Linux path translation
                    (map (lambda (path) (string-alter path "\\" "/")) (cadr lstFile))
                )
            )
            (cadr lstFile) ; original filename list supplied by file-glob
)	)	)
#| Read the file filin line by line removing empty lines and Python comments
(file->strings "C:/Tool/Gimp/forum/Linuxgraphic/dam/session01/action-history")
 |#;->	(	"(history-item \"file-open-recent-01\" 0)"
;			"(history-item \"filters-hue-saturation\" 0)"
;			"(history-item \"filters-invert-perceptual\" 0)"
;			"(history-item \"filters-levels\" 0)"
;			"(history-item \"filters-brightness-contrast\" 0)"
;			"(history-item \"filters-desaturate\" 1)"
;			"(history-item \"plug-in-script-fu-console\" 1)"
;		)
; If filin does not exist, generate: Error: 1000. read-text-file cannot open 
(define (file->strings filin)
	(let loop ((lstLine (read-text-file filin)) (lstOut nil))
		(if (not-pair? lstLine) (reverse lstOut)
			(let*	(	(strLine (string-trim (car lstLine)))
						(lenLine (string-length strLine))
						(isCmt? (or (zero? lenLine)(char=? (string-ref strLine 0) #\#)))
					)
				(if isCmt? (loop (cdr lstLine) lstOut) ; skip empty line & Python comment
					(loop (cdr lstLine)(cons strLine lstOut))
)	)	)	)	)
#| Read the file filin. Return the list of objects removing Python comments otherwise #f
(file->list "C:/Tool/Gimp/forum/Linuxgraphic/dam/session01/action-history")
 |#;->	(	(history-item "file-open-recent-01" 0)
;			(history-item "filters-hue-saturation" 0)
;			(history-item "filters-invert-perceptual" 0)
;			(history-item "filters-levels" 0)
;			(history-item "filters-brightness-contrast" 0)
;			(history-item "filters-desaturate" 1)
;			(history-item "plug-in-script-fu-console" 1)
;		)
(define (file->list filin)
	(call-with-input-file filin
		(lambda (portin)
			(let lp ((lstOut nil) (ch (peek-char portin)))
				(if (eof-object? ch) (reverse lstOut)
					(case ch
						((#\; #\#) (read-line portin) (lp lstOut (peek-char portin)))
						((#\() (lp (cons (read portin) lstOut) (peek-char portin)))
						(else (read-char portin) (lp lstOut (peek-char portin)))
)	)	)	)	)	)
; Return the number of lines of the multiline comment
; The opening and the closing of the multiline comment should not be on the same line.
(define (%count-multiline-cmt initialNbrCmt reader)
	(let* ((nbrCmt (inc initialNbrCmt))) ; Count opening line of the multiline comment
		(let loop ((strCmt (reader)))
			(if (not(eof-object? strCmt))
				(let* ((strTrim (string-trim strCmt)))
					(set! nbrCmt (inc nbrCmt)) ; Count a line inside the multiline cmt
					(cond 	((string-prefix? "#|" strTrim) ; nested multiline cmt
								(set! nbrCmt (%count-multiline-cmt nbrCmt reader))
								(loop (reader))
							)
							((string-prefix? "|#" strTrim) nbrCmt); close multiline cmt
							(else (loop (reader)))
)	)	)	)	)	)
#| Return the number of lines, defines, comments, percentage of comment and blank of .scm
   Otherwise return #f if "YourUserName" is not updated with your own Windows user name
(file-stats "C:/Users/YourUserName/AppData/Roaming/GIMP/2.10/scripts/0ts-stdio.scm")
 |#;->	Line: 1779 define: 78 comment: 611 = 34.3% blank: 38
(define (file-stats fileScm) ; Unix full path of the .scm with slash separators
	(let*	(	(lstLine (read-text-file fileScm))
				(nbrCmt 0)(nbrBlank 0)(nbrLine (length lstLine))(nbrDef 0)
			)
		(let loop ((lstLine lstLine) (multiCmt 0))
			(if (pair? lstLine)
				(let*	((strLine (string-trim (car lstLine))))
					(if (> multiCmt 0)
						(begin
							(set! nbrCmt (inc nbrCmt)) ; Count a line inside the multiline cmt
							(cond 	((string-prefix? "#|" strLine)
										(loop (cdr lstLine) (+ multiCmt 1)) ; nested multiline cmt
									)
									((string-prefix? "|#" strLine)
										(loop (cdr lstLine) (- multiCmt 1)) ; close multiline cmt
									)
									(else (loop (cdr lstLine) multiCmt))
						)	)
						(cond
							((zero? (string-length strLine))
								(set! nbrBlank (inc nbrBlank))	(loop (cdr lstLine) 0)
							)
							((string-prefix? "(define (" strLine)
								(set! nbrDef (inc nbrDef))		(loop (cdr lstLine) 0)
							)
							((string-prefix? ";" strLine)
								(set! nbrCmt (inc nbrCmt))		(loop (cdr lstLine) 0)
							)
							((string-prefix? "#|" strLine)
								(set! nbrCmt (inc nbrCmt))		(loop (cdr lstLine) 1)
							)
							(else (loop (cdr lstLine) 0))
		)	)	)	)	)
		(show "Line: " nbrLine " define: " nbrDef " comment: " nbrCmt " = " 
			(substring (real->string (* 100 (/ nbrCmt nbrLine))) 0 4) "% blank: " nbrBlank
)	)	)
#| Perf:
(let*	((timeStart (gettimeofday)))
	(file-stats "C:/Users/YourUserName/AppData/Roaming/GIMP/2.10/scripts/0ts-stdio.scm")
	(time-stat timeStart (gettimeofday) 1)
)
 |#;-> 14 s 444 ms 428 µs for one run
;       6 s 317 ms 025 µs for one run 2nd call
#| Generate snippet number nbrDef from strCall. Return nbrDef otherwise nbrDef - 1 if "(%"
(%gen-snippet 1 "(file-snippet file)")
 |#;->	1,1,"file-snippet","(file-snippet file",")",0,0,0,
;		1								; ^first and last parameter
#| Optional parameter
(%gen-snippet 1 "(stdio:iprintf out format-string . args)")
 |#;->	1,1,"stdio:iprintf","(stdio:iprintf out"," format-string args)",0,0,0,
;		1								;  ^first  ^second and   ^last parameters
#| No parameter
(%gen-snippet 1 "(void)")
 |#;->	1,1,"void","(void",")",0,0,0,
;		1
#| 
(%gen-snippet 24 "(%gen-snippet nbrDef strCall)")
 |#;->	23   ; ^decremented. Does not show anyting. %function is not a snippet
(define (%gen-snippet nbrDef strCall)
	(let*	(	(strLine	(string-alter strCall " . " " "))
				(len		(string-length strLine))
				(idxParEnd	(string-index-okmij strLine #\)))
				(idxEnd		(if idxParEnd (+ idxParEnd 1) len))
				(strCall	(substring strLine 1 idxEnd))
				(lstWord 	(string-split strCall '(#\space #\))))
				(lstToken	(filter (lambda (str)(positive?(string-length str))) lstWord))
				(strFun		(car lstToken))
			)
		(if (string-prefix? "%" strFun) ; internal define not exported
			(dec nbrDef)
			(let*	(	(callLeft (string-append "(" (string-join (take lstToken 2))))
						(restPrms (drop lstToken 2))
						(restArgs 	(if (null? restPrms) ")"
										(string-append " " (string-join restPrms) ")")
					)	)			)
				(show nbrDef ",1,"
					(dblQuote (list strFun callLeft restArgs) ",")
					",0,0,0,"
				)
				nbrDef
)	)	)	)
#| Parse the given .scm. Return a snippet by line. #f if "YourUserName" not updated
(file-snippet "C:/Users/YourUserName/AppData/Roaming/GIMP/2.10/scripts/0ts-stdio.scm")
 |#;->	1,1,"stdio:iprintf","(stdio:iprintf out"," format-string args)",0,0,0,
;		2,1,"fprintf","(fprintf port"," format args)",0,0,0,
;		...
;		34,1,"file-snippet","(file-snippet file",")",0,0,0,
;
(define (file-snippet filin)
	(call-with-input-file filin
		(lambda	(port)
			(define nbrDef 0)
			(define (top)
				(define strLine (read-line port))
				(if (not(eof-object? strLine))
					(let* ((strTrim (string-trim strLine)))
						(cond 
							((string-prefix? "(define (" strLine)
								(set! nbrDef
									(%gen-snippet (inc nbrDef)(substring/shared strLine 8))
							)	)
							((string-prefix? "#|" strTrim)
								(%count-multiline-cmt 0 (lambda()(read-line port)))
						)	)
						(top)
			)	)	)
			(top)(void)
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
(load "C:/Users/YourUserName/AppData/Roaming/GIMP/2.10/scripts/0ts-stdio.scm")
 |# (closure? sprintf)