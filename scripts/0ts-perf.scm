; 0Perf.scm implements duration for performance analysis

; Section 1: a subset of SRFI-13 functions: String utilities by Oleg Kiselyov 2004
; $Id: srfi-13-local.scm,v 1.2 2004/07/08 20:24:53 oleg Exp oleg $
; $Id: util.scm,v 2.6 2004/07/08 19:51:57 oleg Exp oleg $
; string-index-okmij

; Section 2: time utility
; 2021/09/27: GimpλLib 1.0 by AlSchemist for Gimp 2.10.28 TinyScheme Script-Fu
; Line: 219 define: 7 comment: 123 = 56.1% blank: 9

;_______________________________	Section 1 begin by Oleg Kiselyov 2004
; http://okmij.org/ftp/README.html by Oleg Kiselyov. mij: made in Japan
; The following is free open source copyrighted Oleg Kiselyov
; All multiline comments and pretty prints are adapted by AlSchemist

; http://okmij.org/ftp/README.html
; A subset of SRFI-13 functions
; $Id: srfi-13-local.scm,v 1.2 2004/07/08 20:24:53 oleg Exp oleg $
; string-index-okmij

#|
(string-index-okmij "zero-based index" #\z)
 |#;->	0 ;           ^at index 0 is char z
#|
(string-index-okmij "0-based index" #\x)(string-index-okmij "0-based index" #\Z)
 |#;->	12           ;0123456789012                                          #f
 
; Return the index of the first occurence of a-char in str, or #f
(define (string-index-okmij str a-char) ; subset of corresponding SRFI-13 fun.
	(let*	((len (string-length str))) ; The latter is more generic.
		(let loop ((pos 0))
			(cond
				((>= pos len) #f) ; whole string has been searched, in vain
				((char=? a-char (string-ref str pos)) pos)
				(else (loop (+ pos 1)))
)	)	)	)
; Perf: see comparison with string-index in 2srfi-013-string.scm

;_______________________________	Section 2 begin by AlSchemist 2021

; The following TinyScheme code is free open source copyrighted AlSchemist 2021
; Expand -exponent in "e-05" by its number of zero after the dot of a positive real
#|              12345.                                 12345.
(real-expo->string "7.125058174e-05")(real-expo->string "712.5058174e-05")
 |#;->	      "0.00007125058174"                     "0.007125058174"
#|               12345.
(real-expo->string "31.415e-05")
 |#;->	       "0.00031415"
#| froggy decimal comma
(real-expo->string "31,415e-05")
 |#;->	Error: 1380. real-expo->string: no dot in:  "31,415e-05"
(define (real-expo->string strReal) ; doesn't support parsing negative real
	(let* 	(	(len (string-length strReal)))
		(if (and (> len 5) (substring-equal? "e-" strReal (- len 4) (- len 2))) ; expo?
			(let* 	(	(nbrExp (string->number (substring strReal (- len 2))))
						(idxDot (string-index strReal #\.))
					)
				(if (eq? idxDot #f) (error "1380. real-expo->string: no dot " strReal))
				(let* ((strZero (string-append
									(make-string nbrExp #\0)
									(substring strReal 0 idxDot)
									(substring strReal (+ idxDot 1) (- len 4))
					  ))		)
					(string-append "0." (substring strZero idxDot))
			)	)
			strReal
)	)	)
;-

#|
(time->string 3.141592654)(time->string 3.0140015)(time->string 63.0000015)
|#;->  "3 s 141 ms 592 µs "	"3 s 014 ms 001 µs "	"1 mn 3 s 000 ms 001 µs "
#|
(time->string 7.125058174e-05) (time->string 0)	(time->string 3.14)
 |#;->	"000 ms 071 µs "              		"0 "			 "3 s 140 ms "
; Convert time in second to string in mn s ms µs for "duration"
(define (time->string timeRealValue)
	(let* 	(	(strNbr	(real-expo->string (real->string timeRealValue)))
				(idxDot (string-index-okmij strNbr #\.)) 		
			)
		(if (eq? idxDot #f) (string-append strNbr " ")			 ; not dot
			(let*	(	(strInt (substring strNbr 0 idxDot))	 ; before dot
						(strDec (substring strNbr (+ idxDot 1))) ; after dot
						(strMs	(string-append (string-pad-right strDec 3 #\0) " ms "))
						(strRes
							(if (>= timeRealValue 60) ; before dot
								(let* ((iTime (trunc timeRealValue)))
									(string-append	
										(number->string (quotient  iTime 60)) " mn "
										(number->string (remainder iTime 60)) " s "
										strMs
								)	)
								(if (< timeRealValue 1) strMs
									(string-append strInt " s " strMs)
					)	)	)	)
				(if (< (string-length strDec) 4) strRes ; not microsecond
					(string-append strRes (substring (string-append strDec "000") 3 6) 
						" µs "
)	)	)	)	)	)
#| Perf:
(let*	(	(nbrRun	1000)(timeEnd 0)(timeStart (gettimeofday)))
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
		(time->string 3.141592654)
	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->	811 ms 974 µs for 1K runs. 000 ms 811 µs by run
;-
; Display real duration of a Lisp expr in mn s ms µs for perf
#|
(time-sec->real (gettimeofday)) (gettimeofday) (time-sec->real '(0 0))
 |#;->	1134,205848.0            (1134 205848)  0
; Convert listDayTime to a real in second
(define (time-sec->real listDayTime) ; <start of day in seconds> <in µs>)
	(+ (car listDayTime) (/ (cadr listDayTime) 1000000))
)
;-
#| Convert a float number to its string removing ".0" suffix and replacing "," with "."
(number->string *pi*)(real->string *pi*) (real->string 7.125058174e-05)(real->string 2)
|#;-> "3,141592654.0" "3.141592654"		 "7.125058174e-05"			   "2"
(define (real->string float)
	(let* 	(	(strNbr	(number->string float))
				(len	(string-length strNbr))
				(idxCom (string-index-okmij strNbr #\,))
				(idxDot (string-index-okmij strNbr #\.))
			)
		(if (not (eq? idxCom #f))
			(string-set! strNbr idxCom #\.) ; comma becomes dot
		)
		(if (and (not (eq? idxDot #f)) (not (eq? idxCom #f)) (= idxDot (- len 2))
				(char=? (string-ref strNbr (- len 1)) #\0)	; ".0"?
			)	; Cut suffix
			(substring strNbr 0 (- len 2)) strNbr
)	)	)
#|
(let*	(	(timeStart	(gettimeofday))
			(timeEnd	(begin (usleep 1000000)(gettimeofday)))
		)
	(time-stat  timeStart timeEnd 1)
)
 |#;->	1 s 010 ms 484 µs for one run
#|
(time-stat '(0 0) '(0 921876) 1)
 |#;->	921 ms 876 µs for one run
#|
(time-stat '(0 0) '(0 921876) 1000)
 |#;->	921 ms 876 µs for 1K runs. 000 ms 921 µs by run
(define (time-stat timeStart timeEnd nbrRun)
	;(show "timeStart: " timeStart " timeEnd: " timeEnd)
	(check-arg (lambda(n)(and (integer? n)(positive? n))) nbrRun 'time-stat)
	(let*	(	(timeTotal	(- (time-sec->real timeEnd) (time-sec->real timeStart)))
				(timeRun	(if (> nbrRun 1)(/ timeTotal nbrRun) timeTotal))
			)
		(if (= nbrRun 1) (show (time->string timeRun) "for one run")
			(let*	(	(strUnitNbr (if (< nbrRun 1000) " "
							(begin (set! nbrRun (/ nbrRun 1000)) "K "))
					)	)
				(show (time->string timeTotal)
					"for " (real->string nbrRun) strUnitNbr "runs. " 
					(time->string timeRun) "by run"
)	)	)	)	)
#| One parameter: the quoted list to be measured
(duration '(usleep 3000000))
 |#;->	3 s 017 ms 065 µs for one run
#| Two parameters: the quoted list to be measured and the number of iterations
(duration '(usleep 1000000) 3)
 |#;->	3 s 041 ms 095 µs for 3 runs. 1 s 013 ms 698 µs by run
; Measure the duration of the cmd [run nbrRun times] by default one time
(define (duration cmd . maybe-nbrRun)
	(let*	(	(nbrRun		(if (pair? maybe-nbrRun) (car maybe-nbrRun) 1))
				(timeStart	(gettimeofday))		(timeEnd	())
			)
		(let loop ((idx 0))
			(if (>= idx nbrRun) (set! timeEnd (gettimeofday))
				(begin (eval cmd)	(loop (+ idx 1)))
		)	)
		(time-stat timeStart timeEnd nbrRun)
)	)
#| Perf:
(let*	((nbrRun 100)(timeEnd 0)(timeStart (gettimeofday)))
	(let loop ((idx 0))
		(if (>= idx nbrRun) (time-stat timeStart (gettimeofday) nbrRun)
			(begin	
				(usleep 100000) ; put here code to be measured
				(loop (+ idx 1))
)	)	)	)
 |#;->	10 s 807 ms 478 µs for 100 runs. 108 ms 074 µs by run
;
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
(load "C:/Users/YourUserName/AppData/Roaming/GIMP/2.10/scripts/0ts-perf.scm")
 |# (closure? duration)