; https://srfi.schemers.org/srfi-30/
; SRFI: Scheme Requests for Implementation https://srfi.schemers.org/
; Section 1: Martin Gasbichler	2002	SRFI 30 Nested Multi-line Comments
;			 AlSchemist			2021	TinyScheme integration for Gimp 2.10.28 Script-Fu
;										Add multiline comments, examples and pretty print
; Section 2: utilities by AlSchemist		Add void, check-arg
; 2021/09/27: GimpλLib 1.0 by AlSchemist for Gimp 2.10.28 TinyScheme Script-Fu
;			  Add maybe or optional init nested-level: (skip-comment! 'start 1)
; Line: 211 define: 9 comment: 110 = 52.1% blank: 9

; Revised^6 Report Scheme (R6RS) multiline comment based on TinyScheme *sharp-hook*
; - Goal:  copy and paste Lisp expression starting at col 1 without removing "; "
; - Alternative: If you do not want "*sharp-hook*",
;   prefix each multiline comment by "; " to be compatible with TinyScheme.
;   NotePad++ Shift+Alt+arrow down to select by column a block of lines, then enter "; "

;_______________________________	Section 1 begin by Martin Gasbichler 2002

(define (skip-comment! . maybe-start-state)
	(let lp (	(state (if (null? maybe-start-state) 'start (car maybe-start-state)))
				(nested-level	(if (= (length maybe-start-state) 2)
									(cadr maybe-start-state) 0
			)	)				)   ; ^ AlSchemist 2021
		(define (next-char)
			(let ((c (read-char)))
				(if (eof-object? c) (error "1000. EOF in multiline comment.") c)
		)	)
		(case state
			((start)	(case (next-char)
							((#\|) (lp 'read-bar nested-level))
							((#\#) (lp 'read-sharp nested-level))
							(else (lp 'start nested-level))
			)			)
			((read-bar) (case (next-char)
							((#\#) (if (> nested-level 1)
										(lp 'start (- nested-level 1))
							)		)
							(else (lp 'start nested-level))
			)			)
			((read-sharp) (case (next-char)
							((#\|) (lp 'start (+ nested-level 1)))
							(else (lp 'start nested-level))
)	)	)	)			)
; Copyright (C) Martin Gasbichler (2002). All Rights Reserved.
; 
; Permission is hereby granted, free of charge, to any person 
; obtaining a copy of this software and associated documentation files 
; (the "Software"), to deal in the Software without restriction,
; including without limitation the rights to use, copy, modify, merge,
; publish, distribute, sublicense, and/or sell copies of the Software, 
; and to permit persons to whom the Software is furnished to do so, 
; subject to the following conditions:
; 
; The above copyright notice and this permission notice shall be included 
; in all copies or substantial portions of the Software.
; 
; The software is provided "as is", without warranty of any kind, 
; express or implied, including but not limited to the warranties of
;  merchantability, fitness for a particular purpose and noninfringement.
; In no event shall the authors or copyright holders be liable 
; for any claim, damages or other liability, whether 
; in an action of contract, tort or otherwise, arising from, out of or 
; in connection with the software or the use or other dealings in the software.
;_______________________________	Section 1 end

; TinyScheme tools powered by AlSchemist 2021 for the section 1

; Return nothing visible in the REPL
(define (void) (string->symbol ""))

; Capture the body of a multiline comment between "#|" and "|#".
; #| basic multicomment |#
;-> #<EOF> is the returned value since there is nothing after the multiline comment.
; #| after |#*features*
;-> (srfi-0 tinyscheme) is the value of "*features*", "#| after |#" has been ignored.
; #| before #<EOF> after |#
; #| #<EOF>|#
; #|noSpaceLeft |# #| noSpaceRight|# #|noSpace|#
; #||#
; #|#<EOF> after |##|#<EOF>|#
; #| #= after |# #| '# after |# #| "#" after |# #| # msg |# #| #\# |#
; #| before #(1 2 3) after |#
; #| before ; after |#
; #| SFRI-30 allows #| nested |# multiline comment |#
(define (multiline-comment)
	(skip-comment! 'start 1)	; ...at nested-level 1
	(cons 'void nil) ; return nothing
)
; This hook is called when there is sharp "#" in the input port
(define (*sharp-hook*)
	(let*	(	(charPrev #\#) (charCur (read-char)))
;		(display "*sharp-hook* ")(display ": '")(display charCur)(displayln "'")
		(case charCur
			((#\|)		(multiline-comment))
			((#\U #\u)	(unicode-char))
			((	#\x)	(hexa-int))
)	)	)
; Write multiline comments after *sharp-hook*

;_______________________________	Section 2 begin by AlSchemist 2021

; The following is free TinyScheme open source copyrighted AlSchemist

#| Parse hexa-digits and return the integer
#x3bb				(number->string #x3bb 16)
 |#;->	955							 "3bb"
(define (hexa-int)
	(let lp	((strHexa "") (ch (peek-char)))
		(case ch
			(	(	#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 
					#\A #\B #\C #\D #\E #\F #\a #\b #\c #\d #\e #\f
				)
				(lp (string-append strHexa (string (read-char))) (peek-char))
			)
			(else	(if (> (string-length strHexa) 0)
						(string->number strUni 16)
						(error "1001. hexa-int: EOF in empty integer")
)	)	)	)		)
#| Parse unicode syntax of char #uhhhh with h a hexa-digit
#u3bb
 |#;->	λ
#| Parse Unicode syntax of char #U+HHHH with H a Hexa-digit
(string #U+03BB)	(number->string (char->integer #U+3BB) 16)
 |#;->	"λ"			"3bb"
(define (unicode-char)
	(let lp	((strUni "") (ch (peek-char)))
		(case ch
			(	(	#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 
					#\A #\B #\C #\D #\E #\F #\a #\b #\c #\d #\e #\f
				)
				(lp (string-append strUni (string (read-char))) (peek-char))
			)
			((#\+)	(read-char)
					(if (zero? (string-length strUni)) (lp strUni (peek-char))
						(error "1002. unicode-char: unexpected +")
			)		)
			(else	(if (> (string-length strUni) 0)
						(let* ((uchar (integer->char (string->number strUni 16))))
							(if (eof-object? ch)
								(begin (read-char) (cons 'show (list uchar)))
								uchar
							)
						)
						(error (string-append "1003. unicode-char: EOF: '" (string ch) "'")		
)	)	)	)		)	)
#| Convert unicode character to the string "#U+hhhh" with h a hexa-digit
(unicode-char->string (string-ref "λ" 0))	(unicode-char->string #\A)
 |#;->	"#U+03bb"								"#U+0041"
#|
(unicode-char->string #U+03bb)
 |#;->	"#U+03bb"
(define (unicode-char->string ch)
	(string-append "#U+" (string-pad (number->string (char->integer ch) 16) 4 #\0))
)
#| Check that (predicat? obj) returns #t otherwise error
(check-arg integer? 256 'testOk) 	(check-arg integer? 'symb 'testKo)
 |#;->	256							Error: 1010. test2: integer? symb failed.
#|
(check-arg (lambda(n)(and(integer? n)(>= n 0))) -12 'testλ)
 |#;->	Error: 1010. testλ: (lambda (n) (and (integer? n) (>= n 0))) -12 failed. 
(define (check-arg predicat? obj where)
	(if (predicat? obj) obj
		(let*	(	(msg	(string-append "1010. "
								(symbol->string where) ": "
				)	)		)
			(error
				(if (closure? predicat?) ; can't retrieve its name
					(let*	(	(lst	(get-closure-code predicat?))
								(proc 	(obj->string lst))
							)
						(string-append msg proc)
					)
					(let*	(	(proc 	(obj->string predicat?))
								(indSp	(string-index proc #\space))
							)
						(string-append msg (substring proc 2 indSp))
				)	)
				obj 'failed.
)	)	)	)
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
(load "C:/Users/YourUserName/AppData/Roaming/GIMP/2.10/scripts/0srfi-030-comment.scm")
;								   the first scm sorted by name^
 |# (closure? multiline-comment)
