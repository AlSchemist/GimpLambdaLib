; https://srfi.schemers.org/srfi-0/srfi-0.html
; SRFI: Scheme Requests for Implementation https://srfi.schemers.org/
;; SRFI-0 COND-EXPAND Copyright (C) Marc Feeley (1999). All Rights Reserved.
; Feature-based conditional expansion construct

; Implemented as a macro in script-fu.init supplied by Gimp 2.10.28
; 2021/09/27:	GimpλLib 1.0 by AlSchemist for Gimp 2.10.28 TinyScheme Script-Fu
;				Add multiline comments, examples and pretty print
;				Fix cond-expand-runtime adding "else"
; Line: 86 define: 1 comment: 54 = 62.7% blank: 1
;				Add list of TinyScheme features of the AlSchemist's GimpλLib:
(define *features*
   '(	tinyscheme
		srfi-0  	; cond-expand "else"
		srfi-1  	; list
		srfi-2		; and-let*
		srfi-6  	; open-input-string open-output-string get-output-string
		srfi-8		; receive call-with-values values
		srfi-9		; record
		srfi-13 	; string
        srfi-23 	; error
		srfi-30 	; nested multiline comment
		srfi-32 	; quick sort
		srfi-47 	; array
		srfi-60 	; integers-based bit access
		srfi-66 	; octet u8vector
		srfi-111	; box
)	)
#| C:\Program Files\GIMP 2\share\gimp\2.0\scripts\script-fu.init
(cond-expand
	(srfi-38	(displayln "srfi-38 is supported!"))
	(else		(displayln "srfi-38 is not supported!"))
)
 |#;->	srfi-38 is not supported!
#|
(cond-expand
	(else		(displayln "the feature is not supported!"))
	(srfi-38    (displayln "srfi-38 is supported!"))
)
 |#;->	Error: 9000. cond-expand else clause is not the final one. 
; Generate the body of the macro cond-expand
(define (cond-expand-runtime cond-action-list)
	(if (null? cond-action-list) #t
		(let*	( 	(clause		(car cond-action-list))
					(condition 	(car clause))
					(action 	(cdr clause))
				) ; end local variables
			(cond	((eq? condition 'else)
						(if (null? (cdr cond-action-list))
							`(begin ,@action)
							(error "0000. cond-expand else clause is not the final one.")
					)	)
					((cond-eval condition) `(begin ,@action))
					(else (cond-expand-runtime (cdr cond-action-list)))
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
(load "C:/Users/YourUserName/AppData/Roaming/GIMP/2.10/scripts/1srfi-000-cond-expand.scm")
 |# (closure? cond-expand-runtime)