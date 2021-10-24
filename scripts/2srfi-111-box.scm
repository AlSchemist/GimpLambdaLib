; https://srfi.schemers.org/srfi-111/srfi-111.html
; SRFI: Scheme Requests for Implementation https://srfi.schemers.org/
; Section 1: John Cowan		2013	SRFI-111 Boxes
; Section 2: AlSchemist		2021	TinyScheme integration for Gimp 2.10.28 Script-Fu
;									Add multiline comments, examples and pretty print
; 2021/09/27: GimpλLib 1.0 by AlSchemist for Gimp 2.10.28 TinyScheme Script-Fu
;                                	no override of primitive pair?
; Line: 79 define: 4 comment: 64 = 81.0% blank: 8

;_______________________________	Section 1 begin by John Cowan 2013

;; Unique object in the cdr of a pair flags it as a box.
(define %box-flag 'box-flag) ; was (string-copy "box flag"). Faster box? by AlSchemist

#| Constructor
(define boxing (box 42))
 |#;->	boxing
(define (box x) (cons x %box-flag))

#| Predicate
(box? boxing)
 |#;->	#t
(define (box? x) (and (pair? x) (eq? (cdr x) %box-flag)))

#| Accessor
(unbox boxing)
 |#;->	42
(define (unbox x) (if (box? x) (car x) (error "11110. unbox. Attempt to unbox non-box")))

#| Mutator
(set-box! boxing -42)
 |#;->	(-42 . box-flag)
(define (set-box! x y)
	(if (box? x) (set-car! x y) (error "11111. set-box! Attempt to mutate non-box"))
)
#|
Copyright (C) John Cowan 2013. All Rights Reserved.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 |#
;_______________________________	Section 2 begin by AlSchemist 2021
 
; The following is free TinyScheme open source copyrighted AlSchemist

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
(load "C:/Users/YourUserName/AppData/Roaming/GIMP/2.10/scripts/2srfi-111-box.scm")
 |# (closure? box?)