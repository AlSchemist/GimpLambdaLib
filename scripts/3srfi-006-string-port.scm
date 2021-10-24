; https://srfi.schemers.org/srfi-6/srfi-6.html
; SRFI: Scheme Requests for Implementation https://srfi.schemers.org/

; Section 1: William D Clinger	1999	SRFI-6 Basic String Ports
; Section 2: AlSchemist			2021	port open-input-string is a TinyScheme primitive
; 2021/09/27: GimpλLib 1.0 by AlSchemist for Gimp 2.10.28 TinyScheme Script-Fu
; Line: 81 define: 0 comment: 69 = 85.1% blank: 12

;_______________________________	Section 1 begin by William D Clinger 1999

#|
(define p (open-input-string "(a . (b . (c . ()))) 34"))
 |#;->	p

#|
(input-port? p)
 |#;->	#t

#| Character at the index. The index does not change
(peek-char p)	(peek-char p)
 |#;->	#\(		#\(

#| Read an entire TinyScheme list. The index is incremented after the object
(read p)
 |#;->	(a b c)

#| Character at the index. The index is incremented
(read-char p)	(read p)
 |#;->	#\space	34
 
#|
(eof-object? (peek-char p))
 |#;->	#t

#|
(close-input-port p)
 |#;->	#t

#|
Copyright (C) William D Clinger (1999). All Rights Reserved.
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
(load "C:/Users/YourUserName/AppData/Roaming/GIMP/2.10/scripts/3srfi-006-string-port.scm")
 |# (and (procedure? open-input-string)(procedure? open-output-string))