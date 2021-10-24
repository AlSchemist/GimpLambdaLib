; https://srfi.schemers.org/srfi-9/srfi-9.html
; SRFI: Scheme Requests for Implementation https://srfi.schemers.org/
; Copyright (C) Richard Kelsey (1999). All Rights Reserved.

; Section 1: Richard Kelsey 1999 SRFI-9 Defining Record Types
; Section 2: Aubrey Jaffer  1997 SLIB 3b6-1
; Section 3: AlSchemist      2021
; 2021/09/27: GimpλLib 1.0 by AlSchemist for Gimp 2.10.28 TinyScheme Script-Fu
; Line: 335 define: 18 comment: 180 = 53.7% blank: 16

;_______________________________	Section 1 begin by Richard Kelsey 1999

; Definition of DEFINE-RECORD-TYPE
; define-syntax not available: AlSchemist 2021

; Define the marker
(define record-marker '(record-marker))

; Record types are implemented using vector-like records.
; The first slot of each record contains the record's type, which is itself a record.
(define (record-type record) (vector-ref record 0))
  
; Definitions of the record procedures.
(define (record? obj)
	(and	(vector? obj) (< 0 (vector-length obj)) (eq? (vector-ref obj 0) record-marker))
)
; A utility for getting the offset of a field within a record.
(define (field-index type tag)
	(let loop	((i 1) (tags (record-type-field-tags type)))
		(cond	((null? tags) (error "record type has no such field" type tag))
			((eq? tag (car tags)) i)
			(else (loop (+ i 1) (cdr tags)))
)	)	)
#|
Copyright (C) Richard Kelsey (1999). All Rights Reserved.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 |#
;_______________________________	Section 2 begin by Aubrey Jaffer 1997
; https://people.csail.mit.edu/jaffer/SLIB.html
; "record.scm" record data types
; Written by David Carlton, carlton@husc.harvard.edu.
; Re-Written by Aubrey Jaffer, agj @ alum.mit.edu, 1996, 1997
; Optimized by AlSchemist 2021 for GIMP 2.10.28, 
; This code is in the public domain.

; Implements `record' data structures for Scheme.
; Using only vector and procedures, makes record datatypes and
; record-type-descriptors disjoint from R4RS types and each other

;;2001-07-24: 	Aubrey Jaffer  <agj@alum.mit.edu> SLIB 3b6-1
;;            	changed identifiers containing VECTOR to VECT or VCT.
;			 	A record is a new type different of vector: (vector? rtd) returned #f
; "prevents forgery and corruption (modification without using RECORD-MODIFIER) of records"
; "Need to wrap [vector primitives] to protect record data from being corrupted."

; 2021/07/24:	simplification by AlSchemist for TinyScheme. Perf: don't alter primitives.
; Despit SRFI-9 "Records are disjoint from the types listed in Section 4.2 of R5RS."
; a record is implemented like a vector of six fields. Don't hide magic-cookie.
; rtd-name becomes SRFI-9 record-type-name
; SRFI-9 redefines both vector? and eval, which is no longer negotiable.

;(require 'common-list-functions) ; slib-comlist.scm: has-duplicates? notevery position
;(require 'rev4-optional-procedures)

;; Internal accessor functions.  No error checking.
(define (record-type-name rtd) (vector-ref rtd 1))
; slot 2: future extension
(define (record-type-field-tags rtd) (vector-ref rtd 3))

;; rtd-vfields is padded out to the length of the vector, 1 more than the fields number
(define (rtd-vfields rtd) (cons #f (record-type-field-tags rtd)))

;; rtd-length is the length of the vector.
(define (rtd-length rtd) (vector-ref rtd 4))
(define (rec-disp-str x)
	(let	((name (record-type-name (record-type x))))
		(string-append "#<" (if (symbol? name) (symbol->string name) name) ">")
)	)
; RK: (make-record-type <type-name <field-names>) -> <record-type>
#|
(define :person (make-record-type ':person '(LastName FirstName)))
 |#;->	:person
#|
(field-index :person 'LastName)	(field-index :person 'FirstName)
 |#;->				  1								  2
#|
(record-type-field-tags :person)	(record-type-name :person)
 |#;->	(LastName FirstName)		:person
(define (make-record-type name field-tags)
	(check-arg	(lambda(name)(or (symbol? name) (string? name))) name 'make-record-type)
	(check-arg	(lambda(lst)(and (list? lst) (not (comlist:has-duplicates? lst))
								(comlist:every symbol? lst)
				)			)	field-tags 'make-record-type
	)
	(let*	(	(lenAug (+ 1 (length field-tags)))
				(rtd	(vector record-marker name '() field-tags lenAug #f #f)
			)	)
		(vector-set! rtd 5
			(lambda	(x)	(and	(vector? x) (= (vector-length x) lenAug)
								(eq? (record-type x) rtd)
		)	)			)
		(vector-set! rtd 6
			(lambda	(x)	(and	(vector? x) (>= (vector-length x) lenAug)
								(eq? (record-type x) rtd)
								#t
		)	)			)
		rtd
)	)
; RK: (record-constructor <record-type<field-names>) -> (<constructor> <initial-value> ...)
#|
(define new:person (record-constructor :person '(LastName FirstName)))
 |#;->	new:person
#| Create the first instance of type new:person
(define Ondine (new:person 'C. 'Laetitia))
 |#;->	Ondine
#| Retrieve the name of the type from a given instance
(record-type-name (record-type Ondine))
 |#;->	:person
(define (record-constructor rtd . field-names) ; SRFI-9 tags are not optional
	(check-arg record? rtd 'record-constructor)
	(if	(or (null? field-names) (equal? field-names (record-type-field-tags rtd)))
		(let	((rec-length (- (rtd-length rtd) 1)))
			(lambda	elts
				(if	(= (length elts) rec-length) #t
					(error 'record-constructor (record-type-name rtd) "wrong number of arguments.")
				)
				(apply vector rtd elts)
		)	)
		(let	(	(rec-vfields (rtd-vfields rtd))
					(corrected-rec-length (rtd-length rtd))
					(field-names (car field-names))
				)
			(if	(or	(and (list? field-names) (comlist:has-duplicates? field-names))
					(comlist:notevery (lambda (x) (memq x rec-vfields)) field-names))
				(error	'record-constructor "invalid field-names argument."
						(cdr rec-vfields)
			)	)
			(let	(	(field-length (length field-names))
						(offsets(map (lambda (fld) (comlist:position fld rec-vfields))
												field-names
					)	)		)
				(lambda	elts
					(if	(= (length elts) field-length) #t
						(error	'record-constructor (record-type-name rtd)
								"wrong number of arguments."))
					(let	((result (make-vector corrected-rec-length)))
						(vector-set! result 0 rtd)
						(for-each	(lambda (offset elt) (vector-set! result offset elt))
									offsets elts
						)
						result
)	)	)	)	)	)
; RK: (record-predicate <record-type>) -> (<predicate> <value>) -> <boolean>
#| Generate the predicate to know if an intance belongs to a type
(define (is:person? subject)((record-predicate :person) subject))
 |#;->	is:person?
#|
(is:person? Ondine)	(record? :person)	(record? Ondine)	(vector? Ondine)
 |#;->	#t			#t					#f				 	#t
(define (record-predicate type)
	(check-arg record? type 'record-predicate)
	(vector-ref type 5) ; return the predicate lambda embedded inside the record
)
; RK: (record-accessor <record-type <field-name>) -> (<accessor> <record>) -> <value>
#|
(show	"Lastname: "	((record-accessor :person 'LastName)  Ondine) 
		" Firstname: "  ((record-accessor :person 'FirstName) Ondine)
)
 |#;->	Lastname: C. Firstname: Laetitia
(define (record-accessor type tag)
	(check-arg record? type 'record-accessor)
	(let	(	(index (comlist:position tag (rtd-vfields type)))
				(lenAug (rtd-length type))
			)
		(if	(not index) (error 'record-accessor "invalid tag argument." tag))
		(lambda	(vect)
			(check-arg
				(lambda(vect)	
					(and (vector? vect)(>= (vector-length vect) lenAug)
						(eq? (record-type vect) type))
				) vect 'record-accessor
			)
			(vector-ref vect index)
)	)	)
; RK: (record-modifier <record-type <field-name>)    -> (<modifier> <record> <value>)
#|
(show ((record-modifier :person 'FirstName) Ondine "Laetitia M. Laure"))
 |#;->	C. Laetitia M. Laure
(define (record-modifier type tag)
	(check-arg record? type 'record-modifier)
	(let	(	(index (comlist:position tag (rtd-vfields type)))
				(lenAug (rtd-length type))
			)
		(if	(not index) (error 'record-modifier "invalid tag argument." tag))
		(lambda	(vect value)
			(if	(not(and	(vector? vect)	(>= (vector-length vect) lenAug)
						(eq? (record-type vect) type)
				)	) (error 'record-modifier 'wrong 'record 'type. vect 'not type)
			)
			(vector-set! vect index value)
)	)	)
; Richard Kelsey: This implements a record abstraction that is identical to vectors,
; except that they are not vectors 
; - VECTOR? returns false when given a record
; - RECORD? returns false when given a vector.
; Aubrey Jaffer did it in SLIB 3b6-1.

; AlSchemist removed all alterations of basic primitives about vector.
; about redefining EVAL, Richard Kelsey wrote: 
; "This won't work if ENV is the interaction environment and
; someone has redefined LAMBDA there." so AlSchemist does not overwrite EVAL.
; AlSchemist upgraded Jaffer's implementation to be as close as possible of SRFI-9.
;_______________________________	Section 3 begin by AlSchemist 2021

; The following is free open source copyrighted AlSchemist

#| is an instance of a record type?
(record-instance? Ondine)	(record-instance? person:type)
 |#;->			  #tag						  #f
(define (record-instance? obj)
	(and	(vector? obj) (> (vector-length obj) 1) (record? (record-type obj)))
)
#| Display the definition of a record type
(show :person)
 |#;->	record type: :person (LastName FirstName)
(define (record-display type)
	(check-arg record? type 'show)
	(display "record type ") (display (record-type-name type))
	(display " ") (display (record-type-field-tags type))
)
#| Display an instance of a record
(show Ondine)
 |#;->	C. Laetitia M. Laure
(define (record-instance-display obj)
	(check-arg record-instance? obj 'show)
	(let*	(	(type (record-type obj)))
		(let loop ((lstTag (record-type-field-tags type)))
			(if (pair? lstTag)
				(let*	((first (car lstTag))	(rest (cdr lstTag)))
					(display ((record-accessor type first) obj))
					(if (pair? rest) (display " "))
					(loop (cdr lstTag))
)	)	)	)	)
#|
(define-record-type :movie (new:movie Director Actress) is:movie? info:movie update:movie)
 |#;->	record type :movie (Director Actress) new:movie is:movie? info:movie update:movie
#|
(record? :movie) (map closure? (list new:movie is:movie? info:movie update:movie))
 |#;->	 #t							(#t			#t			#t		#t)
#|
(define Savage-Souls (new:movie "Raoul Ruiz" "Therese")) (is:movie? Savage-Souls)
 |#;->	Savage-Souls														#t
#|
(info:movie Savage-Souls 'Director)(info:movie Savage-Souls 'Actress)
 |#;->					 "Raoul Ruiz"						"Therese"
#|
(info:movie Savage-Souls 'Actress (update:movie Savage-Souls 'Actress "Thérèse"))
 |#;->					  											  "Thérèse"
(define-macro (define-record-type type lstBuild predicate accessor modifier)
	(let*	(	(builder	(car lstBuild))
				(strBuild	(atom->string builder)) ; save as string before creation
				(strPred	(atom->string predicate))
				(field-tags	(cdr lstBuild))
				(strAccess	(atom->string accessor))
				(strMod		(atom->string modifier))
			)
	   `(begin ;                             quote required when type is not yet defined
			(define ,type (make-record-type (quote	,type) (quote ,field-tags)))
			(define ,builder (record-constructor  	,type  (quote ,field-tags)))
			(define ,predicate (record-predicate	,type)) ; type is defined
			(define (,accessor instance tag) ((record-accessor ,type tag) instance))
			(define (,modifier instance tag val) ((record-modifier ,type tag) instance val))
			(show ,type " " ,strBuild " " ,strPred " " ,strAccess " " ,strMod)
)	)	)
#| Perf of the macro define-record-type: each define is not persitent inside let*
(let*	(	; add specific init
			(nbrRun	1000)(timeEnd 0)(timeStart (gettimeofday))
		)
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
		(define-record-type :movie (new:movie Director Actress)
			is:movie? info:movie update:movie
		)
	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->	 9 s 140 ms 181 µs for 1K runs.  9 ms 140 µs by run macro without show
;		11 s 499 ms 456 µs for 1K runs. 11 ms 499 µs by run macro with    show
#| Perf: each define is local to let* so there are not persistent at top level
(let*	(	(nbrRun	1000)(timeEnd 0)(timeStart (gettimeofday))
			(lstTag '(Director Actress))
		)
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
		(define :movie (make-record-type ':movie lstTag))
		(define new:movie (record-constructor :movie lstTag))
		(define (is:movie?		film)	 ((record-predicate :movie)		film))
		(define (info:movie		film tag)((record-accessor  :movie tag) film))
		(define (update:movie	film tag val)((record-modifier  :movie tag) film val))	
	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->	 1 s 265 ms 688 µs for 1K runs. 1 ms 265 µs by run without macro
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
(load "C:/Users/YourUserName/AppData/Roaming/GIMP/2.10/scripts/2srfi-009-record.scm")
 |# (closure? make-record-type)