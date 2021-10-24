; https://legacy.cs.indiana.edu/scheme-repository/code.string.html
; Scheme code for string manipulation
; A scheme pretty-printer, by Marc Feeley (pp.scm).
; File: "pp.scm"   (c) 1991, Marc Feeley

; 2021/09/27: 	GimpλLib 1.0 by AlSchemist for Gimp 2.10.28 TinyScheme Script-Fu
;				Multiline comments tab-based pretty print by AlSchemist
; Line: 743 define: 48 comment: 334 = 44.9% blank: 4
#| Usage: (pp nameOfFunction)
(pp pp-call)
; displays the following tab-based pretty-print definition of the function
; with alignement of open and closed parenthesis if the item needs more than 1 line
(define	(pp-call expr colFrom extra pp-item)
	(let*	(	(colAfter (wr (car expr) (pp-par-open colFrom)))
				(colMargin (- colAfter (remainder colAfter 4)))
			)
		(and colFrom (pp-down (cdr expr) colMargin (+ colMargin 4) extra pp-item))
)	)
 |#
;_______________________________	Section 1 begin by Marc Feeley 1991
#| The original version by Marc Feeley displayed:
(pp pp-call)
; displays the following pretty-print definition of the same function
; an item by line, indentation of 2 spaces, bunch of unaligned final closed parenthesis
(define
  (pp-call expr colFrom extra pp-item)
  (let* ((colAfter (wr (car expr) (pp-par-open colFrom)))
         (colMargin (- colAfter (remainder colAfter 4))))
        (and colFrom
             (pp-down
               (cdr expr)
               colMargin
               (+ colMargin 4)
               extra
               pp-item))))
 |#

; define formatting style: change these to suit your style
(define pp-indentation 4)
(define pp-max-call-head-width 5)
(define pp-line-width 90)
(define pp-max-expr-width pp-line-width)
(define pp-display? #f) ; #f display string without quote. #t write quoted string
(define (pp-output str)(display str) #t)
#| Returning the column where ch should be displayed from col
(pp-char-tab #\A 0)	(pp-char-tab #\tab 10)	(pp-char-tab #\tab 13)
 |#;->		  1						12					16
(define (pp-char-tab ch col) 
	(+ col (if (char=? ch #\tab)(- 4 (remainder col 4)) 1))
)
#| Run pp-output to display str if col. Return the column of the last character of str
(pp-out "Pretty Lae\ti\tia" 0)	(pp-out "Pretty" 2)	(pp-out "Reset\n" 9)
Pretty Lae	i	ia18			Pretty8				0
         1 1   1 1 2
12345678901234567890
 |#;->	         ^18
(define (pp-out str col)
	(if (and (> (string-length ppStrLinePar) 0)
			(not (string=? str ppStrLinePar))
		)
		(begin	;(show "pp-out str:" str " col:" col " flush line par:" ppStrLinePar)
				(pp-flush-line-par)
	)	)
	(and col
		(pp-output str)
		(pp-str-count str col) ; Count the column after str that could contain tab 
)	)
(define (pp-out-old str col)
	(and col
		(pp-output str)
		(string-fold pp-char-tab col str) ; Count the column after str that could contain tab 
)	)
#| Perf: Count the number of tab in the string
(let*	(	(nbrRun	10000)(timeEnd 0)(timeStart (gettimeofday)))
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
		(pp-out "pretty Lae\ti\tia" 0)
	)	(newline)(time-stat timeStart timeEnd nbrRun)
)
 |#;->	7 s 780 ms 823 µs for 10K runs. 778 µs by run
#| Display n spaces from col
(pp-space 3 0)
	4
 |#
(define (pp-space nbr col)
	(if (> nbr 0)
		(if (>= nbr 4)
			(pp-space (- nbr 4) (pp-out "\t" col))
			(pp-out "\t" col) ;was: (pp-out (substring "   " 0 n) col)
		)
		col
)	)
#| Indent until to from col
(pp-indent 5 0)
		8
 |#
(define (pp-indent to col proc)
	(and col
		(if (or (null? col) (< to col))
			(and (pp-out "\n" col) (pp-space to 0))
			(pp-space (- to col) col)
)	)	)
#|
(pp-macro? '(quote a))
 |#;->	#t
(define (pp-macro? l)
	(define (length1? l) (and (pair? l) (null? (cdr l))))
	(let ((head (car l)) (tail (cdr l)))
		(case head ((quote quasiquote unquote unquote-splicing) (length1? tail))
					(else #f)
)	)	)
#|
(pp-macro-body '(quote a))
 |#;->	a
(define (pp-macro-body l) (cadr l))
#|
(pp-macro-prefix '(quote a))
 |#;->	"'"
(define (pp-macro-prefix l)
    (case (car l) ((quote) "'")((quasiquote) "`")((unquote) ",")((unquote-splicing) ",@")
)	)
#|
(pp-string "\"string\"" 0)	(pp-string "\nstring" 0)	(pp-string "C:\\Tool\\Gimp" 0)
 |#;->	"\"string\""12		"\nstring"10				"C:\\Tool\\Gimp"16
(define (pp-string obj col)
	(let loop ((iRef 0) (idx 0) (col (pp-out "\"" col))) ; open string
		(if (and col (< idx (string-length obj)))
			(case (string-ref obj idx)
				((#\\ #\")	(loop idx		(inc idx) 
								(pp-out "\\" (pp-out (substring obj iRef idx) col))
				)			)
				((#\return)	(loop (inc idx) (inc idx) 
								(pp-out "\\r" (pp-out (substring obj iRef idx) col))
				)			)
				((#\newline)(loop (inc idx) (inc idx)
								(pp-out "\\n" (pp-out (substring obj iRef idx) col))
				)			)
				(else (loop iRef (inc idx) col))
			)
			(pp-out "\"" (pp-out (substring obj iRef idx) col)) ; close string
)	)	)
#| Write an object without width limit from col
(wr #(1 2 3) 0)		(wr #\space 0)	(wr "str \"in\"g" 0)
 |#;->	#(1 2 3)8	#\space7		"str \"in\"g"13
 #|
(wr *pi* 0)	(wr (current-output-port) 0)
 |#;->				#<PORT>7
(define (wr obj col)
  (cond ((pair? obj)        (wr-expr obj col))
        ((null? obj)        (wr-lst obj col))
        ((vector? obj)      (wr-lst (vector->list obj) (pp-out "#" col)))
        ((boolean? obj)     (pp-out (if obj "#t" "#f") col))
        ((number? obj)      (pp-out (real->string obj) col))
        ((symbol? obj)      (pp-out (symbol->string obj) col))
		((closure? obj)   	(pp-out "#<CLOSURE>" col))
        ((procedure? obj)   (pp-out "#<PROCEDURE>" col))
        ((string? obj)      (if pp-display? (pp-out obj col) (pp-string obj col)))
        ((char? obj)        (if pp-display?
                                (pp-out (make-string 1 obj) col)
								(pp-out (case obj
											((#\space)   "space") ((#\tab)     "tab")
											((#\return)  "return")((#\newline) "newline")
											(else        (make-string 1 obj))
										)
									(pp-out "#\\" col)
        )                   )	)
        ((input-port? obj)  (pp-out "#<PORT>" col))
        ((output-port? obj) (pp-out "#<PORT>" col))
        ((eof-object? obj)  (pp-out "#<EOF>" col))
        (else               (pp-out "#[unknown]" col))
) )
#| Write a list. Return the col after the list.
(wr-lst '(a b c) 1)
 |#;->	(a b c)8
(define (wr-lst lis col)
  (if (pair? lis)
    (let loop ((lis (cdr lis)) (col (wr (car lis) (pp-out "(" col))))
		(and col
			(cond	((pair? lis)	(loop (cdr lis) (wr (car lis) (pp-out " " col))))
					((null? lis)	(pp-out ")" col))
					(else			(pp-out ")" (wr lis (pp-out " . " col))))
    )	)   )
    (pp-out "()" col)
) )
#| Write an expression
(wr-expr '(+ 1 3) 1)
 |#;->	(+ 1 3)8
(define (wr-expr expr col)
  (if (pp-macro? expr)
    (wr (pp-macro-body expr) (pp-out (pp-macro-prefix expr) col))
    (wr-lst expr col)
) )
#| Return the pp procedure corresponding to the uppercase symbol
(let* ((pp-IF (pp-style 'IF)))(proc->list pp-IF))
 |#;->	(define (pp-IF expr col extra) ((pp-general expr col extra #f pp-expr #f pp-expr)))
(define (pp-style head)
	(case head	((lambda let* letrec define) pp-LAMBDA)
				((if set!)                   pp-IF)
				((cond)                      pp-COND)
				((case)                      pp-CASE)
				((and or)                    pp-AND)
				((let)                       pp-LET)
				((begin)                     pp-BEGIN)
				((do)                        pp-DO)
				(else                        #f)
)	)
#|
(pr '(a b c) 0 0 pp-expr)
 |#;->	(a b c)7
(define (pr obj col extra pp-pair)
	(if (or (pair? obj) (vector? obj)) ; may have to split on multiple lines
		(let(	(result '())
				(left (min (+ (- pp-line-width col extra) 1) pp-max-expr-width))
			)
;			(show "pr obj: " obj " col: " col " extra: "extra)
;			(show "pr pp-line-width: " pp-line-width " left: " left " min: " (+ (- (- pp-line-width col) extra) 1) " " pp-max-expr-width)
			(generic-write obj pp-display? #f
				(lambda (str)	(set! result (cons str result))
								(set! left (- left (string-length str)))
								(> left 0)
			)	)
			(if (> left 0) ; all can be printed on one line
				(begin	;(show "pr result:" result " col:" col)
						(pp-out (string-concatenate-reverse result) col)
				)
				(if (pair? obj)
					(pp-pair obj col extra)
					(pp-list (vector->list obj) (pp-out "#" col) extra pp-expr)
		)	)	)
		(wr obj col)
)	)
#| Display each item of the lst by line at colTo. Close the list at colFrom
(pp-down '(a b c) 0 4 0 #f)
	a	b	c
)1

(pp-down nil 0 4 0 #f)

)1
 |#
(define (pp-down lst colFrom colTo extra pp-item)
; (show "\npp-down start lst:" lst " colFrom:" colFrom " colTo:" colTo)
  (let loop ((lst lst) (col colFrom))
    (and col
		(cond	((pair? lst)
					(let*	(	(head	(car lst))
								(rest	(cdr lst))
								(nextExtra	(pp-next-extra rest extra))
								(nextCol	(pr head (pp-indent colTo col 'pp-down)
												nextExtra pp-item
							)	)			)
;						(if (null? nextCol)
;								(show "\npp-down lst:" lst " colFrom:" colFrom " colTo:" colTo " nextExtra:" nextExtra " rest:" rest " nextCol:" nextCol " col:" col)
;						)
						(loop rest nextCol)
				)	)
;was:			((null? lst)(pp-out ")" col))
				((null? lst)	;(show "\npp-down end. Aligned column: " (pp-stack-dbg))
								(pp-par-close colFrom) 
				)
				(else	(pp-par-close 
							(pr lst	(pp-indent colTo (pp-out "." (pp-indent colTo col 'pp-down2)))
									(+ extra 1) pp-item
) ) )	)		)		)	)
(define (pp-tail rest colFrom colAfter colNextAfter extraPrm pp-2 pp-3)
;(show "\pp-tail rest:" rest " colFrom:" colFrom " colAfter: " colAfter " colNextAfter: " colNextAfter " extraPrm: " extraPrm 
; "\npp-2: " pp-2 " 	pp-3: " pp-3)
	(if (and pp-2 (pair? rest))
		(let*	(	(valPrm (car rest))
					(extra (pp-next-extra (cdr rest) extraPrm))
				)
			(set! rest (cdr rest))
			(set! colAfter (pr valPrm (pp-indent colNextAfter colAfter 'pp-tail) extra pp-2))
	)	)
	(pp-down rest colAfter colFrom extraPrm pp-3)
)
(define (pp-arg rest colFrom colAfter colNextAfter extraPrm pp-1 pp-2 pp-3)
;(show "\npp-arg rest:" rest " colFrom:" colFrom " colAfter: " colAfter " colNextAfter: " colNextAfter 
; " extraPrm: " extraPrm "\npp-1: " pp-1 " pp-2: " pp-2 " pp-3: " pp-3)
	(if (and pp-1 (pair? rest))
		(let*	(	(firstPrm (car rest))
					(extra (pp-next-extra (cdr rest) extraPrm))
				)
			(set! rest (cdr rest))
			(set! colAfter (pr firstPrm (pp-indent colNextAfter colAfter 'pp-arg) extra pp-1))
	)	)
	(pp-tail rest colFrom colAfter colNextAfter extraPrm pp-2 pp-3)
)
#| Display the given expr at col
If expr is:
1) a macro, print pp-macro-body
2.1.1) a short symbol but not a proc, call pp-call
(pp-expr '(IF p1 p2) 0 0)
(IF	p1
	p2
)1

2.1.2) a long symbol but not a proc, call pp-general
(pp-expr '(header p1 p2) 0 0)
(header
	p1
	p2
)1

2.2) a symbol and a proc, for example for "if" according to pp-style, call pp-IF
(pp-expr '(if condition then else) 0 0)
(if	condition
	then
	else
)1

3) otherwise display the list with pp-list
 |#
(define (pp-expr expr col extra)
	(if (pp-macro? expr)
		(pr (pp-macro-body expr) (pp-out (pp-macro-prefix expr) col) extra pp-expr)
		(let ((head (car expr)))
			(if (symbol? head)
				(let	(	(proc	(pp-style head))
							(len	(string-length (symbol->string head)))
						)
					;(show "pp-expr symbol: " head " proc: " proc " len: " len)
					(if proc
						(proc expr col extra)
						(if (> len pp-max-call-head-width)
							(pp-general	expr col extra #f #f #f pp-expr)
							(pp-call	expr col extra pp-expr)
				)	)	)
				(pp-list expr col extra pp-expr)
)	)	)	)
#| Display the call of a function 
(pp-call '(head item1 item2 item3) 0 0 #f)	(pp-call '(IF item1 item2 item3) 0 0 #f)
(head	item1								(IF	item1										
		item2									item2
		item3									item3
)1											)1
 |#
(define (pp-call expr colFrom extra pp-item)
	(let*	(	(colAfter (wr (car expr) (pp-par-open colFrom)))
				(colMargin	(- colAfter (remainder colAfter 4)))
			)
		(and colFrom (pp-down (cdr expr) colMargin (+ colMargin 4) extra pp-item))
)	)
#|
(pp-COND '(cond ((number? nbr) (inc nbr)) ((> nbr 0) (dec nbr))) 0 0)
(cond	((number? nbr) (inc nbr))
	((> nbr 0) (dec nbr))
)1
 |#
;(define (pp-COND expr col extra)	(pp-call expr col extra pp-expr-list)) ; original
(define (pp-COND expr col extra)	
	(pp-general expr col extra #f pp-expr #f pp-expr-list) ; 2021/07/23: AlSchemist
)
#|
(pp-AND '(and (number? nbr) (> nbr 0)) 0 0)
(and	(number? nbr)
		(> nbr 0)
)1
 |#
(define (pp-AND	 expr col extra)	(pp-call expr col extra pp-expr))

#| Display each item of lst by line
(pp-list '(item1 item2 item3) 0 0 #f)
(	item1
	item2
	item3
)1
 |#
(define (pp-list lst colFrom extra pp-item)
	(let ((colTo (pp-par-open colFrom)))
		(pp-down lst colFrom colTo extra pp-item)
)	)
(define (pp-expr-list lst col extra)(pp-list lst col extra pp-expr))
#|
(pp-LAMBDA '(define (inc nbr) (+ nbr 1)) 0 0)
(define	(inc nbr)
	(+ nbr 1)
)1
 |#
(define (pp-LAMBDA	expr col extra)	(pp-general expr col extra #f pp-expr-list	#f pp-expr))
#|
(pp-IF '(if (eq? nbr 1) #t #f) 0 0)
(if	(eq? nbr 1)
	#t
	#f
)1

(pp-IF '(set! pp-output output) 0 0)
(set!	pp-output
	output
)1
 |#
(define (pp-IF		expr col extra)
;	(show "\npp-IF expr:" expr " col:" col " extra:" extra)
	(let* ((retVal (pp-general expr col extra #f pp-expr #f pp-expr)))
		;(show "\npp-IF end.")
		retVal
)	)
#|
(pp-CASE 
   '(case nameSet ; switch to generator by type of filter
		((GimpHueSaturationConfig)        (GenPdbMak tim namePdb     3 nil lstFilter))
		((GimpLevelsConfig) (GenPdbMak tim namePdb 5 (list-head lstFilter 3)(cdddr lstFilter)))
		((GimpBrightnessContrastConfig)   (GenPdbMak tim namePdb  -127 lstFilter nil))
		((GimpGegl-gimp-desaturate-config)(GenPdbMak tim namePdb	 0 lstFilter nil))
	) 0 0
)
(case	nameSet
	((GimpHueSaturationConfig) (GenPdbMak tim namePdb 3 nil lstFilter))
	((GimpLevelsConfig)
		(GenPdbMak tim namePdb 5 (list-head lstFilter 3) (cdddr lstFilter)))
	((GimpBrightnessContrastConfig) (GenPdbMak tim namePdb -127 lstFilter nil))
	((GimpGegl-gimp-desaturate-config) (GenPdbMak tim namePdb 0 lstFilter nil)))80
 |#
(define (pp-CASE expr col extra)
;	(show "\npp-CASE expr:" expr " col:" col " extra:" extra)
	(let* ((retVal (pp-general expr col extra #f pp-expr #f pp-expr-list)))
		;(show "\npp-CASE end.")
		retVal
)	)
#|
(pp-LET '(let* ((obj nil)) obj) 0 0)
(let*	((obj nil))
	obj
)1

(pp-LET '(let loop ((idxNbr 0)) (if (< idxNbr 10) (begin (loop (+ idxNbr 1))))) 0 0)
(let loop	((idxNbr 0))
	(if (< idxNbr 10) (begin (loop (+ idxNbr 1))))
)1

(pp-LET '(let ((head (car expr)))(if (symbol? head)(let ((proc (pp-style head))(len (string-length (symbol->string head)))) body)else)) 0 0)

(let	((head (car expr)))
	(if	(symbol? head)
		(let ((proc (pp-style head)) (len (string-length (symbol->string head)))) body)
		else
)	)5
 |#
(define (pp-LET		expr col extra)
	(let*	(	(rest (cdr expr))
				(named? (and (pair? rest) (symbol? (car rest))))
			)
		(pp-general expr col extra named? pp-expr-list #f pp-expr)
)	)
(define (pp-BEGIN expr col extra)  (pp-general expr col extra #f #f 			#f pp-expr))
(define (pp-DO expr col extra)
	(pp-general expr col extra #f pp-expr-list 	pp-expr-list pp-expr)
)
(define (pp-general expr colFrom extra named? pp-1 pp-2 pp-3)
	(let*	(	(rest (cdr expr))
				(colAfter (wr (car expr) (pp-par-open colFrom)))
			) ;                ^head
		(if (and named? (pair? rest))
			(begin	(set! colAfter (wr (car rest) (pp-out " " colAfter)))
					(set! rest (cdr rest))
			)
		) ;                             ^name
		(pp-arg rest (+ colFrom pp-indentation) colAfter (inc colAfter) extra pp-1 pp-2 pp-3)
)	)
; 'generic-write' is a procedure that transforms a Scheme data value or
; Scheme program expression into its textual representation.
; The interface to the procedure is sufficiently general to easily implement
; other useful formatting procedures such as pretty printing, 
; output to a string and truncated output.
; Parameters:
;   OBJ       Scheme data value to transform.
;   DISPLAY?  Boolean, controls whether characters and strings are quoted.
;   		  (generic-write obj #f #f output) ; write string with quotes
;   		  (generic-write obj #t #f output) ; display string without quotes
;   WIDTH     Extended boolean, selects format:
;               #f = single line format
;               integer > 0 = pretty-print (value = max nb of chars per line)
;   OUTPUT    Procedure of 1 argument of string type, called repeatedly
;             with successive substrings of the textual representation.
;             This procedure can return #f to stop the transformation.
;             (lambda (s) (for-each write-char (string->list s)) #t)
; The value returned by 'generic-write' is the last written column.
(define (generic-write obj display? width output)
	(let*	(	(outputSave pp-output)
				(displaySave pp-display?)
				(widthSave pp-line-width)
				(col 0)
			)
		(set! pp-output output)
		(set! pp-display? display?)
		(set! pp-line-width width)
		(set! col	(if width 
						(pp-out "\n" (pr obj 0 0 pp-expr))
						(wr obj 0)
		)			)
		(set! pp-output outputSave)
		(set! pp-display? displaySave)
		(set! pp-line-width widthSave)
		col
)	)
; (object->string obj) returns the textual representation of 'obj' as a string.
;
; Note: (write obj) = (display (object->string obj))
(define (object->string obj)
	(let	((result '()))
		(set! ppStrLinePar "")
		(generic-write obj #f #f (lambda (str) (set! result (cons str result)) #t))
		(string-concatenate-reverse result)
)	)
#| returns a string containing the first limit characters of obj's textual representation
(object->limited-string (proc->list object->limited-string) 48)
 |# ;->	"(define (object->limited-string obj limit) (let "
(define (object->limited-string obj limit)
	(let ((result '()) (left limit))
		(set! ppStrLinePar "")
		(generic-write obj #f #f
			(lambda (str)
				(let ((len (string-length str)))
					(if (> len left)
						(and(set! result (cons (substring str 0 left) result))		#f)
						(and(set! result (cons str result)) (set! left (- left len))#t)
		)	)	)	)
		(string-concatenate-reverse result)
)	)
#| returns a string with the pretty-printed textual representation of 'obj'.
(pretty-print-to-string (proc->list inc))
"(define (inc nbr) (+ nbr 1))\n"
 |#
(define (pretty-print-to-string obj)
	(let ((result '()))
		(set! ppStrLinePar "")
		(generic-write obj #f 79 (lambda (str) (set! result (cons str result)) #t))
		(string-concatenate-reverse result)
)	)
#| pretty prints 'obj' [on 'port']: current output is not specified
(pretty-print (proc->list pretty-print))
(define	(pretty-print obj . opt)
	(let	((port (if (pair? opt) (car opt) (current-output-port))))
		(set! ppStrLinePar "")
		(generic-write obj #f pp-line-width (lambda (s) (display s port) #t))
)	)
0
 |#
(define (pretty-print obj . opt)
	(let ((port (if (pair? opt) (car opt) (current-output-port))))
		(set! ppStrLinePar "")
		(generic-write obj #f pp-line-width (lambda (s) (display s port) #t))
)	)
#| 
(pp inc)
(pp '(define (inc nbr) (+ nbr 1)))
 |#;->	(define (inc nbr) (+ nbr 1))
(macro (pp lstMacroCall)
 	(let*	(	(proc	(cadr lstMacroCall))
				(prog  `(begin 
							(if (closure? ,proc)
								(pretty-print (proc->list ,proc))
								(pretty-print ,proc)
							)
							(void)
				)		)
			);	(show "\n" (car lstMacroCall) " body: " prog "\n->")
		prog
)	)
;_______________________________	Section 1 end

; Tools powered by AlSchemist 2021 for the section 1
; The following is free open source copyrighted AlSchemist
(define ppStackCol nil)
(define ppStrLinePar "")
#| Push a new column in the stack.
Update ppStackCol
 |#
(define (pp-stack-push col)
	(set! ppStackCol (cons col ppStackCol))
)
#| Get the last column pushed in the stack.
Remove this last column from ppStackCol
 |#
(define (pp-stack-get)
	(if (pair? ppStackCol)
		(let* ((colFrom (car ppStackCol)))
			(set! ppStackCol (cdr ppStackCol))	
			colFrom
		)
		0
)	)
; View the last column pushed on the stack without removing it from the stack
(define (pp-stack-dbg) (if (pair? ppStackCol) (car ppStackCol) 0))
#| Return the column of the last character of str.
(pp-str-count "Pretty Lae\ti\tia" 0)	(pp-str-count "Reset\n" 80)	(pp-str-count "space" 10)
18										0							15
Pretty Lae	i	ia18
         1 1   1 1 2
12345678901234567890
 |#;->	         ^18
(define (pp-str-count str col)
	(let*	((end (string-length str)))
		(let loop	((sum col) (idx 0))
			(if (< idx end)
				(case	(string-ref str idx)
					((#\tab)	(loop (+ sum (- 4 (remainder sum 4))) (inc idx)))
					((#\newline)(loop 0			(inc idx))) ; Reset counter
					(else		(loop (inc sum) (inc idx)))
				)
				sum
)	)	)	)
#| Generate a tab-based string 
(pp-string-tab 5)
 |#;-> "\t\t"
(define (pp-string-tab nbr)
	(if (> nbr 0)
		(if (>= nbr 4)
			(string-append "\t" (pp-string-tab (- nbr 4)))
			"\t"
		)
		""
)	)
; if there is some closed parenthesis in ppStrLinePar, display them then reset to empty line
(define (pp-flush-line-par)
	(if (> (string-length ppStrLinePar) 0)
		(begin	(pp-out ppStrLinePar 0)
				(set! ppStrLinePar "")
)	)	)
#| Display an opening partenthesis at col
Push the col on the stack of the columns of opening parenthesis
 |#
(define (pp-par-open col)
	(pp-flush-line-par)
	(pp-stack-push col)
	(pp-out "(" col)
)
#| Merge two lines containing closing tab-based parenthesis
If ppStrLinePar is "\n	)" and strLine is "\n)", return "\n)	)"
 |#
(define (pp-merge-line strLine)
	(let*	(	(strPrev ppStrLinePar)
				(endPrev (string-length strPrev))
				(endFrom (string-length strLine))
			)
		(set! ppStrLinePar "")
		(let loopCommon ((idPrev 0) (idFrom 0))
			(if (and (< idPrev endPrev) (< idFrom endFrom))
				(let*	(	(chPrev	(string-ref strPrev idPrev))
							(chFrom (string-ref strLine idFrom))
						)
					(if (char=? chPrev chFrom)
						(set! ppStrLinePar (string-append ppStrLinePar (string chFrom)))
						(set! ppStrLinePar (string-append ppStrLinePar (string chFrom) (string chPrev)))
					)
					(loopCommon (inc idPrev) (inc idFrom))
				)
				(let loopRest ((idx idPrev))
					(if (< idx endPrev)
						(let*	(	(chPrev (string-ref strPrev idx)))
							(set! ppStrLinePar (string-append ppStrLinePar (string chPrev)))
							(loopRest (inc idx))
		)	)	)	)	)
		;(show "\nstrPrev:" strPrev "\nstrLine:" strLine "\nppStrLinePar:" ppStrLinePar)
)	)
#| Display a closing partenthesis
Get the column of the aligned opening parenthesis on the stack
 |#
(define (pp-par-close col)
	(let*	(	(colFrom (pp-stack-get))
				(strLine (string-append "\n" (pp-string-tab colFrom) ")"))
			)
		(if (> (string-length ppStrLinePar) 0)
			(pp-merge-line strLine)
			(set! ppStrLinePar (string-copy strLine))
		)
		(if (zero? colFrom)
			(let* ((colAfter (pp-out ppStrLinePar 0)))
				;(show "\ncolFrom is zero.")
				(set! ppStrLinePar "")
				colAfter
		)	)
;		(pp-out "\n" colFrom) (pp-space colFrom 0) (pp-out ")" colFrom)
)	)
(define (pp-next-extra rest extra)
	(if (null? rest) (+ extra 1) 0)
)
#| Return its parameter as symbol. Used for basic procedure, new closure or variable
(proc->symbol car)(symbol? (proc->symbol car))(symbol? car)(closure? car)(procedure? car)
 |#;->	       car                        #t            #f            #f              #t
(macro	(proc->symbol lstMacroCall)
 	(let*	(	(proc	(cadr lstMacroCall))
				(prog	(list 'quote proc))
			);	(show "\n" (car lstMacroCall) " body: " prog "\n->")
		prog
)	)
#| Symbolize a variable or basic procedure.List the definition of a closure.
(proc->list *pi*)(proc->list car)	(proc->list inc)
 |#;->			*pi*			 car				(define (inc nbr) (+ nbr 1))
(define (MakeBody-proc->list proc)
   `(let*	(	(lstLmbd	,(list 'get-closure-code proc))
				(lambda?	(and	(pair? lstLmbd)(eq? (car lstLmbd) 'lambda)
									(pair? (cdr lstLmbd))
				)			)
				(isMacro?	,(list 'macro? proc))
				(isBasic?	,(list 'procedure? proc))
				(isDefVar?	,(list 'and (list 'not (list 'closure? proc))
                                        (list 'defined? (list 'quote proc))
			)   )           )
		(if (not lambda?) (if (or isBasic? isDefVar?) (proc->symbol ,proc) #f)
			(let*	((args	(cons (proc->symbol ,proc) (cadr lstLmbd))))
				(cons* (if isMacro? 'macro 'define) args (cddr lstLmbd))
)	)	)	)
#| List the definition of a macro without its comments
(proc->list catch)
 |#;->	(macro (catch form) (let ((label (gensym))) `(call/cc (lambda (exit) (push-handler (lambda () (exit ,(cadr form)))) (let ((,label (begin ,@(cddr form)))) (pop-handler) ,label)))))
(macro	(proc->list lstMacroCall)
 	(let*	(	(proc	(cadr lstMacroCall))
				(prog	(MakeBody-proc->list proc))
			);	(show "\n" (car lstMacroCall) " body: " prog "\n->")
		prog
)	)
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
(load "C:/Users/YourUserName/AppData/Roaming/GIMP/2.10/scripts/2pp.scm")
 |# (closure? pretty-print)