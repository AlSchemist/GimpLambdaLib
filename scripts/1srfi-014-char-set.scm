; https://srfi.schemers.org/srfi-14/srfi-14.html
; SRFI: Scheme Requests for Implementation https://srfi.schemers.org/
;;; SRFI-14 character-sets library				-*- Scheme -*-

; Section 1: AlSchemist    2021	TinyScheme integration for Gimp 2.10.28 Script-Fu 
; Section 2: Stephen J. Bevan	Implementation of character sets 
; Section 3: Olin Shivers 2000	SRFI-14 character-sets library is the main section
;			 AlSchemist	  2021  Add multiline comments, examples and pretty print
; Section 4: AlSchemist	  2021	Miscellaneous tools for char-set
; 2021/09/27: GimpλLib 1.0 by AlSchemist for Gimp 2.10.28 TinyScheme Script-Fu
; Line: 1569 define: 82 comment: 914 = 58.2% blank: 37

; About the main section 3:
;;; - Ported from MIT Scheme runtime by Brian D. Carlstrom.
;;; - Massively rehacked & extended by Olin Shivers 6/98.
;;; - Massively redesigned and rehacked 5/2000 during SRFI process.
;;; At this point, the code bears the following relationship to the
;;; MIT Scheme code: "This is my grandfather's axe. My father replaced
;;; the head, and I have replaced the handle." Nonetheless, we preserve
;;; the MIT Scheme copyright:
;;;     Copyright (c) 1988-1995 Massachusetts Institute of Technology
;;; The MIT Scheme license is a "free software" license. 
;;; See the end of section 2 for the tedious details.

;_______________________________	Section 1 begin by AlSchemist 2021
;
; The following is free TinyScheme open source copyrighted AlSchemist

#| Return the char-set index of character ch for the given offset
(%char->cs-index #\x00 0)
 |#;->	0
(define (%char->cs-index ch offset) (- (char->integer ch) offset))
#| Return the character at char-set index idx for the given offset
(char->integer (%cs-index->char 0 0))
 |#;->	0
(define (%cs-index->char idx offset) (integer->char (+ idx offset)))
#| Return the lower index by range of 256 charactes
(%offset 255)	(%offset 256)	(%offset 8304)
 |#;->	0		256				8192
(define (%offset iChar)
	(if (< iChar cs-size-max) 0 (* (quotient iChar cs-size-max) cs-size-max))
)
;_______________________________	Section 2 begin by Stephen J. Bevan
; https://legacy.cs.indiana.edu/scheme-repository/code.string.html
; Scheme code for string manipulation
; Stephen J. Bevan's implementation of character sets in Scheme (char-set.tar.gz).

#|
A character set is, as its name might suggest, a set for storing
characters.  Such sets are often particularly useful in string
processing applications.  The reason for treating sets of characters
in a special manner is that until the advent of UNICODE and ISO ???,
character sets only contained a maximum of 255 characters and so this
fact could be taken advantage of in an implementation.  The usual
approach being to store a character set as a bit set.

Note,

1. Bitsets are not used since Scheme does not have them.  This
   implementation actually stores the char-set as a vector of booleans
   which are unlikely to be optimised to a bitset due to the way the
   code is written, hence :-

2. This code has been written with too much knowledge of vectors built
   in.  Sometime, I should re-write this so that it has an abstract
   interface.  This would allow you to (fairly) easily change the
   implementation without having to modify all the functions.

3. The code was written in ignorance of the MIT version of characters
   sets and hence probably contains gratuitious incompatibilities.
 |#

; The number of characters that can be in a char-set.
; If you only want small char-sets, try making this 128
; 000-255: 256 booleans #t if ch belongs to the char-set
(define cs-size-max 256)
(define cs-last-idx (- cs-size-max 1))

; at index 256: size
(define cs-size-idx cs-size-max)
(define char-set-vector-size (+ cs-size-max 2))
(define char-set-at vector-ref)

; at index 257: offset
(define cs-offset-idx (- char-set-vector-size 1))
(define (%char-set-offset! cs offset)(vector-set! cs cs-offset-idx offset)) ; set
(define (%char-set-offset  cs)		 (vector-ref  cs cs-offset-idx))		; get
(define (char-set-offset   cs)
	(%char-set-offset (%cs-check cs 'char-set-offset))
)

#| Create a new empty char-set with all slots to #f
(show (%char-set-make))
 |#;->	0:()
(define (%char-set-make)
	(let* ((cs (make-vector char-set-vector-size #f)))
		(%char-set-size! cs 0)(%char-set-offset! cs 0)
)	)
; Returns #t if the object is a CHAR-SET
; This is cheap and cheerful definition at the moment
#|
(char-set? (%char-set-make))
 |#;->	#t
(define (char-set? cs)
	(and (vector? cs)(= (vector-length cs) char-set-vector-size)
		(boolean? (vector-ref cs 0))(integer? (vector-ref cs cs-size-idx))
)	)
;_______________________________	Section 3 begin by Olin Shivers 2000

#| Create a new char-set and initialize it with the optional given chars
(show "Empty: " (char-set) " cs: " (char-set #\C #\h #\a #\r #\space #\S #\e #\t))
 |#;->	Empty: 0:() cs: 8:(#\x20 C S a e h r t)
(define (char-set . chars)
	(let ((cs (%char-set-make)))
		(%list->char-set! chars cs 'char-set)
)	)
#| Return a new empty char-set
(show "new: "(%default-base nil 'Ok1) " cs: "(%default-base (list(char-set #\A)) 'Ok2))
 |#;->	new: 0:() cs: 1:(A)
#| Return the optional char-set in parameter
(%default-base '(too many prm) 'testKo1)	(%default-base '(bug) 'testKo2)
 |#;->	Error: 1400. testKo1 too many parameters 
;		Error: 1401. testKo2 optional parameter is not a char-set 
(define (%default-base maybe-base proc)
	(if (pair? maybe-base)
		(let ((bcs (car maybe-base)) (tail (cdr maybe-base)))
			(if (pair? tail) (error "1400." proc 'too 'many 'parameters)
				(if (char-set? bcs)
					(char-set-copy bcs) ; Copy the char-set in parameter
					(error "1401." proc 'optional 'parameter 'is 'not 'a 'char-set))
		)	)
		(%char-set-make) ; Create a new empty char-set
)	)
;;; If CS is really a char-set, return it, 
;;; otw report an error msg on behalf of our caller, PROC.
;;; This procedure exists basically to provide explicit error-checking & reporting.
#| Check if cs is a char-set
(show (%cs-check (char-set) 'caller))
 |#;->	0:()
(define (%cs-check cs proc)
	(if (char-set? cs) cs (error "1410. " proc ': 'not 'a 'char-set cs))
)
#| Returns a copy of CHAR-SET
(show (char-set-copy char-set:digit))
 |#;->	10:(0 .. 9)
(define (char-set-copy cs)			; Shivers
	(%cs-check cs 'char-set-copy)
	(let ((r (%char-set-make)))		; Bevan
		(let loop ((idx 0))
			(if (= idx char-set-vector-size) r
				(begin (vector-set! r idx (char-set-at cs idx)) (loop (+ 1 idx)))
)	)	)	)
#| Determines if the two char-sets are equal i.e. contain the same characters.
(char-set= (string->char-set "ABC") (ucs-range->char-set 65 68))
(char-set=) (char-set= char-set:empty)
 |#;->	#t
(define (char-set= . lstCharSet)
	(or (null? lstCharSet)
		(let*	(	(cs1 (%cs-check (car lstCharSet) 'char-set=)))
			(let lp ((rest (cdr lstCharSet)))
				(or (not (pair? rest))
					(and (char-set-equal? cs1 (%cs-check (car rest) 'char-set=))
						 (lp (cdr rest))
)	)	)	)	)	)
#| Determines if the two char-sets are equal i.e. contain the same characters.
(char-set-equal? (string->char-set "ABC") (int-range->char-set 65 67))
 |#;->	#t
(define (char-set-equal? csa csb) ; Bevan
	(let loop	(	(idx cs-last-idx)
					(isEqu? (and(=(char-set-size csa)	(char-set-size csb))
								(=(%char-set-offset csa)(%char-set-offset csb))
				)	)		)
		(cond ((not isEqu?) #f)
			  ((< idx 0)  #t)
			  (else (loop (- idx 1) (eq? (char-set-at csa idx) (char-set-at csb idx))))
)	)	)
#| Returns true if every char-set csi is a subset of next char-set csi+1
(char-set<= (char-range->char-set #\A #\Z) (char-range->char-set #\A #\z))
(char-set<=) (char-set<= char-set:empty)
(char-set<= (char-range->char-set #\a #\z) (char-range->char-set #\A #\z))
 |#;->	  #t
#| char-set upper and lower-case is not <= char-set lower-case
(char-set<= (char-range->char-set #\A #\z) (char-range->char-set #\A #\Z))
 |#;->	  #f
(define (char-set<= . lstCharSet)
	(or (null? lstCharSet)
		(let*	((cs1 (car lstCharSet)))
			(let loopNextCs ((s1 (%cs-check cs1 'char-set<=)) (rest (cdr lstCharSet)))
				(or (not (pair? rest))
					(let*	(	(s2 (%cs-check (car rest) 'char-set<=))
								(rest (cdr rest))
							)
						(if (= (%char-set-offset s1)(%char-set-offset s2))
							(if (char-set-equal? s1 s2) (loopNextCs s2 rest)
								(let loopInside ((idx cs-last-idx))
									(cond	((< idx 0)	(loopNextCs s2 rest))
											(	(and	(char-set-at s1 idx)
														(not (char-set-at s2 idx))
											) #f) ; AlSchemist's implementation
											(else (loopInside (- idx 1)))
							)	)	) #f
)	)	)	)	)	)	)
; Returns #t if CHAR is in CHAR-SET
#|
(char-set-contains? (string->char-set "ABC") #\B)
 |#;->	#t
(define (char-set-contains? cs char)
    (char-set-at (%cs-check cs 'char-set-contains?)  ; Bevan
				(%char->cs-index (check-arg char? char 'char-set-contains?)
					(%char-set-offset cs)
)	)			)
; ditto without checking prms
(define (%char-set-contains? cs char)
    (char-set-at cs (%char->cs-index char (%char-set-offset cs)))
)
#| Perf:
(let*	(	(cs (string->char-set "ABC"))
			(nbrRun	10000)(timeEnd 0)(timeStart (gettimeofday))
		)
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
		(char-set-contains? cs #\B)
	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->	593 ms 282 µs for 10K runs. 59 µs by run
#| Comparison with the dedicated primitive
(and	( char-set-contains? char-set:letter #\z) (char-alphabetic? #\z)
		(%char-set-contains? char-set:letter #\z)
)
 |#;->	#t
#| Perf: 
(let*	(	(nbrRun	100000)(timeEnd 0)(timeStart (gettimeofday)))
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
;		(char-alphabetic? #\z)
		(%char-set-contains? char-set:letter #\z)
;		(char-set-contains? char-set:letter #\z)
	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->	 2 s 775 ms 588 µs for 100K runs.  27 µs by run char-alphabetic?
;		 6 s 437 ms  59 µs for 100K runs.  64 µs by run %char-set-contains?
;		13 s 155 ms 759 µs for 100K runs. 131 µs by run char-set-contains?

#| Get the size of the char-set cs
(char-set-size (string->char-set "SIZE"))
 |#;->	4
(define (char-set-size  cs) (%char-set-size (%cs-check cs 'char-set-size)))
; Ditto than above without checking that cs is a char-set
(define (%char-set-size cs) (vector-ref cs cs-size-idx))

; Return the char-set with its new size
(define (%char-set-size! cs size) (vector-set! cs cs-size-idx size))
;
#| Decrement the size of the char-set
(%char-set-size-dec! (char-set) 'testKo)
 |#;->	Error: 1420. testKo cannot decrement char-set size 
(define (%char-set-size-dec! cs proc)
	(let*	((size	(%char-set-size cs)))
		(if (zero? size) (error "1420." proc 'cannot 'decrement 'char-set 'size)) 
		(%char-set-size! cs (- size 1))
)	)
#| Increment the size of the char-set
(%char-set-size-inc! (char-set-complement (char-set)) 'testKo)
 |#;->	Error: 1421. testKo cannot increment char-set size 
(define (%char-set-size-inc! cs proc)
	(let*	((size	(%char-set-size cs)))
		(if (= size cs-size-max) (error "1421." proc 'cannot 'increment 'char-set 'size))
		(%char-set-size! cs (+ size 1))
)	)
#| Return the number of chars that caused the predicate pred to return #t
(char-set-count char-numeric? char-set:full)
 |#;->	10
(define (char-set-count pred cset)
	(check-arg procedure? pred	'char-set-count)
	(let*	(	(cs (%cs-check cset	'char-set-count))
				(offset (%char-set-offset cset))
			)
		(let lp ((idx cs-last-idx) (count 0))
			(if (< idx 0) count
				(lp (- idx 1)
					(if (and	(char-set-at cset idx)
								(pred (%cs-index->char idx offset))
						)
						(+ count 1) count
)	)	)	)	)	)
;;; -- Adjoin & delete
#| Create a new char-set with each character from the list chars and the given char-set
(show (char-set-adjoin (char-set) #\A #\B #\C))
 |#;->	3:(A .. C)
(define (char-set-adjoin cs . chars)
	(let* ((bs (%default-base (list cs) 'char-set-adjoin)))
		(%list->char-set! chars bs 'char-set-adjoin)
)	)
#| Add each character from the list chars in the given char-set
(show (char-set-adjoin! (char-set) #\A #\B #\C))
 |#;->	3:(A .. C)
(define (char-set-adjoin! cs . chars)
	(%list->char-set! chars 
					(check-arg char-set? cs 'char-set-adjoin!) 'char-set-adjoin!
)	)
#| Create a new char-set from the given charset removing the given char
(let*	(	(csRef		(string->char-set "ABC"))		; remain unchanged
			(csAfter	(char-set-delete csRef #\B))	; created char-set minus B
		)
	(show "csRef: " csRef " csAfter: " csAfter)
)
 |#;->	csRef: 3:(A .. C) csAfter: 2:(A C) ; there is not ".." between A and C
(define (char-set-delete cs . chars)
	(check-arg char-set? cs 'char-set-delete)
	(let*	(	(offset (%char-set-offset cs))
				(bs (%default-base (list cs) 'char-set-delete))
			)
		(let loopNextChar ((lstChar chars))
			(if (not-pair? lstChar) bs
				(let*	((idx (%char->cs-index (car lstChar) offset)))
					(if (char-set-at bs idx) 
						(begin	(%char-set-size-dec! bs 'char-set-delete)
								(vector-set! bs idx #f)
					)	)
					(loopNextChar (cdr lstChar))
)	)	)	)	)
#|
(let* ((cs (string->char-set "ABC"))) (char-set-delete! cs #\B) (show cs))
 |#;->	2:(A C)
(define (char-set-delete! cs . chars)
	(check-arg char-set? cs 'char-set-delete!)
	(let*	((offset (%char-set-offset cs)))
		(let loopNextChar ((lstChar chars))
			(if (not-pair? lstChar) cs
				(let*	((idx (%char->cs-index (car lstChar) offset)))
					(if (char-set-at cs idx) 
						(begin	(%char-set-size-dec! cs 'char-set-delete!)
								(vector-set! cs idx #f)
					)	)
					(loopNextChar (cdr lstChar))
)	)	)	)	)
;;; Cursors
;;; Simple implementation. A cursors is an integer index into the
;;; mark vector, and -1 for the end-of-char-set cursor.
#| Return the zero-based index of the first char in the char-set from right to left
(char-set-cursor (string->char-set "ABC"))	(char-set-cursor char-set:full)
 |#;->	67									255
(define (char-set-cursor cset)
	(%char-set-cursor-next cset cs-size-max 'char-set-cursor)
)
#| Return #t if there is not any more character in the char-set from the given cursor
(end-of-char-set? (char-set-cursor char-set:empty))
 |#;->	#t
(define (end-of-char-set? cursor) (<= cursor 0))
#| Return the character of the char-set at the given cursor
(let* ((cs (string->char-set "ABC"))) (char-set-ref cs (char-set-cursor cs)))
 |#;->	#\C
(define (char-set-ref cset cursor) (%cs-index->char cursor (%char-set-offset cset)))
#| Return the next cursor in the char-set then the character at this cursor
(let* ((cs (string->char-set "ABC")) (cur (char-set-cursor cs)))
	(char-set-ref cs (char-set-cursor-next cs cur))
)
 |#;->	#\B
#| Scan a char-set and return the char list of the char-set
(let* ((cs (char-set #\G #\a #\T #\e #\c #\h)))
	(let lp ((cur (char-set-cursor cs)) (ans '()))
		(if (end-of-char-set? cur) ans
			(lp (char-set-cursor-next cs cur)
				(cons (char-set-ref cs cur) ans)
)	)	)	)
 |#;->	(#\G #\T #\a #\c #\e #\h)
#| Alternative version of the scanner using unfold-right
(let* ((cs (char-set #\G #\a #\T #\e #\c #\h)))
	(unfold-right end-of-char-set? (curry char-set-ref cs)
		(curry char-set-cursor-next cs) (char-set-cursor cs)
)	)
 |#;->	(#\G #\T #\a #\c #\e #\h)
; Return the next cursor in the char-set
(define (char-set-cursor-next cset cursor)
	(check-arg	(lambda (idx)	(and (integer? idx) (exact? idx) 
									(<= 0 idx cs-last-idx)
								)
				) cursor 'char-set-cursor-next)
	(%char-set-cursor-next cset cursor 'char-set-cursor-next)
)
(define (%char-set-cursor-next cset cursor proc)
	(%cs-check cset proc)
	(let lp ((cur (- cursor 1)))
		(if (or (end-of-char-set? cur) (char-set-at cset cur)) cur (lp (- cur 1)))
)	)
;;; -- for-each map fold unfold every any
#| Count the number of unique vowel in the given char-set
(let* ((nbrVowel 0) (cs (string->char-set "Laetitia")))
	(char-set-for-each (lambda (ch)(if (vowel? ch) (set! nbrVowel (inc nbrVowel)))) cs)
	nbrVowel
)
 |#;->	3
(define (char-set-for-each proc cs)
	(check-arg procedure? proc 'char-set-for-each)
	(%cs-check cs 'char-set-for-each)
	(let*	((offset (%char-set-offset cs)))
		(let lp ((idx cs-last-idx))
			(cond	(	(>= idx 0)
						(if (char-set-at cs idx) (proc (%cs-index->char idx offset)))
						(lp (- idx 1))
)	)	)	)	)
#| Create a new char-set mapping the given char-set with proc->char
(show (char-set-map char-upcase (string->char-set "abc")))
 |#;->	3:(A .. C)
(define (char-set-map proc->char cs)
	(check-arg procedure? proc->char 'char-set-map)
	(let*	((lstCh	(map proc->char (char-set->list (%cs-check cs 'char-set-map)))))
  		(list->char-set lstCh)
)	)
#| Alternative version of CHAR-SET-SIZE
(char-set-fold (lambda(ch i) (inc i)) 0 (string->char-set "ABC"))
 |#;->	3   ;   ^kons                 ^knil                123
#| How many vowels in the char set?
(char-set-fold (lambda(ch i)(if (vowel? ch)(inc i) i)) 0 (string->char-set "Laetitia"))
 |#;->	3
#| Alternative version of CHAR-SET->LIST
(char-set-fold cons '() (string->char-set "ABC"))
 |#;->	(#\A #\B #\C)
(define (char-set-fold kons knil cs)
	(check-arg procedure? kons 'char-set-fold)
	(%cs-check cs 'char-set-fold)
	(let*	((offset (%char-set-offset cs)))
		(let lp ((idx cs-last-idx) (ans knil))
			(if (< idx 0) ans
				(lp	(- idx 1) 
					(if (char-set-at cs idx) (kons (%cs-index->char idx offset) ans) ans)
)	)	)	)	)
#| Create a new char-set from the given char-set translated by the char predicate pred 
(char-set-every char-upper-case? (string->char-set "ABC"))
 |#;->	#t
(define (char-set-every pred cs)
	(check-arg procedure? pred 'char-set-every)
	(%cs-check cs 'char-set-every)
	(let*	((offset (%char-set-offset cs)))
		(let lp ((idx cs-last-idx))
			(or (< idx 0)
				(and (or (not(char-set-at cs idx)) (pred (%cs-index->char idx offset)))
					(lp (- idx 1))
)	)	)	)	)
#| Does any character of the given char-set return #t for the given predicate pred
(char-set-any char-upper-case? (string->char-set "aBc"))
(char-set-any char-upper-case? (string->char-set "abc"))
 |#;->	#t	#f
(define (char-set-any pred cs)
	(check-arg procedure? pred 'char-set-any)
	(let*	((offset (%char-set-offset cs)))
		(%cs-check cs 'char-set-any)
		(let lp ((idx cs-last-idx))
			(and	(>= idx 0)
					(or (and (char-set-at cs idx) (pred (%cs-index->char idx offset)))
						(lp (- idx 1))
)	)	)	)		)
#|
(show (%char-set-unfold! 'test null? car cdr (string->char-set "ABC") '(#\D #\E)))
 |#;->	5:(A .. E)
(define (%char-set-unfold! proc pNull? first->char gNext cs seed)
	(check-arg procedure? pNull? proc)
	(check-arg procedure? first->char proc)
	(check-arg procedure? gNext proc)
	(let lp ((seed seed))
		(cond	(	(not (pNull? seed)) ; P says we are done
					(%char-set-insert! cs (first->char seed) proc) ; Add (F SEED) to set
					(lp (gNext seed)) ; Loop on (G SEED)
	)	)		)
	cs
)
#| (list->char-set lis)
(show (char-set-unfold null? car cdr '(#\A #\B #\C)))
 |#;->	3:(A .. C)
(define (char-set-unfold pNull? first->char gNext seed . maybe-base)
	(let ((bs (%default-base maybe-base 'char-set-unfold)))
		(%char-set-unfold! char-set-unfold pNull? first->char gNext bs seed)
)	)
;;; list <--> char-set
#|
(show (list->char-set '(#\A #\B #\C)))
 |#;->	3:(A .. C)
(define (list->char-set chars . maybe-base)
	(let ((bs (%default-base maybe-base 'list->char-set)))
		(%list->char-set! chars bs 'list->char-set)
)	)
#| Perf:
(let*	(	(nbrRun	10000)(timeEnd 0)(timeStart (gettimeofday))	)
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
		(list->char-set '(#\A #\B #\C))
	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->	2 s 968 ms 828 µs for 10K runs. 296 µs by run

#|
(let* ((cs (string->char-set "ABC")))(list->char-set! '(#\D #\E #\F) cs)(show cs))
 |#;->	6:(A .. F)
(define (list->char-set! chars base-cs)
	(%list->char-set! chars (%cs-check base-cs 'list->char-set!) 'list->char-set!)
)
#| Create a new char-set which contains all the characters in CHAR-LIST
(show (%list->char-set! '(#\A) (char-set) 'testOk))
 |#;->	1:(A)
(define (%list->char-set! chars base-cs proc)
	(let loop ((l chars))
		(if (null? l) base-cs
			(begin (%char-set-insert! base-cs (car l) proc) (loop (cdr l)))
)	)	)
;--------
; CAUTION: the primitive error is not able to display Unicode character
#| Unicode character inside of string can be displayed in error
(error "testOk1 α")
 |#;->	Error: testOk1 α
#| Unicode character inside of string can be displayed in error
(error "testOk2 " (string #U+03BB) (string (string-ref "α" 0)))
 |#;->	Error: testOk2  "λ" "α" 
#| Unicode character outside of string cannot be displayed in error
(display "anything")(error "testKo1: " (string-ref "α" 0))
 |#;->	
 ;     ^display nothing. Even "anything" is not displayed.
#| Unicode character cannot be displayed in error
(error "testKo2: " #U+03BB)
 |#;->	
 ;     ^display nothing
;--------
#| Creates a new char-set from the integer list ints
(show (int-list->char-set (iota 5 65 2)))
 |#;->	5:(A C E G I)
(define (int-list->char-set ints)
	(let ((cs (%char-set-make)))
		(let loop ((lst ints))
			(if (null? lst) cs
				(begin
					(%char-set-insert! cs (integer->char (car lst)) 'int-list->char-set)
					(loop (cdr lst))
)	)	)	)	)
#| Insert the character ch in the char-set cs
(show (%char-set-insert! (char-set) #\A 'testAsciiOk1))
 |#;->	1:(A)           ; newly created empty char-set
#| Insert the character ch in the char-set cs
(show (%char-set-insert! (string->char-set "α") #U+03C9 'testGreekOk))
 |#;->	2:(α ω) ;                                ^ω: lowercase omega
#| Cannot add an Ascii char in a Unicode Greek char-set
(%char-set-insert! (string->char-set "α") #\o 'testKo1)
 |#;->	Error: 1430. testKo1 cannot insert "#U+006f" offset 0 with char-set offset 768 
#| Cannot add an Unicode greek char in an Ascii char-set
(%char-set-insert! (string->char-set "omega") #U+03C9 'testKo2)
 |#;->	Error: 1430. testKo2 cannot insert "#U+03c9" offset 768 with char-set offset 0 
(define (%char-set-insert! cs ch proc)
	(let*	(	(offsetRef	(%char-set-offset cs))
				(offsetNew	(%offset (char->integer ch)))
			)
		(if (not (= offsetRef offsetNew))
			(if (zero? (%char-set-size cs)) ; only the first ch can change offset
				(begin	(set! offsetRef offsetNew) ; becomes the new ref
						(%char-set-offset! cs offsetNew)
				)
				(error "1430." proc 'cannot 'insert (unicode-char->string ch)
					'offset offsetNew 'with 'char-set 'offset offsetRef
		)	)	)
		(let* ((idx (%char->cs-index ch offsetRef)))
			(if (char-set-at cs idx) cs
				(begin	(%char-set-size-inc! cs proc)
						(vector-set! cs idx #t)
)	)	)	)	)
; Converts the character set into a list of characters.
; Note the order of the list is not specified, the only guarantee is
; that an element in the char-set will only appear once in the set.
#|
(char-set->list (char-set))		(char-set->list (string->char-set "ABC"))
 |#;->	()						(#\A #\B #\C)
(define (char-set->list cs)
	(check-arg char-set? cs 'char-set->list)
	(let*	((offset (%char-set-offset cs)))
		(let loop ((idx cs-last-idx) (r nil))
			(if (< idx 0) r
				(loop (- idx 1) (if (char-set-at cs idx) 
									(cons (%cs-index->char idx offset) r) r
)	)	)	)	)				)
;-
;;; string <--> char-set

#| Converts STRING into a CHAR-SET
(show (string->char-set! "ABC" (char-set) 'caller))
 |#;->	3:(A .. C)
; every character in the string is added to the character set.
(define (%string->char-set! str bs proc)
	(check-arg string? str proc)
	(let ((len (string-length str)))
		(let loop ((i 0))
			(if (= i len) bs
				(begin (%char-set-insert! bs (string-ref str i) proc) (loop (+ i 1)))
)	)	)	)
#| Return a character set containing the characters in the string str
(show (string->char-set "ABC"))
 |#;->	3:(A .. C)
(define (string->char-set str . maybe-base)
	(let ((bs (%default-base maybe-base 'string->char-set)))
		(%string->char-set! str bs 'string->char-set)
)	)
#| Perf:
(let*	(	(nbrRun	10000)(timeEnd 0)(timeStart (gettimeofday))	)
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
		(string->char-set "ABC")
	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->	3 s 248 ms 424 µs for 10K runs. 324 µs by run

#| Update the given char-set with the characters in string str
(show (string->char-set! "ABC" (char-set #\A #\C)))
 |#;->	3:(A .. C)
(define (string->char-set! str base-cs)
	(%string->char-set! str (%cs-check base-cs string->char-set!) 'string->char-set!)
)
; Convert the char-set into a string
; The order of the characters in the resulting string is the ascending ascii code.
; We guarantee that each element of the char-set will only appear once in the string.
#|
(show (char-set->string (string->char-set "ABC")))
 |#;->	ABC
(define (char-set->string cs)
	(check-arg char-set? cs 'char-set->string)
	(let*	(	(offset (%char-set-offset cs))
				(len (%char-set-size cs)) (str (make-string len))
			)
		(let loop ((idxCs cs-last-idx) (indStr (- len 1)))
			(if (< idxCs 0) str
				(if (char-set-at cs idxCs)
					(begin	(string-set! str indStr (%cs-index->char idxCs offset))
							(loop (- idxCs 1) (- indStr 1))
					)
					(loop (- idxCs 1) indStr)
)	)	)	)	)
;;; -- UCS-range -> char-set
#| Creates new char-set in the ASCII range [LOWER-BOUND UPPER-BOUND[ not inclusive
(show (ucs-range->char-set 65 91))
 |#;->	26:(A .. Z)			;  ^caution: for Z = 90 indicate the next 91
(define (ucs-range->char-set lower upper . maybe-error?-base-cs)
	(let*	(	(error? (if (pair? maybe-error?-base-cs) (car maybe-error?-base-cs) #f))
				(bs (%default-base	(if (> (length maybe-error?-base-cs) 1)
										(cdr maybe-error?-base-cs) nil
									) 'ucs-range->char-set
			)	)	)
		(%ucs-range->char-set! lower upper error? bs 'ucs-range->char-set)
)	)
#| Perf:
(let*	(	(nbrRun	10000)(timeEnd 0)(timeStart (gettimeofday))	)
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
		(ucs-range->char-set 65 91 #t (char-set))
	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->	51 s 515 ms 161 µs for 10K runs. 5 ms 151 µs by run

#| Update char-set with the int range [LOWER-BOUND UPPER-BOUND[ not inclusive
(show (ucs-range->char-set! 97 123 #t (char-set)))
 |#;->	26:(a .. z)
#| Update char-set with the int range [LOWER-BOUND UPPER-BOUND[ not inclusive
(char-set-equal? (ucs-range->char-set! 97 123 #t (char-range->char-set #\A #\Z))
	(char-set-union (char-range->char-set #\a #\z) (char-range->char-set #\A #\Z))
)
 |#;->	#t
(define (ucs-range->char-set! lower upper error? base-cs)
	(%ucs-range->char-set! lower upper error? (%cs-check base-cs 'ucs-range->char-set!)
			'ucs-range->char-set!)
	base-cs
)
#| Perf:
(let*	(	(nbrRun	10000)(timeEnd 0)(timeStart (gettimeofday))	)
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
		(ucs-range->char-set! 65 91 #t (char-set))
	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->	12 s 327 ms 957 µs for 10K runs. 1 ms 232 µs by run

#| Creates new char-set in the ASCII range [LOWER-BOUND UPPER-BOUND[ not inclusive
(show (%ucs-range->char-set! 65 91 #f (char-set) 'caller))
 |#;->	26:(A .. Z) ;            ^caution: for Z = 90 indicate the next 91
(define (%ucs-range->char-set! lower upper error? bs proc)
	(check-arg (lambda (x) (and (integer? x) (exact? x) (<= 0 x))) lower proc)
	(check-arg (lambda (x) (and (integer? x) (exact? x) (<= lower x))) upper proc)
	(if (and (< lower upper) (< cs-size-max upper) error?)
		(error "1440. " proc ': 'Requested 'UCS 'range 'contains 'unavailable
			   'characters 'outside 'Latin-1 lower upper
	)	)
	(let lp ((idx (min (- upper 1) cs-last-idx)))
    	(cond	(	(<= lower idx)
					(if (not (char-set-at bs idx))
						(begin	(%char-set-size-inc! bs proc)
								(vector-set! bs idx #t)
					)	)
					(lp (- idx 1))
	)	)		)
	bs
)
#| Creates new char-set in the integer range [LOWER-BOUND UPPER-BOUND] inclusive
(show (int-range->char-set 65 90))
 |#;->	26:(A .. Z)
(define (int-range->char-set ilb iub)
	(check-arg (lambda(x)(and(integer? x)(exact? x)(<= 0 x))) ilb 'int-range->char-set)
	(check-arg (lambda(x)(and(integer? x)(exact? x)(<= ilb x))) iub 
		'int-range->char-set
	)
	(%int-range->char-set ilb iub (%char-set-make) 'int-range->char-set)
)
#| Perf: checking prm costs 42 µs so 8%
(let*	(	(nbrRun	10000)(timeEnd 0)(timeStart (gettimeofday))	)
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
		(int-range->char-set 65 90)
	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->	5 s 15 ms 658 µs for 10K runs. 501 µs by run

#| ditto than above int-range->char-set but don't check integer prm
(show (%int-range->char-set 65 90 (char-set) 'testOk))
 |#;->	26:(A .. Z)
#| The supplied range ilb=255 .. iub=256 is not in the same 256-based offset
(%int-range->char-set 255 256 (char-set) 'testKo)
 |#;->	Error: 1010. testKo: (lambda (offsetEnd) (= offset offsetEnd)) 256 failed. 
; The given cs must be empty
(define (%int-range->char-set ilb iub cs proc)
	(let*	((sizeCs (+ (- iub ilb) 1)))
		(check-arg	(lambda(size)(<= size cs-size-max)) sizeCs proc)
		(let*	(	(offset (if (< ilb cs-size-max) 0 (%offset ilb)))
					(iStart	(- ilb offset))
					(iEnd	(- iub offset))
				)
			(%char-set-size! cs sizeCs)
			(if (> offset 0) (%char-set-offset! cs offset))
			(check-arg (lambda (offsetEnd) (= offset offsetEnd)) (%offset iub) proc)
			(let loop ((idx iStart))
				(if (> idx iEnd) cs
					(begin	(vector-set! cs idx #t)	(loop (+ 1 idx)))
)	)	)	)	)
#| Perf:
(let*	(	(nbrRun	10000)(timeEnd 0)(timeStart (gettimeofday))	)
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
		(%int-range->char-set 65 90 (%char-set-make) 'testPerf)
	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->	4 s 593 ms 262 µs for 10K runs. 459 µs by run

#| Creates new char-set containing characters [LOWER-BOUND UPPER-BOUND] inclusive
(show (char-range->char-set #\A #\Z))
 |#;->	26:(A .. Z)
(define (char-range->char-set clb cub)
	(%int-range->char-set (char->integer clb) (char->integer cub) (%char-set-make)
		'char-range->char-set
)	)
;;; -- predicate -> char-set
#| Returns a char-set containing every character c in cs such that (pred c) returns #t
(show (%char-set-filter! char-upper-case? char-set:full (char-set) 'caller))
 |#;->	56:(A .. Z À .. Ö Ø .. Þ)
(define (%char-set-filter! pred ds bs proc)
	(check-arg procedure? pred proc)
	(let*	((offset (%char-set-offset ds)))
		(let lp ((idx cs-last-idx))
			(if (< idx 0) bs
				(begin	(if (and	(char-set-at ds idx) ; domain char-set
									(not (char-set-at bs idx)) ; target char-set
									(pred (%cs-index->char idx offset)) ; char predicate
							)
							(begin	(vector-set! bs idx #t) ; update the target
									(%char-set-size-inc! bs proc)
						)	)
						(lp (- idx 1))
)	)	)	)	)
#| Returns a char-set containing every character c in cs such that (pred c) returns #t
(show (char-set-filter char-lower-case? char-set:full))
 |#;->	59:(a .. z µ ß .. ö ø .. ÿ)
(define (char-set-filter predicate domain . maybe-base)
	(let ((bs (%default-base maybe-base					'char-set-filter)))
		(%char-set-filter! predicate (%cs-check domain	'char-set-filter)
			bs											'char-set-filter
)	)	)
#|
(show (char-set-filter! char-whitespace? char-set:full (char-set)))
 |#;->	6:(#\x9 #\xa #\xc #\xd #\x20 #\xa0)
(define (char-set-filter! predicate domain base-cs)
	(%char-set-filter! predicate (%cs-check domain	'char-set-filter!)
		(%cs-check base-cs 'char-set-filter!)		'char-set-filter!
)	)
#| {string, char, char-set, char predicate} -> char-set
(show (->char-set "ABC") " " (->char-set #\A))
 |#;->	3:(A .. C) 1:(A)
(define (->char-set x)
	(cond	((char-set? x) x)
			((string? x) (string->char-set x))
			((char? x) (char-set x))
			(else (error "1450. ->char-set: Not a charset, string or char: " x))
)	)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Set algebra
;;; The exported ! procs are "linear update" -- allowed, but not required, to
;;; side-effect their first argument when computing their result. In other
;;; words, you must use them as if they were completely functional, just like
;;; their non-! counterparts, and you must additionally ensure that their
;;; first arguments are "dead" at the point of call. In return, we promise a
;;; more efficient result, plus allowing you to always assume char-sets are
;;; unchangeable values.
;;; String S represents some initial char-set. (OP s i val) does some
;;; kind of s[i] := s[i] op val update. Do
;;;     S := S OP CSETi
;;; for all the char-sets in the list CSETS. The n-ary set-algebra ops
;;; all use this internal proc.

;;; These do various "s[i] := s[i] op val" operations -- see %CHAR-SET-ALGEBRA. 
;;; They are used to implement the various set-algebra procedures.
; SET a boolean value to the char-set cs at index idx
#|
(show (cs-setval! (char-set) 65 #t 'caller))
 |#;->	1:(A)
#|
(show (cs-setval! (string->char-set "ABC") 65 #f 'caller))
 |#;->	2:(B C)
(define (cs-setval! cs idx val proc)
	(let*	((prev	(char-set-at cs idx)))
		(if (eq? val prev) cs
			(begin
				(if val (%char-set-size-inc! cs proc)(%char-set-size-dec! cs proc))
				(vector-set! cs idx val)
)	)	)	)
#| Remove the char "A" from the char-set using the logical operator not
(show(%cs-not! (string->char-set "ABC") 65 #t 'caller))
 |#;->	2:(B C)
(define (%cs-not!   cs idx val proc) (cs-setval! cs idx (not val) proc))
#| Logical and in the char-set for the char "A" and #f
(show(%cs-and! (string->char-set "ABC") 65 #f 'caller))
 |#;->	2:(B C)
(define (%cs-and!   cs idx val proc) (if (not val) (cs-setval! cs idx #f proc)))
#| Logical or in the char-set about the char "A" or #t
(show(%cs-or! (string->char-set "BC") 65 #t 'caller))
 |#;->	3:(A .. C)
(define (%cs-or!	cs idx val proc) (if val (cs-setval! cs idx #t proc)))
#| char-set "ABC" - "A" removes "A"
(show(%cs-minus! (string->char-set "ABC") 65 #t 'caller))
 |#;->	2:(B C)
(define (%cs-minus! cs idx val proc) (if val (cs-setval! cs idx #f proc)))
#| char-set "ABC"  xor "A" removes "A"
(show(%cs-xor! (string->char-set "ABC") 65 #t 'caller))
 |#;->	2:(B C)
(define (%cs-xor!   cs idx val proc)
	(if val (cs-setval! cs idx (not (char-set-at cs idx)) proc))
)
#| Return the char-set resultat of (op cs cs2 cs3 ...)
(show(%char-set-algebra (string->char-set "ABC") (list(string->char-set "Aaz"))
	%cs-or! 'testOk)
)
 |#;->	5:(A .. C a z)
#| Algebra operators are not available for char-sets of different offets
(%char-set-algebra (string->char-set "Lambda") (list(string->char-set "λ"))
	%cs-not! 'testKo ;				  ^Latin                           ^Greek
)
 |#;->	Error: 1010. testKo: (lambda (offset2) (= offset1 offset2)) 768 failed. 
(define (%char-set-algebra cs csets op proc)
	(let*	((offset1 (%char-set-offset  cs)))
		(for-each
			(lambda (cset)
				(let ((cs2 (%cs-check cset proc)))
					(check-arg (lambda(offset2)(= offset1 offset2))
						(%char-set-offset cs2) proc
					)
					(let lp ((idx cs-last-idx))
						(cond	((>= idx 0) 
									(op cs idx (char-set-at cs2 idx) proc)
									(lp (- idx 1))
			)	)	)	)		)
			csets
	)	)
	cs
)
;;; -- Complement
#|
(show (char-set-complement  (string->char-set "ABC")))
(show (char-set-complement! (string->char-set "ABC")))
 |#;->	253:(#\x0 .. @ D .. #\xff)
(define (char-set-complement csSrc)
	(%cs-check csSrc 'char-set-complement)
	(let ((csComp (char-set)))
		(let lp ((idx cs-last-idx))
			(cond
				((>= idx 0)
					(%cs-not! csComp idx (char-set-at csSrc idx) 'char-set-complement)
					(lp (- idx 1))
		)	)	)
		csComp
)	)
(define (char-set-complement! cset)
	(let ((cs (%cs-check cset 'char-set-complement!)))
		(let lp ((idx cs-last-idx))
			(cond
				((>= idx 0)
					(%cs-not! cset idx (char-set-at cset idx) 'char-set-complement!)
					(lp (- idx 1))
	)	)	)	)
	cset
)
;;; -- Union
#|
(show (char-set-union (string->char-set "AB")(string->char-set "BC")))
 |#;->	3:(A .. C)
(define (char-set-union . csets)
	(if (pair? csets)
		(let ((cs (char-set-copy (%cs-check (car csets) 'char-set-union)))) ; first
			(%char-set-algebra cs (cdr csets) %cs-or! 'char-set-union) ; rest
		)
		(char-set-copy char-set:empty)
)	)

#| Modifies A such that it contains the union of A and B
(show (char-set:union! (string->char-set "AB")(string->char-set "BC")))
 |#;->	3:(A .. C)
(define (char-set:union! csa csb)
	(check-arg	(lambda(offsetb)(=	(%char-set-offset csa) offsetb))
				(%char-set-offset csb) 'char-set:union!
	)
	(let loop ((idx cs-last-idx))
		(if (< idx 0) csa
			(begin
				(if (and (char-set-at csb idx) (not (char-set-at csa idx)))
					(begin	(vector-set! csa idx #t)
							(%char-set-size-inc! csa 'char-set:union!)
				)	)
				(loop (- idx 1))
)	)	)	)
#| Returns a new char-set containing the union of csa and csb
(let*	(	(csLeft (string->char-set "AB")) (csRight (string->char-set "BC"))
			(csUnion(char-set:union csLeft csRight))
		)
	(show "csUnion " csUnion " is " csLeft " + " csRight)
)
 |#;->	csUnion 3:(A .. C) is 2:(A B) + 2:(B C)
(define (char-set:union csa csb)
	(char-set:union! (char-set-copy csa) csb)
)
;;; -- Intersection
#|
(show (char-set-intersection! (string->char-set "abcd") (string->char-set "bc")))
 |#;->	2:(b c)
(define (char-set-intersection! cset1 . csets)
	(%char-set-algebra (%cs-check cset1 'char-set-intersection!)
		csets %cs-and! 'char-set-intersection!
)	)
; Modifies A such that it contains the intersection of the sets A and B.
#|
(show (char-set:intersection! (string->char-set "abcd") (string->char-set "bc")))
(show (char-set-intersection  (string->char-set "abcd") (string->char-set "bc")))
 |#;->	2:(b c)
(define (char-set:intersection! csa csb)
	(let loop ((idx cs-last-idx))
		(if (< idx 0) csa
			(begin
				(if (and (not (char-set-at csb idx)) (char-set-at csa idx))
					(begin	(vector-set! csa idx #f)
							(%char-set-size-dec! csa char-set:intersection!)
				)	)
				(loop (- idx 1))
)	)	)	)
(define (char-set-intersection . csets)
	(if (pair? csets)
		(let ((cs (%cs-check (car csets) 'char-set-intersection)))
			(%char-set-algebra cs (cdr csets) %cs-and! 'char-set-intersection)
		)
		(char-set-copy char-set:full)
)	)
;;; -- Difference
#|
(show (char-set-difference! (string->char-set "abcd") (string->char-set "bc")))
(show (char-set-difference  (string->char-set "abcd") (string->char-set "bc")))
 |#;->	2:(a d)
(define (char-set-difference! cset1 . csets)
	(%char-set-algebra (%cs-check cset1 'char-set-difference!)
		csets %cs-minus! 'char-set-difference!
	)
	cset1
)
; Modifies A such that it contains the difference between A and B.
#|
 (show (char-set:difference! (string->char-set "abcd") (string->char-set "bce")))
 |#;->	2:(a d)
(define (char-set:difference! csa csb)
	(let loop ((idx cs-last-idx))
		(if (< idx 0) csa
			(begin
				(if (and (char-set-at csb idx) (char-set-at csa idx))
					(begin	(vector-set! csa idx #f)
							(%char-set-size-dec! csa 'char-set:difference!)
				)	)
				(loop (- idx 1))
)	)	)	)
(define (char-set-difference cs1 . csets)
	(if (pair? csets)
		(let ((cs (%cs-check cs1 'char-set-difference)))
			(%char-set-algebra cs csets %cs-minus! 'char-set-difference))
		(char-set-copy cs1)
)	)
;;; -- Xor
#|
(show (char-set-xor! (string->char-set "abcd") (string->char-set "bce")))
(show (char-set-xor  (string->char-set "abcd") (string->char-set "bce")))
 |#;->	3:(a d e)
(define (char-set-xor! cset1 . csets)
	(%char-set-algebra (%cs-check cset1 'char-set-xor!) csets %cs-xor! 'char-set-xor!)
)
(define (char-set-xor . csets)
	(if (pair? csets)
		(let ((cs (%cs-check (car csets) 'char-set-xor)))
			(%char-set-algebra cs (cdr csets) %cs-xor! 'char-set-xor)
		)
		(char-set-copy char-set:empty)
)	)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; System character sets
;;; These definitions are for Latin-1.
#|
(show "empty: " char-set:empty	" full: " char-set:full)
 |#;->	empty: 0:() full: 256:(#\x0 .. #\xff)
(define char-set:empty (char-set))
(define char-set:full (char-set-complement char-set:empty))
#|
(show char-set:lower-case)
(char-set= char-set:lower-case (char-set-filter char-lower-case? char-set:full))
 |#;->	59:(a .. z µ ß .. ö ø .. ÿ)							   #t
(define char-set:lower-case
	(let*	(	(cs	(%char-set-size!(%int-range->char-set #xdf #xff
										(%int-range->char-set #x61 #x7A ; a .. z
											(%char-set-make)	'char-set:lower-case
										)						'char-set:lower-case
									) 59
			)	)	)
		(char-set-delete! cs #\xf7)
		(char-set-adjoin! cs #\xb5)
)	)
#| Perf:
(let*	(	(nbrRun	10000)(timeEnd 0)(timeStart (gettimeofday)))
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
		(define char-set:lower-caseSlow
			(let*	(	(a-z	(ucs-range->char-set #x61 #x7B))
						(latin1 (ucs-range->char-set! #xdf #xf7  #t a-z))
						(latin2 (ucs-range->char-set! #xf8 #x100 #t latin1))
					)
				(char-set-adjoin! latin2 (integer->char #xb5))
		)	)
	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->	11 s 532 ms 831 µs for 10K runs. 1 ms 153 µs by run
;		29 s 100 ms 997 µs for 10K runs. 2 ms 910 µs by run ucs-range->char-set

#|
(show char-set:upper-case)
(char-set= char-set:upper-case (char-set-filter char-upper-case? char-set:full))
 |#;->	56:(A .. Z À .. Ö Ø .. Þ)							   #t
(define char-set:upper-case
	(%char-set-size!
		(%int-range->char-set #xd8 #xde ;; Add in the Latin-1 upper-case chars.
			(%int-range->char-set #xc0 #xd6 
				(%int-range->char-set #x41 #x5A ; A .. Z
					(%char-set-make)	'char-set:upper-case
				)						'char-set:upper-case
			)							'char-set:upper-case
		)
		56
)	)
#| Perf:
(let*	(	(nbrRun	10000)(timeEnd 0)(timeStart (gettimeofday)))
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
		(define char-set:upper-caseSlow
			(let*	(	(A-Z	(ucs-range->char-set #x41 #x5B)))
				;; Add in the Latin-1 upper-case chars.
				(ucs-range->char-set! #xd8 #xdf #t
					(ucs-range->char-set! #xc0 #xd7 #t A-Z)
		)	)	)
	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->	 9 s 773 ms 126 µs for 10K runs. 	  977 µs by run
;		26 s 927 ms 785 µs for 10K runs. 2 ms 692 µs by run

#| char-set containing the alphabetic characters i.e. A-Z and a-z
(show char-set:letter)
(char-set= char-set:letter (char-set-filter char-alphabetic? char-set:full))
 |#;->	117:(A .. Z a .. z ª µ º À .. Ö Ø .. ö ø .. ÿ)		#t
(define char-set:letter
	(let*	(	(u/l (char-set-union char-set:upper-case char-set:lower-case)))
		(char-set-adjoin! u/l	#\xaa	; FEMININE ORDINAL INDICATOR
								#\xba	; MASCULINE ORDINAL INDICATOR
)	)	)
#| char-set containing the digits 0 - 9: (string->char-set "0123456789")
(show char-set:digit)
 |#;->	10:(0 .. 9)
(define char-set:digit (char-range->char-set #\0 #\9))
#| Perf:
(let*	(	(nbrRun	10000)(timeEnd 0)(timeStart (gettimeofday)))
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
		(define char-set:digit (char-range->char-set #\0 #\9))
	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->	2 s 644 ms 58 µs for 10K runs. 264 µs by run (char-range->char-set #\0 #\9)

#|
(show char-set:hex-digit)
 |#;->	22:(0 .. 9 A .. F a .. f)
(define char-set:hex-digit
	(%char-set-size!
		(%int-range->char-set #x61 #x66 ; a .. f
			(%int-range->char-set #x41 #x46 ; A .. F
				(%int-range->char-set #x30 #x39 ; 0 .. 9
					(%char-set-make)	'char-set:hex-digit
				)						'char-set:hex-digit
			)							'char-set:hex-digit
		)
		22
)	)
#| Perf:
(let*	(	(nbrRun	10000)(timeEnd 0)(timeStart (gettimeofday)))
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
		(define char-set:hex-digitSlow (string->char-set "0123456789abcdefABCDEF"))
	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->	 5 s 717 ms 502 µs for 10K runs. 	  571 µs by run
;		17 s 313 ms 710 µs for 10K runs. 1 ms 731 µs by run

#|
(show char-set:letter+digit)
 |#;->	127:(0 .. 9 A .. Z a .. z ª µ º À .. Ö Ø .. ö ø .. ÿ)
 (define char-set:letter+digit (char-set-union char-set:letter char-set:digit))
#|
(show char-set:punctuation)
 |#;->	29:(! .. # % .. * , .. / : ; ? @ [ .. ] _ { } ¡ « ­ · » ¿)
(define char-set:punctuation
	(%list->char-set!
	   '(#\! #\" #\# #\% #\& #\' #\( #\) #\* #\, #\- #\. #\/ #\: #\; #\? #\@ #\[ #\\
		 #\] #\_ #\{ #\}
		 #\xA1 #\xAB #\xAD #\xB7 #\xBB #\xBF
		; INVERTED EXCLAMATION MARK, LEFT-POINTING DOUBLE ANGLE QUOTATION MARK,
		; SOFT HYPHEN, MIDDLE DOT, RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
		; INVERTED QUESTION MARK
		) (%char-set-make) 'char-set:punctuation
)	)
#| Perf:
(let*	(	(nbrRun	10000)(timeEnd 0)(timeStart (gettimeofday)))
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
(define char-set:punctuationSlow
	(let	(	(ascii (string->char-set "!\"#%&'()*,-./:;?@[\\]_{}"))
				(latin-1-chars 
					(map integer->char 
					   '(#xA1 ; INVERTED EXCLAMATION MARK
					    #xAB ; LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
					    #xAD ; SOFT HYPHEN
					    #xB7 ; MIDDLE DOT
					    #xBB ; RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
					    #xBF ; INVERTED QUESTION MARK
			)	)	)	)
		(list->char-set! latin-1-chars ascii)
)	)
	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->	21 s 526 ms 922 µs for 10K runs. 2 ms 152 µs by run %list->char-set!
;		27 s 404 ms 869 µs for 10K runs. 2 ms 740 µs by run string->char-set
#|
(show char-set:symbol)
 |#;->	27:($ + < .. > ^ ` | ~ ¢ .. © ¬ ® .. ± ´ ¶ ¸ × ÷)
(define char-set:symbol
	(%list->char-set! 
	   '(#\$ #\+ #\< #\= #\> #\^ #\` #\| #\~
		 #\xAC #\xAE #\xAF #\xB0 ; NOT SIGN, REGISTERED SIGN, MACRON, DEGREE SIGN
		 #\xB1 #\xB4 #\xB6 #\xB8 ; PLUS-MINUS SIGN, ACUTE ACCENT, PILCROW SIGN, CEDILLA
		 #\xD7 #\xF7 ; MULTIPLICATION SIGN, DIVISION SIGN
		)
		(%int-range->char-set #xA2 #xA9 (%char-set-make) 'char-set:symbol)
		; CENT SIGN, POUND SIGN, CURRENCY SIGN, YEN SIGN
		; BROKEN BAR, SECTION SIGN, DIAERESIS, COPYRIGHT SIGN
		'char-set:symbol
)	)
#| Perf:
(let*	(	(nbrRun	10000)(timeEnd 0)(timeStart (gettimeofday)))
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
		(define char-set:symbolSlow
			(let	(	(ascii (string->char-set "$+<=>^`|~"))
						(latin-1-chars
								(map integer->char
								   '(#x00A2 ; CENT SIGN
									 #x00A3 ; POUND SIGN
									 #x00A4 ; CURRENCY SIGN
									 #x00A5 ; YEN SIGN
									 #x00A6 ; BROKEN BAR
									 #x00A7 ; SECTION SIGN
									 #x00A8 ; DIAERESIS
									 #x00A9 ; COPYRIGHT SIGN
									 #x00AC ; NOT SIGN
									 #x00AE ; REGISTERED SIGN
									 #x00AF ; MACRON
									 #x00B0 ; DEGREE SIGN
									 #x00B1 ; PLUS-MINUS SIGN
									 #x00B4 ; ACUTE ACCENT
									 #x00B6 ; PILCROW SIGN
									 #x00B8 ; CEDILLA
									 #x00D7 ; MULTIPLICATION SIGN
									 #x00F7 ; DIVISION SIGN
					)	)		)	)
			(list->char-set! latin-1-chars ascii)
		)	)
	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->	15 s 897 ms 364 µs for 10K runs. 1 ms 589 µs by run %list->char-set! 
;		33 s 780 ms 999 µs for 10K runs. 3 ms 378 µs by run string->char-set

#|
(show char-set:graphic)
 |#;->	183:(! .. ~ ¡ .. ± ´ .. ¸ º » ¿ .. ÿ)
(define char-set:graphic
	(char-set-union char-set:letter+digit char-set:punctuation char-set:symbol)
)
#| CHAR-SET: horizontal Tab, LF, vertical Tab, Form Feed, CR, Space, No-break space
(show char-set:whitespace)
 |#;->	7:(#\x9 .. #\xd #\x20 #\xa0)
(define char-set:whitespace
	(%char-set-size!	(%int-range->char-set #x09 #x0D ; #\x09 .. #\x0D
							(char-set #\x20 #\xA0)	'char-set:whitespace
						)
		7
)	)
#| Perf:
(let*	(	(nbrRun	10000)(timeEnd 0)(timeStart (gettimeofday)))
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
		(define char-set:whitespaceSlow
			(int-list->char-set '(#x09 #x0A #x0B #x0C #x0D #x20 #xA0))
		)
	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->	3 s 807 ms 178 µs for 10K runs. 380 µs by run %char-set-size!   char-set
;		3 s 866 ms 595 µs for 10K runs. 386 µs by run char-set-adjoin! %char-set-make
;		6 s  31 ms 177 µs for 10K runs. 603 µs by run int-list->char-set

#| ; NO-BREAK SPACE
(show char-set:printing)
 |#;->	190:(#\x9 .. #\xd #\x20 .. ~ #\xa0 .. ± ´ .. ¸ º » ¿ .. ÿ)
(define char-set:printing (char-set-union char-set:whitespace char-set:graphic))

#| CHAR-SET containing HORIZONTAL TABULATION, SPACE, NO-BREAK SPACE
(show char-set:blank)
 |#;->	3:(#\x9 #\x20 #\xa0)
(define char-set:blank		(int-list->char-set '(#x09 #x20 #xA0)))
#| Perf:
(let*	(	(nbrRun	10000)(timeEnd 0)(timeStart (gettimeofday)))
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
		(define char-set:blankSlow
			(list->char-set (map integer->char
							   '(#x09 ; HORIZONTAL TABULATION
								 #x20 ; SPACE
								 #xA0 ; NO-BREAK SPACE
		)	)				)	)
	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->	2 s 980 ms 818 µs for 10K runs. 298 µs by run(int-list->char-set '(#x09 #x20 #xA0))
;		3 s  27 ms 484 µs for 10K runs. 302 µs by run(char-set #\x09 #\x20 #\xA0)
;		5 s 191 ms 417 µs for 10K runs. 519 µs by run

#|
(show char-set:iso-control)
 |#;->	65:(#\x0 .. #\x1f #\x7f .. #\x9f)
(define char-set:iso-control
	(%char-set-size!
		(%int-range->char-set #x7F #x9F
			(%int-range->char-set 0 31	(%char-set-make)	'char-set:iso-control
			)												'char-set:iso-control
		)
		65
)	)
#| Perf:
(let*	(	(nbrRun	10000)(timeEnd 0)(timeStart (gettimeofday)))
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
		(define char-set:iso-controlSlow
			(ucs-range->char-set! #x7F #xA0 #t (ucs-range->char-set 0 32))
		)
	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->	10 s  22 ms 874 µs for 10K runs. 1 ms   2 µs by run %int-range->char-set
;		29 s 364 ms 499 µs for 10K runs. 2 ms 936 µs by run ucs-range->char-set

 #|
(show char-set:ascii)
 |#;->	128:(#\x0 .. #\x7f)
(define char-set:ascii (%int-range->char-set 0 127 (%char-set-make) 'char-set:ascii))
#| Perf:
(let*	(	(nbrRun	10000)(timeEnd 0)(timeStart (gettimeofday)))
	(do ((idx 0 (inc idx)))((>= idx nbrRun) (set! timeEnd (gettimeofday)))
		(define char-set:asciiSlow (ucs-range->char-set 0 128))
	)	(time-stat timeStart timeEnd nbrRun)
)
 |#;->	16 s 569 ms   7 µs for 10K runs. 1 ms 656 µs by run %int-range->char-set
;		53 s 441 ms 378 µs for 10K runs. 5 ms 344 µs by run ucs-range->char-set

;;; Porting & performance-tuning notes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; See the section at the beginning of this file on external dependencies.
;;;
;;; First and foremost, rewrite this code to use bit vectors of some sort.
;;; This will give big speedup and memory savings.
;;;
;;; - LET-OPTIONALS* macro.
;;; This is only used once. You can rewrite the use, port the hairy macro
;;; definition (which is implemented using a Clinger-Rees low-level
;;; explicit-renaming macro system), or port the simple, high-level
;;; definition, which is less efficient.
;;;
;;; - :OPTIONAL macro
;;; Very simply defined using an R5RS high-level macro.
;;;
;;; Implementations that can arrange for the base char sets to be immutable
;;; should do so. (E.g., Scheme 48 allows one to mark a string as immutable,
;;; which can be used to protect the underlying strings.) It would be very,
;;; very bad if a client's buggy code corrupted these constants.
;;;
;;; There is a fair amount of argument checking. This is, strictly speaking,
;;; unnecessary -- the actual body of the procedures will blow up if an
;;; illegal value is passed in. However, the error message will not be as good
;;; as if the error were caught at the "higher level." Also, a very, very
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
;;; *especially* the many optional BASE-CS parameters.
;;;
;;; Note that optional arguments are also a barrier to procedure integration.
;;; If your Scheme system permits you to specify alternate entry points
;;; for a call when the number of optional arguments is known in a manner
;;; that enables inlining/integration, this can provide performance 
;;; improvements.
;;;
;;; There is enough *explicit* error checking that *all* internal operations
;;; should *never* produce a type or index-range error. Period. Feel like
;;; living dangerously? *Big* performance win to be had by replacing string
;;; and record-field accessors and setters with unsafe equivalents in the
;;; code. Similarly, fixnum-specific operators can speed up the arithmetic
;;; done on the index values in the inner loops. The only arguments that are
;;; not completely error checked are
;;;   - string lists (complete checking requires time proportional to the
;;;     length of the list)
;;;   - procedure arguments, such as char->char maps & predicates.
;;;     There is no way to check the range & domain of procedures in Scheme.
;;; Procedures that take these parameters cannot fully check their
;;; arguments. But all other types to all other procedures are fully checked.
;;;
;;; This does open up the alternate possibility of simply *removing* these 
;;; checks, and letting the safe primitives raise the errors. On a dumb
;;; Scheme system, this would provide speed (by eliminating the redundant
;;; error checks) at the cost of error-message clarity.
;;;
;;; In an interpreted Scheme, some of these procedures, or the internal
;;; routines with % prefixes, are excellent candidates for being rewritten in C.
;;;
;;; It would also be nice to have the ability to mark some of these
;;; routines as candidates for inlining/integration.
;;; 
;;; See the comments preceding the hash function code for notes on tuning
;;; the default bound so that the code never overflows your implementation's
;;; fixnum size into bignum calculation.
;;;
;;; All the %-prefixed routines in this source code are written
;;; to be called internally to this library. They do *not* perform
;;; friendly error checks on the inputs; they assume everything is
;;; proper. They also do not take optional arguments. These two properties
;;; save calling overhead and enable procedure integration -- but they
;;; are not appropriate for exported routines.

;;; Parse, type-check & default a final optional BASE-CS parameter from
;;; a rest argument. Return a *fresh copy* of the underlying string.
;;; The default is the empty set. The PROC argument is to help us
;;; generate informative error exceptions.

;;; Copyright notice
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1988-1995 Massachusetts Institute of Technology
;;; 
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;_______________________________	Section 4 begin by AlSchemist 2021

; The following is free TinyScheme open source copyrighted AlSchemist

#| vowel depends of the alphabet of your language. Customize the following:
(show char-set:vowel)
 |#;->	14:(A E I O U W Y a e i o u w y)
(define char-set:vowel (string->char-set "AEIOUYWaeiouyw"))
#|
(vowel? #\a)(vowel? #\O)(vowel? #\0)(vowel? #\l)
 |#;->	#t			#t			#f			#f
(define (vowel? ch) (char-set-contains? char-set:vowel ch))

#| Consecutive range of chars in the beginning of lst?
(%char-interval? 10 9 '(#\xb))
 |#;->	#t
(define (%char-interval? iCur iRef lst)
	(and (= iCur (inc iRef)) (pair? lst) (= (char->integer (car lst)) (+ iRef 2))
)	)
#| display a char-set defined in 1srfi-014.scm for show
(show char-set:whitespace)
 |#;->	7:(#\x9 .. #\xd #\x20 #\xa0)
(define (char-set-display cs)
	(check-arg char-set? cs 'show)
	(let*	(	(iPrev -2) (offset (%char-set-offset cs)))
		(let loop ((lst (char-set->list cs)) (lstOut nil))
			(if (pair? lst)
				(let*	(	(ch (car lst))
							(iCur (char->integer ch))
							(range? (%char-interval? iCur iPrev (cdr lst)))
						)
					(if range?
						(let next ((rngStart (inc iPrev)))
							(set! lst (cdr lst)) (set! ch (car lst))
							(set! iCur (char->integer ch))
							(if (%char-interval? iCur rngStart (cdr lst))
								(next (inc rngStart))
					)	)	)
					(if (positive? offset)
						(set! ch (string ch))
						(if (or (<= iCur 32) (<= 127 iCur 160))
							(set! ch (string-append "#\\x" (number->string iCur 16)))
					)	)
					(set! iPrev iCur)
					(loop (cdr lst) (if range? (cons* ch ".." lstOut)(cons ch lstOut)))
				)
				(begin	(display (%char-set-size cs)) (display ":")
						(display (reverse lstOut))
)	)	)	)	)
; To enable an optional char-set beyond Latin, move it outside nested comment
#|  [	Begin nested comment optional char-sets
#| Greek: extract
(show char-set:greek) (char-set->string char-set:greek)
 |#;->	56:(Α .. Ρ Σ .. ω)
;		"ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩΪΫάέήίΰαβγδεζηθικλμνξοπρςστυφχψω"
(define char-set:greek
	(char-set-union (int-range->char-set #x0391 #x03A1)
					(int-range->char-set #x03A3 #x03C9)
)	)
#| Chess from White King to White Pawn then Black King to Black Pawn 
(show char-set:chess (char-set->list char-set:chess))
 |#;->	12:(♔ .. ♟)(♔ ♕ ♖ ♗ ♘ ♙ ♚ ♛ ♜ ♝ ♞ ♟)
(define char-set:chess
	(int-range->char-set #x2654 #x265F)
)
#| Chinese and Japanese parenthesis
(show char-set:asian-parenthesis (char-set->list char-set:asian-parenthesis))
 |#;->	10:(〈 .. 】)(〈 〉 《 》 「 」 『 』 【 】)
(define char-set:asian-parenthesis
	(int-range->char-set #x3008 #x3011)
)
#| Arabic nbr 1 to 20 in circle
(show char-set:arabic-nbr-in-circle (char-set->list char-set:arabic-nbr-in-circle))
 |#;->	20:(① .. ⑳)(① ② ③ ④ ⑤ ⑥ ⑦ ⑧ ⑨ ⑩ ⑪ ⑫ ⑬ ⑭ ⑮ ⑯ ⑰ ⑱ ⑲ ⑳)
(define char-set:arabic-nbr-in-circle
	(int-range->char-set #x2460 #x2473)
)
#| Chinese nbr 1 to 10 in circle
(show char-set:chinese-nbr-in-circle (char-set->list char-set:chinese-nbr-in-circle))
 |#;->	10:(㊀ .. ㊉)(㊀ ㊁ ㊂ ㊃ ㊄ ㊅ ㊆ ㊇ ㊈ ㊉)
(define char-set:chinese-nbr-in-circle
	(int-range->char-set #x3280 #x3289)
)
#| Roman in the Script-Fu console but not in the .scm edited by NotePad++
(show char-set:roman " to list: " (char-set->list char-set:roman))
 |#;->	10:(1 .. 10) to list: (1 2 3 4 5 6 7 8 9 10)
(define char-set:roman
	(int-range->char-set #x2160 #x2169)
)
#| Latin exposants are from #U+00B2 for exponent "2"
(show char-set:exposant-latin)
 |#;->	3:(² ³ ¹)
#| exposant-indice are from #U+2070 for exponent "0"
(char-set->string char-set:exposant-indice)
 |#;->	"⁰ⁱ⁴⁵⁶⁷⁸⁹⁺⁻⁼⁽⁾ⁿ₀₁₂₃₄₅₆₇₈₉₊₋₌₍₎"
(define char-set:exposant-indice
	(char-set-union (int-range->char-set 8304 8305)
					(int-range->char-set 8308 8334)
)	)
#| It is not possible to union two char-sets of different offset
(char-set-union char-set:exposant-latin char-set:exposant-indice)
 |#;->	Error: 1010. char-set-union: (lambda (offset2) (= offset1 offset2)) 8192 failed. 
(define char-set:exposant-latin (int-list->char-set '(178 179 185)))

#| Japanese hiragana
(show char-set:hiragana)
 |#;->	83:(芟 .. 英)
(define char-set:hiragana
	(int-range->char-set #x829F #x82F1)
)
#| Japanese katakana
(show char-set:katakana)
 |#;->	87:(荀 .. 莖)
(define char-set:katakana
	(int-range->char-set #x8340 #x8396)
)
#| Japanese kanji level 1: extract
(show char-set:kanji1)
 |#;->	41:(袟 .. 裇)
(define char-set:kanji1
	(int-range->char-set #x889F #x88C7)
)
#| Japanese kanji level 2: extract
(show char-set:kanji2)
 |#;->	69:(颸 .. 飼)
(define char-set:kanji2
	(int-range->char-set #x98B8 #x98FC)
)
#| Russian cyrillic: extract
(show char-set:cyrillic)
 |#;->	64:(А .. я)
(define char-set:cyrillic
	(int-range->char-set #x0410 #x044F)
)
 |#;]	End nested comment optional char-sets
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
(load "C:/Users/YourUserName/AppData/Roaming/GIMP/2.10/scripts/1srfi-014-char-set.scm")
 |# (closure? char-set?)