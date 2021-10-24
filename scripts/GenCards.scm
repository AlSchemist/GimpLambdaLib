; GenCard.scm generate a card of member from the given template "memberCard.jpg" in Gimp 2.10.28
; 2021/09/27: GimpλLib 1.0 by Gimphried for Gimp 2.10.28 TinyScheme Script-Fu
; usage: (GenCards "C:/Tool/Gimp/forum/Linuxgraphic/GenCards/")
; excepted duration: 3 mn
; https://www.linuxgraphic.org/forums/viewtopic.php?f=4&t=7437&p=46690#p46690
; Required: file-global in 0ts-stdio.scm AlSchemist's Lib 1.0
; Line: 83 define: 2 comment: 40 = 48.1% blank: 2
(define (GenCard pathRoot filin)
	(let*	(	(person		(car (last-pair (strbreakup filin "/"))))
				(basename	(strbreakup (car (strbreakup person ".")) "_"))
				(lastName	(car  basename)) (posX 140) (posY 50) (incrY 20)
				(firstName	(cadr basename))
				(dateBorn	(unbreakupstr (cddr basename) "/"))
				(template 	(string-append pathRoot "memberCard.jpg"))
				(filout		(string-append pathRoot "card/" lastName "_" firstName ".jpg")) ; target
				(imgCard 0)(layerCard 0)(imgTemplate 0)(imgIcon 0)
			) ; end of local variables
		(set! imgCard (car (gimp-image-new 1 1 RGB)))
		(set! layerCard (car (gimp-layer-new imgCard 1 1 RGB-IMAGE "layer main" 100 LAYER-MODE-NORMAL-LEGACY)))
		(gimp-image-insert-layer imgCard layerCard 0 0) ; top of layer stack
		(gimp-context-set-foreground '(0 0 0)) ; black
		(gimp-context-set-background '(255 255 255)) ; white
		(gimp-drawable-fill layerCard BACKGROUND-FILL)
		(set! imgTemplate (car (gimp-file-load-layer 1 imgCard template)))				
		(gimp-image-insert-layer imgCard imgTemplate 0 0) ; add template layer
		
		(script-fu-util-image-resize-from-layer imgCard imgTemplate)
		(gimp-layer-resize-to-image-size layerCard) 
		
		(gimp-text-fontname imgCard imgTemplate posX posY lastName  0 TRUE 18 POINTS "Sans")
		(set! posY (+ posY incrY))
		(gimp-text-fontname imgCard imgTemplate posX posY firstName 0 TRUE 18 POINTS "Sans")
		(gimp-floating-sel-anchor (car (gimp-text-fontname imgCard imgTemplate posX (+ posY incrY) dateBorn 0 TRUE 18 POINTS "Sans")))
		(set! imgIcon (car (gimp-file-load-layer RUN-NONINTERACTIVE imgCard filin)))
		(gimp-image-insert-layer imgCard imgIcon 0 0) ; add icon layer
		(gimp-layer-set-offsets imgIcon 10 10)		  ; shift icon to add border
		(set! layerCard (car (gimp-image-flatten imgCard))) ; merge layers
		(file-jpeg-save RUN-NONINTERACTIVE imgCard layerCard filout filout 0.8 0 1 1 "" 2 1 0 0)
		(gimp-image-clean-all imgCard)		
		(gimp-image-delete imgCard)
)	)
; Check vertical alignement and red color of let* closing parenthesis in NotePad++
; Loop for all *.jpg
(define (GenCards pathRoot)
    (let*   (   (pattern (string-append pathRoot "icon/*.jpg")) ; source
            )
        (let loop   (   (listFile (file-global pattern)))
            (if (pair? listFile)
                (begin
                    (GenCard pathRoot (car listFile)) ; generate the member card
                    (loop (cdr listFile)) ; next cards
)	)	)	)	)
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
(load "C:/Users/Chess/AppData/Roaming/GIMP/2.10/scripts/GenCards.scm")
 |# (closure? GenCards)