; 2021/09/27: GimpλLib 1.0 by AlSchemist for Gimp 2.10.28 TinyScheme Script-Fu
;				SaveAnimDuration.scm for .webp or .gif animated image
; TinyScheme open source without any warranty powered by AlSchemist
; http://echiquierbriochin.fr/webp-gif-le-jpeg/
; 
; Copy in "C:\Users\YourUserName\AppData\Roaming\GIMP\2.10\scripts\"
;                   ^ replace with your Windows user name 
; Where to use?   Gimp menu "Filters" > "Script-Fu" > "Console"
; How to improve? NotePad++ menu "File" > "Open" > "SaveAnimDuration.scm"
; NotePad++ menu "Settings" > "Style Configurator..." > Language: "Scheme" Style: "COMMENT"
; Foreground colour: Grey (RGB 128 128 128) becomes Dark Blue (RGB 14 3 152)
; Line: 136 define: 4 comment: 107 = 78.6% blank: 5
#| 
1. Scroll down https://www.gimp.org/news/2018/11/08/gimp-2-10-8-released/
until ZeMarmot animation:
2. Download gimp-2-10-8-ZeMarmot-frama.webp by Aryeom and Jehan 363 Kb = 371 658 bytes
https://www.gimp.org/news/2018/11/08/gimp-2-10-8-released/gimp-2-10-8-ZeMarmot-frama.webp
 |#
; 3. Gimp menu "File" > "Open" > "gimp-2-10-8-ZeMarmot-frama.webp"
#| Get the current id of the loaded image
(gimp-image-get-id)
 |#;->	1 if it is the first image opened in Gimp

#| Without any image loaded in Gimp:
(gimp-image-get-id)
 |#;->	Error: 3000. gimp-image-get-id: no image is open in Gimp.
 ; How to fix it? Gimp menu "File" > "Open" > .webp or .gif 
 
; Return so called "image" that is to say the id of the current image open in Gimp
(define (gimp-image-get-id)
	(if (zero? (car (gimp-image-list))) ; number of loaded image = 0?
		(error "3000. gimp-image-get-id: no image is open in Gimp.\nHow to fix it? Gimp menu \"File\" > \"Open\" > .webp or .png")
		(vector-ref (cadr (gimp-image-list)) 0)
)	)
#| Perf:
(duration '(gimp-image-get-id) 1000)
 |#;->	1 s 62 ms 496 µs for 1K runs. 1 ms 62 µs by run
;-

#| Please note that id image 1 is already used. Layer ids begin from 2 until 39
(gimp-image-get-layers-id (gimp-image-get-id))
 |#;->	(2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39)
 
; Return list of layers id
(define (gimp-image-get-layers-id image) 
	(reverse (vector->list (cadr (gimp-image-get-layers image))))
)
#| Perf:
(duration '(gimp-image-get-layers-id (gimp-image-get-id)) 1000)
 |#;->	2 s 238 ms 532 µs for 1K runs. 2 ms 238 µs by run
;-
#| Get the name of the first layer
(car (gimp-item-get-name (car (gimp-image-get-layers-id (gimp-image-get-id)))))
 |# ;-> "Image 1 (123ms)"

#| Get the name of the last layer
(car (gimp-item-get-name (car (last-pair (gimp-image-get-layers-id (gimp-image-get-id))))))
 |# ;-> "Image 38 (246ms)"
;-
#|
(gimp-layer-get-duration "Image 38 (246ms)")
 |#;->								246 
#| A frame name could contain several expressions between parenthesis
(gimp-layer-get-duration "Image vidéo 120 (40ms) (combine)")
 |#;->									   40

; Extract the frame duration between parenthesis ending by "ms)"
(define (gimp-layer-get-duration nameLayer)
	(let loop ((lstWord (string-split nameLayer)))
		(if (not (pair? lstWord)) #f
			(let* 	(	(strWord	(car lstWord)) ; token inside the layer name
						(len		(- (string-length strWord) 3))	; token length
					)
				(if (and (> len 0) (string=? (substring strWord len) "ms)"))
					(string->number (substring strWord 1 len))
					(loop (cdr lstWord))
)	)	)	)	)
#| Perf:
(duration '(gimp-layer-get-duration "Frame 38 (246ms)") 1000)
 |#;->	687 ms 84 µs for 1K runs. 687 µs by run
;-
#|
(gimp-layer-get-durations (gimp-image-get-layers-id (gimp-image-get-id)))
 |#;->	(123 82 82 82 82 82 82 82 82 82 82 82 82 82 82 82 82 82 82 82 287 164 82 41 41 41 41 41 41 41 82 82 82 41 41 41 82 246)
 
; Return the list of durations corresponding to each layer
(define (gimp-layer-get-durations lstLayerId)
	(map (lambda (idLayer) (gimp-layer-get-duration (car (gimp-item-get-name idLayer)))) lstLayerId)
)
#| Perf:
(duration '(gimp-layer-get-durations (gimp-image-get-layers-id (gimp-image-get-id))) 100)
 |#;->	3 s 703 ms 972 µs for 100 runs. 37 ms 39 µs by run
;------------
; file-write-by-line is defined in: 
; C:\Users\Chess\AppData\Roaming\GIMP\2.10\scripts\0ts_fileio.scm
; Adapt the supplied path with slash separator in an existing folder.
#| Create frameDuration.txt with Windows CR+LF end of line.
(file-write-by-line "C:/Users/Chess/Pictures/webp/ZeMarmot/frameDuration.txt" (gimp-layer-get-durations (gimp-image-get-layers-id (gimp-image-get-id))))
 |#;->	#t

#| Caution: do not use Windows backslash in the path
(file-write-by-line "C:\Users\Chess\Pictures\webp\ZeMarmot\frameDuration.txt" '(bug))
 |#;->	#t the writing is successfully but with wrong name and not in the wished folder.
 ; Backslashes are eat while "C:" indicates the relative folder by default of the user
 ; "C:\Users\Chess\UsersChessPictureswebpZeMarmotframeDuration.txt" contains "bug\r\n"
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
(load "C:/Users/Chess/AppData/Roaming/GIMP/2.10/scripts/SaveAnimDuration.scm")
 |# (closure? gimp-layer-get-durations)