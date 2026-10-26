#!/usr/bin/env gimp-script-fu-interpreter-3.0
(define (tilemancer image drawable option spacing)
  (let* (
    (image (car (gimp-image-duplicate image)))     
    (width (car (gimp-image-get-width image)))
    (height (car (gimp-image-get-height image)))
    (layers (car (gimp-image-get-layers image)))
    (num-layers (vector-length layers))
    (layer-vect layers))
    (cond ((= option 0) (sheeterize-square image layer-vect (cons width height) spacing))
      ((= option 1) (sheeterize-group image layer-vect (cons width height) 0 spacing)))
    (begin
      (gimp-image-resize-to-layers image)
      (gimp-image-merge-visible-layers image 1)
      (gimp-image-grid-set-spacing image width height)
      (gimp-display-new image)
      (gimp-displays-flush))))

(define (sheeterize-square image item-vect framesize spacing)
  (let ((side (ceiling (sqrt (vector-length item-vect)))))
    (do ((i 0 (+ i 1))) ((= i side)) ; for i = 0; i < side; i++
      (do ((j 0 (+ j 1))) ((= j side)) ; for j = 0; j < side; j++ 
        (let ((frame (- (vector-length item-vect) 1 (+ j (* i side)))))
          (if 
            (>= frame 0) 
            (gimp-item-transform-translate ; Copy input tile layer somewhere to output tileset
                (vector-ref item-vect frame)
                (* j (+ spacing (car framesize))) ; (framesize.x + spacing) * j
                (* i (+ spacing (cdr framesize))) ; (framesize.y + spacing) * i
            )
          )
        )
      )
    )
  )
)

(define (sheeterize-group image item-vect framesize row spacing)
  (let* ((width (car (gimp-image-get-width image)))
    (height (car (gimp-image-get-height image)))
    (cols (vector-length item-vect))) 
    (do ((i 0 (+ i 1))) ((= i cols)) 
      (let* ((current-item (vector-ref item-vect i))
        (xpos (* (- cols 1 i) (+ spacing (car framesize)))))
        (if 
            (= 0 (car (gimp-item-is-group current-item)))
            ; if this particular item is not group, print it on current row
            (gimp-item-transform-translate current-item xpos (* row (+ spacing (cdr framesize))))
            ; if it is group, recurse and use current item index as row index
            (sheeterize-group image (car (gimp-item-get-children current-item)) framesize i spacing))))))

(script-fu-register-filter
    "tilemancer"
    "Tilemancer"
    "Generate sprite sheets from layers"
    "Malte Ehrlen"
    "copyright 2020, Malte Ehrlen, malte@ehrlen.com;"
    "October 19, 2020"
    "*"
    SF-ONE-DRAWABLE
    SF-OPTION     "Shape" '("Square" "One row per layer group")
    SF-ADJUSTMENT  "Tile spacing amount (px)"     '(0 0 1000 1 10 0 0)
)
(script-fu-menu-register "tilemancer" "<Image>/Plug-in")
