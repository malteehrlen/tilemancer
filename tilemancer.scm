(define (tilemancer image drawable option)
  (let* (
    (image (car (gimp-image-duplicate image)))     
    (width (car (gimp-image-width image)))
    (height (car (gimp-image-height image)))
    (layers (gimp-image-get-layers image))
    (num-layers (car layers))
    (layer-vect (cadr layers)))
    (cond ((= option 0) (sheeterize-square image layer-vect (cons width height)))
      ((= option 1) (sheeterize-group image layer-vect (cons width height) 0)))
    (begin
      (gimp-image-resize-to-layers image)
      (gimp-image-merge-visible-layers image 1)
      (gimp-image-grid-set-spacing image width height)
      (gimp-display-new image)
      (gimp-displays-flush))))

(define (sheeterize-square image item-vect framesize)
  (let ((side (ceiling (sqrt (vector-length item-vect)))))
    (do ((i 0 (+ i 1)))
      ((= i side))
      (do ((j 0 (+ j 1))) 
        ((= j side)) 
        (let ((frame (- (vector-length item-vect) 1 (+ j (* i side)))))
          (if (>= frame 0) (gimp-layer-translate (vector-ref item-vect frame) (* j (car framesize)) (* i (cdr framesize)))))))))

(define (sheeterize-group image item-vect framesize row)
  (let* ((width (car (gimp-image-width image)))
    (height (car (gimp-image-height image)))
    (cols (vector-length item-vect))) 
    (do ((i 0 (+ i 1))) 
      ((= i cols)) 
      (let* ((current-item (vector-ref item-vect i))
        (xpos (* (- cols 1 i) (car framesize))))
        (if (= 0 (car (gimp-item-is-group current-item)))
          (gimp-layer-translate current-item xpos (* (- row 1) (cdr framesize)))
          (sheeterize-group image (cadr (gimp-item-get-children current-item)) framesize (+ 1 row)))))))

(script-fu-register
    "tilemancer"
    "tilemancer"
    "Generate sprite sheets from layers"
    "Malte Ehrlen"
    "copyright 2020, Malte Ehrlen, malte@ehrlen.com;"
    "October 19, 2020"
    "*"
    SF-IMAGE      "Image"          0
    SF-DRAWABLE      "Drawable"          0
    SF-OPTION      "Shape" '("Square" "One row per layer group")
)
(script-fu-menu-register "tilemancer" "<Image>/Filters/Animation")
