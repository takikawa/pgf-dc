#lang racket/base

(require "pgf.rkt"
         racket/class
         racket/draw
         (for-syntax "pgf.rkt"))

(provide pgf-dc%)

(define pgf-dc%
  (class* object% (dc<%>)
    (super-new)
    
    ;; output-port?
    (init-field out)
    
    ;; same defaults as other racket/draw dcs
    (define pen (send the-pen-list find-or-create-pen "black" 1 'solid))
    (define brush (send the-brush-list find-or-create-brush "white" 'solid))
    
    (define the-picture
      (new pgf-picture% [out out]))
    
    (define/public (write) (display (send the-picture get-pgf-code)))
    
    (define/public (cache-font-metrics-key)
      (void))
    
    (define/public (clear)
      (set! the-picture
            (new pgf-picture% [out out])))
    
    (define/public (copy x y width height x2 y2)
      (void))
    
    (define/public (draw-arc x y width height start-radians end-radians)
      (void))
    
    (define/public (draw-bitmap source dest-x dest-y
                                [style #f] [color #f] [mask #f])
      (error "Unsupported"))
    
    (define/public (draw-bitmap-section source dest-x dest-y src-x src-y width height
                                        [style #f] [color #f] [mask #f]) (void))
    
    (define/public (draw-ellipse x y width height)
      (pgf-do the-picture
              (pgf-path-ellipse (pgf-point x y)
                                (pgf-point width 0)
                                (pgf-point 0 height))
              (pgf-use-path 'fill 'stroke)))
    
    (define/public (draw-line x1 y1 x2 y2)
      (pgf-do the-picture
              (pgf-path-move-to (pgf-point x1 y1))
              (pgf-path-line-to (pgf-point x2 y2))
              (pgf-use-path 'stroke)))
    
    (define/public (draw-lines points [x 0] [y 0]) (void))
    (define/public (draw-path path [x 0] [y 0] [fill-style #f]) (void))
    (define/public (draw-point x y) (void))
    (define/public (draw-polygon points [x 0] [y 0] [fill-style #f]) (void))
    (define/public (draw-rectangle x y width heigh) (void))
    (define/public (draw-rounded-rectangle x y width height [radius 0]) (void))
    (define/public (draw-spline x1 y1 x2 y2 x3 y3) (void))
    (define/public (draw-text text x y [combine #f] [angle 0] [offset 0]) (void))
    (define/public (end-doc) (void))
    (define/public (end-page) (void))
    (define/public (erase) (void))
    (define/public (flush) (void))
    (define/public (get-alpha) (void))
    (define/public (get-background) (void))
    (define/public (get-brush) brush)
    (define/public (get-char-height) (void))
    (define/public (get-char-width) (void))
    (define/public (get-clipping-region) (void))
    (define/public (get-device-scale) (void))
    (define/public (get-font) (void))
    (define/public (get-gl-context) (void))
    (define/public (get-initial-matrix) (void))
    (define/public (get-origin) (void))
    (define/public (get-pen) pen)
    (define/public (get-rotation) (void))
    (define/public (get-scale) (void))
    (define/public (get-size) (void))
    (define/public (get-smoothing) (void))
    (define/public (get-text-background) (void))
    (define/public (get-text-extent) (void))
    (define/public (get-text-foreground) (void))
    (define/public (get-text-mode) (void))
    (define/public (get-transformation) (void))
    (define/public (glyph-exists?) (void))
    (define/public (ok?) #t)
    (define/public (resume-flush) (void))
    (define/public (rotate angle) (void))
    (define/public (scale x y) (void))
    (define/public (set-alpha alpha) (void))
    (define/public (set-background color) (void))
    (define/public (set-clipping-rect x y width height) (void))
    (define/public (set-clipping-region rgn) (void))
    (define/public (set-font font) (void))
    (define/public (set-initial-matrix m) (void))
    (define/public (set-origin x y) (void))
    
    (public set-brush set-pen)
    (define set-pen 
      (case-lambda [(new-pen)
                    (set! pen new-pen)
                    (do-set-pen-color (send pen get-color))
                    (do-set-pen-style (send pen get-style))]
                   [(color width style)
                    (do-set-pen-color color)
                    (do-set-pen-style style)]))
    
    (define set-brush
      (case-lambda [(new-brush) 
                    (set! brush new-brush)
                    (do-set-brush-color (send brush get-color))
                    (do-set-brush-style (send brush get-style))]
                   [(color style)
                    (do-set-brush-color color)
                    (do-set-brush-style style)]))
    
    (define (do-set-pen-style style)
      (case style
        [(transparent) (pgf-do the-picture (pgf-set-stroke-opacity 0))]
        [(solid xor) (pgf-do the-picture (pgf-set-stroke-opacity 1))]
        [else (void)]))
    
    (define (do-set-brush-style style)
      (case style
        [(transparent) (pgf-do the-picture (pgf-set-fill-opacity 0))]
        [(solid panel xor) (pgf-do the-picture (pgf-set-fill-opacity 1))]
        [else (void)]))
    
    (define (do-set-brush-color color)
      (define name (make-color-name))
      (define-values (r g b) (extract-rgb color))
      (pgf-do the-picture
              (pgf-define-color name r g b)
              (pgf-set-fill-color name)))
    
    (define (do-set-pen-color color)
      (define name (make-color-name))
      (define-values (r g b) (extract-rgb color))
      (pgf-do the-picture
              (pgf-define-color name r g b)
              (pgf-set-stroke-color name)))
    
    (define (extract-rgb color)
      (define color-object 
        (if (string? color)
            (make-object color% color)
            color))
      (values (send color red)
              (send color green)
              (send color blue)))
    
    ;; horrible unhygeinic (in TeX-land) hack
    (define (make-color-name)
      (string-append "racketcolor"
                     (symbol->string (gensym))))
    
    (define/public (set-rotation angle) (void))
    (define/public (set-scale x y) (void))
    (define/public (set-smoothing mode) (void))
    (define/public (set-text-background color) (void))
    
    (define/public (set-text-foreground color)
      (void))
    
    (define/public (set-text-mode mode)
      (void))
    
    (define/public (set-transformation t)
      (void))
    
    (define/public (start-doc message)
      (void))
    
    (define/public (start-page)
      (void))
    
    (define/public (suspend-flush)
      (void))
    
    (define/public (transform m) (void))
    (define/public (translate dx dy) (void))
    (define/public (try-color try result) (void))))
