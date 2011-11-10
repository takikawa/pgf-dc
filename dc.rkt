#lang at-exp racket/base

(require "pgf.rkt"
         racket/class
         racket/draw
         racket/draw/private/local
         racket/vector
         (for-syntax "pgf.rkt"))

(provide pgf-dc%)

(define pgf-dc%
  (class* object% (dc<%>)
    (super-new)

    (init output-file)
    (define out-file output-file)
    (define out #f)
    (init-field [font-width-scale 4.7]
                [font-height-scale 10]
                [font-baseline-scale 3])

    ;; same defaults as other racket/draw dcs
    (define pen (send the-pen-list find-or-create-pen "black" 1 'solid))
    (define brush (send the-brush-list find-or-create-brush "white" 'solid))
    (define alpha 1)
    (define foreground-color "black")
    
    ;; transformation matrix
    (define initial-matrix
      (vector 1 0 0 1 0 0))
    (define matrix
      (vector 0 0 1 1 0))
    
    (define the-picture
      (new pgf-picture%))
    
    (define latex-prelog
      @string-append{\documentclass{article}
                     \usepackage{pgf}
                     \begin{document}})
    (define latex-epilog
      @string-append{\end{document}})
    
    ;; just output the picture without latex pre/epilog
    (define/public (output-picture)
      (display (send the-picture get-pgf-code) out))
    
    (define/public (cache-font-metrics-key) 0)
    
    (define/public (clear)
      (set! the-picture (new pgf-picture%)))
    
    (define/public (copy x y width height x2 y2)
      (void))
    
    (define/public (draw-arc x y width height start-radians end-radians)
      ;; equation for the point along an ellipse
      ;; X(t) = X_c + a cos(t)
      ;; Y(t) = Y_c + b sin(5)
      (define xt (+ (+ x (/ width 2))
                    (* (/ width 2) (cos start-radians))))
      (define yt (- (+ y (/ height 2))
                    (* (/ height 2) (sin start-radians))))
      (pgf-do the-picture
              (pgf-path-move-to (pgf-point xt yt))
              (pgf-path-arc start-radians end-radians
                            (* 1/2 width) (* 1/2 height))
              (pgf-use-path 'stroke)))
    
    (define/public (draw-bitmap source dest-x dest-y
                                [style #f] [color #f] [mask #f])
      (error "Unsupported"))
    
    (define/public (draw-bitmap-section source dest-x dest-y src-x src-y width height
                                        [style #f] [color #f] [mask #f]) (void))
    
    (define/public (draw-ellipse x y width height)
      (pgf-do the-picture
              (pgf-path-ellipse (pgf-point (+ x (* 1/2 width))
                                           (+ y (* 1/2 height)))
                                (pgf-point (* 1/2 width) 0)
                                (pgf-point 0 (* 1/2 height)))
              (pgf-use-path 'fill 'stroke)))
    
    (define/public (draw-line x1 y1 x2 y2)
      (put-line x1 y1 x2 y2)
      (pgf-do the-picture (pgf-use-path 'stroke)))
    
    ;; add a path to the picture without stroking it
    (define (put-line x1 y1 x2 y2)
      (pgf-do the-picture
              (pgf-path-move-to (pgf-point x1 y1))
              (pgf-path-line-to (pgf-point x2 y2))))
    
    (define/public (draw-lines points [x-offset 0] [y-offset 0])
      (put-lines points x-offset y-offset)
      (pgf-do the-picture (pgf-use-path 'stroke)))
      
    (define/public (put-lines points x-offset y-offset)
      (define (extract-xy pt)
        (cond [(is-a? pt point%)
               (values (send pt get-x) (send pt get-y))]
              [(pair? pt)
               (values (car pt) (cdr pt))]))

      ;; after the first point, line to each next one
      (define (put-lines-helper points)
        (cond [(null? points) (void)]
              [else
               (define-values (x y) (extract-xy (car points)))
               (pgf-do the-picture
                       (pgf-path-line-to (pgf-point (+ x x-offset)
                                                    (+ y y-offset))))
               (put-lines-helper (cdr points))]))

      ;; move to the first point before doing anything
      (cond [(null? points) (void)]
            [else
             (define-values (x y) (extract-xy (car points)))
             (pgf-do the-picture
                     (pgf-path-move-to (pgf-point (+ x x-offset) (+ y y-offset))))
             (put-lines-helper (cdr points))]))

    (define/public (draw-path path [x 0] [y 0] [fill-style #f]) (void))
    (define/public (draw-point x y) (void))

    (define/public (draw-polygon points [x-offset 0] [y-offset 0] [fill-style #f])
      (put-lines points x-offset y-offset)
      (pgf-do the-picture
              (pgf-path-close)
              (pgf-use-path 'stroke 'fill)))

    (define/public (draw-rectangle x y width height)
      (pgf-do the-picture
              (pgf-path-rectangle-corners (pgf-point x y)
                                          (pgf-point (+ x width)
                                                     (+ y height))))
      (case (send (get-brush) get-style)
        [(solid xor panel)
         (pgf-do the-picture (pgf-use-path 'stroke 'fill))]
        [else
         (pgf-do the-picture (pgf-use-path 'stroke))]))

    (define/public (draw-rounded-rectangle x y width height [radius -0.25])
      (define-values (x-arc y-arc)
        (if (positive? radius)
            (values radius radius)
            (let ([dim (* (min width height) (abs radius))])
              (values dim dim))))
      (pgf-do the-picture
              (pgf-scope-begin)
              (pgf-set-corners-arced x-arc y-arc)
              (pgf-path-rectangle-corners (pgf-point x y)
                                          (pgf-point (+ x width)
                                                     (+ y height)))
              (pgf-use-path 'stroke 'fill)
              (pgf-scope-end)))

    (define/public (draw-spline x1 y1 x2 y2 x3 y3)
      (pgf-do the-picture
              (pgf-path-move-to (pgf-point x1 y1))
              (pgf-path-quadratic-curve-to (pgf-point x2 y2)
                                           (pgf-point x3 y3))
              (pgf-use-path 'stroke)))

    (define/public (draw-text text x y [combine #f] [offset 0] [angle 0])
      (pgf-do the-picture (pgf-scope-begin))
      (update-text-foreground foreground-color)
      (pgf-do the-picture
              (pgf-set-fill-opacity 1)
              (pgf-text (substring text offset)
                        #:at (pgf-point x y)
                        #:rotation angle)
              (pgf-scope-end)))

    ;; output the document on the given output-port when done
    (define/public (end-doc)
      (display (send the-picture get-pgf-code) out)
      (close-output-port out))

    (define/public (end-page) (void))
    (define/public (erase) (void))
    (define/public (flush) (void))

    (define/public (get-alpha) alpha)

    ;; TODO: stub value
    (define/public (get-background) (make-object color% "white"))

    (define/public (get-brush) brush)
    (define/public (get-char-height) 1)
    (define/public (get-char-width) (void))
    (define/public (get-clipping-matrix) #f)
    (define/public (get-clipping-region) #f)
    (define/public (get-device-scale) (void))
    (define/public (get-font) (void))
    (define/public (get-gl-context) (void))
    (define/public (get-initial-matrix) (void))

    (define/public (get-pen) pen)

    (define/public (get-origin)
      (values (vector-ref matrix 0)
              (vector-ref matrix 1)))

    (define/public (get-rotation)
      (vector-ref matrix 4))

    (define/public (get-scale)
      (values (vector-ref matrix 2)
              (vector-ref matrix 3)))

    (define/public (get-size) (void))

    (define/public (get-smoothing) (void))
    (define/public (get-text-background) (void))
    (define/public (get-text-extent string [font #f] [combine #f] [offset 0])
      (values (* font-height-scale (string-length (substring string offset)))
              font-width-scale
              font-baseline-scale
              0))
    (define/public (get-text-foreground) foreground-color)

    ;; TODO: stub value
    (define/public (get-text-mode) 'solid)

    (define/public (get-transformation)
      (vector-append (vector initial-matrix) matrix))
    
    (define/public (glyph-exists?) (void))
    (define/public (ok?) #t)
    (define/public (resume-flush) (void))

    ;; coordinate transformations

    (define/public (translate dx dy)
      (define-values (dx-cur dy-cur) (get-origin))
      (set-origin (+ dx-cur dx)
                  (+ dy-cur dy)))

    (define/public (rotate angle)
      (set-rotation (+ (get-rotation) angle)))

    (define/public (scale x y)
      (define-values (x-cur y-cur) (get-scale))
      (set-scale (+ x x-cur)
                 (+ y y-cur)))

    (define/public (set-origin x y)
      (vector-set! matrix 0 x)
      (vector-set! matrix 1 y)
      (update-transform))

    (define/public (set-rotation angle)
      (vector-set! matrix 4 angle)
      (update-transform))

    (define/public (set-scale x y)
      (vector-set! matrix 2 x)
      (vector-set! matrix 3 y)
      (update-transform))

    (define/public (set-smoothing mode) (void))
    
    (define/public (transform m) (void))
  
    ;; set the PGF transforms
    (define (update-transform)
      (define-values (x y) (get-origin))
      (define-values (x-scale y-scale) (get-scale))
      (define r (get-rotation))
      (define-values (xx xy yx yy x0 y0)
        (values (vector-ref initial-matrix 0)
                (vector-ref initial-matrix 1)
                (vector-ref initial-matrix 2)
                (vector-ref initial-matrix 3)
                (vector-ref initial-matrix 4)
                (vector-ref initial-matrix 5)))
      (pgf-do the-picture
              (pgf-transform-reset)
              (pgf-transform-shift (pgf-point (+ x x0 (* y yx))
                                              (+ y y0 (* x xy))))
              (pgf-transform-x-scale (* x-scale xx))
              (pgf-transform-y-scale (* y-scale yy))
              (pgf-transform-rotate r)))

    (define/public (set-alpha new-alpha)
      (set! alpha new-alpha))

    (define/public (set-background color) (void))
    (define/public (set-clipping-rect x y width height) (void))
    (define/public (set-clipping-region rgn) (void))
    (define/public (set-font font) (void))
    (define/public (set-initial-matrix m) (void))
    
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
        [(solid xor) (void) #;(pgf-do the-picture (pgf-set-stroke-opacity 1))]
        [else (void)]))
    
    (define (do-set-brush-style style)
      (case style
        [(transparent) (pgf-do the-picture (pgf-set-fill-opacity 0))]
        [(solid panel xor) (void) #;(pgf-do the-picture (pgf-set-fill-opacity 1))]
        [else (void)]))
    
    (define (do-set-brush-color color)
      (define name (make-color-name))
      (define-values (r g b a) (extract-rgba color))
      (pgf-do the-picture
              (pgf-define-color name r g b)
              (pgf-set-fill-opacity (* alpha a))
              (pgf-set-fill-color name)))
    
    (define (do-set-pen-color color)
      (define name (make-color-name))
      (define-values (r g b a) (extract-rgba color))
      (pgf-do the-picture
              (pgf-define-color name r g b)
              (pgf-set-stroke-opacity (* alpha a))
              (pgf-set-stroke-color name)))
    
    (define (extract-rgba color)
      (define color-object 
        (if (string? color)
            (make-object color% color)
            color))
      (values (send color-object red)
              (send color-object green)
              (send color-object blue)
              (send color-object alpha)))
    
    ;; horrible unhygeinic (in TeX-land) hack
    (define (make-color-name)
      (string-append "racketcolor"
                     (symbol->string (gensym))))
    
    (define/public (set-text-background color)
      (void))
    
    (define/public (set-text-foreground color)
      (set! foreground-color color)
      (update-text-foreground color))

    (define/public (update-text-foreground color)
      (define name (make-color-name))
      (define-values (r g b a) (extract-rgba color))
      (pgf-do the-picture
              (pgf-define-color name r g b)
              (pgf-color name)))
    
    (define/public (set-text-mode mode)
      (void))
    
    (define/public (set-transformation t)
      (void))
    
    (define/public (start-doc message)
      (set! out (open-output-file out-file #:exists 'replace))
      (update-transform)
      (update-text-foreground foreground-color))
    
    (define/public (start-page)
      (void))
    
    (define/public (suspend-flush)
      (void))
    
    (define/public (try-color try result)
      (send result set 
            (send try red)
            (send try green)
            (send try blue)))))
