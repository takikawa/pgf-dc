#lang racket/base

;; classes that define pgf commands that are used by the pgf-dc%

(require racket/class
         racket/string
         racket/stxparam
         (for-syntax racket/base
                     syntax/parse))

(provide pgf-picture%
         pgf-do)

(define-syntax-rule (define-pgf-stxparam NAME)
  (define-syntax-parameter NAME
    (λ (stx)
      (raise-syntax-error #f "can only be used in pgf-do" stx))))

(define-syntax-rule (define/provide-pgf-stxparams NAME ...)
  (begin (define-pgf-stxparam NAME) ... (provide NAME) ...))

(define/provide-pgf-stxparams
  pgf-point
  pgf-point-origin
  pgf-path-move-to
  pgf-path-line-to
  pgf-path-ellipse
  pgf-path-arc
  pgf-use-path
  pgf-set-stroke-color
  pgf-set-fill-color
  pgf-define-color
  pgf-set-stroke-opacity
  pgf-set-fill-opacity
  pgf-path-rectangle-corners
  pgf-text
  pgf-transform-reset
  pgf-transform-x-scale
  pgf-transform-y-scale
  pgf-transform-rotate
  pgf-transform-shift)

;; pgf macros
(define-syntax (pgf-do stx)
  (define-syntax-class pgf-fun
    #:description "pgf function"
    #:literals (pgf-point
                pgf-point-origin)
    (pattern (pgf-point n1:expr n2:expr)
             #:attr exp #'(new pgf-point%
                               [args (list (new pgf-coordinate% [val n1])
                                           (new pgf-coordinate% [val (- n2)]))]))
    (pattern (pgf-point-origin)
             #:attr exp #'(new pgf-point-origin% [args '()])))
  
  (define-syntax-class pgf-path-option
    (pattern (~or 'stroke 'fill)))
  
  (define-syntax-class pgf-cmd
    #:description "pgf command"
    #:literals (pgf-path-move-to
                pgf-path-line-to
                pgf-use-path
                pgf-path-ellipse
                pgf-path-arc
                pgf-set-stroke-color
                pgf-set-fill-color
                pgf-define-color
                pgf-set-stroke-opacity
                pgf-set-fill-opacity
                pgf-path-rectangle-corners
                pgf-text
                pgf-transform-reset
                pgf-transform-x-scale
                pgf-transform-y-scale
                pgf-transform-rotate
                pgf-transform-shift)
    (pattern (pgf-path-move-to p:pgf-fun)
             #:attr cmd #'(new pgf-path-move-to%
                               [args (list p.exp)]))
    (pattern (pgf-path-line-to p:pgf-fun)
             #:attr cmd #'(new pgf-path-line-to%
                               [args (list p.exp)]))
    (pattern (pgf-use-path opt:pgf-path-option ...)
             #:attr cmd #'(new pgf-use-path%
                               [args (list opt ...)]))
    (pattern (pgf-path-ellipse p1:pgf-fun p2:pgf-fun p3:pgf-fun)
             #:attr cmd #'(new pgf-path-ellipse%
                               [args (list p1.exp p2.exp p3.exp)]))
    (pattern (pgf-path-arc e1:expr e2:expr e3:expr e4:expr)
             #:attr cmd #'(new pgf-path-arc%
                               [start-angle e1] [end-angle e2]
                               [x-radius e3] [y-radius e4]))
    (pattern (pgf-set-stroke-color e:expr)
             #:attr cmd #'(new pgf-set-stroke-color% [arg e]))
    (pattern (pgf-set-fill-color e:expr)
             #:attr cmd #'(new pgf-set-fill-color% [arg e]))
    (pattern (pgf-define-color name:expr r:expr g:expr b:expr)
             #:attr cmd #'(new pgf-define-color%
                               [color-name name]
                               [red r] [green g] [blue b]))
    (pattern (pgf-set-stroke-opacity e:expr)
             #:attr cmd #'(new pgf-set-stroke-opacity%
                               [args (list (make-object pgf-wrap% e))]))
    (pattern (pgf-set-fill-opacity e:expr)
             #:attr cmd #'(new pgf-set-fill-opacity%
                               [args (list (make-object pgf-wrap% e))]))
    (pattern (pgf-path-rectangle-corners p1:pgf-fun p2:pgf-fun)
             #:attr cmd #'(new pgf-path-rectangle-corners%
                               [args (list p1.exp p2.exp)]))
    (pattern (pgf-text e:expr (~optional (~seq #:at p:pgf-fun)
                                         #:defaults ([p #'#f])))
             #:attr cmd #'(new pgf-text% [text e] [point p.exp]))
    (pattern (pgf-transform-x-scale e:expr)
             #:attr cmd #'(new pgf-transform-x-scale%
                               [args (list (make-object pgf-wrap% e))]))
    (pattern (pgf-transform-y-scale e:expr)
             #:attr cmd #'(new pgf-transform-y-scale%
                               [args (list (make-object pgf-wrap% e))]))
    (pattern (pgf-transform-rotate e:expr)
             #:attr cmd #'(new pgf-transform-rotate% [angle e]))
    (pattern (pgf-transform-shift p:pgf-fun)
             #:attr cmd #'(new pgf-transform-shift%
                               [args (list p.exp)]))
    (pattern (pgf-transform-reset)
             #:attr cmd #'(new pgf-transform-reset% [args '()])))
  
  (syntax-parse stx
    [(_ pic:expr c:pgf-cmd ...)
     #'(begin (send pic add-child c.cmd) ...)]))

;; interface for pgf elements
(define pgf<%>
  (interface ()
    get-pgf-code))

;; represents a pgfpicture
(define pgf-picture%
  (class* object% (pgf<%>)
    (super-new)
    
    (define children '())
    
    (define/public (add-child elem)
      (set! children (append children (list elem))))
    
    (define/public (get-pgf-code)
      (string-append 
       "\\begin{pgfpicture}"
       (string-join (map (λ (c) (send c get-pgf-code)) children)
                    "")
       "\\end{pgfpicture}"))))

;; simple pgf commands
(define pgf-command%
  (class* object% (pgf<%>)
    (super-new)
    (init name)
    (define pgf-name name)
    (init-field args)
    (init-field num-args)
    
    (when (not (and (list? args)
                    (= (length args) num-args)))
      (error (string->symbol name)
             "wrong number of arguments: ~s"
             args))
    
    (define/public (get-pgf-code)
      (string-append 
       "\\" pgf-name
       (string-join (map (λ (arg) 
                           (string-append "{" (send arg get-pgf-code) "}"))
                         args)
                    "")))))

;; wrap a value in a pgf object
(define pgf-wrap%
  (class* object% (pgf<%>)
    (super-new)
    (init-field val)
    (define/public (get-pgf-code)
      (format "~a" val))))

;; for defining simple commands
(define-syntax-rule (define-pgf-command CNAME NAME NUM-ARGS)
  (define CNAME
    (class pgf-command% 
      (super-new [name NAME]
                 [num-args NUM-ARGS]))))

(define-pgf-command pgf-point% "pgfpoint" 2)
(define-pgf-command pgf-point-origin% "pgfpointorigin" 0)
(define-pgf-command pgf-path-move-to% "pgfpathmoveto" 1)
(define-pgf-command pgf-path-line-to% "pgfpathlineto" 1)
(define-pgf-command pgf-path-ellipse% "pgfpathellipse" 3)
(define-pgf-command pgf-set-stroke-opacity% "pgfsetstrokeopacity" 1)
(define-pgf-command pgf-set-fill-opacity% "pgfsetfillopacity" 1)
(define-pgf-command pgf-path-rectangle-corners% "pgfpathrectanglecorners" 2)
(define-pgf-command pgf-transform-reset% "pgftransformreset" 0)
(define-pgf-command pgf-transform-x-scale% "pgftransformxscale" 1)
(define-pgf-command pgf-transform-y-scale% "pgftransformyscale" 1)
(define-pgf-command pgf-transform-shift% "pgftransformshift" 1)

;; utilities
(define (rad->deg rad)
  (real->decimal-string (/ (* 180 rad) 3.1415)))

;; More complicated cases
(define pgf-coordinate%
  (class* object% (pgf<%>)
    (super-new)
    (init-field val)
    (define/public (get-pgf-code)
      ;; this should depend on the default coordinates used
      (format "~apt" (real->decimal-string val)))))

(define pgf-use-path%
  (class* object% (pgf<%>)
    (super-new)
    (init-field args)
    (define/public (get-pgf-code)
      (string-append
       "\\" "pgfusepath"
       (format "{~a}" (string-join (map symbol->string args) ","))))))

;; transforms
(define pgf-transform-rotate%
  (class* object% (pgf<%>)
    (super-new)
    (init-field angle)
    (define/public (get-pgf-code)
      (format "\\pgftransformrotate{~a}"
              (rad->deg angle)))))

;; arcs
(define pgf-path-arc%
  (class* object% (pgf<%>)
    (super-new)
    (init-field start-angle end-angle
                x-radius y-radius)
    (define/public (get-pgf-code)
      (format "\\pgfpatharc{~a}{~a}{~a and ~a}"
              (rad->deg start-angle)
              (rad->deg end-angle)
              (real->decimal-string x-radius)
              (real->decimal-string y-radius)))))

;; for text
(define pgf-text%
  (class* object% (pgf<%>)
    (super-new)
    (init-field text point)
    (define/public (get-pgf-code)
      (if point
          (format "\\pgftext[base,at=~a,left,top]{~a}"
                  (send point get-pgf-code) text)
          (format "\\pgftext{~a}" text)))))

;; commands for setting colors
(define pgf-set-color-base%
  (class* object% (pgf<%>)
    (super-new)
    (init-field name arg)
    (define/public (get-pgf-code)
      (format "\\~a{~a}" name arg))))

(define pgf-set-fill-color%
  (class pgf-set-color-base%
    (super-new [name "pgfsetfillcolor"])))

(define pgf-set-stroke-color%
  (class pgf-set-color-base%
    (super-new [name "pgfsetstrokecolor"])))

(define pgf-define-color%
  (class* object% (pgf<%>)
    (super-new)
    (init-field color-name red green blue)
    (define/public (convert color)
      (real->decimal-string (/ color 255)))
    (define/public (get-pgf-code)
      (format "\\definecolor{~a}{rgb}{~a,~a,~a}"
              color-name 
              (convert red) 
              (convert green)
              (convert blue)))))