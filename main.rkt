#lang racket

;;; Ordinals ;;;

; nat -> ordinal
(define (n2o i) (for/list ([_ i]) '()))
; omega
(define w (list (n2o 1)))
; ordinal -> string
(define (o2s ord) 
  (if (null? ord) "0" (string-join (for/list ([o ord]) (format "w^(~a)" (o2s o))) "+")))

;;; Lists ;;;

; ordinal -> path
(define (o2p ord) (filter char-punctuation? (string->list (string-trim (~a ord) #px"."))))
; path -> ordinal
(define (p2o path) (port->list read (open-input-string (string-join path))))
; get left from pair
(define (l pair) (if (cons? pair) (car pair) '()))
; get right from pair
(define (r pair) (if (cons? pair) (cdr pair) '()))
; slice into list by ord
(define (get ord L)
  (define (get path L) (match path
    [(cons #\( rst) (get rst (l L))]
    [(cons #\) rst) (get rst (r L))]
    [_ L]))
  (get (o2p ord) L))
; prepend onto list by ord ; assumes ord == w^a for some a
(define (put ord L)
  (define (put path M) (match path
    [(cons #\( rst) (cons (put rst (l M)) '())]
    [(cons #\) rst) (cons (l M) (put rst (r M)))]
    [_ L]))
  (put (o2p ord) L))
; get length of L
(define (len L) 
  (define (longest L) (match L
    ['() '()]
    [(cons '() r) (cons ")" (longest r))]
    [(cons l _) (cons "(" (longest l))]))
  (p2o (longest L)))

;;; Drawing ;;;

(require memo)

; dump L into a *.dot file to be rendered with `neato *.dot -n -Tpng -O`
(define (draw L [filename "out.dot"])
  (define file (open-output-file filename #:exists 'replace))

  (define (out [line ""]) (displayln line file))
  (define (name x y) (format "n_~a_~a" x y))
  (define (node x y) 
    (out (format "  ~a \t[pos=\"~a,~a!\", fillcolor=~a];" 
      (name x y) (* -50 x) (* 50 y) (if (> y 0) "grey" "white"))) 
    (cons x y))
  (define (edge x1 y1 x2 y2) (out (format "  ~a \t-> ~a;" (name x1 y1) (name x2 y2))))

  (define/memoize (draw L) (match L
    ['()            (node 0 0)]
    [(cons '() '()) (draw '()) (edge 1 1 0 0) (node 1 1)]
    [(cons l '())   (match-let ([(cons lx ly) (draw l)]) 
      (define x (+ lx 1)) (define y (- ly 1))
      (edge x y lx ly) (node x y)
    )]
    [(cons l r)     (match-let ([(cons lx ly) (draw l)] [(cons rx ry) (draw r)]) 
      (define x (+ rx 1)) (define y (+ ry 1))
      (unless (null? l) (edge x y lx ly))
      (edge x y rx ry) (node x y)
    )]))

  (out "digraph G {")
  (out "  splines=true;")
  (out "  node [shape=circle, style=filled, label=\"\"];")
  (draw L)
  (out "}")

  (close-output-port file)
)

;;; Main ;;;

;; Example 1

(define L '())
(set! L (put (n2o 1) L))                  ; put 1
(set! L (put (n2o 1) L))                  ; put 1
(set! L (put w L))                        ; put w 
(set! L (put (n2o 1) L))                  ; put 1
(set! L (put (n2o 1) L))                  ; put 1
(set! L (put (n2o 1) L))                  ; put 1
(printf "(len L) = ~a~n" (o2s (len L)))   ; w^(w^(0))+w^(0)+w^(0) == w+2
(draw L "out/L1.dot")
(set! L (get w L))                        ; get w
(printf "(len L) = ~a~n" (o2s (len L)))   ; w^(0)+w^(0) == 2
(draw L "out/L2.dot")

;; Example 2
(define M '())
(set! M (put (n2o 1) M))                  ; put 1
(set! M (put (list (list (n2o 1))) M))    ; put w^w
(set! M (put (n2o 1) M))                  ; put 1
(set! M (put (list (n2o 3)) M))           ; put w^3
(set! M (put (n2o 1) M))                  ; put 1
(set! M (put (list (n2o 2)) M))           ; put w^2
(set! M (put (n2o 1) M))                  ; put 1
(set! M (put (list (n2o 1)) M))           ; put w
(set! M (put (n2o 1) M))                  ; put 1
(printf "(len M) = ~a~n" (o2s (len M)))   ; w^(w^(w^(0)))+w^(0) = w^w+1
(draw M "out/M1.dot")
(set! M (get (list (n2o 2) '()) M))       ; get w^2+1
(printf "(len M) = ~a~n" (o2s (len M)))   ; w^(w^(w^(0)))+w^(0) = w^w+1
(draw M "out/M2.dot")
