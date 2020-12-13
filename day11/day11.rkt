#lang racket
(require racket/file
         racket/runtime-path)

(define (->symbol o)
  (string->symbol (format "~a" (if (equal? o #\.) "_" o))))

(struct chart (evo rows columns linear) #:transparent #:mutable)

(define (->chart lines)
  (let ([raw lines])
    (chart 0 (length raw) (string-length (first raw))
           (map ->symbol (apply append (map string->list raw))))))

(define-runtime-path f1 "small.map")
(define small-map (->chart (file->lines f1 #:mode 'text)))
(define-runtime-path f2 "large.map")
(define large-map (->chart (file->lines f2 #:mode 'text)))

(define (rc->ndx ch r c)
  (+ (* r (chart-rows ch)) c))

(define (lookup ch r c)
  (cond
    [(or (>= r (chart-rows ch))
         (>= c (chart-columns ch))
         (< r 0)
         (< c 0))
     '_]
    [else
     (list-ref (chart-linear ch) (rc->ndx ch r c))]))

(module+ test
  (require chk)
  (chk
   #:=
   (lookup small-map 0 0)
   'L
   (lookup small-map 0 1)
   '_
   (lookup small-map 9 9)
   'L
   ))

(define (set-seat! ch r c v)
  (define new-seating
    (list-set (chart-linear ch) (rc->ndx ch r c) v))
  (set-chart-evo! ch (add1 (chart-evo ch)))
  (set-chart-linear! ch new-seating))

(define (set-seat ch r c v)
  (define new-seating
    (list-set (chart-linear ch) (rc->ndx ch r c) v))
  (chart (add1 (chart-evo ch))
         (chart-rows ch)
         (chart-columns ch)
         new-seating))

(module+ test
  (chk
   #:=
   (set-seat small-map 0 0 'x)
   (chart 1 10 10 '(x _ L L _ L L _ L L
                      L L L L L L L _ L L
                      L _ L _ L _ _ L _ _
                      L L L L _ L L _ L L
                      L _ L L _ L L _ L L
                      L _ L L L L L _ L L
                      _ _ L _ L _ _ _ _ _
                      L L L L L L L L L L
                      L _ L L L L L L _ L
                      L _ L L L L L _ L L))

   (set-seat small-map 9 9 'x)
   (chart 1 10 10 '(L _ L L _ L L _ L L
                      L L L L L L L _ L L
                      L _ L _ L _ _ L _ _
                      L L L L _ L L _ L L
                      L _ L L _ L L _ L L
                      L _ L L L L L _ L L
                      _ _ L _ L _ _ _ _ _
                      L L L L L L L L L L
                      L _ L L L L L L _ L
                      L _ L L L L L _ L x))
   ))

(define (empty-chart r c)
  (chart 0 r c (make-list (* r c) 'L)))

(module+ test
  (chk
   #:=
   (empty-chart 2 2)
   (chart 0 2 2 '(L L L L))
   (empty-chart 3 3)
   (chart 0 3 3 '(L L L
                    L L L
                    L L L))
   ))

(define (empty-seat? s)
  (equal? s 'L))

(define (clear-space? s)
  (or (equal? s 'L)
      (equal? s '_)))

(define (get-surrounds ch r c)
  (list (lookup ch (sub1 r) (sub1 c))
        (lookup ch (sub1 r) c)
        (lookup ch (sub1 r) (add1 c))
        (lookup ch r (add1 c))
        (lookup ch (add1 r) (add1 c))
        (lookup ch (add1 r) c)
        (lookup ch (add1 r) (sub1 c))
        (lookup ch r (sub1 c))))

(define (all-clear? ch r c)
  (define surrounds (get-surrounds ch r c))
  (andmap clear-space? surrounds))

(define (surrounding-occupancy ch r c)
  (define surrounds (get-surrounds ch r c))
  (apply + (map (λ (o) (if (equal? o 'o) 1 0))
                surrounds)))

(define (occupied-seat? s)
  (equal? s 'o))
        
(define (rules ch r c)
  (define currently (lookup ch r c))
  (cond
    [(and (empty-seat? currently)
          (all-clear? ch r c))
     'occupied]
    [(and (occupied-seat? currently)
          (>= (surrounding-occupancy ch r c) 4))
     'empty]
    [else 'pass]))

(define (advance ch)
  (define new-ch (empty-chart (chart-rows ch)
                              (chart-columns ch)))
  (for ([r (chart-rows ch)])
    (for ([c (chart-columns ch)])
      (case (rules ch r c)
        [(occupied)
         (set-seat! new-ch r c 'o)
         'occupied]
        [(empty)
         (set-seat! new-ch r c 'L)
         'empty]
        [else
         (set-seat! new-ch r c (lookup ch r c))
         'pass])
      ))  
  new-ch
  )

(define (draw-chart ch)
  (for ([r (chart-rows ch)])
    (for ([c (chart-columns ch)])
      (printf "~a " (lookup ch r c)))
    (printf "~n")))

(module+ test
  (chk
   #:=
   (chart-linear (advance small-map))
   '(o _ o o _ o o _ o o 
       o o o o o o o _ o o 
       o _ o _ o _ _ o _ _ 
       o o o o _ o o _ o o 
       o _ o o _ o o _ o o 
       o _ o o o o o _ o o 
       _ _ o _ o _ _ _ _ _ 
       o o o o o o o o o o 
       o _ o o o o o o _ o 
       o _ o o o o o _ o o)

   (chart-linear
    (advance (advance small-map)))
   '(o _ L L _ L o _ o o 
       o L L L L L L _ L o 
       L _ L _ L _ _ L _ _ 
       o L L L _ L L _ L o 
       o _ L L _ L L _ L L 
       o _ L L L L o _ o o 
       _ _ L _ L _ _ _ _ _ 
       o L L L L L L L L o 
       o _ L L L L L L _ L 
       o _ o L L L L _ o o)
   ))

(define (stabilize ch)
  (define next (advance ch))
  (cond
    [(equal? (chart-linear ch)
             (chart-linear next))
     ch]
    [else (stabilize next)]))


(define (count ch sym)
  (define cnt 0)
  (for ([r (chart-rows ch)])
    (for ([c (chart-columns ch)])
      (when (equal? (lookup ch r c) sym)
        (set! cnt (add1 cnt)))))
  cnt)

(define (count-occupied ch)
  (count ch 'o))

(module+ test
  (chk
   #:=
   (count-occupied (stabilize small-map))
   37))

;; Part 1
(count-occupied (stabilize large-map))