#lang racket
(require racket/file
         racket/runtime-path
         racket/performance-hint)

(define (->symbol o)
  (string->symbol (format "~a" (if (equal? o #\.) "_" o))))

(define make-chart
  (case-lambda
    [(evo r c ls)
     (list->vector (list evo r c (if (list? ls) (list->vector ls) ls)))]
    [(r c)
     (list->vector (append (list 0 r c (make-vector (* r c) '_))))]
    [(ch)
     (make-chart (chart-rows ch) (chart-columns ch))]))

(define (chart-evo ch)
  (vector-ref ch 0))
(define-inline (chart-rows ch)
  (vector-ref ch 1))
(define-inline (chart-columns ch)
  (vector-ref ch 2))
(define (chart-linear ch)
  (vector-ref ch 3))
(define (set-chart-evo! ch v)
  (vector-set! ch 0 v))
(define (set-chart-linear! ch v)
  (vector-set! ch 3 v))
(define (->chart lines)
  (let ([raw lines])
    (make-chart 0 (length raw) (string-length (first raw))
                (map ->symbol (apply append (map string->list raw))))))

(define (copy-map m)
  (define new-v (make-vector (vector-length m)))
  (define new-m (make-vector (vector-length (chart-linear m))))
  (for ([ndx (vector-length (chart-linear m))])
    (vector-set! new-m ndx (vector-ref (chart-linear m) ndx)))
  (for ([ndx (vector-length m)])
    (vector-set! new-v ndx (vector-ref m ndx)))
  (vector-set! new-v 3 new-m)
  new-v)

(define-runtime-path f1 "small.map")
(define small-map (->chart (file->lines f1 #:mode 'text)))
(define-runtime-path f2 "large.map")
(define large-map (->chart (file->lines f2 #:mode 'text)))

(define-inline (rc->ndx ch r c)
  (+ (* r (chart-rows ch)) c))

(define (lookup ch r c)
  (cond
    [(or (>= r (chart-rows ch))
         (>= c (chart-columns ch))
         (< r 0)
         (< c 0))
     '_]
    [else
     (vector-ref (chart-linear ch) (rc->ndx ch r c))]))

(module+ test
  (require rackunit)
  (check-equal?
   (lookup small-map 0 0)
   'L)
  (check-equal?
   (lookup small-map 0 1)
   '_)
  (check-equal?
   (lookup small-map 9 9)
   'L)
  )

(define (set-seat! ch r c v)
  (vector-set! (chart-linear ch) (rc->ndx ch r c) v)
  ;;(set-chart-evo! ch (add1 (chart-evo ch)))
  )

(module+ test
  (check-equal?
   (let ()
     (define new-m (copy-map small-map))
     (set-seat! new-m 0 0 'x)
     (chart-linear new-m))
   (list->vector
    '(x _ L L _ L L _ L L
        L L L L L L L _ L L
        L _ L _ L _ _ L _ _
        L L L L _ L L _ L L
        L _ L L _ L L _ L L
        L _ L L L L L _ L L
        _ _ L _ L _ _ _ _ _
        L L L L L L L L L L
        L _ L L L L L L _ L
        L _ L L L L L _ L L)))

  (check-equal?
   (let ()
     (define new-m (copy-map small-map))
     (set-seat! new-m 9 9 'x)
     (chart-linear new-m))
   (list->vector
    '(L _ L L _ L L _ L L
        L L L L L L L _ L L
        L _ L _ L _ _ L _ _
        L L L L _ L L _ L L
        L _ L L _ L L _ L L
        L _ L L L L L _ L L
        _ _ L _ L _ _ _ _ _
        L L L L L L L L L L
        L _ L L L L L L _ L
        L _ L L L L L _ L x)))
  )

(define empty-chart
  (case-lambda
    [(r c)
     (make-chart r c)]
    [(ch)
     (make-chart ch)]))

(module+ test
  (check-equal?
   (empty-chart 2 2)
   (make-chart 0 2 2 (list->vector '(_ _ _ _))))
  (check-equal?
   (empty-chart 3 3)
   (make-chart 0 3 3 (list->vector '(_ _ _ _ _ _ _ _ _))))
  )

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
        
(define (rules ch r c currently)
  ;; (define currently (lookup ch r c))
  (cond
    [(and (empty-seat? currently)
          (all-clear? ch r c))
     'occupied]
    [(and (occupied-seat? currently)
          (>= (surrounding-occupancy ch r c) 4))
     'empty]
    [else 'pass]))

(define (advance ch new-ch)
  (for ([r (chart-rows ch)])
    (for ([c (chart-columns ch)])
      (define v (lookup ch r c))
      (case (rules ch r c v)
        [(occupied)
         (set-seat! new-ch r c 'o)
         ]
        [(empty)
         (set-seat! new-ch r c 'L)
         ]
        [(pass)
         (set-seat! new-ch r c v)
         ])
      ))
  (set-chart-evo! new-ch (add1 (chart-evo ch)))
  new-ch
  )

(define (draw-chart ch)
  (for ([r (chart-rows ch)])
    (for ([c (chart-columns ch)])
      (printf "~a " (lookup ch r c)))
    (printf "~n")))

(define (chart->string ch)
  (define os (open-output-string))
  (parameterize ([current-output-port os])
    (draw-chart ch))
  (get-output-string os))

(module+ test
  (define new-ch (empty-chart (chart-rows small-map)
                              (chart-columns small-map)))
  (check-equal?
   (let ()
     (define tmp (empty-chart small-map))
     (advance small-map tmp)
     (chart->string tmp))
   (apply
    string-append
    (map (λ (s) (format "~a ~n" s))
         (list 
          "o _ o o _ o o _ o o"
          "o o o o o o o _ o o"
          "o _ o _ o _ _ o _ _"
          "o o o o _ o o _ o o"
          "o _ o o _ o o _ o o"
          "o _ o o o o o _ o o"
          "_ _ o _ o _ _ _ _ _" 
          "o o o o o o o o o o" 
          "o _ o o o o o o _ o" 
          "o _ o o o o o _ o o"))))
  (check-equal?
   (let ()
     (define tmp (empty-chart small-map))
     (define tmp2 (empty-chart small-map))
     (advance (advance small-map tmp)
              tmp2)
     (chart->string tmp2))
   (apply
    string-append
    (map (λ (s) (format "~a ~n" s))
         (list
          "o _ L L _ L o _ o o"
          "o L L L L L L _ L o" 
          "L _ L _ L _ _ L _ _" 
          "o L L L _ L L _ L o" 
          "o _ L L _ L L _ L L" 
          "o _ L L L L o _ o o" 
          "_ _ L _ L _ _ _ _ _" 
          "o L L L L L L L L o" 
          "o _ L L L L L L _ L"
          "o _ o L L L L _ o o"))))
  )

(define (stabilize ch1)
  (define ch2 (empty-chart ch1))
  (let loop ([tick 0])
    ;;(printf "--- ~a ---~n" tick)
    (cond
      [(even? tick)
       ;;(printf (chart->string ch1))
       (advance ch1 ch2)]
      [else
       ;;(printf (chart->string ch2))
       (advance ch2 ch1)])
    (unless (equal? (chart-linear ch1)
                    (chart-linear ch2))
      ;;(printf "~n")
      (loop (add1 tick))))
  
  (if (> (chart-evo ch1)
         (chart-evo ch2))
      ch1 ch2))


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
  (check-equal?
   (count-occupied (stabilize small-map))
   37))

;; Part 1
(define stabilized (stabilize large-map))
(count-occupied stabilized)