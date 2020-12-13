#lang racket
(require racket/file
         racket/runtime-path)

(define-runtime-path f1 "test.txt")
(define test-data (map string->number (file->lines f1 #:mode 'text)))
(define-runtime-path f2 "data.txt")
(define run-data (map string->number (file->lines f2 #:mode 'text)))

(define (slice l offset n)
  (take (drop l offset) n))

(define (get-window lon ndx window)
  (slice lon (- ndx window) window))

(module+ test
  (require chk)
  (define eight '(1 2 3 4 5 6 7 8))
  (chk
   #:=
   (get-window eight 5 3)
   '(3 4 5)
   (get-window eight 7 1)
   '(7)
   (get-window eight 1 1)
   '(1)
   (get-window eight 7 7)
   (reverse (rest (reverse eight)))
   ))

(define (snoc o ls)
  (reverse (cons o (reverse ls))))

(define (make-pairs ls)
  (cond
    [(empty? ls) empty]
    [else
     (append (map (λ (n)
                    (list (first ls) n))
                  (rest ls))
             (make-pairs (rest ls)))]))

(module+ test
  (chk
   #:=
   (make-pairs '(1 2 3))
   '((1 2) (1 3) (2 3))
   (make-pairs '(1 2 3 4))
   '((1 2) (1 3) (1 4) (2 3) (2 4) (3 4))
   ))

(define (sum-in-prev? lon ndx window-length)
  (define window (get-window lon ndx window-length))
  (define pairs (make-pairs window))
  (define sums (map (λ (p) (apply + p)) pairs))
  (define num (list-ref lon ndx))
  (member num sums))

(module+ test
  (chk
   #:t
   (sum-in-prev? '(1 2 3 4 5 6 7 8 9 10) 4 3)
   (sum-in-prev? '(2 4 6 8 10 12 14) 4 3)
   #:!
   (sum-in-prev? '(100 200 300 400 10 12 14) 4 3)
   ))

(define (xmas lon ndx wl)
  (cond
    [(empty? lon) -1]
    [(sum-in-prev? lon ndx wl)
     (xmas lon (add1 ndx) wl)]
    [else
     (list-ref lon ndx)]))

(module+ test
  (chk
   #:=
   (xmas test-data 5 5)
   127
   ))

;; First part
(xmas run-data 25 25)

;; Now, find a contiguous set of numbers that add up to a target number.

(define (sum-range lon ndx wl)
  (apply + (get-window lon ndx wl)))

(define (->key a b)
  (string->symbol (format "s~a-~a" a b)))

(define done (mutable-set))
(define (contiguous-sum tgt lon ndx wl)
  (cond
    [(set-member? done (->key ndx wl)) false]
    [(or (>= ndx (length lon)) (< (- ndx wl) 0)) false]
    [(empty? lon) false]
    [(= (sum-range lon ndx wl) tgt)
     (list (- ndx wl) ndx)]
    [else
     (set-add! done (->key ndx wl))
     (or (contiguous-sum tgt lon (add1 ndx) wl)
         (contiguous-sum tgt lon ndx (add1 wl))
         )]
    ))

(define (smallest lon)
  (first (sort lon <)))
(define (largest lon)
  (first (sort lon >)))

(define (encryption-weakness lon ndx wl)
  (set! done (mutable-set))
  (define x (xmas lon ndx wl))
  (define ndxs (contiguous-sum x lon 2 2))
  ;; (printf "~a - ~a~n" ndxs (map (λ (ndx) (list-ref lon ndx)) ndxs))
  (define w (get-window lon (second ndxs) (- (second ndxs) (first ndxs))))
  ;; (printf "\tw: ~a~n" w)
  ;; SMALLEST AND LARGEST
  ;; Not FIRST AND LAST
  (+ (smallest w) (largest w))
  )

(module+ test
  (chk
   #:=
   (xmas run-data 25 25)
   18272118
   (encryption-weakness test-data 5 5)
   62
   ))

;; Second part
;; 1836907 is too low
;; (394 410) - (720149 1215939)
(encryption-weakness run-data 25 25)
