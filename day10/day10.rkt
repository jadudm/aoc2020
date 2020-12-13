#lang racket
(require racket/file
         racket/runtime-path)

(define-runtime-path f1 "t1.txt")
(define t1 (map string->number (file->lines f1 #:mode 'text)))
(define-runtime-path f2 "t2.txt")
(define t2 (map string->number (file->lines f2 #:mode 'text)))
(define-runtime-path f3 "d1.txt")
(define d1 (map string->number (file->lines f3 #:mode 'text)))

;; Find a number that is 1, 2, or 3 more than the
;; target
(define (find lon tgt)
  (cond
    [(empty? lon) '()]
    [(and (not (zero? (- (first lon) tgt)))
          (<= (- (first lon) tgt) 3)
          (> (- (first lon) tgt) 0)
          )
     (cons (first lon) (find (rest lon) tgt))]
    [else
     (find (rest lon) tgt)]
    ))

(define (smallest lon)
  (first (sort lon <)))

(define (largest lon)
  (first (sort lon >)))

(define (chain lon jolts max diffs)
  (cond
    ;; Make sure we catch the final diff!
    [(empty? lon) (list (list jolts diffs))]
    [else
     (define possible (find lon jolts))
     (define diff (- (smallest possible) jolts))
     (cons (list (smallest possible) diffs)
           (chain (remove (smallest possible) lon)
                  (smallest possible)
                  max
                  (hash-set diffs diff
                            (add1 (hash-ref diffs diff 0)))
                  ))]
    ))

(define (one-by-three lon)
  (define include-adapter (cons (+ 3 (largest lon)) lon))
  (define result (chain include-adapter 0 (largest include-adapter) (hash)))
  (define diffs (second (first (reverse result))))
  (* (hash-ref diffs 1 0)
     (hash-ref diffs 3 0)))

(module+ test
  (require chk)
  (chk
   #:=
   (one-by-three t1)
   35
   (one-by-three t2)
   220
   ))

;; First part
(one-by-three d1)

(define (count lon jolts max)
  (cond
    ;; Make sure we catch the final diff!
    [(empty? lon) 1]
    [(= jolts max) 1]
    [else
     (define possible (find lon jolts))
     ;;(printf "~a ~a ~a~n" lon jolts possible)
     (apply + (map (λ (p) (count (remove p lon) p max))
                   possible))]
    ))

(define (distinct-ways lon)
  (count (sort (cons (+ 3 (largest lon)) lon) <)
         0 (+ 3 (largest lon))))

(module+ test
  (chk
   #:=
   (distinct-ways t1)
   8
   (distinct-ways t2)
   19208
   ))

;; Second part
;;(distinct-ways d1)
          
