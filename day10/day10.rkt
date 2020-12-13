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

(define (count lonh jolts max)
  (cond
    [(= jolts max) 1]
    [else
     (+ (if (hash-ref lonh (+ 1 jolts) false)
            (count lonh (+ 1 jolts) max) 0)
        (if (hash-ref lonh (+ 2 jolts) false)
            (count lonh (+ 2 jolts) max) 0)
        (if (hash-ref lonh (+ 3 jolts) false)
            (count lonh (+ 3 jolts) max) 0))]
    ))

(define (list->hash ls)
  (define h (make-hash))
  (for ([n ls])
    (hash-set! h n 0))
  h)

(define (distinct-ways lon)
  (cond
    [(empty? lon) 1]
    [else
     (define extended (sort (cons (+ 3 (largest lon)) lon) <))
     ;;(printf "e: ~a~n" extended)
     (count (list->hash extended)
            0
            (largest extended))
     ]))

(module+ test
  (chk
   #:=
   (distinct-ways t1)
   8
   (distinct-ways t2)
   19208
   ))

;; Second part

;; There must be a pattern. The hint was that
;; running this brute force won't work. ("trillions")
(distinct-ways '()) ;; 1
(distinct-ways '(1)) ;; 1, and actually (1 4)
(distinct-ways '(1 2)) ;; 2 and actually (1 2 5)
(distinct-ways '(1 2 3)) ;; 4
(distinct-ways '(1 2 3 4)) ;; 7
(distinct-ways '(1 2 3 4 5)) ;; 13
(distinct-ways '(1 2 3 4 5 6)) ;; 24
;; Which is...
;; (+ 1 1 2) = 4
;; (+ 1 2 4) = 7
;; (+ 2 4 7) = 13
;; But this is for a list of contiguous numbers.
(distinct-ways '(0)) ;; 1
(distinct-ways '(0 2)) ;; 1
(distinct-ways '(0 2 4)) ;; 1
(distinct-ways '(0 2 4 6)) ;; 1
(distinct-ways '(0 2 4 6 8)) ;; 1
;; Hmm.
(distinct-ways '(0 1 2 3)) ;; 4
(distinct-ways '(0 1 2 3 4)) ;; 7
(distinct-ways '(0 1 2 3 5)) ;; 6 -> +1 for skip 2, and +1 for skip 3?
(distinct-ways '(0 1 2 3 6)) ;; 4
(distinct-ways '(0 1 2 3 4 6)) ;; 11
(distinct-ways '(0 1 2 3 5 7)) ;; 6
(distinct-ways '(0 1 2 3 5 7 9)) ;; 6
(distinct-ways '(0 1 2 3 4 5)) ;; 13
(distinct-ways '(0 1 2 3 4 5 7 10)) ;; 20
(distinct-ways '(0 1 2 3 4 5 7 10 13)) ;; 20
(distinct-ways '(0 1 2 3 4 5 8 11 14)) ;; 13
(distinct-ways '(0 1 2 3 4 5 8 11 14 15)) ;; 13
(distinct-ways '(0 1 2 3 4 5 8 10 12 14)) ;; 13
(distinct-ways '(0 1 2 3 4 5 7 11 13 16)) ;; 0
(distinct-ways '(0 1 2 3 4 5 7 10 13 16)) ;; 20
(distinct-ways '(0 1 2 3 4 5 7 10 13 15)) ;; 20
(distinct-ways '(0 1 2 3 4 5 7 10 12)) ;; 20
;; Hm. '(1 4 5 6 7 10 11 12 15 16 19)
(distinct-ways '(0 1 4)) ;; 1
(distinct-ways '(0 1 4 5)) ;; 1
(distinct-ways '(0 1 4 5 6)) ;; 2
(distinct-ways '(0 1 4 5 6 7)) ;; 4
(distinct-ways '(0 1 4 5 6 7 10)) ;; 4
(distinct-ways '(0 1 4 5 6 7 10 11)) ;; 7
(distinct-ways '(0 1 4 5 6 7 10 11 12)) ;; 8
(distinct-ways '(0 1 4 5 6 7 10 11 12 15)) ;; 8
(distinct-ways '(0 1 4 5 6 7 10 11 12 15 16)) ;; 8
(distinct-ways '(0 1 4 5 6 7 10 11 12 15 16 19)) ;; 8
(distinct-ways '(0 1 4 5 6 7 10 11 12 15 16 19 20)) ;; 8 
(distinct-ways '(0 1 4 5 6 7 10 11 12 15 16 19 20 21)) ;; 16 (+ 8 8 1)
(distinct-ways '(0 1 4 5 6 7 10 11 12 15 16 19 20 21 22)) ;; 32 (+ 16 8 8)
(distinct-ways '(0 1 4 5 6 7 10 11 12 15 16 19 20 21 22 23)) ;; 56 (+ 32 16 8)

;; Something to do with the length of the
;; contiguous run.
(distinct-ways '()) ;; 1
(distinct-ways '(1)) ;; 1, and actually (1 4)
(distinct-ways '(1 2)) ;; 2 and actually (1 2 5)
(distinct-ways '(1 2 3)) ;; 4
(distinct-ways '(1 2 3 4)) ;; 7
(distinct-ways '(1 2 3 4 5)) ;; 13
(distinct-ways '(1 2 3 4 5 6)) ;; 24
(distinct-ways '(1 2 3 4 5 6 8)) ;; 37
(distinct-ways '(1 2 3 4 5 6 8 10)) ;; 37
(distinct-ways '(1 2 3 4 5 6 8 10 11)) ;; 74 or (+ 37 24 13)
(distinct-ways '(1 2 3 4 5 6 8 10 11 13)) ;; 111 (+ 74 37)
(distinct-ways '(1 2 3 4 5 6 8 10 11 13 15)) ;; 111 (+ 111)
(distinct-ways '(1 2 3 4 5 6 8 10 11 13 15 18)) ;; 111
(distinct-ways '(1 2 3 4 5 6 8 10 11 13 15 18 19)) ;; 111
(distinct-ways '(1 2 3 4 5 6 8 10 11 13 15 18 19 20)) ;; 222 (+ 111 111)

(define (snoc o ls)
  (reverse (cons o (reverse ls))))

(define (diff-n n)
  (λ (a b)
    (= (abs (- a b)) n)))

(define diff-one? (diff-n 1))
(define diff-two? (diff-n 2))
(define diff-three? (diff-n 3))

(define (inc n)
  (if (< n 3)
      (add1 n)
      n))
(define (dec n)
  (if (> n 0)
      (sub1 n)
      n))

(define (safe-take ls n)
  (cond
    [(zero? n) (take ls 1)]
    [(< (length ls) n)
     (take ls (length ls))]
    [else (take ls n)]))

(define (pattern-count lon jolts [ways empty] [tc 0])
  (printf "~a ~a ~a ~a~n" lon jolts ways tc)
  (cond
    [(empty? lon) (apply + (safe-take ways tc))]
    [else
     (define new-tc
       (cond
         [(diff-one? (first lon) jolts) (inc tc)]
         [(diff-two? (first lon) jolts) (dec tc)]
         [else  0]))
     
     (define new-ways
       (cond
         [(empty? ways) (list 1)]
         [else
          (cons (apply + (safe-take ways new-tc))
                ways)]))
     
     (pattern-count (rest lon)
                    (+ (abs (- (first lon) jolts)) jolts)
                    new-ways
                    new-tc)]
    ))

(define (distinct-ways-improved lon)
  (cond
    [(empty? lon) 1]
    [else (pattern-count (snoc (+ 3 (largest lon)) lon) 0 empty 0)]))

'---
;1 (pattern-count empty 0 (list 1) 0) ;; 1
1 (pattern-count '(1) 0 empty 0) ;; 1
2 (pattern-count '(1 2) 0 empty 0) ;; 1
4 (pattern-count '(1 2 3) 0 empty 0) ;; 2
7 (pattern-count '(1 2 3 4) 0 empty 0) ;; 2


(module+ test
  (chk
   #:=
   (distinct-ways-improved (sort t1 <))
   8
   (distinct-ways-improved (sort t2 <))
   19208
   ))
;; Part 2
;;(distinct-ways-improved (sort d1 <))