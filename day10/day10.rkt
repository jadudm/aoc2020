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

;; Part two.

;; There's a pattern. It's either a series or a state machine.

(define l1 '(1))
(define l2 '(1 2))
(define l3 '(1 2 3))
(define l4 '(1 2 3 4))
(define l5 '(1 2 4))
(define l6 '(1 2 4 5))
(define l6a '(1 2 4 5 7))
(define l7 '(1 2 3 6 7))
(define l8 '(1 2 3 6 8))
(define l8a '(1 2 3 6 8 9))
(define l8b '(1 2 3 6 8 10))
(define l9 '(1 2 3 6 9))
(define l10 '(1 2 3 6 9 12))

;; How do these behave?

(define (diff a b)
  (abs (- a b)))

(define (list-of-diffs lon [prev 0])
  (cond
    [(empty? lon) '()]
    [else
     (cons (diff (first lon) prev)
           (list-of-diffs (rest lon) (first lon)))]))

(module+ test
  (chk
   #:=
   (list-of-diffs l1)
   '(1)
   (list-of-diffs l2)
   '(1 1)
   (list-of-diffs l3)
   '(1 1 1)
   (list-of-diffs l5)
   '(1 1 2)
   (list-of-diffs l6)
   '(1 1 2 1)
   ))

(define (slice l offset n)
  (define dropped (drop l offset))
  (cond
    [(>= n (length dropped))
     (take dropped (length dropped))]
    [else
     (take dropped n)]))

(define (get-window ls ndx num)
  (cond
    [(< (length ls) num) ls]
    [(< ndx num) (take ls (add1 ndx))]
    [else
     (slice ls (add1 (- ndx num)) num)]))

(module+ test
  (define test-ls '(1 2 3 4 5 6 7 8))
  (chk
   #:=
   (get-window test-ls 3 3)
   '(2 3 4)
   (get-window test-ls 2 3)
   '(1 2 3)
   (get-window test-ls 0 3)
   '(1)
   ))

(define (sum ls)
  (apply + ls))

(define (snoc o ls)
  (reverse (cons o (reverse ls))))

(define (clamp n low high)
  (cond
    [(<= n low) low]
    [(>= n high) high]
    [else n]))

(define (running-sums lon lod [acc (list 1)] [ndx 0] [prev-diff 1] [how-many 3])
  (cond
    [(empty? lon) acc]
    [else
     (define this-diff (list-ref lod ndx))
     (define new-how-many
       (cond
         [(and (= this-diff 1)
               (= prev-diff 1)) (clamp (add1 how-many) 1 3)]
         [(and (= this-diff 1)
               (= prev-diff 2)) 2]
         [(and (= this-diff 1)
               (= prev-diff 3)) 1]
         [(and (= this-diff 2)
               (= prev-diff 1)) 2]
         [(and (= this-diff 2)
               (= prev-diff 2)) how-many]
         [(and (= this-diff 3)
               (= prev-diff 3)) 1]
         [else 1]
         ))
     (define new-sum (sum (get-window acc ndx new-how-many)))
     (running-sums (rest lon) lod (snoc new-sum acc) (add1 ndx) (list-ref lod ndx) new-how-many)
     ]))


(define (distinct-ways-improved lon)
  (define diffs (list-of-diffs lon))
  (define rs (running-sums lon diffs))
  ;;(printf "l: ~a~n" lon)
  ;;(printf "d: ~a~n" diffs)
  ;;(printf "s: ~a~n" rs)
  (first (reverse rs))
  )

(module+ test
  (chk
   #:=
   (distinct-ways l1)
   (distinct-ways-improved l1)

   (distinct-ways l2)
   (distinct-ways-improved l2)

   (distinct-ways l3)
   (distinct-ways-improved l3)

   (distinct-ways l4)
   (distinct-ways-improved l4)

   (distinct-ways l5)
   (distinct-ways-improved l5)

   (distinct-ways l6)
   (distinct-ways-improved l6)

   (distinct-ways l6a)
   (distinct-ways-improved l6a)

   (distinct-ways l7)
   (distinct-ways-improved l7)

   (distinct-ways l8)
   (distinct-ways-improved l8)

   (distinct-ways l8a)
   (distinct-ways-improved l8a)

   (distinct-ways l8b)
   (distinct-ways-improved l8b)

   (distinct-ways l9)
   (distinct-ways-improved l9)

   (distinct-ways l10)
   (distinct-ways-improved l10)

   (distinct-ways (sort t1 <))
   (distinct-ways-improved (sort t1 <))

   (distinct-ways (sort t2 <))
   (distinct-ways-improved (sort t2 <))
   ))

(distinct-ways-improved (sort d1 <))