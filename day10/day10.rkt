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
(define (snoc o ls)
  (reverse (cons o (reverse ls))))

(define (safe-take ls n)
  (cond
    [(zero? n) (take ls 1)]
    [(< (length ls) n)
     (take ls (length ls))]
    [else (take ls n)]))

(define (generate-n-ways n [acc empty] #:prob [prob 30])
  (cond
    [(<= n 0) (if (zero? (first (reverse acc)))
                  (rest (reverse acc))
                  (reverse acc))]
    [else
     (if (and (> prob (random 100)) (> n 0))
         (generate-n-ways (- n 2) (snoc (- n 1) acc))
         (generate-n-ways (sub1 n) (snoc n acc)))]))

;; There must be a pattern. The hint was that
;; running this brute force won't work. ("trillions")
(for ([i (range 1 20)])
  (printf "~a~a: ~a ~a ~n"
          (if (< i 10) "0" "")
          i
          (range i)
          (distinct-ways (range i))))

(define (skip-pat ls)
  (for ([i (range 1 (length ls))])
    (printf "~a~a: ~a~a ~a ~a ~n"
            (if (< i 10) "0" "")
            i
            (if (< (list-ref ls (sub1 i)) 10) "0" "")
            (list-ref ls (sub1 i))
            (safe-take ls i)
            (distinct-ways (safe-take ls i)))))

;(skip-pat (generate-n-ways 20))
;(skip-pat (generate-n-ways 20))
;(skip-pat (generate-n-ways 20))

;; If the diff is 1 1x, then add the past two
;; If it is 1 2x or more, add the past three
;; So, 0, 1, 2?
(define (make-n-stack n)
  (lambda ()
    (for/list ([i (range n)])
      0)))
;; Push does not care about the stack size.
(define (push v s)
  ;; Drop the last, push to the  front.
  (define sprime (reverse (rest (reverse s))))
  (cons v sprime))
(define (pop s)
  (define sprime (reverse (rest (reverse s))))
  (snoc 0 sprime))

(define make-2-stack (make-n-stack 2))
(define make-3-stack (make-n-stack 3))
(define make-4-stack (make-n-stack 4))

(module+ test
  (chk
   #:=
   (make-2-stack)
   '(0 0)
   (make-3-stack)
   '(0 0 0)
   (push 1 (make-3-stack))
   '(1 0 0)
   (push 2 (push 1 (make-3-stack)))
   '(2 1 0)))

(define (diff a b)
  (abs (- a b)))
(define (sum ls)
  (apply + ls))

(define (one? n) (= n 1))
(define (two? n) (= n 2))
(define (three? n) (= n 3))

(define (calc-ways dhist ways)
  (cond
    [(zero? (sum dhist)) ways]
    [(one? (sum dhist)) (push (first ways) ways)]
    [(two? (sum dhist)) (push (sum (take ways 2)) ways)]
    [(three? (sum dhist)) (push (sum ways) ways)]
    ))

(define (calc-ways2 dhist ways)
  (push (sum (for/list ([ndx (range (length dhist))])
               (if (one? (list-ref dhist ndx))
                   (list-ref ways ndx)
                   0)))
        ways))
     

(define (next-dhist f p dhist)
  (define d (diff f p))
  (cond
    [(zero? d) (push 1 dhist)]
    [(one? d) (push 1 dhist)]
    [(two? d) (push 0 dhist)]
    [(three? d) dhist]
    ))
     

  
(define (distinct-ways-improved lon [prev 0] [ways (push 1 (make-4-stack))] [dhist (push 1 (make-3-stack))])
  (printf "~a p ~a w ~a dh ~a" lon prev ways dhist)  
  (cond
    [(empty? lon)
     (printf "~n")
     (printf "ways: ~a~n" ways)
     (printf "first: ~a~n" (first ways))
     (printf "calc: ~a~n" (calc-ways dhist ways))
     (printf "sum: ~a~n" (sum ways))
     (printf "s2 : ~a~n" (sum (calc-ways dhist ways)))
     (sum (calc-ways dhist ways))]
    [else
     (printf " a ~a b ~a~n" (first lon) prev)
     (define next-dh (next-dhist (first lon) prev dhist))
     (distinct-ways-improved (rest lon)
                             (first lon)
                             (calc-ways next-dh ways)
                             next-dh)]
    ))

(for ([ls (list (range 1 2) (range 1 3) (range 1 4) (range 1 5))])
  (for ([fun (list distinct-ways distinct-ways-improved)])
    (printf "~a: ~a~n" fun (fun ls))))

(define ls '(1 2 3 5 6 7))
(distinct-ways ls)
(distinct-ways-improved ls)
(set! ls '(1 2 3 5 6 7 9 10))
(distinct-ways ls)
(distinct-ways-improved ls)


(for ([i (range 10)])
  (define ls (generate-n-ways 20))
  (printf ":: ~a :: ~n" ls)
  (printf "~a ~a~n" (distinct-ways ls) (distinct-ways-improved ls)))

#;(module+ test
  (chk
   #:=
   (distinct-ways (sort t1 <))
   8
   (distinct-ways-improved (sort t1 <))
   8
   ;(distinct-ways-improved (sort t2 <))
   ;19208
   ))

;; Part 2
;;(distinct-ways-improved (sort d1 <))