#lang racket
(require racket/file
         racket/runtime-path)

(define-runtime-path data-file "data.txt")
(define boarding-passes (file->lines data-file #:mode 'text))

(struct space (lower upper))
(struct range (lower upper))

(define (bp->any bp seat lu) 
  (define half-range
    (ceiling (/ (- (range-upper seat) (range-lower seat)) 2)))
  (cond
    [(empty? bp) (range-lower seat)]
    [(equal? (space-lower lu) (first bp))
     (bp->any (rest bp)
              (range (range-lower seat) (- (range-upper seat) half-range))
              lu)]
    [(equal? (space-upper lu) (first bp))
     (bp->any (rest bp)
              (range (+ (range-lower seat) half-range) (range-upper seat))
              lu)]
    [else (bp->any (rest bp) seat lu)]
    ))

(define (seat->row bp)
  (bp->any (string->list bp) (range 0 127) (space #\F #\B)))
  
(define (seat->column bp)
  (bp->any (string->list bp) (range 0 7) (space #\L #\R)))

(define (seat->id bp)
  (+ (* (seat->row bp) 8)
     (seat->column bp)))

(module+ test
  (require chk)
  (chk
   #:=
   44 (seat->row "FBFBBFFRLR")
   5 (seat->column "FBFBBFFRLR")
   70 (seat->row "BFFFBBFRRR")
   7 (seat->column "BFFFBBFRRR")
   567 (seat->id "BFFFBBFRRR")
   14 (seat->row "FFFBBBFRRR")
   102 (seat->row "BBFFBBFRLL") 
  ))

;; What is the highest ID?
(first (sort (map seat->id boarding-passes) >))

;; I'm somewhere in the middle. There's a missing seat ID.
(define sorted
  (sort (map seat->id boarding-passes) <))

(define missing -1)
(for ([left sorted]
      [right (rest sorted)]
      #:final (not (= (- right left 1)))
      )
  (when (not (= (- right left) 1))
      (set! missing (+ left 1))))
missing