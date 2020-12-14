#lang racket

(define r1 "L.LL.LL.LL")

;; Two representations:
;; One where there are seats.
;; One where there is floor.

;; Convert a string to a seatmap.
;; This is a single digit with bits set to 1
;; everywhere there is a seat.
(define (seats->map s ch)
  (define n 0)
  (define loc (string->list s))
  (for ([c loc]
        [ndx (in-naturals)])
    (when (equal? c ch)
      (define ndxp (- (sub1 (length loc)) ndx))
      ;; (printf "~a ~a~n" ndxp (expt 2 ndxp))
      (set! n (bitwise-ior n (expt 2 ndxp)))
    ))
  n)

(define (seats->seatmap s)
  (seats->map s #\L))
(define (seats->floormap s)
  (seats->map s #\.))

(module+ test
  (require rackunit)
  (define t1
    '(("...L" 1)
      ("..L." 2)
      ("..LL" 3)))
  (for ([t t1])
    (check-equal? (seats->seatmap (first t))
                  (second t)))
  (define t2
    '(("...L" 14)
      ("..L." 13)
      ("..LL" 12)))
  (for ([t t2])
    (check-equal? (seats->floormap (first t))
                  (second t)))
  )


(require racket/file
         racket/runtime-path)
(define-runtime-path f1 "small.map")
(define small-map (file->lines f1 #:mode 'text))

(module+ test
  (check-equal?
   (map seats->seatmap small-map)
   '(731 1019 676 987 731 763 160 1023 765 763)))


;; 1011011011
;; L.LL.LL.LL
;; 1111111011
;; LLLLLLL.LL
;; 1010100100
;; L.L.L..L..


;; Now. Hm.
#|
    If a seat is empty (L) and there are no occupied seats adjacent to it,
      the seat becomes occupied.
    If a seat is occupied (#) and four or more seats adjacent to it are
      also occupied, the seat becomes empty.
    Otherwise, the seat's state does not change.
|#

;; This means the "should it become occupied map" looks like this:

;; If the seat is here:
;;    0 0 0 0 1 0 0 0 0
;; Then the seatmap would be...
;; 1 1 1 1 0 1 1 1
;; Assuming that the row length was three.
;; If it was a 5-wide row?
;; 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0
;; The map would look like
;; 1 1 1 0 0 1 0 1 0 0 1 1 1 0 0
;; This would be:
;; (bitwise-ior 0 (expt 2 5) (expt 2 4) (expt 2 3))
;; on the row prior.
;; (bitwise-ior 0 (expt 2 5) (expt 2 3))
;; On the row with the seat
;; (bitwise-ior 0 (expt 2 5) (expt 2 4) (expt 2 3))
;; Which normalizes to
;; (bitwise-ior 0 (expt 2 ROWLENGTH) (expt 2 (- ROWLENGTH 1)) ...)

