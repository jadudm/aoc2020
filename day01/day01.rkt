#lang racket
(require racket/file
         racket/runtime-path)

(define-runtime-path data-file "data.txt")
(define data
  (map string->number
       (file->lines data-file #:mode 'text)))

(define (first-star)
  (define done? false)
  (for ([one data]
        #:final done?)
    (for ([two data]
          #:final done?)
      (when (= 2020 (+ one two))
        (printf "~a~n" (* one two))
        (set! done? true)
  ))))

(define (is2020? a b)
  (= 2020 (+ a b)))

(define (find-two n lon)
  (cond
    [(empty? lon) 0]
    [(is2020? n (first lon))
     (* n (first lon))]
    [else
     (find-two n (rest lon))]))

(define (find lon)
  (cond
    [(empty? lon) 0]
    [else
     (+ (find-two (first lon) (rest lon))
        (find (rest lon)))]))


(first-star)