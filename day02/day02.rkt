#lang racket
(require racket/file
         racket/runtime-path)

(define-runtime-path data-file "data.txt")
(define data (file->lines data-file #:mode 'text))

;; How many passwords are valid?
;; Given "2-7 g: fmggdgggx", does g appear 2-7 times?
;; Construct a regex from each line.

(define dbpat #px"(\\d+)-(\\d+)\\s+(\\w):\\s+(\\w+)")
(struct bounds (char pass low high) #:transparent)

(define (raw->bounds los)
  (cond
    [(empty? los) '()]
    [else
     (define parts (regexp-match dbpat (first los)))
     (cons
      (bounds (first (string->list (fourth parts)))
              (string->list (fifth parts))
              (string->number (second parts))
              (string->number (third parts)))
      (raw->bounds (rest los)))]))

(define (checks-2-1? b)
  (define count (length (filter (λ (c)
                                  (equal? (bounds-char b) c))
                                (bounds-pass b))))
  (and (>= count (bounds-low b))
       (<= count (bounds-high b))))

(define (checks-2-2? b)
  (xor (equal? (bounds-char b)
               (list-ref (bounds-pass b) (sub1 (bounds-low b))))
       (equal? (bounds-char b)
               (list-ref (bounds-pass b) (sub1 (bounds-high b))))))
              

(define (count lob pred?)
  (cond
    [(empty? lob) 0]
    [(pred? (first lob))
     (+ 1 (count (rest lob) pred?))]
    [else (count (rest lob) pred?)]))

(count (raw->bounds data) checks-2-1?)
(count (raw->bounds data) checks-2-2?)