#lang racket
(require racket/file
         racket/runtime-path)

(define-runtime-path data-file "data.txt")
(define answers (file->lines data-file #:mode 'text))

(define-runtime-path test-file "test.txt")
(define test-answers (file->lines test-file #:mode 'text))

;; Lets make lists of sets instead of strings.
(define (make-groups loa #:acc [acc empty])
  (cond
    [(empty? loa) (list acc)]
    [(equal? (first loa) "")
     (cons acc
           (make-groups (rest loa) #:acc empty))]
    [else
     (make-groups (rest loa)
                  #:acc (cons (list->set (string->list (first loa)))
                              acc))]))

(module+ test
  (require chk)
  (chk
   #:=
   11 (foldl + 0 (map (λ (sos)
                        (set-count (apply set-union sos))) (make-groups test-answers)))
   ))

;; The first answer
(foldl + 0
       (map (λ (sos)
              (set-count (apply set-union sos)))
            (make-groups answers)))

;; For the second answer, we want to know the number of questions to which everyone answered yes.
;; Given...
#|
(list
 (list (set #\c #\a #\b))
 (list (set #\c) (set #\b) (set #\a))
 (list (set #\c #\a) (set #\a #\b))
 (list (set #\a) (set #\a) (set #\a) (set #\a))
 (list (set #\b)))
|#
;; The first group is 3
;; The second group is 0
;; The third group is 1 (a)
;; The fourth group is 1 (a)
;; The last group is 1 (b)
(module+ test
  (require chk)
  (chk
   #:=
   6 (foldl + 0
             (map (λ (sos)
                    (set-count (apply set-intersect sos))) (make-groups test-answers)))
   ))

;; The second answer is...
(foldl + 0
       (map (lambda (los)
              (set-count (apply set-intersect los)))
            (make-groups answers)))