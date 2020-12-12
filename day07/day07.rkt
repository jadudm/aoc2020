#lang racket
(require racket/file
         racket/runtime-path)

(define-runtime-path rules-file "rules.txt")
(define rules (file->lines rules-file #:mode 'text))

(define-runtime-path test-rules-file "testrules.txt")
(define test-rules (file->lines test-rules-file #:mode 'text))
(define-runtime-path test2-file "test2.txt")
(define test2-rules (file->lines test2-file #:mode 'text))

;; Rules follow a pattern.
;; It is a grammar.
#|
rule := <bag-id> bags contain <bag-count>+
bag-count := <n> <bag-id>
           | <n> <bag-id>, <bag-count>
           | "no other bags"
|#
;; which is... close. We'll never see "1 muted yellow bags, no other bags."
;; But that will work for this system. We will see
;; "dotted black bags contain no other bags."
;; which the grammar catches.

;; Transparency both lets me see what is happening, and
;; also is important for equal? checks.
(struct bag-contains (id bags) #:transparent)
(struct bag (id count) #:transparent)
(struct empty-bag () #:transparent)

;; Example
#|
(bag-contains 'lightred
              (list
               (bag 'brightwhite 1)
               (bag 'mutedyellow 2)))
(bag-contains 'fadedblue (list))
|#

;; Now, lets turn the rules file into this set of structures.
(define (bag->contents b)
  ;(printf "b: ~s~n" b)
  (define pat1 (pregexp "(.*?) bags contain (.*)\\."))
  (define m1 (regexp-match pat1 b))
  ;(printf "m1: ~s~n" m1)
  (define bag-id (string->symbol (regexp-replace " " (second m1) "")))
  (define m2 (regexp-split "," (third m1)))
  ;(printf "m2: ~s~n" m2)
  (define pat2 (pregexp "([0-9]+) (.*?) bag"))
  (bag-contains
   bag-id
   (map (λ (bs)
          (define m3 (regexp-match pat2 bs))
          ;(printf "m3: ~s~n" m3)
          (if m3
              (bag (string->symbol (regexp-replace " " (third m3) "")) (string->number (second m3)))
              (empty-bag)))
        m2)))
  
(define (rules->bags lor)
  (cond
    [(empty? lor) '()]
    [else
     (cons (bag->contents (first lor))
           (rules->bags (rest lor)))]))
         
(module+ test
  (require chk)
  (chk
   #:=
   (rules->bags test-rules)
   (list
    (bag-contains 'lightred (list (bag 'brightwhite 1) (bag 'mutedyellow 2)))
    (bag-contains 'darkorange (list (bag 'brightwhite 3) (bag 'mutedyellow 4)))
    (bag-contains 'brightwhite (list (bag 'shinygold 1)))
    (bag-contains 'mutedyellow (list (bag 'shinygold 2) (bag 'fadedblue 9)))
    (bag-contains 'shinygold (list (bag 'darkolive 1) (bag 'vibrantplum 2)))
    (bag-contains 'darkolive (list (bag 'fadedblue 3) (bag 'dottedblack 4)))
    (bag-contains 'vibrantplum (list (bag 'fadedblue 5) (bag 'dottedblack 6)))
    (bag-contains 'fadedblue (list (empty-bag)))
    (bag-contains 'dottedblack (list (empty-bag))))
   ))

;; I think I want that to become a hash table.
(define (rules->hash lor)
  (define h (make-hash))
  (for ([r lor])
    (hash-set! h (bag-contains-id r)
               (bag-contains-bags r)))
  h)
(define the-rules (rules->hash (rules->bags rules)))

;; Given a bag that can hold things, can it hold the holdee?
(define (can-contain? holder holdee #:rules [rules the-rules])
  (define holds (hash-ref rules holder))
  ;; (printf "holder: ~a holdee: ~a h: ~s~n" holder holdee holds)
  (ormap (λ (b)
           (and (bag? b)
                (or (and (equal? holdee (bag-id b))
                         (> (bag-count b) 0))
                    ;; This is the recursion... I had intended to do this
                    ;; separately, but... eh.
                    (can-contain? (bag-id b)
                                  holdee #:rules rules)
                    )))
         holds))

(module+ test
  (chk
   #:t
   (can-contain? 'brightwhite
                 'shinygold
                 #:rules (rules->hash (rules->bags test-rules)))
   (can-contain? 'lightred
                 'shinygold
                 #:rules (rules->hash (rules->bags test-rules)))
   (can-contain? 'mutedyellow
                 'shinygold
                 #:rules (rules->hash (rules->bags test-rules)))
   #:!
   (can-contain? 'darkolive
                 'shinygold
                 #:rules (rules->hash (rules->bags test-rules)))
   ))

;; How many bag colors can eventually contain at least one shiny gold bag?
(define (how-many-can-contain id #:rules [rules the-rules])
  (apply +
         (hash-map rules
                   (λ (k v)
                     (if (can-contain? k id #:rules rules)
                         1 0)))))

(define the-test-rules (rules->hash (rules->bags test-rules)))
(define the-test2-rules (rules->hash (rules->bags test2-rules)))

(module+ test
  (chk
   #:=
   4
   (how-many-can-contain 'shinygold #:rules the-test-rules)
   ))

;; The first answer
(how-many-can-contain 'shinygold)

;; Now, how many bags are inside the bag?
(define (how-many-inside id #:rules [rules the-rules])
  (define holds (hash-ref rules id))
  (apply + (map (λ (b)
                     (if (bag? b)
                         (+ (bag-count b)
                            (* (bag-count b)
                               (how-many-inside (bag-id b) #:rules rules)))
                         0))
                   holds))
     )

(module+ test
  (chk
   #:=
   126
   (how-many-inside 'shinygold #:rules the-test2-rules)
   ))

;; The second answer
(how-many-inside 'shinygold)