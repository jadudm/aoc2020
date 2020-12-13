#lang racket
(require racket/file
         racket/runtime-path)

(define-runtime-path p1-file "p1.txt")
(define p1s (file->lines p1-file #:mode 'text))
(define-runtime-path p2-file "p2.txt")
(define p2s (file->lines p2-file #:mode 'text))

(struct op (rator rand) #:transparent)

(define (strings->instr ps)
  (map (λ (s)
         (define ls (regexp-split " " s))
         (op (string->symbol (first ls))
             (string->number (second ls))))
       ps))

(define p1 (strings->instr p1s))
(define p2 (strings->instr p2s))

(module+ test
  (require chk)
  (chk #:=
       p1
       (list
        (op 'nop 0)
        (op 'acc 1)
        (op 'jmp 4)
        (op 'acc 3)
        (op 'jmp -3)
        (op 'acc -99)
        (op 'acc 1)
        (op 'jmp -4)
        (op 'acc 6))
       ))

(define (interp code ip acc exe)
  (define next (list-ref code ip))
  ;;(printf "~a ~a | ~a, ~a~n" (op-rator next) (op-rand next) ip acc)
  (cond
    [(member ip exe) acc]
    [(< ip (length code))
     (case (op-rator next)
       [(nop) (interp code (add1 ip) acc (cons ip exe))]
       [(acc) (interp code (add1 ip) (+ (op-rand next) acc) (cons ip exe))]
       [(jmp) (interp code (+ (op-rand next) ip) acc (cons ip exe))])]
    [else acc]))

(module+ test
  (chk
   #:= 5
   (interp p1 0 0 empty)
   ))

;; Part 1
(interp p2 0 0 empty)

;; Part 2
(define (slice l offset n)
  ;;(printf "n: ~a~n" n)
  (take (drop l offset) n))

(define (replace ls loc o)
  (append (slice ls 0 loc)
          (list o)
          (slice ls (add1 loc) (- (length ls) (add1 loc)))))

; (replace code ip (op 'nop (op-rand next)))
(module+ test
  (chk
   #:=
   '(x b c) (replace '(a b c) 0 'x)
   '(a x c) (replace '(a b c) 1 'x)
   '(a b x) (replace '(a b c) 2 'x)))

(define (extend v s)
  (set-add s v))

;; Kinda ugly.
;; It has a "flipped" boolean that, if we have flipped an instruction,
;; we won't flip any more. Meh. I'm confident that this would have been
;; nicer with some kind of fancy backtracking thing, but... this worked.
(define (interp2 code ip acc exe flipped?)
  (cond
    [(set-member? exe ip) false]
    [(< ip (length code))
     (define next (list-ref code ip))
     (case (op-rator next)
       [(nop)
        (define res (interp2 code (add1 ip) acc (extend ip exe) flipped?))
        (if (and (not res) (not flipped?))
            (interp2 code (+ ip (op-rand next)) acc (extend ip exe) true)
            res)]
       [(acc) (interp2 code (add1 ip) (+ (op-rand next) acc) (extend ip exe) flipped?)]
       [(jmp)
        (define res (interp2 code (+ (op-rand next) ip) acc (extend ip exe) flipped?))
        (if (and (not res) (not flipped?))
            (interp2 code (add1 ip) acc (extend ip exe) true)
            res)]
        )]
    [else acc]
    ))
       
       
(module+ test
  (chk
   #:=
   (interp2 p1 0 0 empty false)
   8
   ))

;; Part 2
;; Not 1389. Too high.
(interp2 p2 0 0 (set) false)


