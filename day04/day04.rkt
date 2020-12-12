#lang racket
(require racket/file
         racket/runtime-path
         racket/set)

(define-runtime-path data-file "data.txt")
(define-runtime-path test-data-file "testdata.txt")
(define-runtime-path valid-data-file "validdata.txt")
(define data (file->lines data-file #:mode 'text))
(define test-data (file->lines test-data-file #:mode 'text))
(define valid-data (file->lines valid-data-file #:mode 'text))

(define (empty-string? o)
  (and (string? o)
       (zero? (string-length o))))

(define (line->hash str)
  (define h (make-hash))
  (for-each (λ (s)
              (when (regexp-match ":" s)
                (define p (regexp-split ":" s))
                (hash-set! h (string->symbol (first p))
                           (second p))))
            (regexp-split #px"[[:space:]]+" str))
  h)

(module+ test
  (require chk)
  (chk
   #:=
   (make-hash '((cid . "northpole")))
   (line->hash "cid:northpole")
   ))

;; Take a list of strings, and
;; turn them into appended strings, except where
;; there are empty strings... these are "breaks."
(define (cleanup los #:acc [acc ""])
  (cond
    [(empty? los) (list (line->hash acc))]
    [(empty-string? (first los))
     (cons (line->hash acc) (cleanup (rest los) #:acc (first los)))]
    [else
     (cleanup (rest los)
              #:acc (string-append acc
                                   (if (empty-string? acc) acc " ")
                                   (first los)))]))
(module+ test
  (require chk)
  ;; A specialized check.
  (define (hash-equal? loh1 loh2)
    (define same? true)
    (for ([h1 loh1]
          [h2 loh2])
      (for ([(k v) h1])
        (unless (and (hash-has-key? h2 k)
                     (equal? (hash-ref h1 k)
                             (hash-ref h2 k)))
          (set! same? false))))
    same?)

  (chk
   #:eq hash-equal?
   (cleanup test-data)
   '(#hash((byr . "1937")
        (cid . "147")
        (ecl . "gry")
        (eyr . "2020")
        (hcl . "#fffffd")
        (hgt . "183cm")
        (iyr . "2017")
        (pid . "860033327"))
  #hash((byr . "1929") (cid . "350") (ecl . "amb") (eyr . "2023") (hcl . "#cfa07d") (iyr . "2013") (pid . "028048884"))
  #hash((byr . "1931") (ecl . "brn") (eyr . "2024") (hcl . "#ae17e1") (hgt . "179cm") (iyr . "2013") (pid . "760753108"))
  #hash((ecl . "brn") (eyr . "2025") (hcl . "#cfa07d") (hgt . "59in") (iyr . "2011") (pid . "166559648")))))

;; Once the data is cleaned up, it's time for validity checks.
;; Fields are:
(define all-fields (list->set '(byr iyr eyr hgt hcl ecl pid cid)))
;; Optional fields are
(define optional-fields (list->set '(cid)))

;; A valid passport has all fields, save those that are optional.
(define (is-valid? passport
                   #:required [req all-fields]
                   #:optional [opt optional-fields]
                   #:pred? [pred? present?])
  (define must-have (set-subtract req opt))
  (= (set-count must-have)
     (apply + (set-map must-have (pred? passport))))
  )

(define (true? o)
  (and (boolean? o) o))

(define (count-valid passports #:pred? pred)
  (length (filter true? (map (λ (p)
                               (is-valid? p #:pred? pred)) passports))))

(define (present? passport)
  (λ (field)
    (if (hash-has-key? passport field) 1 0)))
  
(count-valid (cleanup data) #:pred? present?)

#|

    byr (Birth Year) - four digits; at least 1920 and at most 2002.
    iyr (Issue Year) - four digits; at least 2010 and at most 2020.
    eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
    hgt (Height) - a number followed by either cm or in:
        If cm, the number must be at least 150 and at most 193.
        If in, the number must be at least 59 and at most 76.
    hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
    ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
    pid (Passport ID) - a nine-digit number, including leading zeroes.
    cid (Country ID) - ignored, missing or not.
|#

;; This exists as a carryover from a previous
;; representation of passports
(define (extract-value f p)
  (hash-ref p f))

(module+ test
  (require chk)
  (define p1
    (line->hash "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980 hcl:#623a2f"))
  (define p2
    (line->hash "hcl:#888785 hgt:164cm byr:2001 iyr:2015 cid:88 pid:545766238 ecl:hzl eyr:2022"))
  (chk
   #:=
   "2012" (extract-value 'iyr p1)
   "74in" (extract-value 'hgt p1)
   "grn" (extract-value 'ecl p1)
   "#623a2f" (extract-value 'hcl p1)
   "2022" (extract-value 'eyr p2)
   ))

(define (number-between? low high)
  (λ (field passport)
    (define value (extract-value field passport))
    (define nv (string->number value))
    (define result (and (number? nv)
                        (and (>= nv low) (<= nv high))))
    result
    ))

(module+ test
  (require chk)
  (chk
   #:t
   ((number-between? 10 20) 'num (line->hash "boog:323 num:10"))
   ((number-between? 10 20) 'num (line->hash "num:13"))
   ((number-between? 10 20) 'num (line->hash "asdf:123 numnum:200 num:19 nummun:323"))
   ))
   

(define (height-between? unit low high)
  (λ (field passport)
    (define value (extract-value field passport))
    (define num (string->number (second (regexp-match #px"([0-9]+)" value))))
    (and (number? num)
         (regexp-match (symbol->string unit) value)
         (>= num low)
         (<= num high))))

(module+ test
  (require chk)
  (chk
   #:t
   ((height-between? 'cm 10 20) 'num (line->hash "boog:323 num:10cm"))
   ((height-between? 'in 10 20) 'num (line->hash "num:13in"))
   ((height-between? 'in 10 20) 'num (line->hash "asdf:123in numnum:200cm num:19in nummun:323in"))
   ))

(define (hair-color? field passport)
  (define color (extract-value field passport))
  (and (= (string-length color) 7)
       (regexp-match #px"#[[:xdigit:]]{6}" color)
       ))

(module+ test
  (define bad-p1
    (line->hash "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980 hcl:#623a2fx"))
  
  (chk
   #:t
   (hair-color? 'hcl p1)
   #:!
   (hair-color? 'hcl bad-p1)
   ))

(define rules
  (hash
   'byr (number-between? 1920 2002)
   'iyr (number-between? 2010 2020)
   'eyr (number-between? 2020 2030)
   'hgt (λ (field passport)
          (ormap (λ (f) (f field passport))
                  (list (height-between? 'cm 150 193)
                        (height-between? 'in 59 76))))
   'hcl hair-color?
   'ecl (λ (field passport)
          (member (string->symbol (extract-value field passport))
                  '(amb blu brn gry grn hzl oth)))
   'pid (λ (field passport)
          (define pid (extract-value field passport))
          (and (= (string-length pid) 9)
               (regexp-match #px"[[:digit:]]{9}" pid)))
))

(define (present-and-valid? passport)
  (λ (field)
    (define result
      (if (and (hash-has-key? passport field)
               ((hash-ref rules field) field passport))
          1 0))
    result))

(module+ test
  (chk
   #:= 4 (count-valid (cleanup valid-data) #:pred? present-and-valid?)
   ))

;; Should be four valid passports in the valid-data.
(count-valid (cleanup data) #:pred? present-and-valid?)

