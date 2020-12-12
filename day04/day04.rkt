#lang racket
(require racket/file
         racket/runtime-path
         racket/set)

(define-runtime-path data-file "data.txt")
(define-runtime-path test-data-file "testdata.txt")
(define data (file->lines data-file #:mode 'text))
(define test-data (file->lines test-data-file #:mode 'text))

(define (empty-string? o)
  (and (string? o)
       (zero? (string-length o))))

;; Take a list of strings, and
;; turn them into appended strings, except where
;; there are empty strings... these are "breaks."
(define (cleanup los #:acc [acc ""])
  (cond
    [(empty? los) (list acc)]
    [(empty-string? (first los))
     (cons acc (cleanup (rest los) #:acc (first los)))]
    [else
     (cleanup (rest los)
              #:acc (string-append acc
                                   (if (empty-string? acc) acc " ")
                                   (first los)))]))
(module+ test
  (require chk)
  (chk
   #:= (cleanup test-data)
   '("ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm"
     "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884 hcl:#cfa07d byr:1929"
     "hcl:#ae17e1 iyr:2013 eyr:2024 ecl:brn pid:760753108 byr:1931 hgt:179cm"
     "hcl:#cfa07d eyr:2025 pid:166559648 iyr:2011 ecl:brn hgt:59in")))

;; Once the data is cleaned up, it's time for validity checks.
;; Fields are:
(define all-fields (list->set '(byr iyr eyr hgt hcl ecl pid cid)))
;; Optional fields are
(define optional-fields (list->set '(cid)))

;; A valid passport has all fields, save those that are optional.
(define (is-valid? passport
                   #:required [req all-fields]
                   #:optional [opt optional-fields])
  (define must-have (set-subtract req opt))
  (= (set-count must-have)
     (apply + (set-map must-have
                       (λ (field)
                         (if (regexp-match (symbol->string field) passport)
                             1 0))))))

(define (true? o)
  (and (boolean? o) o))

(define (count-valid passports)
  (length (filter true? (map is-valid? passports))))

(count-valid (cleanup data))