#lang racket
(require racket/file
         racket/runtime-path)

(define-runtime-path data-file "data.txt")
(define data (file->lines data-file #:mode 'text))

(struct forest (rows columns map))

;; Now, I have a list of strings.
;; I'd like those to become a linear array.
(define the-forest
  (forest (length data)
          (string-length (first data))
          (apply string-append data)))

;; At this point, I can reference into the string.
;; The testing modules below make sure this works.
(define (lookup r c #:forest [forest the-forest])
  (string-ref (forest-map forest)
              (+ (* r (forest-columns forest))
                 (modulo c (forest-columns forest)))))

;; The walk is, we hope, always "n right, m down."
(define (walk right down #:forest [forest the-forest])
  (define tree-count 0)
  (let loop ([r 0]
             [c 0])
    (unless (>= r (forest-rows forest))
      (when (equal? #\# (lookup r c #:forest forest))
        (set! tree-count (add1 tree-count)))
      (loop (+ r down)
            (+ c right))))
  tree-count)
    
;; Solution
(walk 3 1)
;; Part two.
(* (walk 1 1)
   (walk 3 1)
   (walk 5 1)
   (walk 7 1)
   (walk 1 2))

;; Testing.
;; Here are the first few rows.
#|
....#.#..#.#.#.#......#....##.#
..##..#.#..#.##.....#.....#....
....#..#...#..#..####.##.#.##..
...............#.....##..##..#.
|#
(module+ test
  (require chk)
  (define M1
    (forest
     4 31
     (string-append
      "....#.#..#.#.#.#......#....##.#"
      "..##..#.#..#.##.....#.....#...."
      "....#..#...#..#..####.##.#.##.."
      "...............#.....##..##..#.")))
  (chk
   #:=
   #\. (lookup 0 0 #:forest M1)
   #\. (lookup 0 1 #:forest M1)
   #\# (lookup 0 4 #:forest M1)
   #\. (lookup 1 0 #:forest M1)
   #\. (lookup 1 1 #:forest M1)
   #\# (lookup 1 2 #:forest M1)
   ))

;; But, what if I ask for c + 1?
;; Currently, the map is actually represented as a
;; single long string. According to the advent calendar,
;; each row should wrap horizontally. Therefore, the map
;; actually looks like this:
#|
....#.#..#.#.#.#......#....##.#....#.#..#.#.#.#......#....##.#
..##..#.#..#.##.....#.....#......##..#.#..#.##.....#.....#....
....#..#...#..#..####.##.#.##......#..#...#..#..####.##.#.##..
|#
;; I can do this by wrapping the column counter via modulo.
(module+ text
  (require chk)
  (define M2
    (forest
     4 31
     (string-append
      "1...#.#..#.#.#.#......#....##.#"
      "2.##..#.#..#.##.....#.....#...."
      "3...#..#...#..#..####.##.#.##.."
      "4..............#.....##..##..#.")))
  (chk
   #:=
   #\1 (lookup 0 (+ 0 (forest-columns M2)) #:forest M2)
   #\1 (lookup 0 (* 3 (+ 0 (forest-columns M2))) #:forest M2)
   #\. (lookup 0 (+ 0 (forest-columns M2)) #:forest M2)
   #\# (lookup 0 (+ 4 (forest-columns M2)) #:forest M2)
   #\2 (lookup 1 (+ 0 (forest-columns M2)) #:forest M2)
   #\. (lookup 1 (+ 1 (forest-columns M2)) #:forest M2)
   #\# (lookup 1 (+ 2 (forest-columns M2)) #:forest M2)
   #\3 (lookup 2 (+ 0 (forest-columns M2)) #:forest M2)
   ))

(module+ test
  (require chk)
  (define M3
    (forest
     4 31
     (string-append
      "....#.#..#.#.#.#......#....##.#"
      "..##..#.#..#.##.....#.....#...."
      "....#..#...#..#..####.##.#.##.."
      "...............#.....##..##..#.")))
  
  ;; right 1, down 1 should yield... 0
  ;; r3d1 should yield... 1
  ;; r2d1 should yield... 2
  (chk
   #:=
   0 (walk 1 1 #:forest M3)
   2 (walk 2 1 #:forest M3)
   1 (walk 3 1 #:forest M3)
   ))
