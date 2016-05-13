#lang racket





 
 ;; the structure representing a maze of size XxY
(struct maze (X Y tbl))


;; managing cell properties
(define (connections tbl c) (dict-ref tbl c '()))
 
(define (connect! tbl c n) 
  (dict-set! tbl c (cons n (connections tbl c)))
  (dict-set! tbl n (cons c (connections tbl n))))
 
(define (connected? tbl a b) (member a (connections tbl b)))
 

 
;; Returns a maze of a given size
;; build-maze :: Index Index -> Maze
(define (build-maze X Y)
  (define tbl (make-hash))
  (define (visited? tbl c) (dict-has-key? tbl c))
  (define (neigbours c)
    (filter 
     (match-lambda [(list i j) (and (<= 0 i (- X 1)) (<= 0 j (- Y 1)))])
     (for/list ([d '((0 1) (0 -1) (-1 0) (1 0))]) (map + c d))))
  ; generate the maze
  (let move-to-cell ([c (list (random X) (random Y))])
    (for ([n (shuffle (neigbours c))] #:unless (visited? tbl n))
      ( connect! tbl c n)
      (move-to-cell n)))
  ; return the result
  (maze X Y tbl))

;; Shows a maze
(define (show-maze m)
  (match-define (maze X Y tbl) m)
  (for ([i X]) (display "+---"))
  (displayln "+")
  (for ([j Y])
    (display "|")
    (for ([i (- X 1)])
      (if (connected? tbl (list i j) (list (+ 1 i) j))
          (display "    ")
          (display "   |")))
    (display "   |")
    (newline)
    (for ([i X])
      (if (connected? tbl (list i j) (list i (+ j 1)))
          (display "+   ")
          (display "+---")))
    (displayln "+"))
  (newline))

(define t (build-maze 3 3))
   (show-maze t) 
   
 