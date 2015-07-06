#lang racket
(define (luhn number)
  (check-sum (double-alternate (extract number) 1) 0))

(define (extract number)
  (if (< number 10)
      (list number)
      (cons (modulo number 10) (extract (floor (/ number 10))))))

(define (double-alternate num-list count)
  (if (null? num-list)
      '()
      (if (even? count)
          (cons (* 2 (first num-list)) (double-alternate (rest num-list) (add1 count)))
          (cons (first num-list)(double-alternate (rest num-list) (add1 count))))))

(define (check-sum num-list sum)
  (if (null? num-list)
      (= (modulo sum 10) 0)
      (check-sum (rest num-list) (+ (first num-list) sum))))
  