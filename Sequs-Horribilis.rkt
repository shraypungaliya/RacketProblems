#lang racket
(define (sequs-horribilis lat num)
  (reverse (rest (sequs-helper num lat '() 0))))

(define (sequs-helper num orig new-lat sum)
  (cond [(or (null? orig) (= sum num)) (cons sum new-lat)]
        [(list? (first orig)) (sequs-helper num (rest orig) (cons (rest (sequs-helper num (first orig) '() sum)) new-lat ) (first (sequs-helper num (first orig) '() sum)))]
        [(cond [(> (+ sum (first orig)) num) (cons sum new-lat)]
        [(null? (rest orig)) (cons sum (cons (first orig) new-lat))]
        [(sequs-helper num (rest orig) (cons (first orig) new-lat) (+ sum (first orig)))])]))