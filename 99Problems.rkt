#lang racket
(require test-engine/racket-tests)

;;Note: For the purpose of these functions, an element is defined as either a list or an atom

;;Helper method (not part of problem set)
;;Conses the elements of one list into another

(define (cons-elements to-add master-list)
  (cons-elements-helper to-add master-list))

(define (cons-elements-helper to-add master-list)
  (if (null? to-add)
      master-list
      (cons (first to-add) (cons-elements-helper (rest to-add) master-list))))

(check-expect (cons-elements '(a b c) '(d e f)) '(a b c d e f))
(check-expect (cons-elements '() '(d e f)) '(d e f))
(check-expect (cons-elements '(a b c) '()) '(a b c))

;;Problem Set: 99 Lisp Problems

;;P01 Finds the last atom in a list

(define (my-last list)
  (if (null? list)
      '()
      (if (null? (rest list))
          (first list)
          (my-last (rest list)))))

(check-expect (my-last '(a b c)) 'c)
(check-expect (my-last '()) '())
(check-expect (my-last '((a) b (c))) '(c))

;;P02 Creates a list identical to the given except the last element in the list

(define (my-but-last list)
  (butlast-helper list '()))

(define (butlast-helper orig new-list)
  (if (null? orig)
      '()
      (if (not(null? (rest orig)))
          (butlast-helper (rest orig) (cons (first orig) new-list))
          (reverse new-list))))

(check-expect (my-but-last '(a b c)) '(a b))
(check-expect (my-but-last '()) '())
(check-expect (my-but-last '((a b) c)) '((a b)))
(check-expect (my-but-last '(a (b c))) '(a))
(check-expect (my-but-last '((a b c))) '())

;;P03 Finds the element at the given position in a list

(define (element-at list pos)
  (element-at-helper list pos 1))

(define (element-at-helper list pos count)
  (if (null? list)
      '()
      (if (equal? count pos)
          (first list)
          (element-at-helper (rest list) pos (+ 1 count)))))

(check-expect (element-at '(a b c) 2) 'b)
(check-expect (element-at '() 2) '())
(check-expect (element-at '(a b c) 4) '())

;;P04 Counts the number of elements in a list

(define (count-elements list)
  (count-elements-helper list 0))

(define (count-elements-helper list count)
  (if (null? list)
      count
      (count-elements-helper (rest list) (+ 1 count))))

(check-expect (count-elements '(a b c)) 3)
(check-expect (count-elements '()) 0)

;;P05 Reverses a list

(define (reverse list)
  (reverse-helper list '()))

(define (reverse-helper orig new-list)
  (if (null? orig)
      new-list
      (reverse-helper (rest orig) (cons (car orig) new-list))))

(check-expect (reverse '(a b c)) '(c b a))
(check-expect (reverse '()) '())

;;P06 Checks if the list is a palindrome

(define (palindrome? list)
  (if (> (count-elements list) 1)
      (if (equal? (first list) (my-last list))
          (palindrome? (my-but-last (rest list)))
          #f)
      #t))

(check-expect (palindrome? '(a b c)) #f)
(check-expect (palindrome? '(a b c b a)) #t)
(check-expect (palindrome? '(a b c c b a)) #t)
(check-expect (palindrome? '()) #t)

;;P07 Removes all elements from sublists and puts them in the main list, eliminating all sublists

(define (my-flatten list)
  (reverse(my-flatten-helper list '())))
  

(define (my-flatten-helper orig new-list)
  (if (null? orig)
      new-list
      (if (not (list? (first orig)))
          (my-flatten-helper (rest orig)
                             (cons (first orig) new-list))
          (my-flatten-helper (rest orig)
                             (my-flatten-helper (first orig) new-list)))))
      
(check-expect (my-flatten '(a b c)) '(a b c))
(check-expect (my-flatten '((a) b c)) '(a b c))
(check-expect (my-flatten '((a b c))) '(a b c))
(check-expect (my-flatten '(a (((b c))))) '(a b c))
(check-expect (my-flatten '(((())))) '())

;;P08 Eliminates consecutive elements that are the same

(define (compress list)
  (if (null? list)
      '()
      (cons (first list)
            (reverse (compress-helper (rest list) (first list) '())))))

(define (compress-helper orig prev new-list)
  (if (null? orig)
      new-list
      (if (not (equal? (first orig) prev))
          (compress-helper (rest orig) (first orig) (cons (first orig) new-list))
          (compress-helper (rest orig) (first orig) new-list))))

(check-expect (compress '(a b b c c b)) '(a b c b))
(check-expect (compress '(a b c)) '(a b c))
(check-expect (compress '()) '())

;;P09 "Packs" consecutive same elements into sublists

(define (pack list)
  (if (null? list)
      '()
      (let* ([returned-list (pack-helper list (first list))])
        (cons returned-list (pack (sublist list (+ 1 (count-elements returned-list)) (+ 1 (count-elements list))))))))


(define (pack-helper list first-atom)
  (if (null? list)
      '()
      (if (equal? (first list) first-atom)
          (cons first-atom (pack-helper (rest list) first-atom))
          '())))

(check-expect (pack '(a a a a b c c a a d e e e e)) '((a a a a) (b) (c c) (a a) (d) (e e e e)))
(check-expect (pack '((a a) (a a) ()() b c c a a d e e e e)) '(((a a) (a a)) (()()) (b) (c c) (a a) (d) (e e e e)))
(check-expect (pack '()) '())

;;P10 Encodes a list to show the number of an element and the element itself as a list within the main list
;;P11 Modifies P10 so that when there is only 1 of an element, it shows as an atom of the element rather than a list saying (1 ...)

(define (encode list)
  (encode-helper (pack list)))

(define (encode-helper orig)
  (if (null? orig)
      '()
      (if (>  (count-elements (first orig)) 1) 
          (cons (cons (count-elements (first orig)) (cons (first (first orig)) '())) (encode-helper (rest orig)))
          (cons (first (first orig)) (encode-helper (rest orig))))))

(check-expect (encode '(a a a a b c c a a d e e e e)) '((4 a) b (2 c) (2 a) d (4 e)))
(check-expect (encode '()) '())

;;P12 Decodes a list encoded as encoded by the above method

(define (decoder list)
  (if (null? list)
      '()
      (cons-elements (individual-decoder (first list) '()) (decoder (rest list)))))

(define (individual-decoder orig new-list)
  (if (null? orig)
      new-list
      (if (list? orig)
          (individual-decoder-helper (first orig) (first (rest orig)) 0 '())
          (cons orig new-list))))

(define (individual-decoder-helper num atom count new-list)
  (if (< count num)
      (cons atom (individual-decoder-helper num atom (+ 1 count) new-list))
      new-list))

(check-expect (decoder '((4 a) b (2 c) (2 a) d (4 e))) '(a a a a b c c a a d e e e e))
(check-expect (decoder '()) '())

;;P13 Same as P10/11 except does encoding by counting and not creating separate lists

(define (direct-encoder list)
  (direct-encoder-helper list 1))

(define (direct-encoder-helper orig  count)
  (if (null? orig)
      '()
      (if (null? (rest orig))
          (if (= count 1)
              (cons (first orig) '())
              (cons (cons count (cons (first orig) '())) '()))
          (if (= count 1)
              (if (equal? (first orig) (first (rest orig)))
                  (direct-encoder-helper (rest orig)  (+ 1 count))
                  (cons (first orig)(direct-encoder-helper (rest orig)  1)))
              (if (equal? (first orig) (first (rest orig)))
                  (direct-encoder-helper (rest orig)  (+ 1 count))
                  (cons (cons count (cons (first orig) '())) (direct-encoder-helper (rest orig)  1)))))))
      
      
(check-expect (direct-encoder '(a a a a b c c a a d e)) '((4 a) b (2 c) (2 a) d  e))
(check-expect (direct-encoder '()) '())

;;P14/15 Duplicates a list a specific number of times

(define (repli list total)
  (if (null? list)
      '()
      (cons-elements (individual-repli (first list) total 0) (repli (rest list) total))))

(define (individual-repli element total count)
  (if (< count total)
      (cons element (individual-repli element total (+ 1 count)))
      '()))
      
(check-expect (repli '(a b c) 4) '(a a a a b b b b c c c c))
(check-expect (repli '() 4) '())

;;P16 Removes every Nth element from the list

(define (drop list nth)
  (drop-helper list nth 1))

(define (drop-helper list nth count)
  (if (null? list)
      '()
      (if (= count nth)
          (drop-helper (rest list) nth 1)
          (cons (first list) (drop-helper (rest list) nth (+ count 1))))))

(check-expect  (drop '(a a a a a b b b b b c c c c c) 5) '(a a a a b b b b c c c c))
(check-expect  (drop '(a b c d) 5) '(a b c d))

;;P17 Splits a list into separate sublists

(define (split list pos)
  (if (null? list)
      '()
      (cons (sublist list 1 (+ 1 pos)) (cons (sublist list (+ 1 pos) (+ 1 (count-elements list))) '()))))

(check-expect (split '(a a a a b b b b c c c c) 5) '((a a a a b) (b b b c c c c)))
(check-expect (split '(a b c d) 5) '((a b c d) ()))
(check-expect (split '() 5) '())

;;P18 Extracts a "slice" from a list

(define (sublist list beg end)
  (sublist-helper list beg end 1))

(define (sublist-helper orig beg end count)
  (if (or (null? orig) (< (- end 1) count))
      '()
      (if (< count beg) 
          (sublist-helper (rest orig) beg end (+ 1 count))
          (cons (first orig) (sublist-helper (rest orig) beg end (+ 1 count))))))
  
(check-expect (sublist '(a a a a a b b b b b c c c c c) 5 10) '(a b b b b))
(check-expect (sublist '() 5 10) '())

;;P19 Rotate a list N spaces to the left (n is denoted by pos)

(define (rotate list pos)
  (cons-elements (sublist list (+ 1 pos) (+ 1 (count-elements list))) (sublist list 1 (+ 1 pos))))

(check-expect (rotate '(a a a a a b b b b b c c c c c) 5) '(b b b b b c c c c c a a a a a))
(check-expect (rotate '(a a b) 5) '(a a b))

;;P20 Removes an element from a list

(define (remove-at list pos)
  (if (or (null? list) (null? (rest list)))
      '()
      (cons-elements (sublist list 1 pos) (sublist list (+ 1 pos) (+ 1 (count-elements list))))))

(check-expect (remove-at '(a a a a a b b b b b c c c c c) 5) '(a a a a b b b b b c c c c c))
(check-expect (remove-at '( c c c) 5) '(c c c))


(test)



























  
      

