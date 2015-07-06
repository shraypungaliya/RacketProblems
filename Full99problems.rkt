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
      (let ([returned-list (pack-helper list (first list))])
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
(check-expect (remove-at '(c c c) 5) '(c c c))

;;P21 Inserts an element at the given position

(define (insert-at list element pos)
  (cons-elements (sublist list 1 pos) (cons element (sublist list pos (+ 1 (count-elements list))))))

(check-expect (insert-at '(a b d) 'c 3) '(a b c d))
(check-expect (insert-at '() '() 3) '(()))
(check-expect (insert-at '(a b d) 'c 5) '(a b d c))

;;P22 Creates a list of numbers given the range

(define (range beg end)
  (if (<= beg end)
      (cons beg (range (+ 1 beg) end))
      '()))

(check-expect (range 3 5) '(3 4 5))
(check-expect (range 5 5) '(5))
(check-expect (range 5 3) '())

;;P23 Chooses n number of random terms from a given list
;;Cannot directly check. However, has been tested to produce correct results

(define (rnd-select list num)
  (if (< (count-elements list) num)
      list
      (if (> num 0)
          (cons (element-at list (+ 1 (random (count-elements list))))
                (rnd-select list (- num 1)))
          '())))


;;P24 Draw n different random numbers from a given range
;;Cannot directly check. However, has been tested to produce correct results

(define (lotto-select beg end num)
  (rnd-select (range beg end) num))

;;P25 Generate a random permutation of a list
;;Cannot directly check. However, has been tested to produce correct results

(define (rnd-permu list)
  (if (null? list)
      '()
      (let ([element (first (rnd-select list 1))])
        (cons element (rnd-permu (remove element list))))))

;;P26 Generate all possible combinations of a certain length from a list

(define (combos list len)
  (if (< (length list) 2)
      list
      (combo-helper (combinations list) len)))

(define (combo-helper list len)
  (if (null? list)
      '()
      (if (= (length(first list)) len)
          (cons (first list) (combo-helper (rest list) len))
          (combo-helper (rest list) len))))
    
(define (combinations lis)
  (if (= 2 (length lis))
      (list lis (cons (first lis) '()) (rest lis))
      (cons-elements (combinations (rest lis)) (cons-to-all (first lis) (combinations (rest lis))))))

(check-expect (combos '(a b c) 2) '((b c) (a b) (a c)))

      
(define (cons-to-all element list)
  (if (null? list)
      '()
      (cons (cons element (first list)) (cons-to-all element (rest list)))))

;;P27 Find all possible combinations of combinations given the length of each internal combination in a list of 9 items

(define (group list-items list-nums)
  (if (or (null? list-items) (null? (rest list-nums)))
      list-items
      (group-helper list-items list-nums '() '())))

(define (group-helper list-items list-nums final-combo current-combo)
  (if (null? list-nums)
      (if (list? current-combo)
                 (my-but-last (cons current-combo final-combo))
                 (my-but-last (cons (cons current-combo '()) final-combo)))
      (my-group-map group-helper
                    (bulk-remove current-combo list-items)
                    (rest list-nums)
                    (cons current-combo final-combo)
                    (combos (bulk-remove current-combo list-items) (first list-nums)))))

  
(define (bulk-remove remove-list master-list)
  (if (null? remove-list)
      master-list
      (bulk-remove (rest remove-list) (remove (first remove-list) master-list))))

(define (my-group-map fun list-items list-nums final-combo current-combo)
  (if (null?(rest current-combo))
      (fun list-items list-nums final-combo (first current-combo))
      (cons (fun list-items list-nums final-combo (first current-combo)) (my-group-map fun list-items list-nums final-combo (rest current-combo)))))
      
;;P28 Sort a list by the length of its sublists

(define (lsort list)
  (lsort-helper list '()))

(define (lsort-helper list new-list)
  (if (null? list)
      '()
      (insert-list (first list) (lsort-helper (rest list) new-list))))

(define (insert-list to-add new-list)
  (if (null? new-list)
      (cons to-add new-list)
      (if (> (count-elements (first new-list)) (count-elements to-add))
          (cons to-add new-list)
          (cons (first new-list) (insert-list to-add (rest new-list))))))
  
(check-expect (lsort '((a b) (c d e) (f g) (h i j k l m) (n) (o p q) (r s t u) (v w x y z))) '((n)
(f g)
(a b)
(o p q)
(c d e)
(r s t u)
(v w x y z)
(h i j k l m)))
(check-expect (lsort '()) '())

;;P28b Sort the lists by the frequency of their length

(define (lfsort list)
  (lfpack (lsort list)))


(define (lfpack list)
  (if (null? list)
      '()
      (let ([returned-list (lfpack-helper list (first list))])
        (cons-elements returned-list (lfpack (sublist list (+ 1 (count-elements returned-list)) (+ 1 (count-elements list))))))))


(define (lfpack-helper list first-elem)
  (if (null? list)
      '()
      (if (equal? (count-elements(first list)) (count-elements first-elem))
          (cons (first list) (lfpack-helper (rest list) first-elem))
          '())))

(check-expect (lsort '((a b) (c d e) (f g) (h i j k l m) (n) (o p q) (r s t u) (v w x y z))) '((n)
  (f g)
  (a b)
  (o p q)
  (c d e)
  (r s t u)
  (v w x y z)
  (h i j k l m)))
(check-expect (lsort '()) '())
;;P31 Check if a number is prime

(define (prime? n)
  (if (integer? (sqrt n))
      #f
      (prime?-helper (reverse (true-list #t n)))))

(define (prime?-helper bool-list)
  (if (equal? bool-list (insert-at (my-but-last(true-list #f (count-elements bool-list))) #f (count-elements bool-list)))
      #t
      (if (my-last bool-list)
          (if (= (find-first bool-list #t 1) (count-elements bool-list))
              #t
              (prime?-helper (turn-false-every (find-first bool-list #t 1) bool-list)))
          #f)))

(define (turn-false-every n bool-list)
  (if (< (count-elements bool-list) n)
      bool-list
      (cons-elements ( sublist bool-list 1 n ) (cons #f (turn-false-every n (sublist  bool-list (+ 1 n) (+ 1 (count-elements bool-list))))))))
       
(define (true-list bool? len)
  (if (= len 1)
      (list (not bool?))
      (cons bool? (true-list bool? (- len 1)))))

(define (find-first list elem count)
  (if (null? list)
      0
      (if (equal? (first list) elem)
          count
          (find-first (rest list) elem (+ 1 count)))))

(check-expect (prime? 25) #f)
(check-expect (prime? 29) #t)
(check-expect (prime? 0) #f)
(check-expect (prime? 1) #f)

;;P32 Finds the Greatest Common Factor of two numbers

(define (gcf num1 num2)
  (if (or (= num1 0) (= num2 0))
      0
      (if (= num1 num2)
          num1
          (if (< num1 num2)
              (gcf-helper num1 num2 (remainder num2 num1))
              (gcf-helper num2 num1 (remainder num1 num2))))))

(define (gcf-helper small big remaind)
  (if (= 0 remaind)
      big
      (if (= 0 (remainder small remaind))
          remaind
          (gcf-helper remaind small (remainder small remaind)))))

(check-expect (gcf 25 10) 5)
(check-expect (gcf 0 1) 0)

;;P33 Checks if two numbers are coprime

(define (coprime? num1 num2)
  (= 1 (gcf num1 num2)))

(check-expect (coprime? 25 10) #f)
(check-expect (coprime? 26 19) #t)

;;P34 Find all numbers before a given number that are coprime to that number

(define (phi num)
  (phi-helper num (range 2 (- num 1))))

(define (phi-helper num list)
  (if (null? list)
      '(1)
      (if (coprime? num (first list))
          (cons (first list) (phi-helper num (rest list)))
          (phi-helper num (rest list)))))

(check-expect (coprime? 25 10) #f)
(check-expect (coprime? 26 19) #t)

;;P35 Find all the prime factors of a number

(define (prime-numbers num)
  (if (= num 0)
      0
      (prime-numbers-helper num (all-primes (range 2 num)))))

(define (all-primes list)
  (if (null? list)
      '()
      (if (prime? (first list))
          (cons (first list) (all-primes (rest list)))
          (all-primes (rest list)))))

(define (prime-numbers-helper num prime-list)
  (if (= num 1)
      '()
      (if (integer? (/ num (first prime-list)))
          (cons (first prime-list) (prime-numbers-helper (/ num (first prime-list)) prime-list))
          (prime-numbers-helper num (rest prime-list)))))

(check-expect (prime-numbers 100) '(2 2 5 5))
(check-expect (prime-numbers 0) '0)


;;P36 Encode the above problem's answer

(define (prime-encoder list)
  (if (= 0 list)
      0
      (encode (prime-numbers list))))

(check-expect (prime-encoder 100) '((2 2) (2 5)))
(check-expect (prime-encoder 0) '0)






;;P37 Calculate the number of elements in the list of phi of n (given the encoded list)

(define (phi-calculator list)
  (if (null? list)
      0
      (+ (expt (* (- (second (first list)) 1) (second (first list))) (- (first (first list)) 1)) (phi-calculator (rest list)))))

(check-expect (phi-calculator '((3 2) (5 1) (7 1))) 4)

;;P39 Get the primes in a certain range

(define (prime-range start end)
  (all-primes (range start end)))

(check-expect (prime-range 2 20) '(2 3 5 7 11 13 17 19))

;;P40 Find two prime numbers that add up to the given even number

(define (goldbach num)
  (if (or (odd? num) (= num 0))
      0
      (if (= num 2)
          2
          (goldbach-helper num (prime-range 2 num))
          )))

(define (goldbach-helper num primes)
  (if (prime? (- num (first primes)))
      (list (first primes) (- num (first primes)))
      (goldbach-helper num (rest primes))))

;;P41 Given a range of integers, find the goldbach for each of the even numbers in the range

(define (goldbach-list start end)
  (goldbach-list-helper (range start end)))

(define (goldbach-list-helper list)
  (if (null? list)
      '()
      (if (and (even? (first list)) (not (= 2 (first list))))
          (cons (goldbach (first list)) (goldbach-list-helper (rest list)))
          (goldbach-list-helper (rest list)))))
          


(test)


