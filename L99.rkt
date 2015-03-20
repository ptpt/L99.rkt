#lang racket/base

;; L-99: Ninety-Nine Lisp Problems

;; Based on a Prolog problem list by werner.hett@hti.bfh.ch

(require rackunit)

;; utils
(define (sum lista)
  (foldl + 0 lista))

(define (make-list count element)
  (if (> count 0)
      (cons element (make-list (sub1 count) element))
      '()))

(define mapconcat
  (lambda args
    (apply append (apply map args))))


;;;;;;;;;;;;;;;;;;;;;;;;
;; Working with lists ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; P01 (*) Find the last box of a list.
;; Example:
;; * (my-last '(a b c d))
;; (D)

(define (my-last lista)
  (cond [(null? lista) '()]
        [(null? (cdr lista)) (car lista)]
        [else (my-last (cdr lista))]))

(check-equal? (my-last '(a b c))
              'c)
(check-equal? (my-last '(c))
              'c)
(check-equal? (my-last '())
              '())


;; P02 (*) Find the last but one box of a list.
;; Example:
;; * (my-but-last '(a b c d))
;; (C D)


;; P03 (*) Find the K'th element of a list.
;; The first element in the list is number 1.
;; Example:
;; * (element-at '(a b c d e) 3)
;; C

(define (element-at lista at)
  (if (or (< at 1) (null? lista))
      (error "not found")
      (if (= at 1)
          (car lista)
          (element-at (cdr lista) (sub1 at)))))

(check-equal? (element-at '(1 2 a b c) 2)
              2)
(check-exn exn:fail?
           (lambda ()
             (element-at '(1 2 a b c) 100)))
(check-exn exn:fail?
           (lambda ()
             (element-at '(1 2) 0)))


;; P04 (*) Find the number of elements of a list.

(define (count-elements list)
  (if (null? list)
      0
      (add1 (count-elements (cdr list)))))

(check-equal? (count-elements '())
              0)
(check-equal? (count-elements '(1))
              1)
(check-equal? (count-elements '(1 2))
              2)


;; P05 (*) Reverse a list.
(define (reverse-list lista)
  (if (null? lista)
      '()
      (append (reverse-list (cdr lista))
              (list (car lista)))))

(check-equal? (reverse-list '(a b c))
              '(c b a))
(check-equal? (reverse-list '(c))
              '(c))
(check-equal? (reverse-list '())
              '())

;; P06 (*) Find out whether a list is a palindrome.
;; A palindrome can be read forward or backward; e.g. (x a m a x).

(define (palindrome lista)
  (equal? (reverse-list lista) lista))

(check-true (palindrome '(a b c b a)))
(check-false (palindrome '(a b c)))
(check-true (palindrome '()))


;; P07 (**) Flatten a nested list structure.

;; Transform a list, possibly holding lists as elements into a `flat'
;; list by replacing each list with its elements (recursively).

;; Example:
;; * (my-flatten '(a (b (c d) e)))
;; (A B C D E)
;; Hint: Use the predefined functions list and append.

(define (my-flatten list)
  (cond [(null? list) '()]
        [(list? (car list))
         (append (my-flatten (car list))
                 (my-flatten (cdr list)))]
        [else
         (cons (car list) (my-flatten (cdr list)))]))

(check-equal? (my-flatten '(1 (2 3)))
              '(1 2 3))
(check-equal? (my-flatten '(((((1)))) 2 ((3) 4) (c)))
              '(1 2 3 4 c))
(check-equal? (my-flatten '(() () (())))
              '())
(check-equal? (my-flatten '())
              '())


;; P08 (**) Eliminate consecutive duplicates of list elements.

;; If a list contains repeated elements they should be replaced with a
;; single copy of the element. The order of the elements should not be
;; changed.

;; Example:
;; * (compress '(a a a a b c c a a d e e e e))
;; (A B C A D E)

(define (compress lista)
  (if (null? lista) '()
      (let ([head (car lista)]
            [compressed (compress (cdr lista))])
        (cond [(null? compressed) lista]
              [(equal? (car compressed) head) compressed]
              [else (cons head compressed)]))))

(check-equal? (compress '(a a a a b c c a a d e e e e))
              '(a b c a d e))
(check-equal? (compress '(a a a a))
              '(a))
(check-equal? (compress '())
              '())


;; P09 (**) Pack consecutive duplicates of list elements into sublists.
;; If a list contains repeated elements they should be placed in separate sublists.

;; Example:
;; * (pack '(a a a a b c c a a d e e e e))
;; ((A A A A) (B) (C C) (A A) (D) (E E E E))

(define (pack lista)
  ;; tail recursion
  (define (recur lista result sublist)
    (if (null? lista)
        (if (null? sublist)
            result
            (append result (list sublist)))
        (if (or (null? sublist)
                (equal? (car lista) (car sublist)))
            (recur (cdr lista) result (append sublist (list (car lista))))
            (recur (cdr lista) (append result (list sublist)) (list (car lista))))))
  (recur lista '() '()))

(check-equal? (pack '(a a a a b c c a a d e e e e))
              '((a a a a) (b) (c c) (a a) (d) (e e e e)))
(check-equal? (pack '(a))
              '((a)))
(check-equal? (pack '())
              '())


;; P10 (*) Run-length encoding of a list.

;; Use the result of problem P09 to implement the so-called run-length
;; encoding data compression method. Consecutive duplicates of
;; elements are encoded as lists (N E) where N is the number of
;; duplicates of the element E.

;; Example:
;; * (encode '(a a a a b c c a a d e e e e))
;; ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))

(define (encode lista)
  (map (lambda (sublist)
         (list (length sublist) (car sublist)))
       (pack lista)))

(check-equal? (encode '(a a a a b c c a a d e e e e))
              '((4 a) (1 b) (2 c) (2 a) (1 d) (4 e)))
(check-equal? (encode '(a))
              '((1 a)))
(check-equal? (encode '())
              '())


;; P11 (*) Modified run-length encoding.

;; Modify the result of problem P10 in such a way that if an element
;; has no duplicates it is simply copied into the result list. Only
;; elements with duplicates are transferred as (N E) lists.

;; Example:
;; * (encode-modified '(a a a a b c c a a d e e e e))
;; ((4 A) B (2 C) (2 A) D (4 E))

(define (encode-modified lista)
  (map (lambda (sublist)
         (if (= (car sublist) 1)
             (cadr sublist)
             sublist))
       (encode lista)))

(check-equal? (encode-modified '(a a a a b c c a a d e e e e))
              '((4 a) b (2 c) (2 a) d (4 e)))
(check-equal? (encode-modified '(a b c))
              '(a b c))


;; P12 (**) Decode a run-length encoded list.

;; Given a run-length code list generated as specified in problem
;; P11. Construct its uncompressed version.

(define (decode lista)
  (if (null? lista) '()
      (let ([sublist (car lista)])
        (append (if (list? sublist)
                    (make-list (car sublist) (cadr sublist))
                    (list sublist))
                (decode (cdr lista))))))

(check-equal? (decode '((4 a) b (2 c) (2 a) d (4 e)))
              '(a a a a b c c a a d e e e e))
(check-equal? (decode '(a b c))
              '(a b c))


;; P13 (**) Run-length encoding of a list (direct solution).

;; Implement the so-called run-length encoding data compression method
;; directly. I.e. don't explicitly create the sublists containing the
;; duplicates, as in problem P09, but only count them. As in problem
;; P11, simplify the result list by replacing the singleton lists (1
;; X) by X.

;; Example:
;; * (encode-direct '(a a a a b c c a a d e e e e))
;; ((4 A) B (2 C) (2 A) D (4 E))

(define (encode-direct lista)
  (if (null? lista) '()
      (let ([encoded (encode-direct (cdr lista))]
            [head (car lista)])
        (cond [(null? encoded)
               (list head)]
              [(list? (car encoded))
               (let ([n (caar encoded)]
                     [e (cadar encoded)])
                 (if (equal? e head)
                     (cons (list (+ n 1) e)
                           (cdr encoded))
                     (cons head encoded)))]
              [else (if (equal? (car encoded) head)
                        (cons (list 2 head)
                              (cdr encoded))
                        (cons head encoded))]))))

(check-equal? (encode-direct '(a a a a b c c a a d e e e e))
              '((4 a) b (2 c) (2 a) d (4 e)))
(check-equal? (encode-direct '(a a a a))
              '((4 a)))
(check-equal? (encode-direct '(a b c d))
              '(a b c d))
(check-equal? (encode-direct '())
              '())
(check-equal? (encode-direct '(a))
              '(a))


;; P14 (*) Duplicate the elements of a list.
;; Example:
;; * (dupli '(a b c c d))
;; (A A B B C C C C D D)

(define (dupli lista)
  (if (null? lista)
      '()
      (append (list (car lista) (car lista))
              (dupli (cdr lista)))))

(check-equal? (dupli '(a b c c d))
              '(a a b b c c c c d d))
(check-equal? (dupli '(()))
              '(() ()))


;; P15 (**) Replicate the elements of a list a given number of times.
;; Example:
;; * (repli '(a b c) 3)
;; (A A A B B B C C C)

(define (repli-1 lista n)
  (mapconcat (lambda (e) (make-list n e)) lista))

(define (repli-2 lista n)
  (define (recur lista count)
    (if (null? lista)
        '()
        (if (> count 0)
            (cons (car lista) (recur lista (sub1 count)))
            (recur (cdr lista) n))))
  (recur lista n))

(check-equal? (repli-1 '(a b c) 3)
              '(a a a b b b c c c))
(check-equal? (repli-2 '(a b c) 3)
              '(a a a b b b c c c))
(check-equal? (repli-1 '(a b c) 1)
              '(a b c))
(check-equal? (repli-2 '(a b c) 1)
              '(a b c))
(check-equal? (repli-1 '(a b c) 0)
              '())
(check-equal? (repli-2 '(a b c) 0)
              '())


;; P16 (**) Drop every N'th element from a list.
;; Example:
;; * (drop '(a b c d e f g h i k) 3)
;; (A B D E G H K)

(define (my-drop lista n)
  (define (recur lista n m)
    (if (null? lista)
        '()
        (if (= n 1)
            (recur (cdr lista) m m)
            (cons (car lista) (recur (cdr lista) (sub1 n) m)))))
  (recur lista n n))

(check-equal? (my-drop '(a b c d e f g) 3)
              '(a b d e g))
(check-equal? (my-drop '(a b c d) 4)
              '(a b c))
(check-equal? (my-drop '(a b c) 100)
              '(a b c))
(check-equal? (my-drop '(a b c) 1)
              '())


;; P17 (*) Split a list into two parts; the length of the first part is given.
;; Do not use any predefined predicates.

;; Example:
;; * (split '(a b c d e f g h i k) 3)
;; ( (A B C) (D E F G H I K))

(define (split lista n)
  (define (recur left right m)
    (if (null? right)
        (list left '())
        (if (zero? m)
            (list left right)
            (recur (append left (list (car right)))
                   (cdr right) (sub1 m)))))
  (recur '() lista n))

(check-equal? (split '(a b c d e f g h i k) 3)
              '((a b c) (d e f g h i k)))
(check-equal? (split '(a b c d e f g h i k) 0)
              '(() (a b c d e f g h i k)))
(check-equal? (split '(a b c d e f g h i k) 100)
              '((a b c d e f g h i k) ()))
(check-equal? (split '() 100)
              '(() ()))
(check-equal? (split '() 0)
              '(() ()))


;; P18 (**) Extract a slice from a list.

;; Given two indices, I and K, the slice is the list containing the
;; elements between the I'th and K'th element of the original list
;; (both limits included). Start counting the elements with 1.

;; Example:
;; * (slice '(a b c d e f g h i k) 3 7)
;; (C D E F G)

(define (slice lista m n)
  (let ([start (if (< m n) m n)]
        [end (if (> m n) m n)])
    (if (null? lista) '()
        (cond [(> start 1)
               (slice (cdr lista) (sub1 start) (sub1 end))]
              [(> end 0)
               (cons (car lista)
                     (slice (cdr lista) (sub1 start) (sub1 end)))]
              [else '()]))))

(check-equal? (slice '(a b c d e f g h i k) 3 7)
              '(c d e f g))
(check-equal? (slice '(a b c d e f g h i k) 7 3)
              '(c d e f g))
(check-equal? (slice '(a b c d e f g h i k) 1 1)
              '(a))


;; P19 (**) Rotate a list N places to the left.
;; Examples:
;; * (rotate '(a b c d e f g h) 3)
;; (D E F G H A B C)

;; * (rotate '(a b c d e f g h) -2)
;; (G H A B C D E F)

;; Hint: Use the predefined functions length and append, as well as
;; the result of problem P17.

(define (rotate lista n)
  (if (null? lista)
      '()
      (let ([sp (split lista (if (> n 0) n (+ (length lista) n)))])
        (apply append (list (car (cdr sp)) (car sp))))))

(check-equal? (rotate '(a b c d e f g h) 3)
              '(d e f g h a b c))
(check-equal? (rotate '(a b c d e f g h) -2)
              '(g h a b c d e f))
(check-equal? (rotate '(a b c d) 4)
              '(a b c d))
(check-equal? (rotate '(a b c d) 5)
              '(a b c d))


;; P20 (*) Remove the K'th element from a list.
;; Example:
;; * (remove-at '(a b c d) 2)
;; (A C D)

(define (remove-at lista n)
  (if (null? lista)
      '()
      (if (> n 1)
          (cons (car lista) (remove-at (cdr lista) (sub1 n)))
          (cdr lista))))

(check-equal? (remove-at '(a b c d) 2)
              '(a c d))
(check-equal? (remove-at '(a b c d) 1)
              '(b c d))
(check-equal? (remove-at '(a b c d) 100)
              '(a b c d))


;; P21 (*) Insert an element at a given position into a list.
;; Example:
;; * (insert-at 'alfa '(a b c d) 2)
;; (A ALFA B C D)

(define (insert-at e lista n)
  (if (null? lista)
      (list e)
      (if (> n 1)
          (cons (car lista) (insert-at e (cdr lista) (sub1 n)))
          (cons e lista))))

(check-equal? (insert-at 'alfa '(a b c d) 2)
              '(a alfa b c d))
(check-equal? (insert-at 1 '(a b c) 1)
              '(1 a b c))
(check-equal? (insert-at 100 '(a b c) 3)
              '(a b 100 c))
(check-equal? (insert-at 100 '(a b c) 4)
              '(a b c 100))
(check-equal? (insert-at 100 '(a b c) 5)
              '(a b c 100))


;; P22 (*) Create a list containing all integers within a given range.
;; If first argument is smaller than second, produce a list in decreasing order.
;; Example:
;; * (range 4 9)
;; (4 5 6 7 8 9)

(define (my-range start end)
  (if (= start end)
      (list start)
      (cons start (my-range ((if (< start end) add1 sub1) start) end))))

(check-equal? (my-range 1 1)
              '(1))
(check-equal? (my-range 1 9)
              '(1 2 3 4 5 6 7 8 9))
(check-equal? (my-range 4 2)
              '(4 3 2))


;; P23 (**) Extract a given number of randomly selected elements from a list.
;; The selected items shall be returned in a list.
;; Example:
;; * (rnd-select '(a b c d e f g h) 3)
;; (E D A)
;; Hint: Use the built-in random number generator and the result of problem P20.

(define (rnd-select lista n)
  (if (or (< n 1)
          (null? lista))
      '()
      (let ([rnd (random (length lista))])
        (cons (list-ref lista rnd)
              (rnd-select (remove-at lista (add1 rnd)) (sub1 n))))))

(check-equal? (length (rnd-select '(a b c d e f g h) 3))
              3)
(check-equal? (length (rnd-select '(a b c d) 100))
              4)
(check-equal? (rnd-select '(1 2 3) 0)
              '())
(check-equal? (rnd-select '() 3)
              '())


;; P24 (*) Lotto: Draw N different random numbers from the set 1..M.
;; The selected numbers shall be returned in a list.
;; Example:
;; * (lotto-select 6 49)
;; (23 1 17 33 21 37)
;; Hint: Combine the solutions of problems P22 and P23.

(define (lotto-select n m)
  (rnd-select (my-range 1 m) n))

(check-equal? (length (lotto-select 6 49))
              6)


;; P25 (*) Generate a random permutation of the elements of a list.
;; Example:
;; * (rnd-permu '(a b c d e f))
;; (B A D C E F)
;; Hint: Use the solution of problem P23.

(define (rnd-permu lista)
  (rnd-select lista (length lista)))


;; Generate all permutations of a list
(define (rpermutation-1 r lista)
  (let ([n (length lista)])
    (cond [(> r n) '()]
          [(zero? r) '(())]
          [else
           (append
            (mapconcat (lambda (perm)
                         (map (lambda (pos) (insert-at (car lista) perm pos))
                              (my-range 1 r)))
                       (rpermutation-1 (sub1 r) (cdr lista)))
            (rpermutation-1 r (cdr lista)))])))

(define (rpermutation-2 r lista)
  (define (recur r remain result)
    (let ([n (length remain)])
      (cond [(> r n) '()]
            [(zero? r) (list result)]
            [else
             (mapconcat (lambda (pos)
                          (recur (sub1 r)
                                 (remove-at remain pos)
                                 (append result (list (list-ref remain (sub1 pos))))))
                        (my-range 1 n))])))
  (recur r lista '()))

(define (permutation lista)
  (rpermutation-2 (length lista) lista))

(check-equal? (rpermutation-1 0 '())
              '(()))
(check-equal? (rpermutation-2 0 '())
              '(()))

(check-equal? (rpermutation-1 2 '(1))
              '())
(check-equal? (rpermutation-2 2 '(1))
              '())

(check-equal? (rpermutation-1 1 '(1))
              '((1)))
(check-equal? (rpermutation-2 1 '(1))
              '((1)))

(check-equal? (rpermutation-1 1 '(1 2))
              '((1) (2)))
(check-equal? (rpermutation-2 1 '(1 2))
              '((1) (2)))

(check-equal? (rpermutation-1 3 '(1 2 3))
              '((1 2 3) (2 1 3) (2 3 1) (1 3 2) (3 1 2) (3 2 1)))
(check-equal? (rpermutation-2 3 '(1 2 3))
              '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1)))


;; P26 (**) Generate the combinations of K distinct objects chosen
;; from the N elements of a list

;; In how many ways can a committee of 3 be chosen from a group of 12
;; people? We all know that there are C(12,3) = 220 possibilities
;; (C(N,K) denotes the well-known binomial coefficients). For pure
;; mathematicians, this result may be great. But we want to really
;; generate all the possibilities in a list.

;; Example:
;; * (combination 3 '(a b c d e f))
;; ((A B C) (A B D) (A B E) ... )

(define (combination-1 r lista)
  (let ([n (length lista)])
    (cond [(> r n) '()]
          [(zero? r) '(())]
          [else
           (append (map (lambda (com)
                          (cons (car lista) com))
                        (combination-1 (sub1 r) (cdr lista)))
                   (combination-1 r (cdr lista)))])))

(define (combination-2 r lista)
  (define (recur r remain result)
    (let ([n (length remain)])
      (cond [(> r n) '()]
            [(zero? r) (list result)]
            [else
             (append (recur (sub1 r)
                            (cdr remain)
                            (cons (car remain) result))
                     (recur r (cdr remain) result))])))
  (recur r lista '()))

(define combination combination-2)

(check-equal? (combination-1 3 '(1 2 3 4))
              '((1 2 3) (1 2 4) (1 3 4) (2 3 4)))
(check-equal? (combination-2 3 '(1 2 3 4))
              '((3 2 1) (4 2 1) (4 3 1) (4 3 2)))

(check-equal? (combination-1 1 '(1 2 3))
              '((1) (2) (3)))
(check-equal? (combination-2 1 '(1 2 3))
              '((1) (2) (3)))

(check-equal? (combination-1 1 '())
              '())
(check-equal? (combination-2 1 '())
              '())

(check-equal? (combination-1 0 '())
              '(()))
(check-equal? (combination-2 0 '())
              '(()))


;; P27 (**) Group the elements of a set into disjoint subsets.

;; a) In how many ways can a group of 9 people work in 3 disjoint
;; subgroups of 2, 3 and 4 persons? Write a function that generates
;; all the possibilities and returns them in a list.

;; Example:
;; * (group3 '(aldo beat carla david evi flip gary hugo ida))
;; ( ( (ALDO BEAT) (CARLA DAVID EVI) (FLIP GARY HUGO IDA) )
;; ... )

;; b) Generalize the above predicate in a way that we can specify a
;; list of group sizes and the predicate will return a list of groups.

;; Example:
;; * (group '(aldo beat carla david evi flip gary hugo ida) '(2 2 5))
;; ( ( (ALDO BEAT) (CARLA DAVID) (EVI FLIP GARY HUGO IDA) )
;; ... )

;; Note that we do not want permutations of the group members;
;; i.e. ((ALDO BEAT) ...) is the same solution as ((BEAT ALDO)
;; ...). However, we make a difference between ((ALDO BEAT) (CARLA
;; DAVID) ...) and ((CARLA DAVID) (ALDO BEAT) ...).

;; You may find more about this combinatorial problem in a good book
;; on discrete mathematics under the term "multinomial coefficients".

(define (group-split lista remain r)
  (let ([n (length lista)])
    (cond [(> r n) '()]
          [(zero? r)
           (list (list '() (append lista remain)))]
          [else (append (map (lambda (result)
                               (list (cons (car lista)
                                           (car result))
                                     (cadr result)))
                             (group-split (cdr lista) remain (sub1 r)))
                        (group-split (cdr lista) (cons (car lista) remain) r))])))

(define (group lista sizes)
  (if (null? sizes)
      '(())
      (mapconcat (lambda (result)
                   (map (lambda (g) (cons (car result) g))
                        (group (cadr result) (cdr sizes))))
                 (group-split lista '() (car sizes)))))

(check-equal? (group '(1 2 3) '(2 1))
              '(((1 2) (3))
                ((1 3) (2))
                ((2 3) (1))))
(check-equal? (group '(1 2 3 4) '(2 2 3))
              '())
(check-equal? (group '(1 2 3) '(2))
              '(((1 2)) ((1 3)) ((2 3))))
(check-equal? (group '(1 2 3) '(1))
              '(((1)) ((2)) ((3))))
;; TODO: why?
(check-equal? (group '(1 2 3) '(0))
              '((())))


;; P28 (**) Sorting a list of lists according to length of sublists

;; a) We suppose that a list contains elements that are lists
;; themselves. The objective is to sort the elements of this list
;; according to their length. E.g. short lists first, longer lists
;; later, or vice versa.

;; Example:
;; * (lsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
;; ((O) (D E) (D E) (M N) (A B C) (F G H) (I J K L))

(define (lsort lista)
  (sort lista (lambda (a b) (< (length a) (length b)))))

(check-equal? (lsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
              '((o) (d e) (d e) (m n) (a b c) (f g h) (i j k l)))


;; b) Again, we suppose that a list contains elements that are lists
;; themselves. But this time the objective is to sort the elements of
;; this list according to their length frequency; i.e., in the
;; default, where sorting is done ascendingly, lists with rare lengths
;; are placed first, others with a more frequent length come later.

;; Example:
;; * (lfsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
;; ((i j k l) (o) (a b c) (f g h) (d e) (d e) (m n))

(define (lfsort lista)
  (define (insert a lista)
    (let ([n (length a)])
      (cond [(null? lista)
             ;; not found
             (list (list 1 a))]
            [(= (length (cadar lista)) n)
             ;; merge
             (cons (cons (add1 (caar lista))
                         (cons a (cdar lista)))
                   (cdr lista))]
            [else
             ;; continue inserting
             (cons (car lista)
                   (insert a (cdr lista)))])))

  (define (group lista)
    (if (null? lista)
        '()
        (insert (car lista)
                (group (cdr lista)))))

  (mapconcat cdr
             (sort (group lista)
                   (lambda (a b) (< (car a) (car b))))))

(check-equal? (lfsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
              '((o) (i j k l) (a b c) (f g h) (d e) (d e) (m n)))
(check-equal? (lfsort '())
              '())
(check-equal? (lfsort '((a) ()))
              '(() (a)))


;; Note that in the above example, the first two lists in the result
;; have length 4 and 1, both lengths appear just once. The third and
;; forth list have length 3 which appears twice (there are two list of
;; this length). And finally, the last three lists have length 2. This
;; is the most frequent length.

;;;;;;;;;;;;;;;;
;; Arithmetic ;;
;;;;;;;;;;;;;;;;

;; P31 (**) Determine whether a given integer number is prime.
;; Example:
;; * (is-prime 7)
;; T

(define (divisible? m n)
  (zero? (modulo m n)))

(define (prime? n)
  (define (test m)
    (or (> (* m m) n)
        (and
         (not (divisible? n m))
         (test (add1 m)))))
  (and (>= n 2)
       (test 2)))

(check-false (prime? 1))
(check-true (prime? 2))
(check-true (prime? 3))
(check-false (prime? 4))
(check-true (prime? 5))
(check-false (prime? 6))
(check-true (prime? 7))
(check-false (prime? 8))
(check-false (prime? 12))
(check-true (prime? 13))
(check-true (prime? 101))


;; P32 (**) Determine the greatest common divisor of two positive integer numbers.
;; Use Euclid's algorithm.
;; Example:
;; * (gcd 36 63)
;; 9

(define (gcd m n)
  (cond [(zero? m) n]
        [(zero? n) m]
        [(= m n) m]
        [else (if (> m n)
                  (gcd n (modulo m n))
                  (gcd m (modulo n m)))]))

(check-equal? (gcd 63 36) 9)
(check-equal? (gcd 36 63) 9)
(check-equal? (gcd 36 0) 36)


;; P33 (*) Determine whether two positive integer numbers are coprime.
;; Two numbers are coprime if their greatest common divisor equals 1.
;; Example:
;; * (coprime 35 64)
;; T

(define (coprime? m n)
  (= 1 (gcd m n)))

(check-true (coprime? 35 64))
(check-true (coprime? 64 35))


;; P34 (**) Calculate Euler's totient function phi(m).

;; Euler's so-called totient function phi(m) is defined as the number
;; of positive integers r (1 <= r < m) that are coprime to m.

;; Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.

;; * (totient-phi 10)
;; 4

;; Find out what the value of phi(m) is if m is a prime
;; number. Euler's totient function plays an important role in one of
;; the most widely used public key cryptography methods (RSA). In this
;; exercise you should use the most primitive method to calculate this
;; function (there are smarter ways that we shall discuss later).

(define (totient-phi n)
  (define (count m)
    (if (>= m 0)
        (+ (if (coprime? n m) 1 0)
           (count (sub1 m)))
        0))
  (count (sub1 n)))

(check-equal? (totient-phi 10)
              4)
(check-equal? (totient-phi 2)
              1)
(check-equal? (totient-phi 1)
              1)


;; P35 (**) Determine the prime factors of a given positive integer.
;; Construct a flat list containing the prime factors in ascending order.
;; Example:
;; * (prime-factors 315)
;; (3 3 5 7)

(define (prime-factors n)
  (define (recur m n)
    (if (>= n m)
        (if (zero? (modulo n m))
            (cons m (recur 2 (/ n m)))
            (recur (add1 m) n))
        '()))
  (sort (recur 2 n) <))

(check-equal? (prime-factors 100)
              '(2 2 5 5))
(check-equal? (prime-factors 315)
              '(3 3 5 7))
(check-equal? (prime-factors 121)
              '(11 11))
(check-equal? (prime-factors 11)
              '(11))
(check-equal? (prime-factors 2)
              '(2))
(check-equal? (prime-factors 1)
              '())


;; P36 (**) Determine the prime factors of a given positive integer (2).
;; Construct a list containing the prime factors and their multiplicity.
;; Example:
;; * (prime-factors-mult 315)
;; ((3 2) (5 1) (7 1))
;; Hint: The problem is similar to problem P13.

(define (prime-factors-mult n)
  (map reverse
       (encode (prime-factors n))))

(check-equal? (prime-factors-mult 315)
              '((3 2) (5 1) (7 1)))
(check-equal? (prime-factors-mult 11)
              '((11 1)))
(check-equal? (prime-factors-mult 10)
              '((2 1) (5 1)))
(check-equal? (prime-factors-mult 1)
              '())


;; P37 (**) Calculate Euler's totient function phi(m) (improved).

;; See problem P34 for the definition of Euler's totient function. If
;; the list of the prime factors of a number m is known in the form of
;; problem P36 then the function phi(m) can be efficiently calculated
;; as follows: Let ((p1 m1) (p2 m2) (p3 m3) ...) be the list of prime
;; factors (and their multiplicities) of a given number m. Then phi(m)
;; can be calculated with the following formula:

;; phi(m) = (p1 - 1) * p1 ** (m1 - 1) + (p2 - 1) * p2 ** (m2 - 1) + (p3 - 1) * p3 ** (m3 - 1) + ...

;; Note that a ** b stands for the b'th power of a.

(define (improved-totient-phi n)
  (if (= n 1)
      ;; special case
      1
      (sum (map
            (lambda (e)
              (let ([p (car e)] [m (cadr e)])
                (* (sub1 p) (expt p (sub1 m)))))
            (prime-factors-mult n)))))

(check-equal? (improved-totient-phi 10)
              5)
(check-equal? (improved-totient-phi 1)
              1)


;; P38 (*) Compare the two methods of calculating Euler's totient function.

;; Use the solutions of problems P34 and P37 to compare the
;; algorithms. Take the number of logical inferences as a measure for
;; efficiency. Try to calculate phi(10090) as an example.

(check-equal? (totient-phi 10090)
              4032)
;; TODO: why it doesn't work
(check-equal? (improved-totient-phi 10090)
              4032)


;; P39 (*) A list of prime numbers.

;; Given a range of integers by its lower and upper limit, construct a
;; list of all prime numbers in that range.

(define (primes-until m)
  (define (generate n primes)
    (if (> n m)
        primes
        ;; return #t if no prime is able to divide n
        (if (andmap (lambda (p) (not (divisible? n p))) primes)
            (generate (add1 n) (append primes (list n)))
            (generate (add1 n) primes))))

  (generate 2 '()))

(define (primes m n)
  (filter (lambda (p) (>= p m))
          (primes-until n)))

(check-equal? (primes-until 1)
              '())
(check-equal? (primes-until 2)
              '(2))
(check-equal? (primes-until 10)
              '(2 3 5 7))
(check-equal? (primes 2 8)
              '(2 3 5 7))
(check-equal? (primes 8 2)
              '())
(check-equal? (primes 2 2)
              '(2))


;; P40 (**) Goldbach's conjecture.

;; Goldbach's conjecture says that every positive even number greater
;; than 2 is the sum of two prime numbers. Example: 28 = 5 + 23. It is
;; one of the most famous facts in number theory that has not been
;; proved to be correct in the general case. It has been numerically
;; confirmed up to very large numbers (much larger than we can go with
;; our Prolog system). Write a predicate to find the two prime numbers
;; that sum up to a given even integer.

;; Example:
;; * (goldbach 28)
;; (5 23)

(define (goldbach n)
  (define (test primes rprimes)
    (if (or (null? primes)
            (null? rprimes))
        '()
        (let ([p (car primes)]
              [r (car rprimes)])
          (cond [(> (+ p r) n)
                 (test primes (cdr rprimes))]
                [(< (+ p r) n)
                 (test (cdr primes) rprimes)]
                [else
                 (list p r)]))))
  (let ([primes (primes-until n)])
    (test primes (reverse primes))))

(check-equal? (goldbach 28)
              '(5 23))
(check-equal? (goldbach 10)
              '(3 7))
(check-equal? (goldbach 100)
              '(3 97))
(check-equal? (goldbach 10000)
              '(59 9941))
(check-equal? (goldbach 12322)
              '(41 12281))


;; P41 (**) A list of Goldbach compositions.

;; Given a range of integers by its lower and upper limit, print a
;; list of all even numbers and their Goldbach composition.

;; Example:
;; * (goldbach-list 9 20)
;; 10 = 3 + 7
;; 12 = 5 + 7
;; 14 = 3 + 11
;; 16 = 3 + 13
;; 18 = 5 + 13
;; 20 = 3 + 17

(define (goldbach-list m n)
  ;; filter out empty lists
  (filter pair?
          (map goldbach
               (filter even? (my-range m n)))))

(check-equal? (goldbach-list 9 20)
              '((3 7) (5 7) (3 11) (3 13) (5 13) (3 17)))


;; In most cases, if an even number is written as the sum of two prime
;; numbers, one of them is very small. Very rarely, the primes are
;; both bigger than say 50. Try to find out how many such cases there
;; are in the range 2..3000.

;; Example (for a print limit of 50):
;; * (goldbach-list 1 2000 50)
;; 992 = 73 + 919
;; 1382 = 61 + 1321
;; 1856 = 67 + 1789
;; 1928 = 61 + 1867

(define (goldbach-list-with-limit n m limit)
  (filter (lambda (p)
            (> (car p) limit))
          (goldbach-list n m)))

(check-equal? (goldbach-list-with-limit 1 2000 50)
              '((73 919) (61 1321) (67 1789) (61 1867)))


;;;;;;;;;;;;;;;;;;;;;
;; Logic and Codes ;;
;;;;;;;;;;;;;;;;;;;;;

;; P46 (**) Truth tables for logical expressions.

;; Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and
;; equ/2 (for logical equivalence) which succeed or fail according to
;; the result of their respective operations; e.g. and(A,B) will
;; succeed, if and only if both A and B succeed. Note that A and B can
;; be Prolog goals (not only the constants true and fail).

;; A logical expression in two variables can then be written in prefix
;; notation, as in the following example: and(or(A,B),nand(A,B)).

;; Now, write a predicate table/3 which prints the truth table of a
;; given logical expression in two variables.

;; Example:
;; * table(A,B,and(A,or(A,B))).
;; true true true
;; true fail true
;; fail true fail
;; fail fail fail

(define (prefix-eval exp env)
  (cond [(pair? exp)
         (cond [(eq? 'and (car exp))
                (andmap (lambda (e) (prefix-eval e env)) (cdr exp))]
               [(eq? 'or (car exp))
                (ormap (lambda (e) (prefix-eval e env)) (cdr exp))]
               [(eq? 'not (car exp))
                (not (prefix-eval (cadr exp) env))]
               [else (error "invalid operator")])]
        [(symbol? exp) (cdr (assq exp env))]
        [else exp]))

(let ([prefix '(and A (or A (not (not B))))])
  (check-equal? (list (prefix-eval prefix '((A . #t) (B . #t)))
                      (prefix-eval prefix '((A . #t) (B . #f)))
                      (prefix-eval prefix '((A . #f) (B . #t)))
                      (prefix-eval prefix '((A . #f) (B . #f))))
                '(#t #t #f #f)))


;; P47 (*) Truth tables for logical expressions (2).

;; Continue problem P46 by defining and/2, or/2, etc as being
;; operators. This allows to write the logical expression in the more
;; natural way, as in the example: A and (A or not B). Define operator
;; precedence as usual; i.e. as in Java.

;; Example:
;; * table(A,B, A and (A or not B)).
;; true true true
;; true fail true
;; fail true fail
;; fail fail fail

(define (infix->prefix infix)
  (define operator-property
    ;; alist of (op priority number-of-operands)
    '((or 1 2) (and 2 2) (not 3 1)))

  (define (push operators prefix)
    "merge operators with prefix and push them into prefix"
    (if (null? operators)
        prefix
        (let ([operands (caddr (assq (car operators) operator-property))])
          (push (cdr operators)
                (cons (cons (car operators)
                            (reverse (car (split prefix operands))))
                      (cadr (split prefix operands)))))))

  (define (pop op stack output)
    "pop out all operators that have higher priority than op and
return (operators . stack)"
    (if (null? stack)
        (cons output stack)
        (let [[p (cadr (assq op operator-property))]]
          (if (< p (cadr (assq (car stack) operator-property)))
              (pop op (cdr stack) (cons (car stack) output))
              (cons output stack)))))

  (define (convert infix prefix stack)
    "convert an infix expression to a prefix expression"
    (cond [(null? infix)
           (car (push stack prefix))]
          [(memq (car infix) '(not and or))
           (let ([stk (pop (car infix) stack '())])
             (convert (cdr infix)
                      (push (car stk) prefix)
                      (cons (car infix) (cdr stk))))]
          [(pair? (car infix))
           (convert (cdr infix)
                    (cons (convert (car infix) '() '()) prefix)
                    stack)]
          [else
           (convert (cdr infix)
                    (cons (car infix) prefix)
                    stack)]))

  (convert infix '() '()))

(check-equal? (infix->prefix '(A and B))
              '(and A B))
(check-equal? (infix->prefix '(not not A))
              '(not (not A)))
(check-equal? (infix->prefix '(not (not A)))
              '(not (not A)))
(check-equal? (infix->prefix '((B) and not (not A)))
              '(and B (not (not A))))
(check-equal? (infix->prefix '(A and B or not not C and (a or b)))
              '(or (and A B) (and (not (not C)) (or a b))))

(define (infix-eval infix env)
  (prefix-eval (infix->prefix infix) env))

(check-equal? (list (infix-eval '(A and (A or not B)) '((A . #t) (B . #t)))
                    (infix-eval '(A and (A or not B)) '((A . #t) (B . #f)))
                    (infix-eval '(A and (A or not B)) '((A . #f) (B . #t)))
                    (infix-eval '(A and (A or not B)) '((A . #f) (B . #f))))
              '(#t #t #f #f))


;; P48 (**) Truth tables for logical expressions (3).

;; Generalize problem P47 in such a way that the logical expression
;; may contain any number of logical variables. Define table/2 in a
;; way that table(List,Expr) prints the truth table for the expression
;; Expr, which contains the logical variables enumerated in List.

;; Example:
;; * table([A,B,C], A and (B or C) equ A and B or A and C).
;; true true true true
;; true true fail true
;; true fail true true
;; true fail fail true
;; fail true true true
;; fail true fail true
;; fail fail true true
;; fail fail fail true

;;;; See P47


;; P49 (**) Gray code.

;; An n-bit Gray code is a sequence of n-bit strings constructed
;; according to certain rules. For example,

;; n = 1: C(1) = ['0','1'].
;; n = 2: C(2) = ['00','01','11','10'].
;; n = 3: C(3) = ['000','001','011','010','110','111','101','100'].

;; Find out the construction rules and write a predicate with the
;; following specification:

;; % gray(N,C) :- C is the N-bit Gray code

;; Can you apply the method of "result caching" in order to make the
;; predicate more efficient, when it is to be used repeatedly?

(define (half n)
  (floor (/ n 2)))

(define (gray? n c)
  (cond [(zero? n)
         (or (null? c)
             (andmap (compose1 zero? length) c))]
        [else
         (let* ([a (split c (half (length c)))]
                [first-half (car a)]
                [second-half (cadr a)])
           (and (andmap (lambda (c) (= 0 (car c))) first-half)
                (andmap (lambda (c) (= 1 (car c))) second-half)
                (equal? (map cdr first-half) (map cdr (reverse second-half)))
                (gray? (sub1 n) (map cdr first-half))))]))

(check-true (gray? 0 '(())))
(check-true (gray? 1 '((0) (1))))
(check-true (gray? 2 '((0 0) (0 1) (1 1) (1 0))))
(check-true (gray? 3 '((0 0 0) (0 0 1) (0 1 1) (0 1 0) (1 1 0) (1 1 1) (1 0 1) (1 0 0))))


;; P50 (***) Huffman code.

;; First of all, consult a good book on discrete mathematics or
;; algorithms for a detailed description of Huffman codes!

;; We suppose a set of symbols with their frequencies, given as a list
;; of fr(S,F) terms. Example:
;; [fr(a,45),fr(b,13),fr(c,12),fr(d,16),fr(e,9),fr(f,5)]. Our
;; objective is to construct a list hc(S,C) terms, where C is the
;; Huffman code word for the symbol S. In our example, the result
;; could be Hs = [hc(a,'0'), hc(b,'101'), hc(c,'100'), hc(d,'111'),
;; hc(e,'1101'), hc(f,'1100')] [hc(a,'01'),...etc.]. The task shall be
;; performed by the predicate huffman/2 defined as follows:

;; % huffman(Fs,Hs) :- Hs is the Huffman code table for the frequency table Fs




;;;;;;;;;;;;;;;;;;
;; Binary Trees ;;
;;;;;;;;;;;;;;;;;;

(define (root tree)
  (car tree))

(define (left tree)
  (cadr tree))

(define (right tree)
  (caddr tree))

(define (leaf? node)
  (and (null? (right node))
       (null? (left node))))

;; draw trees
(require pict/tree-layout)

(define (tree->layout tree)
  (cond [(null? tree)
         '()]
        [(leaf? tree)
         (tree-layout)]
        [else (apply tree-layout
                     (filter (compose1 not null?)
                             (list
                              (tree->layout (left tree))
                              (tree->layout (right tree)))))]))

(define (show-tree tree)
  (naive-layered (tree->layout tree)))


;; A binary tree is either empty or it is composed of a root element
;; and two successors, which are binary trees themselves.

;; In Lisp we represent the empty tree by 'nil' and the non-empty tree
;; by the list (X L R), where X denotes the root node and L and R
;; denote the left and right subtree, respectively. The example tree
;; depicted opposite is therefore represented by the following list:

;; (a (b (d nil nil) (e nil nil)) (c nil (f (g nil nil) nil)))

;; Other examples are a binary tree that consists of a root node only:

;; (a nil nil) or an empty binary tree: nil.

;; You can check your predicates using these example trees. They are
;; given as test cases in p54.lisp.

;; P54A (*) Check whether a given term represents a binary tree

;; Write a predicate istree which returns true if and only if its
;; argument is a list representing a binary tree.

;; Example:
;; * (istree (a (b nil nil) nil))
;; T
;; * (istree (a (b nil nil)))
;; NIL

(define (binary-tree? tree)
  (and (list? tree)
       (or (null? tree)
           (and (= 3 (length tree))
                (binary-tree? (left tree))
                (binary-tree? (right tree))))))

(check-true (binary-tree? '(a () ())))
(check-true (binary-tree? '()))
(check-false (binary-tree? '(a c ())))
(check-false (binary-tree? '(a)))
(check-false (binary-tree? '(a ())))
(check-false (binary-tree? '(a (b) (c))))
(check-false (binary-tree? '(a (b () (())))))


;; P55 (**) Construct completely balanced binary trees

;; In a completely balanced binary tree, the following property holds
;; for every node: The number of nodes in its left subtree and the
;; number of nodes in its right subtree are almost equal, which means
;; their difference is not greater than one.

;; Write a function cbal-tree to construct completely balanced binary
;; trees for a given number of nodes. The predicate should generate
;; all solutions via backtracking. Put the letter 'x' as information
;; into all nodes of the tree.

;; Example:
;; * cbal-tree(4,T).
;; T = t(x, t(x, nil, nil), t(x, nil, t(x, nil, nil))) ;
;; T = t(x, t(x, nil, nil), t(x, t(x, nil, nil), nil)) ;
;; etc......No

(define (product a b)
  (if (null? a)
      '()
      (append
       (map (lambda (b) (cons (car a) b)) b)
       (product (cdr a) b))))

(check-equal? (product '(1 2 3) '(4 5 6))
              '((1 . 4) (1 . 5) (1 . 6)
                (2 . 4) (2 . 5) (2 . 6)
                (3 . 4) (3 . 5) (3 . 6)))

(define (count-nodes tree)
  (if (null? tree)
      0
      (+ 1
         (count-nodes (left tree))
         (count-nodes (right tree)))))

(check-equal? (count-nodes '(x () ()))
              1)
(check-equal? (count-nodes '())
              0)
(check-equal? (count-nodes '(x (x () ()) ()))
              2)

(define (cbal-tree? tree)
  (let ([c1 (count-nodes (left tree))]
        [c2 (count-nodes (right tree))])
    (or (= c1 c2)
        (= (add1 c1) c2)
        (= c1 (add1 c2)))))

(define (make-tree val left right)
  (list val left right))

(define (generate-cbal-trees n)
  (cond [(<= n 0) '(())]
        [(odd? n)
         (let ([h (generate-cbal-trees (half (sub1 n)))])
           (map (lambda (t) (make-tree 'x (car t) (cdr t))) (product h h)))]
        [else
         (let ([l (generate-cbal-trees (half (sub1 n)))]
               [r (generate-cbal-trees (add1 (half (sub1 n))))])
           (append (map (lambda (t) (make-tree 'x (car t) (cdr t))) (product l r))
                   (map (lambda (t) (make-tree 'x (car t) (cdr t))) (product r l))))]))

(map show-tree (generate-cbal-trees 6))
(check-equal? (generate-cbal-trees 3)
              '((x (x () ()) (x () ()))))
(check-true (andmap cbal-tree? (generate-cbal-trees 20)))


;; P56 (**) Symmetric binary trees

;; Let us call a binary tree symmetric if you can draw a vertical line
;; through the root node and then the right subtree is the mirror
;; image of the left subtree. Write a predicate symmetric/1 to check
;; whether a given binary tree is symmetric. Hint: Write a predicate
;; mirror/2 first to check whether one tree is the mirror image of
;; another. We are only interested in the structure, not in the
;; contents of the nodes.

(define (mirror? lt rt)
  (or (and (null? lt) (null? rt))
      (and (not (null? lt))
           (not (null? rt))
           (eq? (root lt) (root rt))
           (mirror? (left lt) (right rt))
           (mirror? (right lt) (left rt)))))

(define (symmetric? tree)
  (or (null? tree)
      (mirror? (left tree) (right tree))))

(check-true (symmetric? '()))
(check-true (symmetric? '(X () ())))
(check-true (symmetric? '(x (x () ()) (x () ()))))
(check-true (symmetric? '(x (x (x () ()) ()) (x () (x () ())))))
(check-false (symmetric? '(x (x () (x () ())) (x () (x () ())))))


;; P57 (**) Binary search trees (dictionaries)

;; Use the predicate add/3, developed in chapter 4 of the course, to
;; write a predicate to construct a binary search tree from a list of
;; integer numbers.

;; Example:
;; * construct([3,2,5,7,1],T).
;; T = t(3, t(2, t(1, nil, nil), nil), t(5, nil, t(7, nil, nil)))

;; Then use this predicate to test the solution of the problem P56.
;; Example:
;; * test-symmetric([5,3,18,1,4,12,21]).
;; Yes
;; * test-symmetric([3,2,5,7,1]).
;; No

(define (insert-bst val tree)
  (cond [(null? tree)
         (make-tree val '() '())]
        [(< val (root tree))
         (make-tree (root tree)
                    (insert-bst val (left tree))
                    (right tree))]
        [else
         (make-tree (root tree)
                    (left tree)
                    (insert-bst val (right tree)))]))

(define (make-bst lista)
  (foldl insert-bst '() lista))

(check-equal? (insert-bst 9 (insert-bst 1 '()))
              '(1 () (9 () ())))
(check-equal? (make-bst '(2 1 4 5 9))
              '(2 (1 () ()) (4 () (5 () (9 () ())))))


;; P58 (**) Generate-and-test paradigm

;; Apply the generate-and-test paradigm to construct all symmetric,
;; completely balanced binary trees with a given number of
;; nodes. Example:

;; * sym-cbal-trees(5,Ts).

;; Ts = [t(x, t(x, nil, t(x, nil, nil)), t(x, t(x, nil, nil), nil)), t(x, t(x, t(x, nil, nil), nil), t(x, nil, t(x, nil, nil)))]

;; How many such trees are there with 57 nodes? Investigate about how
;; many solutions there are for a given number of nodes? What if the
;; number is even? Write an appropriate predicate.

(define (sym-cbal-trees n)
  (filter symmetric? (generate-cbal-trees n)))

(check-equal? (sym-cbal-trees 8)
              '())
(check-equal? (sym-cbal-trees 1)
              '((x () ())))
(check-equal? (length (sym-cbal-trees 57))
              256)


;; P59 (**) Construct height-balanced binary trees

;; In a height-balanced binary tree, the following property holds for
;; every node: The height of its left subtree and the height of its
;; right subtree are almost equal, which means their difference is not
;; greater than one.

;; Write a predicate hbal-tree/2 to construct height-balanced binary
;; trees for a given height. The predicate should generate all
;; solutions via backtracking. Put the letter 'x' as information into
;; all nodes of the tree.

;; Example:
;; * hbal-tree(3,T).
;; T = t(x, t(x, t(x, nil, nil), t(x, nil, nil)), t(x, t(x, nil, nil), t(x, nil, nil))) ;
;; T = t(x, t(x, t(x, nil, nil), t(x, nil, nil)), t(x, t(x, nil, nil), nil)) ;
;; etc......No

(define (tree-height tree)
  (if (null? tree)
      0
      (add1 (max (tree-height (left tree))
                 (tree-height (right tree))))))

(check-equal? (tree-height '())
              0)
(check-equal? (tree-height '(x () ()))
              1)
(check-equal? (tree-height '(x () (x () ())))
              2)

(define (balance? t1 t2)
  (let ([h1 (tree-height t1)]
        [h2 (tree-height t2)])
    (or (= h1 h2)
        (= (add1 h1) h2)
        (= h1 (add1 h2)))))

(check-true (balance? '(x () ()) '()))

(define (hbal-tree-1 n)
  (cond [(<= n 0) '(())]
        [(= 1 n) (list (make-tree 'x '() '()))]
        [else
         (let ([n1 (hbal-tree-1 (sub1 n))]
               [n2 (hbal-tree-1 (- n 2))])
           (map (lambda (c) (make-tree 'x (car c) (cdr c)))
                (append (product n1 n2)
                        (product n2 n1)
                        (product n1 n1))))]))

(define (hbal-tree-2 n)
  (define (recur count n1 n2)
    (if (zero? count)
        n2
        (recur (sub1 count)
               (map (lambda (c) (make-tree 'x (car c) (cdr c)))
                    (append (product n1 n2)
                            (product n2 n1)
                            (product n1 n1)))
               n1)))
  (recur n (list (make-tree 'x '() '())) (list '())))

(check-true (andmap (lambda (t) (balance? (left t) (right t))) (hbal-tree-2 3)))
(check-true (andmap (lambda (t) (balance? (left t) (right t))) (hbal-tree-1 3)))


;; P60 (**) Construct height-balanced binary trees with a given number of nodes

;; Consider a height-balanced binary tree of height H. What is the
;; maximum number of nodes it can contain?

;; Clearly, MaxN = 2**H - 1. However, what is the minimum number MinN?
;; This question is more difficult. Try to find a recursive statement
;; and turn it into a predicate minNodes/2 defined as follwos:

;; % minNodes(H,N) :- N is the minimum number of nodes in a
;; % height-balanced binary tree of height H.

;; (integer,integer), (+,?)

;; On the other hand, we might ask: what is the maximum height H a
;; height-balanced binary tree with N nodes can have?

;; % maxHeight(N,H) :- H is the maximum height of a height-balanced
;; % binary tree with N nodes

;; (integer,integer), (+,?)

;; Now, we can attack the main problem: construct all the
;; height-balanced binary trees with a given nuber of nodes.

;; % hbal-tree-nodes(N,T) :- T is a height-balanced binary tree with N nodes.

;; Find out how many height-balanced trees exist for N = 15.




;; ;; P61 (*) Count the leaves of a binary tree
;; ;; A leaf is a node with no successors. Write a predicate count-leaves/2 to count them.

;; ;; % count-leaves(T,N) :- the binary tree T has N leaves

(define (count-leaves tree)
  (cond [(null? tree) 0]
        [(leaf? tree) 1]
        [else (+ (count-leaves (left tree))
                 (count-leaves (right tree)))]))

(check-equal? (count-leaves '()) 0)
(check-equal? (count-leaves '(x () ())) 1)
(check-equal? (count-leaves '(x (x () ()) (x () (x () ())))) 2)


;; P61A (*) Collect the leaves of a binary tree in a list

;; A leaf is a node with no successors. Write a predicate leaves/2 to
;; collect them in a list.

;; % leaves(T,S) :- S is the list of all leaves of the binary tree T

(define (leaves tree)
  (cond [(null? tree) '()]
        [(leaf? tree) (list (root tree))]
        [else (append (leaves (left tree))
                      (leaves (right tree)))]))


;; ;; P62 (*) Collect the internal nodes of a binary tree in a list

;; ;; An internal node of a binary tree has either one or two non-empty
;; ;; successors. Write a predicate internals/2 to collect them in a
;; ;; list.

;; ;; % internals(T,S) :- S is the list of internal nodes of the binary tree T.

(define (internals tree)
  (cond [(null? tree) '()]
        [(leaf? tree) '()]
        [else
         (cons (root tree)
               (append (internals (left tree))
                       (internals (right tree))))]))

(check-equal? (internals '(x () ()))
              '())
(check-equal? (internals '(a () (b () ())))
              '(a))


;; P62B (*) Collect the nodes at a given level in a list

;; A node of a binary tree is at level N if the path from the root to
;; the node has length N-1. The root node is at level 1. Write a
;; predicate atlevel/3 to collect all nodes at a given level in a
;; list.

;; % atlevel(T,L,S) :- S is the list of nodes of the binary tree T at level L

;; Using atlevel/3 it is easy to construct a predicate levelorder/2
;; which creates the level-order sequence of the nodes. However, there
;; are more efficient ways to do that.

(define (children parent)
  (if (null? parent)
      '()
      (filter pair? (cdr parent))))

(check-equal? (children '(x () ()))
              '())
(check-equal? (children '(x () (x () ()) () (x () ())))
              '((x () ()) (x () ())))

(define (maplevel proc tree)
  (define (mapdown parents depth)
    (if (null? parents)
        '()
        (cons (proc parents depth)
              (mapdown (mapconcat children parents)
                       (add1 depth)))))
  (if (null? tree)
      '()
      (mapdown (list tree) 0)))

(check-equal? (maplevel (lambda args (car args))'())
              '())
(check-equal? (maplevel (lambda args (car args)) '(x () ()))
              '(((x () ()))))
(check-equal? (maplevel (lambda args (car args)) '(x (x () (x () ())) (x () ())))
              '(((x (x () (x () ())) (x () ())))
                ((x () (x () ())) (x () ()))
                ((x () ()))))

(define (atlevel tree level)
  (define (recur nodes count)
    (if (or (null? nodes) (zero? count))
        (map root nodes)
        (recur (mapconcat children nodes)
               (sub1 count))))
  (if (null? tree)
      '()
      (recur (list tree) level)))

(check-equal? (atlevel '() 10)
              '())
(check-equal? (atlevel '(x () ()) 1)
              '())
(check-equal? (atlevel '(x () ()) 0)
              '(x))
(check-equal? (atlevel '(x () (b () ())) 1)
              '(b))
(check-equal? (atlevel '(x (c () ()) (b () ())) 1)
              '(c b))


;; P63 (**) Construct a complete binary tree

;; A complete binary tree with height H is defined as follows: The
;; levels 1,2,3,...,H-1 contain the maximum number of nodes (i.e
;; 2**(i-1) at the level i, note that we start counting the levels
;; from 1 at the root). In level H, which may contain less than the
;; maximum possible number of nodes, all the nodes are
;; "left-adjusted". This means that in a levelorder tree traversal all
;; internal nodes come first, the leaves come second, and empty
;; successors (the nil's which are not really nodes!) come last.

;; Particularly, complete binary trees are used as data structures (or
;; addressing schemes) for heaps.

;; We can assign an address number to each node in a complete binary
;; tree by enumerating the nodes in levelorder, starting at the root
;; with number 1. In doing so, we realize that for every node X with
;; address A the following property holds: The address of X's left and
;; right successors are 2*A and 2*A+1, respectively, supposed the
;; successors do exist. This fact can be used to elegantly construct a
;; complete binary tree structure. Write a predicate
;; complete-binary-tree/2 with the following specification:

;; % complete-binary-tree(N,T) :- T is a complete binary tree with N nodes. (+,?)

;; Test your predicate in an appropriate way.

(define (complete-binary-tree n)
  (define (connect n children)
    (cond [(zero? n)
           '()]
          [(null? children)
           (list* '() '()
                  (connect (sub1 n) children))]
          [(null? (cdr children))
           (cons (make-tree 'x (car children) '())
                 (connect (sub1 n) (cdr children)))]
          [else
           (cons (make-tree 'x (car children) (cadr children))
                 (connect (sub1 n) (cddr children)))]))

  (define (subtrees count parents)
    (if (<= parents 0)
        '()
        (connect parents
                 (subtrees (- count parents)
                           (min count (* 2 parents))))))
  (car (subtrees n 1)))

(check-equal? (complete-binary-tree 0)
              '())
(check-equal? (complete-binary-tree 1)
              '(x () ()))
(check-equal? (complete-binary-tree 3)
              '(x (x () ()) (x () ())))
(check-equal? (complete-binary-tree 4)
              '(x (x (x () ()) ()) (x () ())))


;; P64 (**) Layout a binary tree (1)

;; Given a binary tree as the usual Prolog term t(X,L,R) (or nil). As
;; a preparation for drawing the tree, a layout algorithm is required
;; to determine the position of each node in a rectangular
;; grid. Several layout methods are conceivable, one of them is shown
;; in the illustration below.

;; In this layout strategy, the position of a node v is obtained by
;; the following two rules:

;; x(v) is equal to the position of the node v in the inorder sequence
;; y(v) is equal to the depth of the node v in the tree


;; In order to store the position of the nodes, we extend the Prolog
;; term representing a node (and its successors) as follows:

;; % nil represents the empty tree (as usual)

;; % t(W,X,Y,L,R) represents a (non-empty) binary tree with root W
;; % "positioned" at (X,Y), and subtrees L and R

;; Write a predicate layout-binary-tree/2 with the following specification:

;; % layout-binary-tree(T,PT) :- PT is the "positioned" binary tree
;; % obtained from the binary tree T. (+,?)

;; Test your predicate in an appropriate way.

(define (vlayout tree [y 0])
  (if (null? tree)
      '()
      (make-tree y
                 (vlayout (left tree) (add1 y))
                 (vlayout (right tree) (add1 y)))))

(define (hlayout-1 tree [leftmost 0])
  (define (layout-from-left tree leftmost)
    (if (null? tree)
        (cons leftmost '())
        (let* ([ll (layout-from-left (left tree) leftmost)]
               [root (if (null? (cdr ll)) leftmost (add1 (car ll)))]
               [rl (layout-from-left (right tree) (add1 root))])
          (cons (if (null? (cdr rl)) root (car rl))
                (make-tree root (cdr ll) (cdr rl))))))
  (if (null? tree)
      '()
      (let ([root (+ leftmost (count-nodes (left tree)))])
        (make-tree root
                   (cdr (layout-from-left (left tree) leftmost))
                   (cdr (layout-from-left (right tree) (add1 root)))))))

(check-equal? (hlayout-1 '(x (x () (x () ())) (x () ())))
              '(2 (0 () (1 () ())) (3 () ())))
(check-equal? (hlayout-1 '(x () ()))
              '(0 () ()))
(check-equal? (hlayout-1 '(x () (x () ())) 12)
              '(12 () (13 () ())))

(define (layout-binary-tree tree hlayout vlayout)
  (define (make-layout tree hl vl)
    (if (null? tree)
        '()
        (list (car tree) (car hl) (car vl)
              (make-layout (left tree) (left hl) (left vl))
              (make-layout (right tree) (right hl) (right vl)))))
  (make-layout tree (hlayout tree) (vlayout tree)))

(define (layout-binary-tree-1 tree)
  (layout-binary-tree tree hlayout-1 vlayout))

(check-equal? (layout-binary-tree-1 '())
              '())
(check-equal? (layout-binary-tree-1 '(x () ()))
              '(x 0 0 () ()))
(check-equal? (layout-binary-tree-1 '(x (x () ()) ()))
              '(x 1 0 (x 0 1 () ()) ()))


;; P65 (**) Layout a binary tree (2)

;; An alternative layout method is depicted in the illustration
;; opposite. Find out the rules and write the corresponding Prolog
;; predicate. Hint: On a given level, the horizontal distance between
;; neighboring nodes is constant.

;; Use the same conventions as in problem P64 and test your predicate
;; in an appropriate way.

(define (horizontally-shift tree offset)
  (if (null? tree)
      '()
      (make-tree (+ offset (root tree))
                 (horizontally-shift (left tree) offset)
                 (horizontally-shift (right tree) offset))))

(define (hlayout-2 tree)
  (define (hlayout tree root height)
    (if (null? tree)
        '()
        (let ([width (floor (expt 2 (- height 2)))]
              [next-height (sub1 height)])
          (make-tree root
                     (hlayout (left tree) (- root width) next-height)
                     (hlayout (right tree) (+ root width) next-height)))))

  (define (leftmost tree)
    (cond [(null? tree) '()]
          [(null? (left tree)) tree]
          [else (leftmost (left tree))]))

  (let* ([htree (hlayout tree 0 (tree-height tree))]
         [offset (- (if (null? htree) 0 (root (leftmost htree))))])
    (horizontally-shift htree offset)))

(define (layout-binary-tree-2 tree)
  (layout-binary-tree tree hlayout-2 vlayout))

(check-equal? (layout-binary-tree-2 '())
              '())
(check-equal? (layout-binary-tree-2 '(x () ()))
              '(x 0 0 () ()))
(check-equal? (layout-binary-tree-2 '(x (x () ()) ()))
              '(x 1 0 (x 0 1 () ()) ()))
(check-equal? (layout-binary-tree-2 '(x (x (x () ()) ()) ()))
              '(x 3 0 (x 1 1 (x 0 2 () ()) ()) ()))
(check-equal? (layout-binary-tree-2 '(x (x (x () ()) ()) (x () ())))
              '(x 3 0 (x 1 1 (x 0 2 () ()) ()) (x 5 1 () ())))
(check-equal? (layout-binary-tree-2 '(n (k (c (a () ()) (e (d () ()) (g () ()))) (m () ()))
                                        (u (p () (q () ())) ())))
              '(n 14 0
                  (k 6 1
                     (c 2 2
                        (a 0 3 () ())
                        (e 4 3
                           (d 3 4 () ())
                           (g 5 4 () ())))
                     (m 10 2 () ()))
                  (u 22 1
                     (p 18 2 () (q 20 3 () ()))
                     ())))


;; P66 (***) Layout a binary tree (3)

;; Yet another layout strategy is shown in the illustration
;; opposite. The method yields a very compact layout while maintaining
;; a certain symmetry in every node. Find out the rules and write the
;; corresponding Prolog predicate. Hint: Consider the horizontal
;; distance between a node and its successor nodes. How tight can you
;; pack together two subtrees to construct the combined binary tree?

;; Use the same conventions as in problem P64 and P65 and test your
;; predicate in an appropriate way. Note: This is a difficult
;; problem. Don't give up too early!






;; P67 (**) A string representation of binary trees

;; Somebody represents binary trees as strings of the following type
;; (see example opposite):

;; a(b(d,e),c(,f(g,)))

;; a) Write a Prolog predicate which generates this string
;; representation, if the tree is given as usual (as nil or t(X,L,R)
;; term). Then write a predicate which does this inverse; i.e. given
;; the string representation, construct the tree in the usual
;; form. Finally, combine the two predicates in a single predicate
;; tree-string/2 which can be used in both directions.

(define (tree->string tree)
  (cond [(null? tree) ""]
        [(leaf? tree)
         (symbol->string (root tree))]
        [else
         (string-append (symbol->string (root tree))
                        "("
                        (tree->string (left tree))
                        ","
                        (tree->string (right tree))
                        ")")]))

(check-equal? (tree->string '())
              "")
(check-equal? (tree->string '(x () ()))
              "x")
(check-equal? (tree->string '(x (y () ()) ()))
              "x(y,)")
(check-equal? (tree->string '(x (y () ()) (j () ())))
              "x(y,j)")


;; b) Write the same predicate tree-string/2 using difference lists
;; and a single predicate tree-dlist/2 which does the conversion
;; between a tree and a difference list in both directions.

;; For simplicity, suppose the information in the nodes is a single
;; letter and there are no spaces in the string.

(define (char->symbol char)
  (string->symbol (make-string 1 char)))

(define (string->tree string)
  (define (eof index)
    (>= index (string-length string)))

  (define (expect index char)
    (and (not (eof index))
         (equal? (string-ref string index) char)))

  (define (parse index)
    (if (or (eof index)
            (expect index #\,)
            (expect index #\)))
        (cons index '())
        (let* ([root (char->symbol (string-ref string index))]
               [index (add1 index)]
               [open (expect index #\()]
               [x (if open
                      (parse (add1 index))
                      (cons index '()))]
               [y (if (and open (expect (car x) #\,))
                      (parse (add1 (car x)))
                      (cons (car x) '()))])
          (if open
              (if (expect (car y) #\))
                  (cons (add1 (car y))
                        (make-tree root (cdr x) (cdr y)))
                  (error "unbalanced parentheses"))
              (cons index
                    (make-tree root '() '()))))))

  (cdr (parse 0)))

(check-equal? (string->tree "")
              '())
(check-equal? (string->tree "a(b,c)")
              '(a (b () ()) (c () ())))
(check-equal? (string->tree "a(,c(d,))")
              '(a () (c (d () ()) ())))
(check-equal? (string->tree "a(,c())")
              '(a () (c () ())))
(check-equal? (string->tree "a(,c(,))")
              '(a () (c () ())))


;; P68 (**) Preorder and inorder sequences of binary trees

;; We consider binary trees with nodes that are identified by single
;; lower-case letters, as in the example of problem P67.

;; a) Write predicates preorder/2 and inorder/2 that construct the
;; preorder and inorder sequence of a given binary tree,
;; respectively. The results should be atoms, e.g. 'abdecfg' for the
;; preorder sequence of the example in problem P67.

(define (preorder tree)
  (if (null? tree)
      '()
      (append (list (root tree))
              (preorder (left tree))
              (preorder (right tree)))))

(check-equal? (preorder '(a (b (c () ()) (d (e () ()) ())) (f () (h () ()))))
              '(a b c d e f h))

(define (inorder tree)
  (if (null? tree)
      '()
      (append (inorder (left tree))
              (list (root tree))
              (inorder (right tree)))))

(check-equal? (inorder '())
              '())
(check-equal? (inorder '(4 (2 (1 () ()) (3 () ())) (8 () ())))
              '(1 2 3 4 8))


;; b) Can you use preorder/2 from problem part a) in the reverse
;; direction; i.e. given a preorder sequence, construct a
;; corresponding tree? If not, make the necessary arrangements.

(define (preorder-construct lista)
  (define (recur lista left right)
    (if (> left right)
        '()
        (let ([m (+ left (half (- right left)))])
          (make-tree
           (list-ref lista m)
           (recur lista left (sub1 m))
           (recur lista (add1 m) right)))))
  (recur lista 0 (sub1 (length lista))))

(check-equal? (preorder-construct '(1 2 3 4 45 5))
              '(3 (1 () (2 () ())) (45 (4 () ()) (5 () ()))))


;; c) If both the preorder sequence and the inorder sequence of the
;; nodes of a binary tree are given, then the tree is determined
;; unambiguously. Write a predicate pre-in-tree/3 that does the job.

(define (split-at a left right)
  (cond [(null? right)
         '()]
        [(equal? (car right) a)
         (cons left (cdr right))]
        [else
         (split-at a
                   (append left (list (car right)))
                   (cdr right))]))

(check-equal? (split-at 'a '() '(a b c d))
              (cons '() '(b c d)))
(check-equal? (split-at 'd '() '(a b c d))
              (cons '(a b c) '()))
(check-equal? (split-at 'e '() '(a b c d))
              '())

(define (pre-in-tree pre-list in-list)
  (cond [(and (null? pre-list) (null? in-list))
         '()]
        [(= (length pre-list) (length in-list))
         (let* ([root (car pre-list)]
                [in-left-right (split-at root '() in-list)]
                [pre-left-right (if (null? in-left-right)
                                    '()
                                    (split (cdr pre-list)
                                           (length (car in-left-right))))])
           (if (null? in-left-right)
               (error "two lists are not matched")
               (make-tree root
                          (pre-in-tree (car pre-left-right) (car in-left-right))
                          (pre-in-tree (cadr pre-left-right) (cdr in-left-right)))))]
        [else (error "two lists are not matched")]))

(check-equal? (pre-in-tree '(1 2 3) '(2 1 3))
              '(1 (2 () ()) (3 () ())))
(check-exn exn:fail?
           (lambda ()
             (pre-in-tree '(1) '())))
(check-exn exn:fail?
           (lambda ()
             (pre-in-tree '() '(1))))
(check-exn exn:fail?
           (lambda ()
             (pre-in-tree '(1 2 3) '(3 1 2))))


;; d) Solve problems a) to c) using difference lists. Cool! Use the
;; predefined predicate time/1 to compare the solutions.







;; What happens if the same character appears in more than one
;; node. Try for instance pre-in-tree(aba,baa,T).

;; P69 (**) Dotstring representation of binary trees

;; We consider again binary trees with nodes that are identified by
;; single lower-case letters, as in the example of problem P67. Such a
;; tree can be represented by the preorder sequence of its nodes in
;; which dots (.) are inserted where an empty subtree (nil) is
;; encountered during the tree traversal. For example, the tree shown
;; in problem P67 is represented as 'abd..e..c.fg...'. First, try to
;; establish a syntax (BNF or syntax diagrams) and then write a
;; predicate tree-dotstring/2 which does the conversion in both
;; directions. Use difference lists.

(define (count-tree tree)
  (if (null? tree)
      1
      (+ 1
         (count-tree (left tree))
         (count-tree (right tree)))))

(check-equal? (count-tree '(1 () ())) 3)

(define (dotstring->tree dots)
  (cond [(null? dots) '()]
        [(null? (car dots)) '()]
        [else
         (let* ([left (dotstring->tree (cdr dots))]
                [remain (list-tail (cdr dots) (count-tree left))]
                [right (dotstring->tree remain)])
           (make-tree (car dots) left right))]))

(check-equal? (dotstring->tree '(a b d () () e () () c () f g () () ()))
              '(a (b (d () ()) (e () ())) (c () (f (g () ()) ()))))

(define (tree->dotstring tree)
  (if (null? tree)
      (list '())
      (append (list (root tree))
              (tree->dotstring (left tree))
              (tree->dotstring (right tree)))))

(check-equal? (dotstring->tree (tree->dotstring '()))
              '())
(check-equal? (dotstring->tree (tree->dotstring '(a (b (d () ()) (e () ())) (c () (f (g () ()) ())))))
              '(a (b (d () ()) (e () ())) (c () (f (g () ()) ()))))


;;;;;;;;;;;;;;;;;;;;
;; Multiway Trees ;;
;;;;;;;;;;;;;;;;;;;;

;; A multiway tree is composed of a root element and a (possibly
;; empty) set of successors which are multiway trees themselves. A
;; multiway tree is never empty. The set of successor trees is
;; sometimes called a forest.


;; In Prolog we represent a multiway tree by a term t(X,F), where X
;; denotes the root node and F denotes the forest of successor trees
;; (a Prolog list). The example tree depicted opposite is therefore
;; represented by the following Prolog term:

;; T = t(a,[t(f,[t(g,[])]),t(c,[]),t(b,[t(d,[]),t(e,[])])])

;; P70B (*) Check whether a given term represents a multiway tree

;; Write a predicate istree/1 which succeeds if and only if its
;; argument is a Prolog term representing a multiway tree.

;; Example:
;; * istree(t(a,[t(f,[t(g,[])]),t(c,[]),t(b,[t(d,[]),t(e,[])])])).
;; Yes

(define (mtree? mtree)
  (or (symbol? mtree)
      (and (not (null? mtree))
           (list? mtree))))


;; P70C (*) Count the nodes of a multiway tree
;; Write a predicate nnodes/1 which counts the nodes of a given multiway tree.
;; Example:
;; * nnodes(t(a,[t(f,[])]),N).
;; N = 2

;; Write another version of the predicate that allows for a flow pattern (o,i).

(define (mchildren parent)
  (if (or (null? parent)
          (symbol? parent))
      '()
      (cdr parent)))

(define (count-mnodes mtree)
  (add1
   (if (symbol? mtree)
       0
       (sum (map count-mnodes (mchildren mtree))))))


(check-equal? (count-mnodes '(x))
              1)
(check-equal? (count-mnodes '(x y z))
              3)
(check-equal? (count-mnodes '(x (x (x))))
              3)
(check-equal? (count-mnodes '(x (x (x y z))))
              5)


;; P70 (**) Tree construction from a node string

;; We suppose that the nodes of a multiway tree contain single
;; characters. In the depth-first order sequence of its nodes, a
;; special character ^ has been inserted whenever, during the tree
;; traversal, the move is a backtrack to the previous level.

;; By this rule, the tree in the figure opposite is represented as: afg^^c^bd^e^^^

;; Define the syntax of the string and write a predicate
;; tree(String,Tree) to construct the Tree when the String is
;; given. Work with atoms (instead of strings). Make your predicate
;; work in both directions.







;; P71 (*) Determine the internal path length of a tree

;; We define the internal path length of a multiway tree as the total
;; sum of the path lengths from the root to all nodes of the tree. By
;; this definition, the tree in the figure of problem P70 has an
;; internal path length of 9. Write a predicate ipl(Tree,IPL) for the
;; flow pattern (+,-).







;; P72 (*) Construct the bottom-up order sequence of the tree nodes

;; Write a predicate bottom-up(Tree,Seq) which constructs the
;; bottom-up sequence of the nodes of the multiway tree Tree. Seq
;; should be a Prolog list. What happens if you run your predicate
;; backwords?









;; P73 (**) Lisp-like tree representation

;; There is a particular notation for multiway trees in Lisp. Lisp is
;; a prominent functional programming language, which is used
;; primarily for artificial intelligence problems. As such it is one
;; of the main competitors of Prolog. In Lisp almost everything is a
;; list, just as in Prolog everything is a term.

;; The following pictures show how multiway tree structures are represented in Lisp.

;; Note that in the "lispy" notation a node with successors (children)
;; in the tree is always the first element in a list, followed by its
;; children. The "lispy" representation of a multiway tree is a
;; sequence of atoms and parentheses '(' and ')', which we shall
;; collectively call "tokens". We can represent this sequence of
;; tokens as a Prolog list; e.g. the lispy expression (a (b c)) could
;; be represented as the Prolog list ['(', a, '(', b, c, ')',
;; ')']. Write a predicate tree-ltl(T,LTL) which constructs the "lispy
;; token list" LTL if the tree is given as term T in the usual Prolog
;; notation.

;; Example:
;; * tree-ltl(t(a,[t(b,[]),t(c,[])]),LTL).
;; LTL = ['(', a, '(', b, c, ')', ')']

;; As a second, even more interesting exercise try to rewrite
;; tree-ltl/2 in a way that the inverse conversion is also possible:
;; Given the list LTL, construct the Prolog tree T. Use difference
;; lists.








;;;;;;;;;;;;
;; Graphs ;;
;;;;;;;;;;;;

;; A graph is defined as a set of nodes and a set of edges, where each
;; edge is a pair of nodes.

;; There are several ways to represent graphs in Prolog. One method is
;; to represent each edge separately as one clause (fact). In this
;; form, the graph depicted below is represented as the following
;; predicate:

;; edge(h,g).
;; edge(k,f).
;; edge(f,b).
;; ...

;; We call this edge-clause form. Obviously, isolated nodes cannot be
;; represented. Another method is to represent the whole graph as one
;; data object. According to the definition of the graph as a pair of
;; two sets (nodes and edges), we may use the following Prolog term to
;; represent the example graph:

;; graph([b,c,d,f,g,h,k],[e(b,c),e(b,f),e(c,f),e(f,k),e(g,h)])

;; We call this graph-term form. Note, that the lists are kept sorted,
;; they are really sets, without duplicated elements. Each edge
;; appears only once in the edge list; i.e. an edge from a node x to
;; another node y is represented as e(x,y), the term e(y,x) is not
;; present. The graph-term form is our default representation. In
;; SWI-Prolog there are predefined predicates to work with sets.

;; A third representation method is to associate with each node the
;; set of nodes that are adjacent to that node. We call this the
;; adjacency-list form. In our example:

;; [n(b,[c,f]), n(c,[b,f]), n(d,[]), n(f,[b,c,k]), ...]

;; The representations we introduced so far are Prolog terms and
;; therefore well suited for automated processing, but their syntax is
;; not very user-friendly. Typing the terms by hand is cumbersome and
;; error-prone. We can define a more compact and "human-friendly"
;; notation as follows: A graph is represented by a list of atoms and
;; terms of the type X-Y (i.e. functor '-' and arity 2). The atoms
;; stand for isolated nodes, the X-Y terms describe edges. If an X
;; appears as an endpoint of an edge, it is automatically defined as a
;; node. Our example could be written as:

;; [b-c, f-c, g-h, d, f-b, k-f, h-g]

;; We call this the human-friendly form. As the example shows, the
;; list does not have to be sorted and may even contain the same edge
;; multiple times. Notice the isolated node d. (Actually, isolated
;; nodes do not even have to be atoms in the Prolog sense, they can be
;; compound terms, as in d(3.75,blue) instead of d in the example).

;; When the edges are directed we call them arcs. These are
;; represented by ordered pairs. Such a graph is called directed
;; graph. To represent a directed graph, the forms discussed above are
;; slightly modified. The example graph opposite is represented as
;; follows:

;; Arc-clause form
;; arc(s,u).
;; arc(u,r).
;; ...
;; Graph-term form
;; digraph([r,s,t,u,v],[a(s,r),a(s,u),a(u,r),a(u,s),a(v,u)])
;; Adjacency-list form
;; [n(r,[]),n(s,[r,u]),n(t,[]),n(u,[r]),n(v,[u])]

;; Note that the adjacency-list does not have the information on
;; whether it is a graph or a digraph.

;; Human-friendly form
;; [s > r, t, u > r, s > u, u > s, v > u]

;; Finally, graphs and digraphs may have additional information
;; attached to nodes and edges (arcs). For the nodes, this is no
;; problem, as we can easily replace the single character identifiers
;; with arbitrary compound terms, such as city('London',4711). On the
;; other hand, for edges we have to extend our notation. Graphs with
;; additional information attached to edges are called labelled
;; graphs.


;; Arc-clause form
;; arc(m,q,7).
;; arc(p,q,9).
;; arc(p,m,5).
;; Graph-term form
;; digraph([k,m,p,q],[a(m,p,7),a(p,m,5),a(p,q,9)])
;; Adjacency-list form
;; [n(k,[]),n(m,[q/7]),n(p,[m/5,q/9]),n(q,[])]

;; Notice how the edge information has been packed into a term with
;; functor '/' and arity 2, together with the corresponding node.

;; Human-friendly form
;; [p>q/9, m>q/7, k, p>m/5]

;; The notation for labelled graphs can also be used for so-called
;; multi-graphs, where more than one edge (or arc) are allowed between
;; two given nodes.









;; P80 (***) Conversions

;; Write predicates to convert between the different graph
;; representations. With these predicates, all representations are
;; equivalent; i.e. for the following problems you can always pick
;; freely the most convenient form. The reason this problem is rated
;; (***) is not because it's particularly difficult, but because it's
;; a lot of work to deal with all the special cases.








;; P81 (**) Path from one node to another one

;; Write a predicate path(G,A,B,P) to find an acyclic path P from node
;; A to node b in the graph G. The predicate should return all paths
;; via backtracking.






;; P82 (*) Cycle from a given node

;; Write a predicate cycle(G,A,P) to find a closed path (cycle) P
;; starting at a given node A in the graph G. The predicate should
;; return all cycles via backtracking.









;; P83 (**) Construct all spanning trees

;; Write a predicate s-tree(Graph,Tree) to construct (by backtracking)
;; all spanning trees of a given graph. With this predicate, find out
;; how many spanning trees there are for the graph depicted to the
;; left. The data of this example graph can be found in the file
;; p83.dat. When you have a correct solution for the s-tree/2
;; predicate, use it to define two other useful predicates:
;; is-tree(Graph) and is-connected(Graph). Both are five-minutes
;; tasks!











;; P84 (**) Construct the minimal spanning tree

;; Write a predicate ms-tree(Graph,Tree,Sum) to construct the minimal
;; spanning tree of a given labelled graph. Hint: Use the algorithm of
;; Prim. A small modification of the solution of P83 does the
;; trick. The data of the example graph to the right can be found in
;; the file p84.dat.









;; P85 (**) Graph isomorphism

;; Two graphs G1(N1,E1) and G2(N2,E2) are isomorphic if there is a
;; bijection f: N1 -> N2 such that for any nodes X,Y of N1, X and Y
;; are adjacent if and only if f(X) and f(Y) are adjacent.

;; Write a predicate that determines whether two graphs are
;; isomorphic. Hint: Use an open-ended list to represent the function
;; f.









;; P86 (**) Node degree and graph coloration

;; a) Write a predicate degree(Graph,Node,Deg) that determines the
;; degree of a given node.

;; b) Write a predicate that generates a list of all nodes of a graph
;; sorted according to decreasing degree.

;; c) Use Welch-Powell's algorithm to paint the nodes of a graph in
;; such a way that adjacent nodes have different colors.








;; P87 (**) Depth-first order graph traversal (alternative solution)

;; Write a predicate that generates a depth-first order graph
;; traversal sequence. The starting point should be specified, and the
;; output should be a list of nodes that are reachable from this
;; starting point (in depth-first order).





;; P88 (**) Connected components (alternative solution)
;; Write a predicate that splits a graph into its connected components.
;; P89 (**) Bipartite graphs
;; Write a predicate that finds out whether a given graph is bipartite.









;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous Problems ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; P90 (**) Eight queens problem

;; This is a classical problem in computer science. The objective is
;; to place eight queens on a chessboard so that no two queens are
;; attacking each other; i.e., no two queens are in the same row, the
;; same column, or on the same diagonal.

;; Hint: Represent the positions of the queens as a list of numbers
;; 1..N. Example: [4,2,7,3,6,8,5,1] means that the queen in the first
;; column is in row 4, the queen in the second column is in row 2,
;; etc. Use the generate-and-test paradigm.










;; P91 (**) Knight's tour

;; Another famous problem is this one: How can a knight jump on an NxN
;; chessboard in such a way that it visits every square exactly once?

;; Hints: Represent the squares by pairs of their coordinates of the
;; form X/Y, where both X and Y are integers between 1 and N. (Note
;; that '/' is just a convenient functor, not division!) Define the
;; relation jump(N,X/Y,U/V) to express the fact that a knight can jump
;; from X/Y to U/V on a NxN chessboard. And finally, represent the
;; solution of our problem as a list of N*N knight positions (the
;; knight's tour).







;; P92 (***) Von Koch's conjecture

;; Several years ago I met a mathematician who was intrigued by a
;; problem for which he didn't know a solution. His name was Von Koch,
;; and I don't know whether the problem has been solved since.

;; Anyway the puzzle goes like this: Given a tree with N nodes (and
;; hence N-1 edges). Find a way to enumerate the nodes from 1 to N
;; and, accordingly, the edges from 1 to N-1 in such a way, that for
;; each edge K the difference of its node numbers equals to K. The
;; conjecture is that this is always possible.

;; For small trees the problem is easy to solve by hand. However, for
;; larger trees, and 14 is already very large, it is extremely
;; difficult to find a solution. And remember, we don't know for sure
;; whether there is always a solution!

;; Write a predicate that calculates a numbering scheme for a given
;; tree. What is the solution for the larger tree pictured above?












;; P93 (***) An arithmetic puzzle

;; Given a list of integer numbers, find a correct way of inserting
;; arithmetic signs (operators) such that the result is a correct
;; equation. Example: With the list of numbers [2,3,5,7,11] we can
;; form the equations 2-3+5+7 = 11 or 2 = (3*5+7)/11 (and ten
;; others!).









;; P94 (***) Generate K-regular simple graphs with N nodes

;; In a K-regular graph all nodes have a degree of K; i.e. the number
;; of edges incident in each node is K. How many (non-isomorphic!)
;; 3-regular graphs with 6 nodes are there? See also a table of
;; results and a Java applet that can represent graphs geometrically.








;; P95 (**) English number words

;; On financial documents, like cheques, numbers must sometimes be
;; written in full words. Example: 175 must be written as
;; one-seven-five. Write a predicate full-words/1 to print
;; (non-negative) integer numbers in full words.









;; P96 (**) Syntax checker (alternative solution with difference lists)

;; In a certain programming language (Ada) identifiers are defined by
;; the syntax diagram (railroad chart) opposite. Transform the syntax
;; diagram into a system of syntax diagrams which do not contain
;; loops; i.e. which are purely recursive. Using these modified
;; diagrams, write a predicate identifier/1 that can check whether or
;; not a given string is a legal identifier.

;; % identifier(Str) :- Str is a legal identifier








;; P97 (**) Sudoku
;; Sudoku puzzles go like this:
;;    Problem statement                 Solution

;;     .  .  4 | 8  .  . | .  1  7      9  3  4 | 8  2  5 | 6  1  7
;;             |         |                      |         |
;;     6  7  . | 9  .  . | .  .  .      6  7  2 | 9  1  4 | 8  5  3
;;             |         |                      |         |
;;     5  .  8 | .  3  . | .  .  4      5  1  8 | 6  3  7 | 9  2  4
;;     --------+---------+--------      --------+---------+--------
;;     3  .  . | 7  4  . | 1  .  .      3  2  5 | 7  4  8 | 1  6  9
;;             |         |                      |         |
;;     .  6  9 | .  .  . | 7  8  .      4  6  9 | 1  5  3 | 7  8  2
;;             |         |                      |         |
;;     .  .  1 | .  6  9 | .  .  5      7  8  1 | 2  6  9 | 4  3  5
;;     --------+---------+--------      --------+---------+--------
;;     1  .  . | .  8  . | 3  .  6      1  9  7 | 5  8  2 | 3  4  6
;;             |         |                      |         |
;;     .  .  . | .  .  6 | .  9  1      8  5  3 | 4  7  6 | 2  9  1
;;             |         |                      |         |
;;     2  4  . | .  .  1 | 5  .  .      2  4  6 | 3  9  1 | 5  7  8

;; Every spot in the puzzle belongs to a (horizontal) row and a
;; (vertical) column, as well as to one single 3x3 square (which we
;; call "square" for short). At the beginning, some of the spots carry
;; a single-digit number between 1 and 9. The problem is to fill the
;; missing spots with digits in such a way that every number between 1
;; and 9 appears exactly once in each row, in each column, and in each
;; square.













;; P98 (***) Nonograms

;; Around 1994, a certain kind of puzzles was very popular in
;; England. The "Sunday Telegraph" newspaper wrote: "Nonograms are
;; puzzles from Japan and are currently published each week only in
;; The Sunday Telegraph. Simply use your logic and skill to complete
;; the grid and reveal a picture or diagram." As a Prolog programmer,
;; you are in a better situation: you can have your computer do the
;; work! Just write a little program ;-).

;; The puzzle goes like this: Essentially, each row and column of a
;; rectangular bitmap is annotated with the respective lengths of its
;; distinct strings of occupied cells. The person who solves the
;; puzzle must complete the bitmap given only these lengths.

;;           Problem statement:          Solution:

;;           |_|_|_|_|_|_|_|_| 3         |_|X|X|X|_|_|_|_| 3
;;           |_|_|_|_|_|_|_|_| 2 1       |X|X|_|X|_|_|_|_| 2 1
;;           |_|_|_|_|_|_|_|_| 3 2       |_|X|X|X|_|_|X|X| 3 2
;;           |_|_|_|_|_|_|_|_| 2 2       |_|_|X|X|_|_|X|X| 2 2
;;           |_|_|_|_|_|_|_|_| 6         |_|_|X|X|X|X|X|X| 6
;;           |_|_|_|_|_|_|_|_| 1 5       |X|_|X|X|X|X|X|_| 1 5
;;           |_|_|_|_|_|_|_|_| 6         |X|X|X|X|X|X|_|_| 6
;;           |_|_|_|_|_|_|_|_| 1         |_|_|_|_|X|_|_|_| 1
;;           |_|_|_|_|_|_|_|_| 2         |_|_|_|X|X|_|_|_| 2
;;            1 3 1 7 5 3 4 3             1 3 1 7 5 3 4 3
;;            2 1 5 1                     2 1 5 1

;; For the example above, the problem can be stated as the two lists
;; [[3],[2,1],[3,2],[2,2],[6],[1,5],[6],[1],[2]] and
;; [[1,2],[3,1],[1,5],[7,1],[5],[3],[4],[3]] which give the "solid"
;; lengths of the rows and columns, top-to-bottom and left-to-right,
;; respectively. Published puzzles are larger than this example,
;; e.g. 25 x 20, and apparently always have unique solutions.












;; P99 (***) Crossword puzzle

;; Given an empty (or almost empty) framework of a crossword puzzle
;; and a set of words. The problem is to place the words into the
;; framework.

;; The particular crossword puzzle is specified in a text file which
;; first lists the words (one word per line) in an arbitrary
;; order. Then, after an empty line, the crossword framework is
;; defined. In this framework specification, an empty character
;; location is represented by a dot (.). In order to make the solution
;; easier, character locations can also contain predefined character
;; values. The puzzle opposite is defined in the file p99a.dat, other
;; examples are p99b.dat and p99d.dat. There is also an example of a
;; puzzle (p99c.dat) which does not have a solution.

;; Words are strings (character lists) of at least two characters. A
;; horizontal or vertical sequence of character places in the
;; crossword puzzle framework is called a site. Our problem is to find
;; a compatible way of placing words onto sites.

;; Hints: (1) The problem is not easy. You will need some time to
;; thoroughly understand it. So, don't give up too early! And remember
;; that the objective is a clean solution, not just a quick-and-dirty
;; hack!

;; (2) Reading the data file is a tricky problem for which a solution
;; is provided in the file p99-readfile.lisp. Use the predicate
;; read_lines/2.

;; (3) For efficiency reasons it is important, at least for larger
;; puzzles, to sort the words and the sites in a particular order. For
;; this part of the problem, the solution of P28 may be very helpful.
