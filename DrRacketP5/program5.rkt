#lang racket

; Question 1
; if null, return empty set
; else cons the first element with (), and then use recursion to get rest of the list
(define (wrap M)
  (if(null? M)
     '()
     (cons (cons (first M) '())(wrap (rest M)))))
; Test
; (wrap '(1 ()))
; (wrap '(1 (2) 3))


; Question 2
; if null, return 2 for empty set
; then the first atom of M is null add 2, use recursion on rest
; then the first element of M is a list
; else an atom, no parenth, continue
(define (count-parens-all M)
  (cond [(null? M) 2]
        [(null? (first M))(+2 (count-parens-all (rest M)))]
        [(list? (first M))(+ (count-parens-all (first M))(count-parens-all (rest M)))]
        [else (count-parens-all (rest M))]))
; Test
; (count-parens-all '())
; (count-parens-all '((a b) c))
; (count-parens-all '((a b) () c)) - Will fail


; Question 3 - NOT DONE
; if null, return empty set
; then pairs
; else
(define(insert-left-all new old M)
  (cond [(null? M) '()]
        [(pair? (first M))(cons(insert-left-all new old(first M))(insert-left-all new old(rest M)))]
        [else (insert-left-all new old(rest M))]))
; Test
; (insert-left-all 'z 'a '(a b (a c (a))))
; (insert-left-all 'dog 'cat '(my dog is fun))


; Question 4
; Source
; file:///C:/Program%20Files/Racket/doc/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._reverse%29%29
; if null, return empty set 
; else construct a list with the reversed first list, and then recurse the rest of the lists
(define (invert M)
  (if (null? M)
      '()
      (cons (reverse(first M))(invert(rest M)))))
; Test
; (invert '((a 1) (a 2) (b 1) (b 2)))
; (invert '((a 1) (a 2) (b 1) (()) (b 2)))


; Question 5
; Source slide 37
; if there is no occurance 0, reach end of list use index value
; then if atom equal then store in index, update base, recur rest
; else atom not equal, increase base by 1, recur rest
(define (last a L)
  (local ((define (helper a L base index)  
          (cond [(null? L) index]
                [(eq? (first L) a) (helper a (rest L) (+ 1 base) (+ 1 base))]
                [else (helper a (rest L) (+ 1 base) index)])))(helper a L 0 0)))
; Test
; (last 3 '(2 1 3 4))
; (last 3 '(2 1 3 3 4))
; (last 3 '(2 1 3 3 4 ()))


; Question 6
; if null, return empty set
; then pred is true, cons the first, recur on rest
; else pred is false, recur on rest
(define (select pred L)
  (cond [(null? L) '()]
        [(pred (first L)) (cons (first L) (select pred (rest L)))]
        [else (select pred (rest L))]))
; Test
; (select number? '(a 2 #f b 7))
; (select symbol? '(a 2 #f b 7))
; (select symbol? '(a 2 #f b 7 ()))


; Question 7 - NOT DONE 
; Source Slide
; if null return empty set
; then if null return empty set, have two lists need to check them both
; then
; else
; map allows atoms of M1 to be applied to each atom in M2, return results
;(define (summatrices M1 M2)
 ; (local ((define sumrow f g)
  ;        (cond [(null? f) '()]
  ;              [(null? g) '()]
  ;              [else (cons (+ (first f)(first g))(sumrow(rest f)(rest g)))]))(sumrow f g)))

          ;(if(or(null? f)(null? g))
          ;   '()
          ;   (cons(+ (first f) (first g))(sumrow (rest f)(rest g)))))))
          
  ;(map(lambda (M1 M2)(+ M1 M2)) M1 M2))
; Test
; (summatrices '((1 2 3)) '((4 5 6)))
; (summatrices '((1 2 3)(4 5 6)) '((10 10 10)(20 20 20))


; Question 8
; if null return empty set
; then if the first atom equals a1 atom, add to list, recur
; then if the first atom equals a2 atom, add to list, recur
; then if its a list, recur on the atom, recur
; else they dont match, add to list, recur
(define (swapper a1 a2 M)
  (cond [(null? M) '()]
        [(eq? (first M) a1)(cons a2(swapper a1 a2 (rest M)))]
        [(eq? (first M) a2)(cons a1(swapper a1 a2 (rest M)))]
        [(list? (first M)) (cons (swapper a1 a2(first M))(swapper a1 a2 (rest M)))]
        [else (cons (first M)(swapper a1 a2 (rest M)))]))
; Test
; (swapper 'a 'd '(a b c d))
; (swapper 'x 'y '((x) y (z(x))))
; (swapper 'x 'y '((x) y () (z(x))))


; Question 9
; if null return empty set
; then if list has atom, append, recur
; else cons first atom, recur rest
(define (flatten M)
  (cond [(null? M) '()]
        [(list? (first M))(append(flatten(first M))(flatten(rest M)))]
        [else (cons(first M)(flatten(rest M)))]))
; Test
; (flatten '(a b c))
; (flatten '((a) () (b ()) () (((c)))))


; Question 10 - NOT DONE
; if
; then
; else
;(define (


; Question 11
(define (abstract-fn base f x g)
  (local ((define(helper a M)
            (cond [(null? M) base]
                  [(not (pair? (first M))) (if (eq? a (first M)) (f a M) (x a M))]
                  [else (g a M)])))helper))
         
(define rember* (abstract-fn
                 '()
                 (lambda (a M)(rember* a (rest M)))
                 (lambda (a M)(cons (first M) (rember* a (rest M))))
                 (lambda (a M)(cons (rember* a (first M))
                                    (rember* a (rest M))))))

(define count*  (abstract-fn
                 0
                 (lambda (a M)(add1 (count* a (rest M))))
                 (lambda (a M)(count* a (rest M)))
                 (lambda (a M)(+ (count* a (first M))
                                 (count* a (rest M))))))
; Test
; (rember* '0 '(a b c))
; (count* '0 '(a b c))

        