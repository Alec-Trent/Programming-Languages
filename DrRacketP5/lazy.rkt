#lang lazy

; Question 12
; f(0) = 1, f(1) = 2, f(2) = 3, f(n) = 2*f(n-1)+3*f(n-3) for n >= 3
(define f-list (cons 1 (cons 2 (cons 3 (map (lambda (x y) (+ (* 2 x)(* 3 y))) f-list(rest(rest f-list)))))))

; Test
; (!! (take insertNumberHere f-list))
; Note: !! = force evaluation
