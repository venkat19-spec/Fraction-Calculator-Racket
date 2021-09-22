#lang racket/base

; GCD FUNCTION TO REDUCE FRACTION TO LOWEST FORM
(define (gcd a b )
  (if ( = b 0)
      a
      (gcd b (remainder a b))))

; CONSTRUCTOR TO RETURN NUM AND DENOM PAIR IN LOWEST FORM
(define ( proper_improper numer denom )
  (cons (/ numer (gcd numer denom))  ( / denom (gcd numer denom)))
)

; SELECTOR FUNCTIONS TO RETURN NUM AND DENOM SEPARATELY
(define (numer x)
  (car x )
)

(define (denom x)
  (cdr x)
)
 
; FUNCTION TO COMPUTE ADDITION OF FRACTIONS
(define (total_add n)
  
  (define (addition x y )
    ( proper_improper ( + (*(numer x)(denom y)) (*(numer y)(denom x))) (*(denom x)(denom y)) )
  )
  
  (define (n_add n sum)
    (if(= (length n) 0)
       sum 
       (n_add (cdr n)
              (addition sum (proper_improper (numerator (car n)) (denominator (car n))))
       )
     )
  )

  (n_add n  (proper_improper  0 1))
  
)