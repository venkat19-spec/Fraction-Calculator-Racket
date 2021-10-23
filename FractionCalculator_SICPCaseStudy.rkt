#lang racket/base


; GCD FUNCTION TO REDUCE FRACTION TO LOWEST FORM
(define (gcd a b )
  (if ( = b 0)
      a
      (gcd b (remainder a b))))

#|========== PROPER/IMPROPER FRACTIONS ==========|#

; TO CREATE PROPER/IMPROPER FRACTION
(define ( proper_improper numer denom )
  (cons (/ numer (gcd numer denom))  ( / denom (gcd numer denom)))
)

; SELECTORS TO RETURN NUM AND DENOM SEPARATELY
(define (numer x)
  (car x )
)

(define (denom x)
  (cdr x)
)

; PRINT PROPER/IMPROPER FRACTION
(define (print-fraction x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
)
#|=====================================|#

#|========== MIXED FRACTIONS ==========|#

; 
(define ( make-mixed-fraction whole-part numer denom )
  (cons whole-part (cons numer denom))
)

;SELECTORS TO RETURN NUM, DENOM AND WHOLEPART SEPARATELY
(define (numer-mixed x)     
  (cadr x)
)

(define (denom-mixed x)
  (cddr x)
)

(define (wholepart-mixed x)
  (car x)
)

; PRINT MIXED FRACTION
(define (print-mixedfraction x)
  (newline)
  (display (wholepart-mixed x))
  (display " ")
  (display (numer-mixed x))
  (display "/")
  (display (denom-mixed x))
)
  

#|====================================|#


;FUNCTIONS

; CHECK IF THE VALUE IS MIXED FRACTION OR NOT
(define (is_mixed_frac x)
  (if(pair? (cdr x))
     #t
     #f
   )
)

; CONVERTING MIXED TO IMPROPER FRACTION
(define ( convert-mixed-to-frac x )

  (proper_improper (+ (* (wholepart-mixed x) (denom-mixed x)) (numer-mixed x)) (denom-mixed x))
)

; CONVERTING IMPROPER TO MIXED FRACTION
(define ( convert-frac-to-mixed x)

  (make-mixed-fraction (quotient (numer x) (denom x)) (remainder (numer x) (denom x)) (denom x)) 
)

;EQUALITY BETWEEN TWO FRACTIONS
(define (equality x y)
    (and (= (numer x) (numer y) ) (= (denom x) (denom y)) )
)

;INVERT A FRACTION
(define (invert x)
  (proper_improper (denom x) (numer x))
  (print-fraction (proper_improper (denom x) (numer x)))
)



;TO ADD TWO FRACTIONS
(define (addition x y )
    ( proper_improper ( + (*(numer x)(denom y)) (*(numer y)(denom x))) (*(denom x)(denom y)) )
)

;TO MULTIPLY TWO FRACTIONS
(define (multiplication x y )
  ( proper_improper (*(numer x)(numer y)) (*(denom x)(denom y)) )
)

;TO SUBTRACT TWO FRACTIONS
(define (subtraction x y ) 
   ( proper_improper ( - (*(numer x)(denom y)) (*(numer y)(denom x))) (*(denom x)(denom y)) )
)


;TO DIVIDE TWO FRACTIONS
(define (divide x y )
   ( proper_improper (*(numer x)(denom y)) (*(denom x)(numer y)) )
)




;checking version 4

; Function to check mixed fraction - If mixed convert it into improper fraction else improper fraction
(define (check-mixed-frac-to-improper x)
  (if(is_mixed_frac x) 
     (convert-mixed-to-frac x)
     x
   )
 )


; Function to check all values in list and then change to improper fraction and then store it in new list
; li - original list containing mixed fractions, improper and proper fractions
; new_li - new list containing fractions in proper/improper format

(define (conv-list-pro-impro li new_li)
  (if(= (length li ) 0)
     new_li
     (conv-list-pro-impro (cdr li) (append new_li (list (check-mixed-frac-to-improper (car li)) ))     
     )
   )
)

(define (convert-list-pro-impro li)
  (conv-list-pro-impro li (list))
)


;PROCEDURAL ABSTRACTION - TERM(ADD,SUB,MUL,DIV) ---- new
(define (arithmetic-operations term n sum)
    (if(= (length n) 0)
       (print-fraction sum)
       (arithmetic-operations term (cdr n) (term sum (car n))
       )
    )
)

;FUNCTION TO ADD A LIST OF NUMBERS - TAKES ONE PARAMETER N(LIST OF NUMBERS) ----- new
(define (add-list n)
  (arithmetic-operations addition (convert-list-pro-impro n) (proper_improper  0 1))
)


;FUNCTION TO MULTIPLY A LIST OF NUMBERS - TAKES ONE PARAMETER N(LIST OF NUMBERS)---new
(define (mul-list n)
  (if (= (length n) 0)
      (proper_improper  0 1)
      (arithmetic-operations multiplication (convert-list-pro-impro n) (proper_improper  1 1))
  )
)


(define (tot_div_sub func term n)
  (if(= (length n) 1)
       (car n)  
       (func term (cdr n) (car n))
  )
)


(define (sub-list n)
  (if (= (length n) 0)
      (display "Error - Enter atleast one element")
      (tot_div_sub arithmetic-operations subtraction (convert-list-pro-impro n))
  )
)

(define (div-list n)
  (if (= (length n) 0)
      (display "Error - Enter atleast one element")
      (tot_div_sub arithmetic-operations divide (convert-list-pro-impro n))
  )
)


;sample variables
(define m1 (make-mixed-fraction 1 3 4))
(define m2 (make-mixed-fraction 3 1 4))

(define p1 (proper_improper 1 2))
(define p2 (proper_improper 3 2))

(define l1 (list p1 m1))
(define l2 (list p1 m1 m2 p2))
(define el (list ))
