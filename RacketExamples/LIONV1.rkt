#lang plait

; Part 1: Define the language structure
; Exp can be a number, a sum of two expressions, or a subtraction of two expressions
(define-type Exp
  [Num (n : Number)] 
  [Sum (l : Exp) (r : Exp)]
  [Sub (l : Exp) (r : Exp)])


; Part 2: Create the interpreter
; eval-exp evaluates an Exp and returns a Number result
(define (eval-exp [e : Exp]) : Number
  (type-case Exp e
    [(Num n) n]
    [(Sum l r) (+ (eval-exp l) (eval-exp r))]
    [(Sub l r) (- (eval-exp l) (eval-exp r))]
    ))


; Part 3: Test the language
(define e1 (Num 10))   
(define e2 (Num 5))    

; At least two tests for addition
(define e3 (Sum e1 e2))
(define e5 (Sum (Num 7) (Sub (Num 9) (Num 2)))) 

; At least two tests for subtraction
(define e4 (Sub e1 e2))
(define e6 (Sub (Sum (Num 20) (Num 3)) (Num 8))) 

; Evaluate the expressions 
(eval-exp e3) 
(eval-exp e4) 
(eval-exp e5) 
(eval-exp e6) 