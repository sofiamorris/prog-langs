#lang typed/racket
(require typed/rackunit)

;;Sofia Morris, Anissa Soungpanya
;;<Submission progress comment>

;;represents types of arithmetic extressions
(define-type ArithC (U NumC BinopC))
;;represents a number
(struct NumC([n : Real]) #:transparent)
;;represents types of binary operations
(struct BinopC ([op : Symbol] [left : ArithC] [right : ArithC]))

;;takes in an operator symbol and two arithmetic functions and returns the result of the operation
(define (binop-interp [op : Symbol] [l : ArithC] [r : ArithC]) : Real
  (match op
    ['* (* (interp l) (interp r))]
    ['+ (+ (interp l) (interp r))]
    ['/ (/ (interp l) (interp r))]
    ['- (- (interp l) (interp r))]))

;;takes in an arithmetic expression and reduces it to its value
(define (interp [exp : ArithC])
  (match exp 
    [(NumC n) n]
    [(BinopC op l r) (binop-interp op l r)]))

(interp (BinopC '+ (NumC 2) (NumC 2)))
;;parser in Arith takes in an s-expression and returns a corresponding ArithC or signals an error