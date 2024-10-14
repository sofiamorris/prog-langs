#lang typed/racket
(require typed/rackunit)

;;Sofia Morris, Anissa Soungpanya
;;<Submission progress comment>

;;represents types of arithmetic extressions
(define-type ExprC (U NumC BinopC))
;;represents a number
(struct NumC([n : Real]) #:transparent)
;;represents types of binary operations
(struct BinopC ([op : Symbol] [left : ExprC] [right : ExprC]) #:transparent)

;;takes in an operator symbol and two arithmetic functions and returns the result of the operation
(define (binop-interp [op : Symbol] [l : ExprC] [r : ExprC]) : Real
  (match op
    ['* (* (interp l) (interp r))]
    ['+ (+ (interp l) (interp r))]
    ['/ (/ (interp l) (interp r))]
    ['- (- (interp l) (interp r))]))

;;takes in an arithmetic expression and reduces it to its value
(define (interp [exp : ExprC])
  (match exp 
    [(NumC n) n]
    [(BinopC op l r) (binop-interp op l r)]))

(check-equal? (interp (BinopC '+ (NumC 2) (NumC 2))) 4)
(check-equal? (interp (BinopC '* (NumC 3) (NumC 2))) 6)
(check-equal? (interp (BinopC '/ (NumC 2) (NumC 2))) 1)
(check-equal? (interp (BinopC '- (NumC 2) (NumC 2))) 0)

;;parser in Arith takes in an s-expression and returns a corresponding ArithC or signals an error
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? n) (NumC n)]
    [(list (? symbol? op) l r) (BinopC op (parse l) (parse r))]
    [other (error 'parse "AAQZ expected a valid Sexp, got ~e" other)]))

(check-equal? (parse '7) (NumC 7))
(check-equal? (parse '{* {+ 2 3} 7}) (BinopC '* (BinopC '+ (NumC 2) (NumC 3)) (NumC 7)))
(check-exn #rx"AAQZ" (lambda () (parse "hi")))


