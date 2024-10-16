#lang typed/racket
(require typed/rackunit)

;;Sofia Morris, Anissa Soungpanya
;;<Submission progress comment>

;;represents types of arithmetic extressions
(define-type ExprC (U NumC BinopC ifleq0? Symbol AppC IdC))
;;represents a number
(struct NumC([n : Real]) #:transparent)
;;represents types of binary operations
(struct BinopC ([op : Symbol] [left : ExprC] [right : ExprC]) #:transparent)
;;represents conditional <=0
(struct ifleq0? ([a : ExprC] [b : ExprC] [c : ExprC]) #:transparent)
;;represents function definitions
(struct FundefC ([name : Symbol] [args : (Listof Symbol)] [body : ExprC]) #:transparent)
;; represents application form
(struct AppC ([fun : Symbol] [arg : Symbol]) #:transparent)
;; represents idC
(struct IdC ([name : Symbol]))

;;takes in an operator symbol and two arithmetic functions and returns the result of the operation
(define (binop-interp [op : Symbol] [l : ExprC] [r : ExprC] [funs : (Listof FundefC)]) : Real
  (match op
    ['* (* (interp l funs) (interp r funs))]
    ['+ (+ (interp l funs) (interp r funs))]
    ['/ (/ (interp l funs) (interp r funs))]
    ['- (- (interp l funs) (interp r funs))]))

;;takes in an arithmetic expression and reduces it to its value
(define (interp [exp : ExprC] [funs : (Listof FundefC)]) : Real
  (match exp 
    [(NumC n) n]
    [(AppC fun args) (subst args (FundefC-args (find-fun fun funs)) (FundefC-body (find-fun fun funs)))]
    [(BinopC op l r) (binop-interp op l r funs)]
    [(ifleq0? a b c) (if (<= (interp a) (interp (NumC 0))) (interp b) (interp c))]))



;; helper function to find function definition
(define (find-fun [fun : Symbol][funs : (Listof FundefC)]) : FundefC
  (cond
    [(empty? funs) (error 'find-fun "AAQZ function not found")]
    [(equal? fun (FundefC-name (first(funs)))) (first(funs))]
    [else (find-fun (rest funs))]))

;; helper function to interpret single-argument applications
(define (subst [what : ExprC][for : Symbol][in : ExprC]) : ExprC
  (match in
  [(NumC n) in]
  [(IdC s) (cond
             [(= s for) what]
             [else in])]
  [(AppC f a) (AppC f (subst what for a))]
  [(BinopC op l r) (BinopC op
                      (subst what for l)
                      (subst what for r))]))

(check-equal? (interp (BinopC '+ (NumC 2) (NumC 2))) 4)
(check-equal? (interp (BinopC '* (NumC 3) (NumC 2))) 6)
(check-equal? (interp (BinopC '/ (NumC 2) (NumC 2))) 1)
(check-equal? (interp (BinopC '- (NumC 2) (NumC 2))) 0)
(check-equal? (interp (ifleq0? (NumC 5) (NumC 1) (NumC 0))) 0)
(check-equal? (interp (ifleq0? (NumC -1) (NumC 1) (NumC 0))) 1)

;;parser in Arith takes in an s-expression and returns a corresponding ArithC or signals an error
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? n) (NumC n)]
    [(? symbol? s) s]
    [(list (? symbol? fun) (? symbol? arg)) (AppC fun arg)]
    [(list (? symbol? op) l r) (BinopC op (parse l) (parse r))]
    [(list 'ifleq0? a b c) (ifleq0? (parse a) (parse b) (parse c))]
    [other (error 'parse "AAQZ expected a valid Sexp, got ~e" other)]))

(check-equal? (parse '7) (NumC 7))
(check-equal? (parse '{* {+ 2 3} 7}) (BinopC '* (BinopC '+ (NumC 2) (NumC 3)) (NumC 7)))
(check-equal? (parse '{ifleq0? 5 1 0}) (ifleq0? (NumC 5) (NumC 1) (NumC 0)))
(check-exn #rx"AAQZ" (lambda () (parse "hi")))

;; top-interp takes in s-expression and
;; calls parse and interp, reducing the
;; expression to a value
(define (top-interp [s : Sexp]) : Real
  (interp(parse s)))

(check-equal? (top-interp '7) 7)
(check-equal? (top-interp '{* {+ 2 3} 7}) 35)
(check-equal? (top-interp '{ifleq0? 5 1 0}) 0)
(check-exn #rx"AAQZ" (lambda () (top-interp "hi")))


;; parser that takes s-expression and returns FundefC's
(define (parse-fundef [s : Sexp]) : FundefC
  (match s
    [(list 'def (? symbol? name) (list (list params ...)
           '=> body))
     (if (has-dup (cast (first (third s)) (Listof Symbol)))
         (error 'parse-fundef "AAQZ duplicate arg")
         (FundefC name (cast (first (third s)) (Listof Symbol)) (parse body)))]
    [other (error 'parse-fundef "AAQZ improper syntax ~e" other)]))

;; helper function to check for duplicate parameters
(define (has-dup [lst : (Listof Symbol)]) : Boolean
  (cond
    [(or (empty? lst) (empty? (rest lst))) #f]
    [(member (first lst) (rest lst)) #t]
    [else (has-dup (rest lst))]))

(check-equal? (parse-fundef '{def f {(x y) => {+ x y}}}) (FundefC 'f '(x y) (BinopC '+ 'x 'y)))
(check-exn #rx"AAQZ" (lambda () (parse-fundef '{def err {(x x) => x}})))
(check-exn #rx"AAQZ" (lambda () (parse-fundef '{def err {(x y)}})))


