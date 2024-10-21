#lang typed/racket
(require typed/rackunit)

;;Sofia Morris, Anissa Soungpanya

;;represents types of arithmetic extressions
(define-type ExprC (U NumC BinopC AppC IdC ifleq0?))
;;represents a number
(struct NumC([n : Real]) #:transparent)
;;represents types of binary operations
(struct BinopC ([op : Symbol] [left : ExprC] [right : ExprC]) #:transparent)
;;represents function defnitions
(struct FundefC ([name : Symbol] [args : (Listof Symbol)] [body : ExprC]) #:transparent)
;; represents application form
(struct AppC ([fun : Symbol] [arg : (Listof ExprC)]) #:transparent)
;; represents idC
(struct IdC ([name : Symbol]) #:transparent)
;; represents a string
(struct StrC ([str : String]))
;; represents an if statement - 1st expression is the condition, 2nd evaluates if true,
;; 3rd evaluates if false
(struct IfC ([if : Symbol] [cond : ExprC] [true : ExprC] [false : ExprC]))
;; represents an anonymous function
(struct LamC ([args : (Listof Symbol)] [body : ExprC]))

;;(define-type Value (U NumV CloV BoolV StrV PrimV))
;;(define NumV Real)
;;(define CloV )
;;(define BoolV Boolean)
;;(define StrV String)
;;(define PrimV )

(struct Binding ([name : Symbol] [val : Real]))
 
(define-type Env (Listof Binding))
(define mt-env empty)

;; accepts any AAQZ4 value and returns a string
(define (serialize [v : Value]) : String
  (match v
    [(NumV n) (format "~v" n)]
    [(BoolV b) (format "~v" n)]
    [(StrV s) (format "~v" s)]
    [(CloV c) ("#<procedure>")]
    [(PrimV p) ("#<primop>")]))

;;takes in an operator symbol and two arithmetic functions and returns the result of the operation
(define (binop-interp [op : Symbol] [l : ExprC] [r : ExprC] [env : Env]
                      [funs : (Listof FundefC)]) : Real
  (match op
    ['* (* (interp l env funs) (interp r env funs))]
    ['+ (+ (interp l env funs) (interp r env funs))]
    ['/ (define div (interp r env funs))
     (if (equal? div 0)
        (error 'binop-interp "AAQZ cannot divide by 0")
        (/ (interp l env funs) div))]
    ['- (- (interp l env funs) (interp r env funs))]))


;; helper function to find function definition
(define (find-fun [fun : Symbol][funs : (Listof FundefC)]) : FundefC
  (cond
    [(empty? funs) (error 'find-fun "AAQZ function not found")]
    [(equal? fun (FundefC-name (first funs))) (first funs)]
    [else (find-fun fun (rest funs))]))

(check-equal? (find-fun 'f (list (FundefC 'f '(x y)
                                          (BinopC '+ (IdC 'x) (IdC 'y)))))
              (FundefC 'f '(x y) (BinopC '+ (IdC 'x) (IdC 'y))))
(check-exn #rx"AAQZ" (lambda () (find-fun 'hi (list
                                               (FundefC 'f '(x y) (BinopC '+ (IdC 'x) (IdC 'y)))))))

;;substitutes value in env for symbol if exists
(define (lookup [for : Symbol] [env : Env]) : Real
  (cond
    [(empty? env) (error 'lookup "AAQZ name not found in env directory")]
    [else (cond
            [(symbol=? for (Binding-name (first env)))
             (Binding-val (first env))]
            [else (lookup for (rest env))])]))

(check-exn #rx"AAQZ" (lambda () (lookup 'a '())))

;;takes in the function body and body of AppC and returns a list of bindings for the environment
(define (extend-env [fargs : (Listof Symbol)] [args : (Listof ExprC)] [env : Env]
                    [funs : (Listof FundefC)]) : (Listof Binding)
  (match fargs
    ['() mt-env]
    [else (cons (Binding (first fargs) (interp (first args) env funs))
                (extend-env (rest fargs) (rest args) env funs))]))

;;takes in an arithmetic expression and reduces it to its value
(define (interp [exp : ExprC] [env : Env] [funs : (Listof FundefC)]) : Real
  (match exp 
    [(NumC n) n]
    [(IdC n) (lookup n env)]
    [(BinopC op l r) (binop-interp op l r env funs)]
    [(AppC fun args) (local ([define fd (find-fun fun funs)])
                       (interp (FundefC-body fd)
                               (extend-env (FundefC-args fd) args env funs)
                               funs))]))

(check-equal? (interp (BinopC '+ (NumC 2) (NumC 2)) mt-env '()) 4)
(check-equal? (interp (AppC 'f (list (NumC 1)(NumC 2))) mt-env
                      (list (FundefC 'f '(x y) (BinopC '+ (IdC 'x)(IdC 'y))))) 3)
(check-equal? (interp (AppC 'f (list (NumC 1)(AppC 'f (list (NumC 1)(NumC 1))))) mt-env
                      (list (FundefC 'f '(x y) (BinopC '+ (IdC 'x)(IdC 'y))))) 3)
