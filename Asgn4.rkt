#lang typed/racket
(require typed/rackunit)

;;Sofia Morris, Anissa Soungpanya

;;represents types of arithmetic extressions
(define-type ExprC (U NumC AppC IdC StrC IfC LamC))
;;represents a number
(struct NumC([n : Real]) #:transparent)
;; represents application form
(struct AppC ([fun : ExprC] [arg : (Listof ExprC)]) #:transparent)
;; represents idC
(struct IdC ([name : Symbol]) #:transparent)
;; represents a string
(struct StrC ([str : String]) #:transparent)
;; represents an if statement - 1st expression is the condition, 2nd evaluates if true,
;; 3rd evaluates if false
(struct IfC ([cond : ExprC] [true : ExprC] [false : ExprC]) #:transparent)
;; represents an anonymous function
(struct LamC ([args : (Listof Symbol)] [body : ExprC]) #:transparent)


(define-type Value (U NumV CloV BoolV StrV PrimV))
(struct NumV ([val : Real]) #:transparent)
(struct CloV ([args : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)
(struct BoolV ([val : Boolean]) #:transparent)
(struct StrV ([val : String]) #:transparent)
(struct PrimV ([val : Symbol]) #:transparent)

(struct Binding ([name : Symbol] [val : Value]) #:transparent)
 
(define-type Env (Listof Binding))
(define mt-env empty)
(define top-env (list
                 (Binding '+ (PrimV '+))
                 (Binding '- (PrimV '-))
                 (Binding '* (PrimV '*))
                 (Binding '/ (PrimV '/))
                 (Binding '<= (PrimV '<=))
                 (Binding 'equal? (PrimV 'equal?))
                 (Binding 'true (PrimV 'true))
                 (Binding 'false (PrimV 'false))
                 (Binding 'error (PrimV 'error))))

;; accepts any AAQZ4 value and returns a string
(define (serialize [v : Value]) : String
  (match v
    [(NumV n) (format "~v" n)]
    [(BoolV b) (format "~v" b)]
    [(StrV s) (format "~v" s)]
    [(CloV arg bod env) (format "~v~v~v" arg bod env)]
    [(PrimV p) (format "~v" p)]))

(check-equal? (serialize (NumV 12)) "12")

;;takes in a symbol and checks if it exists in the top environment for built-in functions
;;returns true if within list, false otherwise
(define (top-env? [for : Symbol]) : Boolean
  (match for
    ['* #t]
    ['+ #t]
    ['/ #t]
    ['- #t]
    ['<= #t]
    ['equal? #t]
    ['true #t]
    ['false #t]
    ['error #t]
    [else #f]))

(define (primv-interp [val : Symbol] [l : ExprC] [r : (Listof ExprC)] [env : Env]) : Value
  (printf "~a\n" l)
  (printf "~a\n" (first r))
  (match val
    ['true (BoolV #t)]
    ['false (BoolV #f)]
    ['error (error 'primv-interp "AAQZ Error provided in function")]
    ['equal? (define left (interp l env))
             (define right (interp (first r) env))
             (cond
               #;[(and (BoolV? left) (BoolV? right)
                     (equal? (BoolV-val left) (BoolV-val right))) #t]
               #;[(and (NumV? left) (NumV? right)
                     (equal? (NumV-val left) (NumV-val right))) #t]
               [(and (and (not (PrimV? left)) (not (PrimV? right)))
                     (and (not (CloV? left)) (not (CloV? right)))
                     (equal? left right)) (BoolV #t)]
               [else (BoolV #f)])]
    [else (define left (interp l env))
          (define right (interp (first r) env))
          (printf "~a\n" left)
          (printf "~a\n" right)
          (if (and (NumV? left) (NumV? right))
              (match val
                ['* (NumV (* (NumV-val left) (NumV-val right)))]
                ['+ (NumV (+ (NumV-val left) (NumV-val right)))]
                ['/ (if (equal? (NumV-val right) 0)
                        (NumV (/ (NumV-val left) (NumV-val right)))
                        (error 'binop-interp "AAQZ cannot divide by 0"))]
                ['- (NumV (- (NumV-val left) (NumV-val right)))]
                ['<= (BoolV (<= (NumV-val left) (NumV-val right)))])
              (error 'primv-interp "one argument was not a number"))]))

;;substitutes value in env for symbol if exists
(define (lookup [for : Symbol] [env : Env]) : Value
  (cond
    [(empty? env) (error 'lookup "AAQZ name not found in env directory")]
    [else (cond
            [(symbol=? for (Binding-name (first env)))
             (cond
               [(top-env? for) (PrimV for)] 
               [else (Binding-val (first env))])]
            [else (lookup for (rest env))])]))

(check-exn #rx"AAQZ" (lambda () (lookup 'a '())))

;;takes in the function body and body of AppC and returns a list of bindings for the environment
(define (extend-env [fargs : (Listof Symbol)] [args : (Listof ExprC)]
                    [outer-env : Env] [clov-env : Env]) : (Listof Binding)
  (printf "extend ~a & ~a\n" fargs args)
  (match fargs
    ['() clov-env]
    [else (cons (Binding (first fargs) (interp (first args) outer-env))
                (extend-env (rest fargs) (rest args) outer-env clov-env))]))

;;takes in an arithmetic expression and reduces it to its value
(define (interp [exp : ExprC] [env : Env]) : Value
  (printf "interp: ~a\n" exp)
  (printf "env : ~a\n" env)
  (match exp 
    [(NumC n) (NumV n)]
    [(IdC id) (lookup id env)]
    [(StrC str) (StrV str)]
    [(IfC cond t f) (if (interp cond env) (interp t env) (interp f env))]
    [(AppC fun args) (local ([define f-value (interp fun env)])
                       (printf "f-value : ~a\n" f-value)
                       (match f-value
                         [(PrimV p) (printf "~a\n" args) (primv-interp p (first args) (rest args) env)]
                         [(CloV a b e) (interp b
                               (extend-env a
                                           args env e))]))]
    [(LamC args body) (CloV args body env)]))

(check-equal? (interp (AppC (LamC '(x y) (AppC (IdC '*) (list (IdC 'x) (IdC 'y))))
    (list (NumC 4) (NumC 7))) top-env) (NumV 28))

#;(check-equal? (interp (AppC 'f (list (NumC 1)(AppC 'f (list (NumC 1)(NumC 1))))) mt-env
                      (list (FundefC 'f '(x y) (BinopC '+ (IdC 'x)(IdC 'y))))) 3)

#;(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))
