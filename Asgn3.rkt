#lang typed/racket
(require typed/rackunit)

;;Sofia Morris, Anissa Soungpanya
;;<Submission progress comment>

;;represents types of arithmetic extressions
(define-type ExprC (U NumC BinopC AppC IdC ifleq0?))
;;represents a number
(struct NumC([n : Real]) #:transparent)
;;represents types of binary operations
(struct BinopC ([op : Symbol] [left : ExprC] [right : ExprC]) #:transparent)
;;represents conditional <=0
(struct ifleq0? ([a : ExprC] [b : ExprC] [c : ExprC]) #:transparent)
;;represents function defnitions
(struct FundefC ([name : Symbol] [args : (Listof Symbol)] [body : ExprC]) #:transparent)
;; represents application form
(struct AppC ([fun : Symbol] [arg : (Listof ExprC)]) #:transparent)
;; represents idC
(struct IdC ([name : Symbol]) #:transparent)




;;takes in an operator symbol and two arithmetic functions and returns the result of the operation
(define (binop-interp [op : Symbol] [l : ExprC] [r : ExprC] [funs : (Listof FundefC)]) : Real
  (match op
    ['* (* (interp l funs) (interp r funs))]
    ['+ (+ (interp l funs) (interp r funs))]
    ['/ (/ (interp l funs) (interp r funs))]
    ['- (- (interp l funs) (interp r funs))]))




;; helper function to find function definition
(define (find-fun [fun : Symbol][funs : (Listof FundefC)]) : FundefC
  (cond
    [(empty? funs) (error 'find-fun "AAQZ function not found")]
    [(equal? fun (FundefC-name (first funs))) (first funs)]
    [else (find-fun fun (rest funs))]))

(check-equal? (find-fun 'f (list (FundefC 'f '(x y) (BinopC '+ (IdC 'x) (IdC 'y))))) (FundefC 'f '(x y) (BinopC '+ (IdC 'x) (IdC 'y))))
(check-exn #rx"AAQZ" (lambda () (find-fun 'hi (list (FundefC 'f '(x y) (BinopC '+ (IdC 'x) (IdC 'y)))))))




;; helper function that returns index of IdC in for
(define (find-IdC [s : Symbol][lst : (Listof Symbol)][index : Real]) : Real
  (cond
    [(empty? lst) -1]
    [(equal? (first lst) s) index]
    [else (find-IdC s (rest lst) (+ index 1))]))




;; helper function to match IdC to ExprC in what
(define (replace-IdC [lst : (Listof ExprC)][index : Real]) : ExprC
  (cond
    [(= index 0) (first lst)]
    [else (replace-IdC (rest lst) (- index 1))]))




;; helper function to see if IdC is in what
(define (symbol-in-lst [s : Symbol][lst : (Listof Symbol)]) : Boolean
  (cond
    [(empty? lst) #f]
    [(equal? s (first lst)) #t]
    [else (symbol-in-lst s (rest lst))]))

(check-equal? (symbol-in-lst 'f '(f g)) #t)
(check-equal? (symbol-in-lst 'f '(g)) #f)




;; helper function to interpret applications
(define (subst [what : (Listof ExprC)][for : (Listof Symbol)][in : ExprC]) : ExprC
  (match in
    [(NumC n) in]
    [(IdC s) (cond
               [(symbol-in-lst s for) (replace-IdC what (find-IdC s for 0))]
               [else in])]
    [(AppC f args) (AppC f (for/list ([x args]) (subst what for x)))]
    [(BinopC op l r) (if (not (= (length what) (length for)))
                         (error 'interp "AAQZ number of args does not match number of params")
                         (BinopC op
                                 (subst what for l)
                                 (subst what for r)))]))

(check-equal? (subst (list (NumC 1)(NumC 2)) '(x y) (BinopC '+ (IdC 'x)(IdC 'y)))
              (BinopC '+ (NumC 1)(NumC 2)))
(check-exn #rx"AAQZ" (lambda () (subst (list (NumC 1)) '(x y) (BinopC '+ (IdC 'x)(IdC 'y)))))




;;takes in an arithmetic expression and reduces it to its value
(define (interp [exp : ExprC][funs : (Listof FundefC)]) : Real
  (match exp 
    [(NumC n) n]
    [(AppC fun args) (interp (subst args (FundefC-args (find-fun fun funs)) (FundefC-body (find-fun fun funs))) funs)]
    [(BinopC op l r) (binop-interp op l r funs)]
    [(ifleq0? a b c) (if (<= (interp a funs) (interp (NumC 0) funs)) (interp b funs) (interp c funs))]))

(check-equal? (interp (BinopC '+ (NumC 2) (NumC 2)) '()) 4)
(check-equal? (interp (BinopC '* (NumC 3) (NumC 2)) '()) 6)
(check-equal? (interp (BinopC '/ (NumC 2) (NumC 2)) '()) 1)
(check-equal? (interp (BinopC '- (NumC 2) (NumC 2)) '()) 0)
(check-equal? (interp (AppC 'f (list (NumC 1)(NumC 2)))
                      (list (FundefC 'f '(x y) (BinopC '+ (IdC 'x)(IdC 'y))))) 3)
(check-equal? (interp (ifleq0? (NumC 5) (NumC 1) (NumC 0)) '()) 0)
(check-equal? (interp (ifleq0? (NumC -1) (NumC 1) (NumC 0)) '()) 1)




;; takes in a list of function definitions and returns the final value
;; differentiate main function from other function definitions and calls
;; interp using application in main and rest of functions
(define (interp-fns [funs : (Listof FundefC)]) : Real
  (cond
    [(empty? funs) (error 'interp-fns "AAQZ main not found")]
    [else (define main-fn (filter (lambda (fn)
                                    (equal? 'main (FundefC-name (cast fn FundefC)))) funs))
          (interp (FundefC-body (first main-fn)) funs)]))

#;(check-equal? (interp-fns
               (list (FundefC 'f '(x) (BinopC '+ 'x 'x)) (FundefC 'main '() (AppC 'f (NumC 1))))) 2)




;;parser in Arith takes in an s-expression and returns a corresponding ArithC or signals an error
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? n) (NumC n)]
    [(? symbol? s) (IdC s)]
    [(list (? symbol? op) l r) (if (member op '(+ - * /))
                                   (BinopC op (parse l) (parse r))
                                   (AppC op (cast (map parse (rest s)) (Listof ExprC))))]
    [(list 'ifleq0? a b c) (ifleq0? (parse a) (parse b) (parse c))]
    [other (error 'parse "AAQZ expected a valid Sexp, got ~e" other)]))

(check-equal? (parse '7) (NumC 7))
(check-equal? (parse '{* {+ 2 3} 7}) (BinopC '* (BinopC '+ (NumC 2) (NumC 3)) (NumC 7)))
(check-equal? (parse '{ifleq0? 5 1 0}) (ifleq0? (NumC 5) (NumC 1) (NumC 0)))
(check-exn #rx"AAQZ" (lambda () (parse "hi")))
(check-equal? (parse '{f 1 2}) (AppC 'f (list (NumC 1) (NumC 2))))
(check-equal? (parse 'f) (IdC 'f))




;; helper function to check for duplicate parameters
(define (has-dup [lst : (Listof Symbol)]) : Boolean
  (cond
    [(or (empty? lst) (empty? (rest lst))) #f]
    [(member (first lst) (rest lst)) #t]
    [else (has-dup (rest lst))]))

(check-equal? (has-dup '{h h}) #t)
(check-equal? (has-dup '{w s}) #f)




;; parser that takes s-expression and returns FundefC's
(define (parse-fundef [s : Sexp]) : FundefC
  (match s
    [(list 'def (? symbol? name) (list (list params ...)
                                       '=> body))
     (if (has-dup (cast (first (third s)) (Listof Symbol)))
         (error 'parse-fundef "AAQZ duplicate arg")
         (FundefC name (cast (first (third s)) (Listof Symbol)) (parse body)))]
    [other (error 'parse-fundef "AAQZ improper syntax ~e" other)]))

(check-equal? (parse-fundef '{def f {(x y) => {+ x y}}}) (FundefC 'f '(x y) (BinopC '+ (IdC 'x) (IdC 'y))))
(check-equal? (parse-fundef '{def g {() => 5}}) (FundefC 'g '() (NumC 5)))
(check-exn #rx"AAQZ" (lambda () (parse-fundef '{def err {(x x) => x}})))
(check-exn #rx"AAQZ" (lambda () (parse-fundef '{def err {(x y)}})))




;; parser that takes s-expression with FundefC and returns list of FundefC
(define (parse-prog [s : Sexp]) : (Listof FundefC)
  (match s
    [(list r ...) (map parse-fundef s)]
    [other (error 'parse-prog "AAQZ improper syntax ~e" other)]))

(check-equal? (parse-prog '{{def f {(x y) => {+ x y}}}
                            {def y {(y x) => {* y x}}}})
              (list (FundefC 'f '(x y) (BinopC '+ (IdC 'x) (IdC 'y)))
                    (FundefC 'y '(y x) (BinopC '* (IdC 'y) (IdC 'x)))))
(check-exn #rx"AAQZ" (lambda () (parse-prog 'def)))




;; top-interp takes in s-expression and
;; calls parse and interp, reducing the
;; expression to a value
#;(define (top-interp [fun-sexps : Sexp]) : Real
    (interp-fn (parse-prog fun-sexps)))

#;(check-equal? (top-interp '{{def f {(x y) => {+ x y}}}
                              {def main {() => {f 1 2}}}}) 3)


