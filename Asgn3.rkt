#lang typed/racket
(require typed/rackunit)

;;Sofia Morris, Anissa Soungpanya
;;finished all steps of assignment

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
    ['/ (if (equal? r (NumC 0))
        (error 'binop-interp "AAQZ cannot divide by 0")
        (/ (interp l funs) (interp r funs)))]
    ['- (- (interp l funs) (interp r funs))]))


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


;; helper function that returns index of IdC in for
(define (find-IdC [s : Symbol][lst : (Listof Symbol)][index : Real]) : Real
  (cond
    [(equal? (first lst) s) index]
    [else (find-IdC s (rest lst) (+ index 1))]))

(check-equal? (find-IdC 'x (list 'x 'y) 0) 0)
(check-equal? (find-IdC 'y (list 'x 'y) 0) 1)


;; helper function to match IdC to ExprC in what
(define (replace-IdC [lst : (Listof ExprC)][index : Real]) : ExprC
  (cond
    [(= index 0) (first lst)]
    [else (replace-IdC (rest lst) (- index 1))]))

(check-equal? (replace-IdC (list (NumC 1)(NumC 2)) 1) (NumC 2))

;; helper function to see if IdC is in what
(define (symbol-in-lst [s : Symbol][lst : (Listof Symbol)]) : Boolean
  (cond
    [(empty? lst) #f]
    [(equal? s (first lst)) #t]
    [else (symbol-in-lst s (rest lst))]))

(check-equal? (symbol-in-lst 'x '()) #f)


;; helper function to interpret applications
(define (subst [what : (Listof ExprC)][for : (Listof Symbol)][in : ExprC]) : ExprC
  (match in
    [(NumC n) in]
    [(IdC s) (cond
               [(symbol-in-lst s for) (replace-IdC what (find-IdC s for 0))]
               [else in])]
    [(AppC f args) (AppC f (for/list ([x args]) (subst what for x)))]
    [(ifleq0? a b c) (ifleq0? (subst what for a) (subst what for b) (subst what for c))]
    [(BinopC op l r) (if (not (= (length what) (length for)))
                         (error 'subst "AAQZ number of args does not match number of params")
                         (BinopC op
                                 (subst what for l)
                                 (subst what for r)))]))

(check-equal? (subst (list (NumC 1)(NumC 2)) '(x y) (BinopC '+ (IdC 'x)(IdC 'y)))
              (BinopC '+ (NumC 1)(NumC 2)))
(check-equal? (subst '() '() (AppC 'g '())) (AppC 'g '()))
(check-equal? (subst (list (NumC 1)) '(x) (AppC 'g (list (IdC 'x)))) (AppC 'g (list (NumC 1))))
(check-equal? (subst (list (NumC 3)) '(a) (IdC 'b)) (IdC 'b))
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
(check-equal? (interp (AppC 'f (list (NumC 1)(AppC 'f (list (NumC 1)(NumC 1)))))
                      (list (FundefC 'f '(x y) (BinopC '+ (IdC 'x)(IdC 'y))))) 3)
(check-exn #rx"AAQZ" (lambda () (interp (BinopC '/ (NumC 2) (NumC 0)) '())))


;; takes in a list of function definitions and returns the final value
;; differentiate main function from other function definitions and calls
;; interp using application in main and rest of functions
(define (interp-fns [funs : (Listof FundefC)]) : Real
  (cond
    [(empty? funs) (error 'interp-fns "AAQZ given empty list of functions")]
    [else (define main-fn (filter (lambda ([fn : FundefC])
                                    (equal? 'main (FundefC-name fn))) funs))
          (if (empty? main-fn)
              (error 'interp-fns "AAQZ main not found")
              (interp (FundefC-body (first main-fn)) funs))]))

(check-equal? (interp-fns
               (list (FundefC 'f '(x) (BinopC '+ (IdC 'x) (IdC 'x)))
                     (FundefC 'main '() (AppC 'f (list (NumC 1)))))) 2)
(check-exn #rx"AAQZ" (lambda () (interp-fns (list ))))
(check-exn #rx"AAQZ" (lambda () (interp-fns
               (list (FundefC 'f '(x) (BinopC '+ (IdC 'x) (IdC 'x)))))))
(check-equal? (interp-fns
               (list (FundefC 'f '() (NumC 5)) (FundefC 'main '() (BinopC '+ (AppC 'f '()) (AppC 'f '()))))) 10)




;;take in a sexp representing an id and check that it is a symbol that is not a
;;+, - , *, /, def, ifleq0?, or =>
;;returns true if valid and false if not valid
(define (valid-id? [id : Sexp]) : Boolean
  (match id
    [(? symbol?) (match id
                   ['+ #f]
                   ['- #f]
                   ['* #f]
                   ['/ #f]
                   ['def #f]
                   ['ifleq0? #f]
                   ['=> #f]
                   [else #t])]
    [else #f]))

(check-equal? (valid-id? 's) #t)
(check-equal? (valid-id? '+) #f)
(check-equal? (valid-id? '-) #f)
(check-equal? (valid-id? '*) #f)
(check-equal? (valid-id? '/) #f)
(check-equal? (valid-id? 'def) #f)
(check-equal? (valid-id? 'ifleq0?) #f)
(check-equal? (valid-id? '=>) #f)
(check-equal? (valid-id? 5) #f)

;;takes in a sexp function name and checks that it is a symbol and returns false if the
;;symbol is 'main and true otherwise
(define (valid-name? [fun : Sexp]) : Boolean
    (match fun
    [(? symbol?) (match fun
                     ['+ #f]
                     ['- #f]
                     ['* #f]
                     ['/ #f]
                     ['def #f]
                     ['ifleq0? #f]
                     ['=> #f]
                     [else #t])]
    [else #f]))

(check-equal? (valid-name? 'add) #t)
(check-equal? (valid-name? 5) #f)
(check-equal? (valid-name? '+) #f)
(check-equal? (valid-name? '-) #f)
(check-equal? (valid-name? '*) #f)
(check-equal? (valid-name? '/) #f)
(check-equal? (valid-name? 'def) #f)
(check-equal? (valid-name? 'ifleq0?) #f)
(check-equal? (valid-name? '=>) #f)

;;takes in a sexp operator and checks that it is a symbol and returns true if the
;;symbol is a valid binary operator and false if not
(define (valid-op? [op : Sexp]) : Boolean
    (match op
    [(? symbol?) (match op
                     ['+ #t]
                     ['- #t]
                     ['* #t]
                     ['/ #t]
                     [else #f])]
    [else #f]))

(check-equal? (valid-op? '+) #t)
(check-equal? (valid-op? '-) #t)
(check-equal? (valid-op? '*) #t)
(check-equal? (valid-op? '/) #t)
(check-equal? (valid-op? '=>) #f)
(check-equal? (valid-op? 6) #f)

;;parser in Arith takes in an s-expression and returns a corresponding ArithC or signals an error
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? n) (NumC n)]
    [(? valid-id? id) (IdC (cast id Symbol))]
    [(list (? valid-name? f)) (AppC (cast f Symbol) '())]
    [(list (? valid-name? f) r ...)
     (AppC (cast f Symbol) (cast (map parse (rest s)) (Listof ExprC)))]
    [(list (? valid-op? op) l r) (BinopC (cast op Symbol) (parse l) (parse r))]
    [(list 'ifleq0? a b c) (ifleq0? (parse a) (parse b) (parse c))]
    [other (error 'parse "AAQZ expected a valid Sexp, got ~e" other)]))

(check-equal? (parse '7) (NumC 7))
(check-equal? (parse '{* {+ 2 3} 7}) (BinopC '* (BinopC '+ (NumC 2) (NumC 3)) (NumC 7)))
(check-equal? (parse '{ifleq0? 5 1 0}) (ifleq0? (NumC 5) (NumC 1) (NumC 0)))
(check-exn #rx"AAQZ" (lambda () (parse "hi")))
(check-equal? (parse '{+ {f} {f}}) (BinopC '+ (AppC 'f '()) (AppC 'f '())))


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
    [(list 'def (? valid-name? name) (list (list params ...)
                                       '=> body))
     (if (has-dup (cast (first (third s)) (Listof Symbol)))
         (error 'parse-fundef "AAQZ duplicate arg")
         (FundefC (cast name Symbol) (cast (first (third s)) (Listof Symbol)) (parse body)))]
    [other (error 'parse-fundef "AAQZ improper syntax ~e" other)]))

(check-equal? (parse-fundef '{def f {(x y) => {+ x y}}})
              (FundefC 'f '(x y) (BinopC '+ (IdC 'x) (IdC 'y))))
(check-equal? (parse-fundef '{def g {() => 5}}) (FundefC 'g '() (NumC 5)))
(check-exn #rx"AAQZ" (lambda () (parse-fundef '{def err {(x x) => x}})))
(check-exn #rx"AAQZ" (lambda () (parse-fundef '{def err {(x y)}})))
(check-equal? (parse-fundef '{def f {() => 5}}) (FundefC 'f '() (NumC 5)))
(check-equal? (parse-fundef '{def main {() => {+ {f} {f}}}})
              (FundefC 'main '() (BinopC '+ (AppC 'f '()) (AppC 'f '()))))
(check-exn  #rx"AAQZ" (lambda () (parse-fundef '(def + (() => 13)))))


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
(check-equal? (parse-prog '{{def f {() => 5}}
                      {def main {() => {+ {f} {f}}}}})
              (list (FundefC 'f '() (NumC 5))
                    (FundefC 'main '() (BinopC '+ (AppC 'f '()) (AppC 'f '())))))



;; top-interp takes in s-expression and
;; calls parse and interp, reducing the
;; expression to a value
(define (top-interp [fun-sexps : Sexp]) : Real
    (interp-fns (parse-prog fun-sexps)))

(check-equal? (top-interp '{{def f {(x y) => {+ x y}}}
                              {def main {() => {f 1 2}}}}) 3)

(check-equal? (top-interp '{{def f {() => 5}}
                            {def main {() => {+ {f} {f}}}}}) 10)

(check-equal? (top-interp '{{def main {() => {twice {minus 8 5}}}}
                            {def minus {(x y) => {+ x {* -1 y}}}}
                            {def twice {(x) => {* 2 x}}}} ) 6)

(check-equal? (top-interp '{{def main {() => {+ {f 13} {f 0}}}}
                            {def f {(qq) => {ifleq0? qq qq (+ qq 1)}}}}) 14)

(check-equal? (top-interp '{{def f {(x y z) => {g y z x}}}
                            {def g {(a b c) => b}}
                            {def main {() => {f 1 2 3}}}}) 3)

(check-exn #rx"AAQZ" (lambda () (top-interp '{{def ignoreit {(x) => {+ 3 4}}}
                            {def main {() => {ignoreit {/ 1 {+ 0 0}}}}}})))
(top-interp '{{def ignoreit {(x) => {+ 3 4}}}
                            {def main {() => {ignoreit {/ 1 {+ 0 0}}}}}})

;;expected exception with message containing AAQZ on test expression:
;;'(top-interp '((def ignoreit ((x) => (+ 3 4))) (def main (() => (ignoreit (/ 1 (+ 0 0)))))))
