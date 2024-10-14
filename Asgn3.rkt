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

;;hashtable to lookup Binary operators
(define ht
  #hash(['+ . +] ['* . *] ['- . -] ['/ . /]))

;;takes in an arithmetic expression and reduces it to its value
(define (interp [exp : ArithC])
  (match exp 
    [(NumC n) (printf "~a" n)]
    [(BinopC op l r)
     (define sign
       (hash-ref ht op)) (printf "~a" sign)]))

(interp (BinopC '+ (NumC 2) (NumC 2)))
;;parser in Arith takes in an s-expression and returns a corresponding ArithC or signals an error