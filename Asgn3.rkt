#lang typed/racket
(require typed/rackunit)

;;Sofia Morris, Anissa Soungpanya
;;<Submission progress comment>

;;represents types of arithmetic extressions
(define-type ArithC (U NumC BinopC))
;;represents types of binary operations
(define-type BinopC (U PlusC MultC SubC DivC SquareC))
;;represents a number
(struct NumC([n : Real]) #:transparent)
;;represents addition
(struct PlusC([left : ArithC] [right : ArithC]) #:transparent)
;;represents multiplication
(struct MultC([left : ArithC] [right : ArithC]) #:transparent)
;;represents subtraction
(struct SubC([left : ArithC] [right : ArithC]) #:transparent)
;;represents division
(struct DivC([left : ArithC] [right : ArithC]) #:transparent)
;;represents squares
(struct SqrC([arg : ArithC]) #:transparent)

;;hashtable to lookup Binary operators
(define ht
  (make-immutable-hash '([PlusC . '+] [MultC . '*] [SubC . '-] [DivC . '/] [SqrC . '^2])))

;;takes in an arithmetic expression and reduces it to its value
(define (interp [exp : ArithC]) : Real
  (match exp 
    [(NumC n) n]
    [() ("hi")]))

;;parser in Arith takes in an s-expression and returns a corresponding ArithC or signals an error