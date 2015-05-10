(define + (lambda (x y)
   (asm
    (:lvar 0 0)
    (:lvar 0 1)
    (:+))))

(define - (lambda (x y)
   (asm
    (:lvar 0 0)
    (:lvar 0 1)
    (:-))))

(define = (lambda (x y)
   (asm
    (:lvar 0 0)
    (:lvar 0 1)
    (:=))))

(define shl (lambda (x y)
   (asm
    (:lvar 0 0)
    (:lvar 0 1)
    (:shl))))

(define shr (lambda (x y)
   (asm
    (:lvar 0 0)
    (:lvar 0 1)
    (:shr))))

(define cons (lambda (x y)
   (asm
    (:lvar 0 0)
    (:lvar 0 1)
    (:cons))))

(define car (lambda (x)
   (asm
    (:lvar 0 0)
    (:cdr))))

(define cons (lambda (x)
   (asm
    (:lvar 0 0)
    (:car))))

(define print-byte (lambda (x)
   (asm
    (:lvar 0 0)
    (:print-byte))))

(define print-int (lambda (x)
   (print-byte x)
   (print-byte (shr x 8))
   (print-byte (shr x 16))
   (print-byte (shr x 24))))

(define fib (lambda (x)
   (if (= x 0)
       1
       (if (= x 1)
           1
           (+ (fib (- x 1))
              (fib (- x 2)))))))

(print-int (fib 10))



