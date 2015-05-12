(defpackage :cutboard-retrokitchen
  (:use :common-lisp
        :iterate
        :retrokitchen-asm))

(in-package :cutboard-retrokitchen)

;;; register
(defparameter *zero  31)
(defparameter *1<<20 30)
(defparameter *dst   29)
(defparameter *rst   28)
(defparameter *lenv  27)
(defparameter *genv  26)
(defparameter *heap  25)
(defparameter *ret   24)
(defparameter *curheap 23)
(defparameter *limheap 22)

;;; tag
(defparameter *tsym      0)
(defparameter *tint      1)
(defparameter *tbool     2)
(defparameter *tconsp    3)
(defparameter *tfuncp    4)
(defparameter *tframep   5)
(defparameter *tcodep    6)
(defparameter *tconsh    7)
(defparameter *tfunch    8)
(defparameter *tframeh   9)
(defparameter *tdontcare 10)

;; memory
(defparameter *genv-addr #x03000)
(defparameter *dst-bottom #x04000)
(defparameter *rst-bottom #x7ffff)
(defparameter *heap0 #x80000)
(defparameter *heap1 #xc0000)
(defparameter *heap0-lim #xbffff)
(defparameter *heap1-lim #xfffff)

;;; utility
(defstruct (tword (:constructor make-tword (data &key (tag 15))))
  tag data)

(defun to-word (val)
  (cond ((eq val :f)    (make-tword 0 :tag *tbool))
        ((eq val :t)    (make-tword 1 :tag *tbool))
        ((symbolp val)  (make-tword (symbol-id val) :tag *tsym))
        ((integerp val) (make-tword (logand val (1- (ash 1 32))) :tag *tint))))

(defvar *label-id* 0)

(defun gen-label ()
  (intern (format nil "__cutboard_~a" (incf *label-id*)) :keyword))

(defvar *symbol-table* '((nil 0)))
(defvar *symbol-id* 1)

(defun symbol-id (sym)
  (let ((a (assoc sym *symbol-table*)))
    (if a
        (cdr a)
        (prog1 *symbol-id*
          (incf *symbol-id*)))))

(defun lower16 (n)
  (logand n (1- (ash 1 16))))

(defun upper16 (n)
  (let ((x (ash n -16)))
    (assert (<= 0 x (1- (ash 1 16))))
    x))

;;; mid-level instructions
(define-instruction :mov (from to)
  (microcode `((:addl/i ,from 0 ,to))))

(define-instruction :put-const (reg word)
  (microcode
   `((:lda  ,reg ,*zero ,(lower16 (tword-data word)))
     (:ldah ,reg ,reg   ,(upper16 (tword-data word)) :tag ,(tword-tag word)))))

(define-instruction :put-label (reg label)
  (if *preprocess*
      (microcode
       `((:lda  ,reg ,*zero 0)
         (:ldah ,reg ,reg   0 :tag 0)))
      (microcode
       (let ((tword (make-tword (label-addr label))))
         `((:lda  ,reg ,*zero ,(lower16 (tword-data tword)))
           (:ldah ,reg ,reg   ,(upper16 (tword-data tword)) :tag ,(tword-tag tword)))))))

(define-instruction :push-data (reg)
  (microcode
   `((:stl    ,reg ,*dst 1)
     (:addl/i ,*dst 1 ,*dst))))

(define-instruction :pop-data (reg)
  (microcode
   `((:ldl ,reg ,*dst 0)
     (:subl/i ,*dst 1 ,*dst))))

(define-instruction :push-ret (pcreg envreg)
  (microcode
   `((:subl/i ,*rst 2 ,*rst)
     (:stl ,pcreg ,*rst 0)
     (:stl ,envreg ,*rst 1))))

(define-instruction :pop-ret (pcreg envreg)
  `((:ldl ,pcreg ,*rst 0)
    (:ldl ,envreg ,*rst 1)
    (:addl/i ,*rst 2 ,*rst)))

(define-instruction :cur-env (reg)
  (microcode
   `((:mov ,*lenv ,reg))))

(define-instruction :func-pc (fn reg)
  `((:ldl ,reg ,fn 0 :tag ,*tfuncp)))

(define-instruction :func-env (fn reg)
  `((:ldl ,reg ,fn 1 :tag ,*tfuncp)))

;;; memory management
(define-instruction :allocate-frame (reg size)
  (microcode `((:allocate 0 ,(1+ size))
               (:ldah 0 0 0 :tag ,*tframep)
               (:ldah 1 1 ,(ash size 4) :tag ,*tframeh)
               (:bis 1 ,*lenv 2)
               (:stl 1 0 0 :tag ,*tframep)
               (:mov 0 ,reg))))

(define-instruction :allocate-cons (reg)
  (microcode `((:allocate 0 2)
               (:ldah 0 0 0 :tag ,*tconsp)
               (:mov 0 ,reg))))

(define-instruction :allocate-func (reg)
  (microcode `((:allocate 0 2)
               (:ldah 0 0 0 :tag ,*tfuncp)
               (:mov 0 ,reg))))

(define-instruction :allocate (reg size)
  (microcode `((:put-const 0 ,(to-word size))
               (:br ,*ret :gc-allocate)
               (:mov 0 ,reg))))

(define-instruction :copy-obj (obj preg)
  (microcode `())) ;;fixme

(define-instruction :for (reg from to step &rest code)
  (let ((tmp 16)
        (label1 (gen-label))
        (label2 (gen-label)))
    (microcode `((:mov ,from ,reg)
                 ,label1
                 (:cmple ,reg ,to ,tmp)
                 (:bne ,tmp ,label2)
                 ,@code
                 (:addl/i ,reg ,step ,reg)
                 (:br ,*zero ,label1)
                 ,label2))))

(define-instruction :while (pred &rest code)
  (let ((label1 (gen-label))
        (label2 (gen-label)))
    (microcode `(,label1
                 ,pred
                 (:bne 16 ,label2)
                 ,@code
                 (:br ,*zero ,label1)
                 ,label2))))

(define-instruction :cb-init ()
  nil)

(define-instruction :cb-lib ()
  (let ((set0 (gen-label))
        (set1 (gen-label))
        (st 0)
        (unscanned 1))
    (microcode `(:gc-allocate
                 (:addl 0 ,*curheap 2)
                 (:cmplt ,*limheap 2 3)
                 (:bne 2 :run-gc)
                 (:addl 0 ,*curheap 2)
                 (:cmplt ,*limheap 2 3)
                 (:bne 3 :gc-error)
                 (:sll/i 0 20 1)
                 (:mov ,*curheap 0)
                 (:mov 2 ,*curheap)
                 (:jmp ,*zero ,*ret 0)

                 :run-gc
                 (:beq ,*heap ,set0)
                 (:put-const ,*curheap ,(make-tword *heap1))
                 (:put-const ,*limheap ,(make-tword *heap1-lim))
                 (:put-const ,*heap ,(to-word 1))
                 (:br ,*zero ,set1)
                 ,set0
                 (:put-const ,*curheap ,(make-tword *heap0))
                 (:put-const ,*limheap ,(make-tword *heap0-lim))
                 (:put-const ,*heap ,(to-word 0))
                 ,set1
                 (:mov ,*curheap ,unscanned)
                 (:for ,st ,*dst-bottom ,*dst 1
                       (:copy-obj ,st ,unscanned))
                 (:for ,st ,*rst ,*rst-bottom 2
                       (:copy-obj ,st ,unscanned))
                 (:for ,st ,*genv ,(1- *dst-bottom) 1
                       (:copy-obj ,st ,unscanned))
                 (:while (:cmple ,*curheap ,unscanned 16)
                         (:copy-obj ,*curheap ,unscanned))

                 :gc-error
                 (:halt)))))

;;; macrocode instructions
(define-instruction :const (value)
  (unless *preprocess* (format *error-output* "(:const ~s)~%" value))
  (microcode
   `((:put-const 0 ,(to-word value))
     (:push-data 0))))

(define-instruction :lvar (i j)
  (unless *preprocess* (format *error-output* "(:lvar ~s ~s)~%" i j))
  (microcode
   `((:cur-env 0)
     ,@(iter (repeat i)
             (collect `(:env-parent 0 0)))
     (:ldl 1 0 ,(+ j 1))
     (:push-data 1))))

(define-instruction :gvar (sym)
  (unless *preprocess* (format *error-output* "(:gset ~s)~%" sym))
  (microcode
   `((:ldl 0 ,*genv ,(symbol-id sym))
     (:push-data 0))))

(define-instruction :lset (i j)
  (unless *preprocess* (format *error-output* "(:lset ~s ~s)~%" i j))
  (microcode
   `((:cur-env 0)
     ,@(iter (repeat i)
             (collect `(:env-parent 0 0)))
     (:pop-data 1)
     (:stl 1 0 ,(+ j 1)))))

(define-instruction :gset (sym)
  (unless *preprocess* (format *error-output* "(:gset ~s)~%" sym))
  (microcode
   `((:pop-data 0)
     (:stl 0 ,*genv ,(symbol-id sym)))))

(define-instruction :pop ()
  (unless *preprocess* (format *error-output* "(:pop)~%"))
  (microcode `((:pop-data 31))))

(define-instruction :tjump (label)
  (unless *preprocess* (format *error-output* "(:tjump ~s)~%" label))
  (microcode `((:pop-data 0)
               (:bne 0 ,label :tag ,*tbool))))

(define-instruction :fjump (label)
  (unless *preprocess* (format *error-output* "(:fjump ~s)~%" label))
  (microcode `((:pop-data 0)
               (:beq 0 ,label :tag ,*tbool))))

(define-instruction :jump (label)
  (unless *preprocess* (format *error-output* "(:label ~s)~%" label))
  (microcode `((:br ,*zero ,label))))

(define-instruction :ret ()
  (unless *preprocess* (format *error-output* "(:ret)~%"))
  (microcode `((:pop-ret 0 ,*lenv)
               (:jmp ,*zero 0 0 :tag ,*tcodep))))

(define-instruction :args (n)
  (unless *preprocess* (format *error-output* "(:args ~s)~%" n))
  (microcode `((:allocate-frame 0 ,n)
               ,@(iter (for i from 1 to n)
                       (collect `(:pop-data 1))
                       (collect `(:stl 1 0 ,i :tag ,*tframep)))
               (:mov 0 ,*lenv))))

(define-instruction :call ()
  (unless *preprocess* (format *error-output* "(:call)~%"))
  (microcode `((:br 2 1)
               (:push-ret ,2 ,*lenv)
               (:pop-data 0)
               (:func-pc  0 1)
               (:func-env 0 ,*lenv)
               (:jmp 31 1 0 :tag ,*tcodep))))

(define-instruction :fn (label)
  (unless *preprocess* (format *error-output* "(:fn ~s)~%" label))
  (microcode `((:allocate-func 0)
               (:put-label 1 ,label)
               (:stl 1 0 1 :tag ,*tframep)
               (:stl ,*lenv 0 2 :tag ,*tframep)
               (:push-data 0))))

(define-instruction :halt ()
  (unless *preprocess* (format *error-output* "(:halt)~%"))
  (microcode `((:br 31 0))))

;; arithmetic instruction
(define-instruction :+ ()
  (unless *preprocess* (format *error-output* "(:+)~%"))
  (microcode `((:pop-data 0)
               (:pop-data 1)
               (:addl 0 1 2 :tag ,*tint)
               (:push-data 2))))

(define-instruction :- ()
  (unless *preprocess* (format *error-output* "(:-)~%"))
  (microcode `((:pop-data 0)
               (:pop-data 1)
               (:addl 0 1 2 :tag ,*tint)
               (:push-data 2))))

(define-instruction := ()
  (unless *preprocess* (format *error-output* "(:=)~%"))
  (microcode `((:pop-data 0)
               (:pop-data 1)
               (:cmpeq 0 1 2 :tag ,*tint)
               (:ldah 3 2 0 :tag ,*tbool)
               (:push-data 3))))

(define-instruction :shl ()
  (unless *preprocess* (format *error-output* "(:shl)~%"))
  (microcode `((:pop-data 0)
               (:pop-data 1)
               (:sll 0 1 2 :tag ,*tint)
               (:push-data 2))))

(define-instruction :shr ()
  (unless *preprocess* (format *error-output* "(:shr)~%"))
  (microcode `((:pop-data 0)
               (:pop-data 1)
               (:srl 0 1 2 :tag ,*tint)
               (:push-data 2))))

;; cons instruction
(define-instruction :cons ()
  (unless *preprocess* (format *error-output* "(:cons)~%"))
  (microcode `((:allocate-cons 0)
               (:pop-data 1)
               (:pop-data 2)
               (:stl 1 0 1)
               (:stl 2 0 2)
               (:push-data 0))))

(define-instruction :car ()
  (unless *preprocess* (format *error-output* "(:car)~%"))
  (microcode `((:pop-data 0)
               (:ldl 1 0 0 :tag ,*tconsp)
               (:push-data 0))))

(define-instruction :cdr ()
  (unless *preprocess* (format *error-output* "(:cdr)~%"))
  (microcode `((:pop-data 0)
               (:ldl 1 0 1 :tag ,*tconsp)
               (:push-data 0))))

(define-instruction :print-byte ()
  (unless *preprocess* (format *error-output* "(:print-byte)~%"))
  (microcode `((:pop-ret 0 ,*lenv)
               (:stl 0 ,*1<<20 1))))
