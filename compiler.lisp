(defpackage cutboard-compiler
  (:use :common-lisp
        :iterate)
  (:export :comp-top
           :asm))

(in-package :cutboard-compiler)

;;; utilities
(defun singlep (l)
  (and (consp l) (null (cdr l))))

(defparameter *label-id* 0)

(defun gen-label (str)
  (prog1 (gensym  (format nil "~a~a" str *label-id*))
    (incf *label-id*)))

;;; env
(defun make-topenv ()
  nil)

(defun extend-env (env args)
  (cons args env))

(defun locate-var (var env)
  (iter (for i from 0)
        (for frame in env)
        (iter (for j from 0)
              (for var1 in frame)
              (when (eq var var1)
                (return-from locate-var (list i j)))))
  :toplevel)

;;; generator
(defun gen (&rest args)
  (values (list args) nil))

(defmacro seq (&rest body)
  (if (null body)
      `(values nil nil)
      (let ((code1 (gensym))
            (code2 (gensym))
            (fns1 (gensym))
            (fns2 (gensym)))
        `(multiple-value-bind (,code2 ,fns2) (seq ,@(cdr body))
           (multiple-value-bind (,code1 ,fns1) ,(car body)
             (values (append ,code1 ,code2) (append ,fns1 ,fns2)))))))

;;; compiler
(defun label (label)
  (values (list label) nil))

(defun comp-top (code)
  (multiple-value-bind (code fn-codes) (comp code (make-topenv))
    (append code (gen :halt) (reduce #'append fn-codes))))

(defun comp (code env)
  (cond
    ((symbolp code)
     (comp-ref code env))
    ((atom code)
     (gen :const code))
    (t
     (case (first code)
       (quote  (gen :const (second code)))
       (begin  (comp-begin (rest code) env))
       (if     (destructuring-bind (_ pred then else) code
                 (declare (ignore _))
                 (comp-if pred then else env)))
       (lambda (destructuring-bind (_ args &rest body) code
                 (declare (ignore _))
                 (comp-lambda args body env)))
       (asm    (destructuring-bind (_ &body body) code
                 (declare (ignore _))
                 (values body nil)))
       (otherwise (seq (comp-args (cdr code) env)
		       (comp (first code) env)
		       (gen :call)))))))

(defun comp-ref (code env)
  (let ((loc (locate-var code env)))
    (if (eq loc :toplevel)
        (gen :gvar code)
        (gen :lvar (first loc) (second loc)))))

(defun comp-begin (body env)
  (cond ((null body) (gen :const nil))
	((singlep body) (comp (car body) env))
	(t (seq (comp (car body) env)
		(gen :pop)
		(comp-begin (cdr body) env)))))

(defun comp-args (body env)
  (cond ((null body) (values nil nil))
	((singlep body) (comp (car body) env))
	(t (seq (comp (car body) env)
		(comp-args (cdr body) env)))))

(defun comp-if (pred then else env)
  (let ((label1 (gen-label "if-body"))
        (label2 (gen-label "if-else")))
    (seq (comp pred env)
         (gen :fjump label1)
         (comp then env)
         (gen :jump label2)
         (label label1)
         (comp else env)
         (label label2))))

(defun comp-lambda (args body env)
  (let ((new-env (extend-env env args)))
    (multiple-value-bind (label fn-code) (make-function args body new-env)
      (values (gen :fn label)
              fn-code))))

(defun make-function (args body env)
  (let ((fn (gen-label "func"))
        (new-env (extend-env env args)))
    (multiple-value-bind (c1 c2) (seq (label fn)
                                      (gen :args (length args))
                                      (comp-begin body new-env)
                                      (gen :ret))
      (values fn (cons c1 c2)))))
