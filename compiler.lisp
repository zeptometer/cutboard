(defpackage cutboard-compiler
  (:import :common-lisp)
  (:export :compile))

(defun compile (code env)
  (cond
    ((symbolp code)
     (compile-ref code env))
    ((atom code)
     (gen 'const code))
    (t
     (case (first code)
       (quote  (gen 'const (second code)))
       (begin  (compile-begin (rest code) env))
       (if     (destructuring-bind (_ pred then else) code
		(compile-if pred then else env)))
       (lambda (destructuring-bind (_ args &rest body) code
		 (gen 'fn (compile-lambda args body env))))
       (otherwise (seq (reduce #'append (mapcar (lambda (x) (compile x env))
						(rest code)))
		       (compile (first x env))
		       (gen 'call (length (rest x)))))))))

(defun singlep (l)
  (and (consp l) (null (cdr l))))

(defun compile-begin (body env)
  (cond ((null body) (gen 'const nil))
	((singlep body) (comp (car body) env))
	(t (seq (compile (car body) env)
		(gen 'pop)
		(compile-begin (cdr body) env)))))

(defun compile-)
