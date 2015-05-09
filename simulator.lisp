(defpackage cutboard-simulator
  (:use :common-lisp
        :iterate)
  (:export :simulate))

(in-package :cutboard-simulator)

(defstruct (machine (:constructor machine (table code genv lenv data return)))
  table
  code
  genv
  lenv
  data
  return)

(defun make-jump-table (code)
  (let ((table (make-hash-table)))
    (iter (for l in code)
          (for c on code)
          (when (symbolp l)
            (setf (gethash l table) c)))
    table))

(defun lookup-lenv (machine i j)
  (aref (nth i (machine-lenv machine)) j))

(defun set-lenv (machine i j val)
  (setf (aref (nth i (machine-lenv machine)) j)
	val))

(defun lookup-genv (machine sym)
  (gethash sym (machine-genv machine)))

(defun set-genv (machine sym val)
  (setf (gethash sym (machine-genv machine)) val))

(defun jump-to (machine label)
  (setf (machine-code machine) (gethash label (machine-table machine))))

(defun make-frame (data n)
  (let ((frame (make-array n)))
    (iter (for i from 0 below n)
          (for d in data)
          (setf (aref frame i) d))
    frame))

(defun push-frame (frame machine)
  (push frame (machine-lenv machine)))

(defun make-machine (code)
  (machine (make-jump-table code) code (make-hash-table) nil nil nil))

(defmacro inst-bind (value &rest clauses)
  (let ((operands (gensym)))
    `(if (consp ,value)
         (let ((,operands (cdr ,value)))
           (ecase (car ,value)
             ,@(mapcar (lambda (x)
                         (destructuring-bind (matcher &body body) x
                           (if (eq matcher :otherwise)
                               `(otherwise ,@body)
                               `(,(car matcher)
                                  (destructuring-bind ,(cdr matcher) ,operands
                                    ,@body)))))
                       clauses)))
         (progn ,@(cdr (assoc :otherwise clauses))))))

(defun run1 (machine)
  (destructuring-bind (head &rest rest) (machine-code machine)
    (let ((halt (equal '(:halt) head)))
      (setf (machine-code machine) rest)
      (inst-bind head
        ;; control
        ((:const data)
         (push data (machine-data machine)))
        ((:lvar i j)
         (push (lookup-lenv machine i j) (machine-data machine)))
	((:lset i j)
	 (set-lenv machine i j (pop (machine-data machine))))
        ((:gvar sym)
         (push (lookup-genv machine sym) (machine-data machine)))
	((:gset sym)
	 (set-genv machine sym (pop (machine-data machine))))
        ((:pop)
         (pop (machine-data machine)))
        ((:tjump label)
         (when (pop (machine-data machine))
           (jump-to machine label)))
        ((:fjump label)
         (unless (pop (machine-data machine))
           (jump-to machine label)))
        ((:jump label)
         (jump-to machine label))
        ((:ret)
         (destructuring-bind (pc . env) (pop (machine-return machine))
           (setf (machine-code machine) pc)
           (setf (machine-lenv machine) env)))
        ((:args n)
         (let ((data (machine-data machine)))
           (push-frame (make-frame data n) machine)
           (setf (machine-data machine)
                 (nthcdr n (machine-data machine)))))
        ((:call)
         (push (cons (machine-code machine)
                     (machine-lenv machine))
               (machine-return machine))
         (destructuring-bind (label . env) (pop (machine-data machine))
           (jump-to machine label)
           (setf (machine-lenv machine) env)))
        ((:fn label)
         (push (cons label (machine-lenv machine))
               (machine-data machine)))
        ((:halt) nil)
        ;; arithmetic
        ((:+)
         (push (+ (pop (machine-data machine))
                  (pop (machine-data machine)))
               (machine-data machine)))
        ((:-)
         (push (- (pop (machine-data machine))
                  (pop (machine-data machine)))
               (machine-data machine)))
        ((:=)
         (push (= (pop (machine-data machine))
                  (pop (machine-data machine)))
               (machine-data machine)))
        ((:shl)
         (push (ash (pop (machine-data machine))
                    (pop (machine-data machine)))
               (machine-data machine)))
        ((:shr)
         (push (ash (pop (machine-data machine))
                    (- (pop (machine-data machine))))
               (machine-data machine)))
        ;; cons
        ((:cons)
         (push (cons (pop (machine-data machine))
                     (pop (machine-data machine)))
               (machine-data machine)))
        ((:car)
         (push (car (pop (machine-data machine)))
               (machine-data machine)))
        ((:cdr)
         (push (cdr (pop (machine-data machine)))
               (machine-data machine)))
        ;; IO
        ((:print-byte)
         (format t "~c" (code-char (mod (pop (machine-data machine)) 256))))
        ;; others
        (:otherwise (unless (symbolp head)
                      (error "no matching instruction"))))
      (values machine halt))))

(defun run* (machine)
  (multiple-value-bind (m fin) (run1 machine)
    (if fin
        machine
        (run* m))))

(defun simulate (code)
  (run* (make-machine code)))
