;;; 8_10.lisp
;;;
;;; version: 1.8.17
;;; license: WTFPL
;;;
;;; commentary:
;;; 8_10.lisp is my solution for exercise 8.10 in subsection 8.8 of Touretzky's textbook
;;;
;;; problem description:
;;; for x, a non-negative int, and y, a positive int, x+y = x+1+(y-1). If y = 0, then x+y = x. Use
;;; these facts to build a recursive version of `+' called REC-PLUS out of ADD1, SUB1, COND, and
;;; ZEROP. 
;;;
;;; code:

(defun add1 (x)
  "adds 1 to x"
  (+ x 1))

(defun sub1 (x)
  "subtracts 1 from x"
  (- x 1))

(defun rec-plus (x y)
  "returns the sum of non-negative int x and positive int y"
  (cond ((zerop y) x)
        (t (rec-plus (add1 x) (sub1 y)))))
