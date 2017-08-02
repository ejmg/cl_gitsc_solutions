;;; 8_26.lisp
;;;
;;; version: 1.8.17
;;; license: WTFPL
;;;
;;; commentary:
;;; 8_26.lisp is my solution for exercise 8.26 in subsection 8.12.1 of Touretzky's textbook
;;;
;;; problem description:
;;; write COUNT-DOWN, function that counts down from n using list-consing recursion ***and***
;;; that ends with 0
;;;
;;; code:

(defun count-down (n)
  (cond ((null n) nil)
        ((zerop n) (list 0))
        (t (cons n (count-down (- n 1))))))
