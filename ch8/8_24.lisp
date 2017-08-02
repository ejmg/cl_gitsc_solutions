;;; 8_24.lisp
;;;
;;; version: 1.8.17
;;; license: WTFPL
;;;
;;; commentary:
;;; 8_24.lisp is my solution for exercise 8.24 in subsection 8.12.1 of Touretzky's textbook
;;;
;;; problem description:
;;; write COUNT-DOWN, function that counts down from n using list-consing recursion.
;;;
;;; code:

(defun count-down (n)
  (cond ((null n) nil)
        ((zerop n) nil)
        (t (cons n (count-down (- n 1))))))
