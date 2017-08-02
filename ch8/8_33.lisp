;;; 8_33.lisp
;;;
;;; version: 2.8.17
;;; license: WTFPL
;;;
;;; commentary:
;;; 8_33.lisp is my solution for exercise 8.33 in subsection 8.12.3 of Touretzky's textbook
;;;
;;; problem description:
;;; write MY-REMOVE, recursive version of REMOVE
;;; code:

(defun my-remove (elem ls)
  (cond ((null ls) nil)
        ((equal elem (car ls))
         (my-remove elem (cdr ls)))
        (t (cons (car ls) (my-remove elem (cdr ls))))))
