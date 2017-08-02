;;; 8_27.lisp
;;;
;;; version: 1.8.17
;;; license: WTFPL
;;;
;;; commentary:
;;; 8_27.lisp is my solution for exercise 8.27 in subsection 8.12.1 of Touretzky's textbook
;;;
;;; problem description:
;;; write SQUARE-LIST, a recursive function that takes a list of numbers as input and returns a list
;;; of their squares
;;;
;;; code:

(defun square-list (ls)
  (cond ((null ls) nil)
        (t (cons (* (car ls)
                    (car ls))
                 (square-list (cdr ls))))))
