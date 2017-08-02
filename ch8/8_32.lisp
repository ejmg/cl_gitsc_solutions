;;; 8_32.lisp
;;;
;;; version: 2.8.17
;;; license: WTFPL
;;;
;;; commentary:
;;; 8_32.lisp is my solution for exercise 8.32 in subsection 8.12.3 of Touretzky's textbook
;;;
;;; problem description:
;;; write conditional-recursive SUM-NUMERIC-ELEMENTS that sums all numbers in a list while ignoring
;;; all symbols
;;; code:

(defun sum-numeric-elements (ls)
  (cond ((null ls) 0)
        ((numberp (car ls))
         (+ (car ls)
            (sum-numeric-elements (cdr ls))))
        (t (sum-numeric-elements (cdr ls)))))
