;;; 8_17.lisp
;;;
;;; version: 1.8.17
;;; license: WTFPL
;;;
;;; commentary:
;;; 8_17.lisp is my solution for exercise 8.17 in subsection 8.11 of Touretzky's textbook
;;;
;;; problem description:
;;; use double-test tail recursion to write FIND-FIRST-ODD
;;;
;;; code:

(defun find-first-odd (ls)
  "finds the first odd number in a list"
  (cond ((null ls) nil)
        ((oddp (car ls)) (car ls))
        (t (find-first-odd (cdr ls)))))
