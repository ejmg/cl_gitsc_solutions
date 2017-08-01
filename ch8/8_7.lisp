;;; 8_7.lisp
;;;
;;; version: 31.7.17
;;; license: WTFPL
;;;
;;; commentary:
;;; 8_7.lisp is my solution for exercise 8.7 in subsection 8.8 of Touretzky's textbook
;;;
;;; problem description:
;;; write a recursive version of MEMBER
;;;
;;; code:

(defun my-member (elem ls)
  "checks if given elem is member of list, returning sublist starting at elem if found"
  (cond ((null ls) nil)
        ((equal elem (car ls)) ls)
        (t (my-member elem (cdr ls)))))
