;;; 8_9.lisp
;;;
;;; version: 1.8.17
;;; license: WTFPL
;;;
;;; commentary:
;;; 8_9.lisp is my solution for exercise 8.9 in subsection 8.8 of Touretzky's textbook
;;;
;;; problem description:
;;; write a recursive version of NTH
;;;
;;; code:

;; assumes given valid int and ls
(defun my-nth (index ls)
  "returns nth elem of list"
  (cond ((null ls) nil)
        ((zerop index) (car ls))
        (t (my-nth (- index 1) (cdr ls)))))
