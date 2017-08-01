;;; 8_6.lisp
;;;
;;; author: elias garcia
;;; version: 31.7.17
;;; license: WTFPL
;;;
;;; commentary:
;;; 8_6.lisp is my solution for exercise 8.6 in subsection 8.8 of Touretzky's textbook
;;;
;;; 8.6: Write ALLODDP, a recursive function that returns T if all the numbers in a list are odd
;;;
;;; code:

;; note, solution is predicated on input being 1) a list 2) of ints
(defun alloddp (x)
  "returns t if all elems of list are odd ints"
  (cond ((null x) t)
        ((evenp (car x)) nil)
        (t (alloddp (cdr x)))))
