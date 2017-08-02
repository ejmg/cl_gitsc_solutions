;;; 8_34.lisp
;;;
;;; version: 2.8.17
;;; license: WTFPL
;;;
;;; commentary:
;;; 8_34.lisp is my solution for exercise 8.34 in subsection 8.12.3 of Touretzky's textbook
;;;
;;; problem description:
;;; write MY-INTERSECTION, recursive version of INTERSECTION
;;; code:

(defun my-intersection (ls-1 ls-2)
  (cond ((or (null ls-1)
             (null ls-2))
         nil)
        ((member (car ls-1) ls-2)
         (cons (car ls-1)
               (my-intersection (cdr ls-1)
                                ls-2)))
        (t (my-intersection (cdr ls-1) ls-2))))
