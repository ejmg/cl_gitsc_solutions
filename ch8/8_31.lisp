;;; 8_31.lisp
;;;
;;; version: 2.8.17
;;; license: WTFPL
;;;
;;; commentary:
;;; 8_31.lisp is my solution for exercise 8.31 in subsection 8.12.2 of Touretzky's textbook
;;;
;;; problem description:
;;; write triple-simultaneously recursive funct COMPARE-LENGTHS that returns SAME-LENGTH,
;;; FIRST-IS-LONGER, or SECOND-IS-LONGER
;;; code:

(defun compare-lengths (ls-1 ls-2)
  (cond ((and (null ls-1)
              (null ls-2))
         'SAME-LENGTH)
        ((null ls-1) 'SECOND-IS-LONGER)
        ((null ls-2) 'FIRST-IS-LONGER)
        (t (compare-lengths (cdr ls-1) (cdr ls-2)))))
