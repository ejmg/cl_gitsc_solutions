;;; 8_22.lisp
;;;
;;; version: 1.8.17
;;; license: WTFPL
;;;
;;; commentary:
;;; 8_22.lisp is my solution for exercise 8.22 in subsection 8.11 of Touretzky's textbook
;;;
;;; problem description:
;;; write recursive ALL-EQUAL that returns T if the first elem is equal to the second, the second
;;; equal to third, so on, returning T for any list less than 2.
;;;
;;; code:

(defun all-eq (ls)
  "checks if all elems are equal in a list, returning T"
  (cond ((null ls) nil)
        ((equal 1 (length ls)) t)
        (t (and (equal (car ls) (second ls))
                (all-eq (cdr ls))))))
