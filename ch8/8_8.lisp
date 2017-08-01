;;; 8_8.lisp
;;;
;;; version: 1.8.17
;;; license: WTFPL
;;;
;;; commentary:
;;; 8_8.lisp is my solution for exercise 8.8 in subsection 8.8 of Touretzky's textbook
;;;
;;; problem description:
;;; write a recursive version of ASSOC
;;;
;;; code:

(defun my-assoc (key table)
  "searches table and returns entry containing key value as car if found"
  (cond ((null table) nil)
        ((equal key (car (car table))) (car table))
        (t (my-assoc key (cdr table)))))

;; test query:

(my-assoc 1 '((3 c) (4 d) (1 a) (2 b)))
