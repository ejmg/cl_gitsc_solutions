;;; genealogy.lisp
;;;
;;; author: elias garcia
;;; version: 25.7.17
;;; license: WTFPL
;;;
;;; commentary:
;;; genealogy.lisp is my solution set for Touretzky's CH8 keyboard exercise for recursion
;;;
;;; code:


;; note: each database entry is of the format (name father mother)
;; so the person ivan with father george and mother ellen, the entry would be (ivan george ellen)

;;LOAD DATABASE 'FAMILY'
(load "~/projects/learning/cl_gitsc_solutions/ch8/family-database.lisp")

(defun FATHER (person)
   "returns the father of a given person from the family dbs"
   (if (null person)
      nil
      (second (assoc person family))))

(defun MOTHER (person)
   "returns the mother of a given person from the family dbs"
   (if (null person)
      nil
      (third (assoc person family))))

(defun PARENTS (person)
   "returns the parents of a given person from the family dbs"
   (if (null person) nil
      (remove-if #'null (list (father person) (mother person)))))

(defun CHILDREN (person)
   "returns the children of a given person from the family dbs"
   (if (null person)
      nil
      (remove-if #'null
         (mapcar #'(lambda (fam)
                      (cond ((equal person (father (car fam))) (car fam))
                         ((equal person (mother (car fam))) (car fam))))
            family))))
