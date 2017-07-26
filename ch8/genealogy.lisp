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

;; problem A) description:
;; Create functions FATHER, MOTHER, PARENTS, and CHILDREN that return the respective names 
;; of the individuals by given person's relation. Return nil if none exist or nil is provided.

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

;; problem B) description:
;; Create SIBLINGS fn that returns list of a person's siblings, including *genetic half-siblings*

(defun SIBLINGS (person)
   "finds the biological siblings, including step, of a given person from the family dbs"
   (cond ((null person) nil)
      ((and
          (null (father person))
          (null (mother person)))
         nil)
      ((null (father person))
         (children (mother person)))
      ((null (mother person))
         (children (father person)))
      (t (set-difference (union (children (father person))
                            (children (father person))) (list person)))))

;; problem C) description:
;; Create MAPUNION applicative operator that takes a fn and a list, applies the fn to each elem in
;; the list, then applies union to the results of the previous operation

(defun MAPUNION (fn ls))
