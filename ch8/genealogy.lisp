;;; genealogy.lisp
;;;
;;; author: elias garcia
;;; version: 25.7.17
;;; license: WTFPL
;;;
;;; commentary:
;;; genealogy.lisp is my solution set for Touretzky's CH8 keyboard exercise for recursion.
;;;
;;; Where I feel it appropriate, I include the author's solutions (aka, when my solution turned out
;;; to be incredibly hacky or just bad).
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
   (second (assoc person family)))

(defun MOTHER (person)
   "returns the mother of a given person from the family dbs"
   (third (assoc person family)))

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
                            (children (father person)))
            (list person)))))

;; textbook solution because mine clearly was not as good in comparison

(defun SIBLINGS-SOLN (person)
   (set-difference (union (children (father person)
                             (children (mother person)))
                      (list person))))

;; problem C) description:
;; Create MAPUNION applicative operator that takes a fn and a list, applies the fn to each elem in
;; the list, then applies union to the results of the previous operation

(defun MAPUNION (fn ls)
   "takes a function and a list, applies function to each elem in list and then union on results"
   (reduce #'union
      (funcall #'(lambda (elem)
                    (mapcar fn elem))
         ls)))


;; textbook solution: wowzers do i suck at lisp
(defun MAPUNION-SOLN (fn ls)
   (and ls (reduce #'union (mapcar fn ls))))

;; problem D) description:
;; write function GRANDPARENTS that uses MAPUNION to return the grandparents of a person

(defun GRANDPARENTS (person)
   "returns the grandparents for a given person from family dbs"
   (mapunion #'parents (parents person)))

;; problem E) description:
;; write function COUSINS that uses MAPUNION to return the cousins of a person

(defun COUSINS (person)
   "returns the cousins for a given person from family dbs"
   (mapunion #'children
      (mapunion #'siblings
         (parents person))))

;; problem F) description:
;; write a 2 input recursive DESCENDED-FROM function that takes two person and returns t if the
;; first input  is a descendant of the second.

(defun DESCENDED-FROM (descendant descendee)
   "takes two persons and determines whether the first is a descendant of the second, returning t"
   (cond ((or (null descendant)
             (null descendee))
            nil)
      ((member descendee (parents descendant))
         t)
      (t (or (descended-from (father descendant) descendee)
            (descended-from (mother descendant) descendee)))))

;; problem G) description:
;; write recursive function ANCESTORS that returns a person's set of ancestors

(defun ANCESTORS (person)
   "returns the ancestors for the given person from the family dbs"
   (cond ((or (null person)
             (null (parents person)))
            nil)
      ((null (grandparents person))
         (parents person))
      (t (union (parents person)
            (mapunion #'ancestors (parents person))))))

;; problem H) description:
;; write a 2 input recursive function GENERATION-GAP that returns the number of generations
;; separating a person and one of their ancestors.

(defun GENERATION-GAP (descendant descendee)
   "returns the number of generations between two relatives from the family dbs"
   (cond ((or (null (descended-from descendant descendee))
             (null (or descendant descendee)))
            nil)
      ((or (equal (mother descendant) descendee)
          (equal (father descendant) descendee))
         (list 1))
      (t (list (cond ((descended-from (mother descendant) descendee)
                  (reduce #'+ (cons '1 (generation-gap (mother descendant) descendee))))
            ((descended-from (father descendant) descendee)
               (reduce #'+ (cons '1 (generation-gap (father descendant) descendee)))))))))


;; textbook solution for problem H) for comparison against my horribly hacky code

(defun GENERATION-GAP-SOLN (descendant descendee)
   (gen-gap-helper descendant descendee 0))

(defun GEN-GAP-HELPER (descendant descendee n)
   (cond ((null descendant) nil)
      ((equal descendant descendee) n)
      (t (or (gen-gap-helper (father descendant) descendee (1+ n))
            (gen-gap-helper (mother descendant) descendee (1+ n))))))
