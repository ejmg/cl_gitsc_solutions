;;; blocks_world.lisp
;;;
;;; author: elias garcia
;;; version: 20.7.17
;;; license: WTFPL
;;;
;;; commentary:
;;; blocks_world.lisp is my solution set for Touretzky's CH7 keyboard exercise for
;;; functional programming
;;;
;;; code:

(load "~/projects/learning/cl_gitsc_solutions/ch7/keyboard_exercise/blocks_world_database.lisp")

(defun MATCH-ELEMENT (elem-1 elem-2)
   "checks if two symbols are equal OR if one is a ? mark, returning true and nil otherwise"
   (cond ((equal elem-1 elem-2))
      ((equal elem-1 '\?))
      ((equal elem-2 '\?))))

(defun MATCH-TRIPLE (assertion pattern)
   "takes an assertion and pattern, returns T if assertion matches pattern, nil otherwise"
   (every #'match-element assertion pattern))

(defun FETCH (pattern)
   "takes a pattern and returns all entries in the database that match"
   (remove-if-not #'(lambda (entry)
                (match-triple entry pattern))
      database))

(defun RETURN-PATTERN (block-name)
   "Given a block name, returns relevant query to find its color"
   (list block-name 'color '\?))

(defun RETURN-SHAPE (block-name)
   "given a block, returns relevant query to find its shape"
   (list block-name 'shape '\?))

(defun SUPPORTERS (block-name)
   "given a block-name, returns all blocks that support it"
   (mapcar #'third (fetch
                      (cons block-name '(supported-by \?)))))

(defun SUPP-CUBE (block-name)
   (mapcar #'(lambda (supporter)
                (member 'cube (car (fetch (return-shape supporter)))))
      (supporters block-name)))

(defun supp-cube (block-name)
   (mapcar #'(lambda (supporter)
                (if (member 'cube (car (fetch (return-shape supporter)))) t))
      (supporters block-name)))
