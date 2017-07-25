(defun THROW-DIE ()
   "simulates a 6-sided dice by returning a random # from 1 to 6"
   (+ (random 6) 1))

(defun THROW-DICE ()
   "simulates throwing two 6-sided dice as a list"
   (list
      (+ (random 6) 1)
      (+ (random 6) 1)))

(defun SNAKE-EYES-P (roll)
   "predicate checks if rolled value is snake eyes, (1 1)"
   (let ((snake-eyes '(1 1)))
      (if (equal rolled snake-eyes) t nil)))

(defun BOXCARS-P (roll)
   "predicate checks if rolled value is boxcars, (6 6)"
   (let ((boxcars '(6 6)))
     (if (equal rolled boxcars) t nil)))

(defun INSTANT-WIN-P (roll)
   "predicate checks if rolled value is instant win value, 7 or 11"
   (let ((roll-sum (+ (car roll) (cadr roll))))
      (if (or
             (equal roll-sum 7)
             (equal roll-sum 11))
         t)))

(defun INSTANT-LOSS-P (roll)
   "predicate checks if rolled value is instant loss value, 2 or 3 or 12"
   (let ((roll-sum (+ (car roll) (cadr roll))))
      (if (or
             (equal roll-sum 2)
             (equal roll-sum 3)
             (equal roll-sum 12))
         t))
   )

(defun SAY-THROW (roll)
   "returns symbols SNAKE-EYES or BOXCARS if sum of roll is 2 or 12, returns
   the sum of the throw otherwise"
   (let ((roll-sum (+ (car roll) (cadr roll))))
      (cond
         ((equal roll-sum 2) 'SNAKE-EYES)
         ((equal roll-sum 12) 'BOXCARS)
         (t roll-sum)))
   )

(defun CRAPS ()
   "plays a game of craps, rolling two die and telling you whether you have
   instantly won or loss or your running point otherwise"
   (let* (
            (roll-1 (throw-die))
            (roll-2 (throw-die))
            (roll-pair (list roll-1 roll-2))
            (roll-sum (+ roll-1 roll-2)))
      (cond
         ((instant-win-p roll-pair) (list 'throw roll-1 'and roll-2 '-- roll-sum 'you 'win ))
         ((instant-loss-p roll-pair) (list 'throw roll-1 'and roll-2 '-- (say-throw roll-pair) '-- 'you 'lose))
         (t (list 'throw roll-1 'and roll-2 '-- 'your 'point 'is roll-sum))))
   )

(defun TRY-FOR-POINT (point)
   (let* (
           (lose 7)
           (roll-1 (throw-die))
           (roll-2 (throw-die))
           (roll-sum (+ roll-1 roll-2)))
      (cond
         ((equal roll-sum lose) (list 'throw roll-1 'and roll-2 '-- roll-sum '-- 'you 'lose))
         ((equal roll-sum point) (list 'throw roll-1 'and roll-2 '-- roll-sum '-- 'you 'win))
         (t (list 'throw roll-1 'and roll-2 '-- 'throw 'again))))
   )
