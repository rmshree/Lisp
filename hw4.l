;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;          PART 1          ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use loops to execute the first and second problem. I think they could be implemented
;; with recursion, but I'm just going to stick with these.

(defun cycle(x y)
    (cond
        ((null y) NIL)
        ((> x 0)
            (loop for a from 1 to x
                do(setq y (append (cdr y) (list (car y))))
            )
            y
        )
        (t y)
    )
)

(defun split-list(x y)
    (cond
        ((null y) NIL)
        (t 
            (setq newList '())
            (setq count 1)
            (setq len (length y))
            (setq first (car y))
            
            (loop while (<= count len) do
            
                (setq chunk '())
                
                (loop while (and (not (eql x first)) (<= count len)) do
                
                    (setq chunk (append chunk (list(car y))))
                    (setq count (+ count 1))
                    (setq first (car (cdr y)))
                    (setq y (cdr y))
                )
                (if (not (null chunk))
                    (setq newList (append newList  (list chunk)))
                )
                (setq count (+ count 1))
                (setq first (car (cdr y)))
                (setq y (cdr y))
            )
            newList
        )
    )
)

;; can use recursion to find start, sum and max value
;; continously serached for the minimum value, max or added till the end of the list

(defun range(l)
    (cond
        ((null l) NIL)
	    ( t (list (minl l) (/ (suml l) (length l)) (maxl l)) )
	)
)

(defun minl(l)
	(cond 
		((null (cdr l))(car l))
		(t (min(car l)(minl(cdr l))))
	)
)

(defun maxl(l)
	(cond
		((null (cdr l))(car l))
		(t (max(car l)(maxl(cdr l))))
	)
)

(defun suml(l)
    (cond
        ((null (cdr l)) (car l))
        (t (+ (car l) (suml (cdr l))))
    )
)

;; if you find x then increment and search the rest of hte list
;; else just serach the next value in the list; ends when there is end
(defun search-count (x y)
  (cond

    ((null y) 0)
    ((equal x (car y)) (+ 1 (search-count x (cdr y))))
    (t (search-count x (cdr y)))
    
  )
)

;; each function searched for either greater or less than and put it into a list
;; at the end I combined the two lists together 
(defun pivot(x l)
    (cond
        ((null l) '(NIL NIL))
	    ( t (list (greatereq x l) (lessthan x l) ) )
	)
)

(defun lessthan(x l)
    (cond
        ((null l) '())
        (( <= x (car l)) (cons (car l) (lessthan x (cdr l))))
        (t (lessthan x (cdr l)))
    )   
)


(defun greatereq(x l)
    (cond
        ((null l) '())
        (( > x (car l)) (cons (car l) (greatereq x (cdr l))))
        (t (greatereq x (cdr l)))
    )   
)

;; quicksort uses greatereq to create list of values smaller than the first value of
;; the list and then puts car in the right location,lessthan does similar except with 
;; values greater than the first element. It continues to call quick sort on each new list respectively
;; until there is nothing else to sort anymore

(defun quicksort (l)
    (cond
        ((null l) nil)
        (t
            (append
                (quicksort (greatereq (car l) (cdr l)))
                (cons (car l) nil) 
                (quicksort (lessthan (car l) (cdr l)))
            )
        )
    )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;          PART 2          ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Just used essentially a bunch of if else statements for all possible conditions
;; that were described in part 2

(defun match(k l)
	(cond
		((or (null k)(null l)) (eq k l))
		((eq (car k) '?)(match (cdr k) (cdr l)))
		((and (eq (car k) '!) (match (cdr k) (cdr l))))
		((eq (car k) '!) (match k (cdr l)))
		(t (and(eq (car k) (car l)) (match (cdr k) (cdr l))))
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;          PART 3          ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Part 3 Matrix-add 

(defun matrix-add(x y)
    (cond
        ((or (null y) (null x)) NIL)
        (t 
            (cons (madd (car x) (car y)) (matrix-add (cdr x) (cdr y)))
        )
    
    )

)

(defun madd (x y)
    (cond 
        ((or (null y) (null x)) NIL)
        (t 
            (cons (+ (car x) (car y)) (madd (cdr x) (cdr y)))
        )
    )
)
;; Part 3 Matrix-multiplication

(defun matrix-multiply(x y)
    (cond 
        ((null x) '())
        (t 
            (setq y (matrix-transpose y))
            (cons (multhelper (car x) y) (matrix-multiply (cdr x) (matrix-transpose y)))
        )
    )
)

(defun multhelper(x y)
    (cond
        ((or (null y) (null x)) '())
        (t 
            (cons (multhelper2 x (car y)) (multhelper x (cdr y)))
        )
    )
)
(defun multhelper2(x y)
    (cond
        ((or (null y) (null x)) 0)
        (t 
            (+ (* (car x) (car y)) (multhelper2 (cdr x) (cdr y)))
        )
    )
)

;; Part 3 Matrix Transpose

(defun matrix-transpose(y)
    (cond
        ((null (car y)) '())
        (t 
            (setq x (mtrans y))
            (setq z (mrm y))
            (cons x (matrix-transpose z))
        )
    )
)

(defun mtrans (y)
    (cond 
        ((null y) '())
        (t 
            (cons (car (car y)) (mtrans (cdr y)))
        ) 
    )
)

(defun mrm (y)
    (cond
        ((null y) '())
        (t 
            (cons (cdr (car y)) (mrm (cdr y)))
        )
    )
)
