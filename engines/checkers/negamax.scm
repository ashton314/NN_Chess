;;; Checkers engine
;;; Ashton Wiersdorf
;;; Part of the NN_Chess project

(load-option 'format)

(define (best-move-dumb board turn)
  #f)

(define (best-move-smart board turn net)
  #f)

(define (score board)
  ;; Returns a score for the current board, relative to white
  0)

;; (define (negamax board turn)
;;   (define (negamax-primary brd alpha beta depth-remaining)
;;     (if (= 0 depth-remaining)
;; 	(score brd)
;; 	(let ((moves 



;; Move generation

(define (possible-moves board turn)
  (reduce append '() 
	  (map (lambda (coord)
		 (delete-duplicates!
		  (reduce append '() 
			  (map sub-jumps
			       (map (lambda (chain) (cons coord chain))
				    (generate-possible-moves board coord #f turn)))
			  )))
	       (collect-coordinates board turn))))

(define (generate-possible-moves board coordinate must-jump? turn)
  ;; if must-jump? is #t, then this must return legal jumps
  (let ((jumps (filter (lambda (move) (not (condition? (ignore-errors (lambda () (assert-legal coordinate move board turn))))))
		       (collect-diagnal-squares coordinate 2)))
	(single-moves (if must-jump? '()
			  (filter (lambda (move) (not (condition? (ignore-errors (lambda () (assert-legal coordinate move board turn))))))
				  (collect-diagnal-squares coordinate 1)))))
    (append (if (null? jumps) '()
		(apply append (map (lambda (jump)
				     (let ((new-board (car (copy-board-with-modifications coordinate jump board))))
				       (let ((chains (generate-possible-moves new-board jump #t turn)))
					 (if (null? chains)
					     (list (list jump))
					     (map (lambda (chain) (cons jump chain)) chains)))))
				   jumps)))
	    (map list single-moves))))

(define (sub-jumps chain)
  ;; All this does is return successive reversed CDRs of a reversed list
  (define (loop lst acc)
    (if (= (length lst) 1)
	acc
	(loop (cdr lst) (cons lst acc))))
  (define (post-process this-list acc)
    (if (null? this-list)
	acc
	(post-process (cdr this-list) (cons (reverse (car this-list)) acc))))
  (post-process (loop (reverse chain) '()) '()))
  
(define (collect-diagnal-squares square distance)
  (let ((sqrs
	 (filter (lambda (sqr)
		   (and (< 0 (car sqr)) (< 0 (cdr sqr))
			(> 9 (car sqr)) (> 9 (cdr sqr))))
		 (map (lambda (x y) (cons (+ (car square) (* distance x))
					  (+ (cdr square) (* distance y))))
		      '(1 1 -1 -1)
		      '(1 -1 1 -1)))))
    sqrs))

(define (collect-coordinates board turn)
  (define (loop row col acc)
    (if (and (= row 8) (= col 8))
	acc
	(loop (if (= col 8) (+ row 1) row)
	      (if (= col 8) 1 (+ col 1))
	      (if (or (and (even? col) (even? row))
		      (and (odd? col) (odd? row)))
		  acc
		  (if ((if (eq? turn 'white) white-piece? black-piece?) (sqr-get (cons row col) board))
		      (cons (cons row col) acc)
		      acc)))))
  (loop 1 1 '()))
