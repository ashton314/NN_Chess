;;; Checkers engine
;;; Ashton Wiersdorf
;;; Part of the NN_Chess project

(load-option 'format)

(define (best-move-dumb board turn depth)
  (define (best-car best lst)
    (if (null? lst)
	best
	(best-car (if (< (car best) (caar lst)) best (car lst)) (cdr lst))))

  (let ((moves (possible-moves board turn)))
    (best-car '(1000000) (map (lambda (move)
				(let ((ret (list (negamax (car (do-move move board turn)) (other-side turn) depth #t) move)))
				  (newline)
				  ret)) moves))))

(define (best-move-smart board turn net)
  #f)

(define (score board)
  (let ((scr 0)
	(white 0)
	(black 0))
    (vector-map (lambda (row)
		  (vector-map (lambda (cell)
				(if (white-piece? cell) (inc! white) (if (black-piece? cell) (inc! black)))
				(inc! scr cell)) row)) board)
    (cond ((= 0 white) -1000)
	  ((= 0 black) 1000)
	  (else scr))))

(define (negamax board turn depth loudp)
  (define (negamax-primary brd trn alpha beta depth-remaining history)
    (if (= 0 depth-remaining)
	(let ((scr (* (score brd) (if (eq? trn 'white) 1 -1)))) ; negate score if black's turn
;;	(let ((scr (- (score brd))))
;;	  (format #t " SCORE: ~A" (- scr))
	  scr)
	(let ((moves (possible-moves brd trn)))

	  (define (loop mvs best-alpha)
	    (if (and loudp (not (null? mvs)))
		(negamax-status (car mvs) history best-alpha alpha beta))
	    (if (null? mvs)
		best-alpha
		(let ((this-score (- (negamax-primary (car (do-move (car mvs) brd trn)) (other-side trn)
						      (- beta) (- best-alpha) (- depth-remaining 1) (if loudp (cons (car mvs) history) '())))))
		  (negamax-finish (car mvs) history this-score)
		  (if (>= this-score beta)
		      (begin
;			(format #t "~%## PRUNE!!")
			beta)
		      (if (> this-score best-alpha)
			  (loop (cdr mvs) this-score)
			  (loop (cdr mvs) best-alpha))))))

	  (if (null? moves)
	      (let ((final (* (score brd) (if (eq? trn 'white) 1 -1))))
;		(format #t "~%TERMINAL NODE! SCORE: ~A ~%" final)
		final)
	      (loop moves alpha)))))
  (negamax-primary board turn -1000000 1000000 depth '()))

(define (negamax-status currently-considering history best-alpha alpha beta)
;;   (newline)
;;   (map (lambda (nul) (write-string "|  ")) history)
;;   (format #t "/--  ~A --> Alpha: ~A Beta: ~A" currently-considering best-alpha beta)
  (write-string "\rConsidering: ")
  (format #t "~A" (apply string-append (map (lambda (choice) (format #f "~A " choice)) (reverse history))))
  (format #t "~A ### Alpha: ~A Beta: ~A" currently-considering best-alpha beta)
;  (format #t "~27A" (apply string-append (map (lambda (choice) (format #f "~A " choice)) (reverse history))))
;  (format #t "~15A  ### Best-Alpha: ~@8A Alpha: ~@8A Beta: ~@8A" currently-considering best-alpha alpha beta)
)
;  (read-line))

(define (negamax-finish current history scr)
;  (newline)
  (format #t " ~A SCORE: ~A " current scr))
;;   (map (lambda (nul) (write-string "|  ")) history)
;;   (format #t "\\--  ~A --> RETURNING: ~A" current scr))


;; Move generation
(define (possible-moves board turn)
  (reduce append '() 
	  (map (lambda (coord)
		 (delete-duplicates!
		  (reduce append '() 
			  (map sub-jumps
			       (map (lambda (chain) (cons (collapse-num coord) (map collapse-num chain)))
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

;; Utilities
(define (collapse-num pair)
  (+ (* (car pair) 10) (cdr pair)))

(define (other-side side)
  (case side
    ('white 'black)
    ('black 'white)
    (else (error (format #f "Unknown turn passed to other-side: '~A'" side)))))