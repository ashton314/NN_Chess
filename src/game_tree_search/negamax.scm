;;; Search engine
;;; Ashton Wiersdorf
;;; Part of the NN_Chess project

(load-option 'format)

(declare (usual-integrations)
	 (integrate white-piece? black-piece? collapse-num sub-jumps))

(define *root-node* #(#(1 1 1 1) #(1 1 1 1) #(1 1 1 1)
		      #(0 0 0 0) #(0 0 0 0)
		      #(-1 -1 -1 -1) #(-1 -1 -1 -1) #(-1 -1 -1 -1)))
(define *root-turn* 'white)

(define-integrable (right-turn sqr turn)
  (eq? turn (case sqr ((1 2) 'white) ((-1 -2) 'black))))

(define-integrable (king? piece)
  (= (abs piece) 2))

(define (white-piece? piece)
  (case piece ((1 2) #t) (else #f)))

(define (black-piece? piece)
  (case piece ((-1 -2) #t) (else #f)))

(define-integrable (best-move-dumb board turn depth)
  (define (best-car best lst)
    (if (null? lst)
	best
	(best-car (if (< (car best) (caar lst)) best (car lst)) (cdr lst))))

  (let ((moves (possible-moves board turn)))
    (best-car '(100000000) (map (lambda (move)
				  (let ((ret (list (negamax (car (do-move move board turn)) (other-side turn) depth #t (list move)) move)))
				    (newline)
				    ret)) moves))))

(define-integrable (score board)
  ;; Returns simple score of board, relative to white
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

(define-integrable (negamax board turn depth loudp history)
  (define (negamax-primary brd trn alpha beta depth-remaining hist)
    (if (= 0 depth-remaining)
	(* (score brd) (if (eq? trn 'white) 1 -1))
	(let ((moves (possible-moves brd trn)))

	  (define (loop mvs best-alpha)
	    (if (and loudp (not (null? mvs)))
		(negamax-status (car mvs) hist best-alpha alpha beta))
	    (if (null? mvs)
		best-alpha
		(let ((this-score (- (negamax-primary (car (do-move (car mvs) brd trn)) (other-side trn)
						      (- beta) (- best-alpha) (- depth-remaining 1) (if loudp (cons (car mvs) hist) '())))))
		  (if (>= this-score beta)
		      beta
		      (if (> this-score best-alpha)
			  (loop (cdr mvs) this-score)
			  (loop (cdr mvs) best-alpha))))))

	  (if (null? moves)
	      (let ((final (* (score brd) (if (eq? trn 'white) 1 -1))))
		(if loudp
		    (begin
		      (negamax-status (car (if (null? hist) '(foo) hist)) (cdr (if (null? hist) '(0) hist)) final alpha beta)
		      (write-string " TERMINAL NODE")))
		final)
	      (loop moves alpha)))))
  (negamax-primary board turn -1000000 1000000 depth history))

(define-integrable (negamax-status currently-considering history best-alpha alpha beta)
  (write-string
   "\r                                                                                                                                                    ")
  (write-string "\rConsidering: ")
  (format #t "~A" (apply string-append (map (lambda (choice) (format #f "~A " choice)) (reverse history))))
  (format #t "~A ### Alpha: ~A Beta: ~A" currently-considering best-alpha beta))

(define-integrable (negamax-finish current history scr)
  (format #t " ~A SCORE: ~A " current scr))

;; Move generation
(define-integrable (possible-moves board turn)
  (reduce append '() 
	  (map (lambda (coord)
		 (delete-duplicates!
		  (reduce append '() 
			  (map sub-jumps
			       (map (lambda (chain) (cons (collapse-num coord) (map collapse-num chain)))
				    (generate-possible-moves board coord #f turn)))
			  )))
	       (collect-coordinates board turn))))

(define-integrable (generate-possible-moves first-board first-coordinate first-must-jump? first-turn)
  ;; if must-jump? is #t, then this must return legal jumps
  (define (loop board coordinate must-jump? turn)
    (let ((jumps (filter (lambda (move) (not (condition? (ignore-errors (lambda () (assert-legal coordinate move board turn))))))
			 (collect-diagnal-squares coordinate 2)))
	  (single-moves (if must-jump? '()
			    (filter (lambda (move) (not (condition? (ignore-errors (lambda () (assert-legal coordinate move board turn))))))
				    (collect-diagnal-squares coordinate 1)))))
      (append (if (null? jumps) '()
		  (apply append (map (lambda (jump)
				       (let ((new-board (car (copy-board-with-modifications coordinate jump board))))
					 (let ((chains (loop new-board jump #t turn)))
					   (if (null? chains)
					       (list (list jump))
					       (map (lambda (chain) (cons jump chain)) chains)))))
				     jumps)))
	      (map list single-moves))))
  (loop first-board first-coordinate first-must-jump? first-turn))

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
  
(define-integrable (collect-diagnal-squares square distance)
  (let ((sqrs
	 (filter (lambda (sqr)
		   (and (< 0 (car sqr)) (< 0 (cdr sqr))
			(> 9 (car sqr)) (> 9 (cdr sqr))))
		 (map (lambda (x y) (cons (+ (car square) (* distance x))
					  (+ (cdr square) (* distance y))))
		      '(1 1 -1 -1)
		      '(1 -1 1 -1)))))
    sqrs))

(define-integrable (collect-coordinates board turn)
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

(define-integrable (other-side side)
  (case side
    ('white 'black)
    ('black 'white)
    (else (error (format #f "Unknown turn passed to other-side: '~A'" side)))))
