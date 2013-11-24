;;; AI Routines
;;; Ashton Wiersdorf
;;; Part of the NN_Chess project

(load "engine.scm")

;; Load neural network
(define *network-stream* (open-input-file "NN_DATA/network.scm"))
(define *output-nodes* (read *network-stream*))
(define *hidden-layer* (read *network-stream*))
(close-port *network-stream*)

  ; etc.


;; Feature detectors
;; NOTE: I'll need to scale these to values between -1 and 1 for the network
(define (pawn-compare board)
  ;; Returns #white_pawns - #black_pawns

  ;; NOTE: I'm sure there's room for optimizing this routine and
  ;; king-compare by memotizing the boards
  (let ((whites 0)
	(blacks 0))
    (vector-map (lambda (row)
		  (vector-map (lambda (cell) (cond ((= cell 1) (inc! whites)) ((= cell -1) (inc! blacks)))) row)) board)
    (- whites blacks)))

(define (king-compare board)
  ;; Returns #white_kings - #black_kings

  (let ((whites 0)
	(blacks 0))
    (vector-map (lambda (row)
		  (vector-map (lambda (cell) (cond ((= cell 2) (inc! whites)) ((= cell -2) (inc! blacks)))) row)) board)
    (- whites blacks)))

(define (advancement-potential board side)
  ;; Returns how many pieces are within two spaces of the relitive
  ;; "back" of the board
  (let ((count 0)
	(val (if (eq? side 'white) 1 (if (eq? side 'black) -1 (error "Unknown side passed to advancement-potential")))))
    (vector-map (lambda (row)
		  (vector-map (lambda (cell) (if (= cell val) (inc! count))) row))
		(if (eq? side 'white)
		    (vector-tail board 5)
		    (vector-head board 3)))
    count))

(define (white-advancement-potential board)
  (advancement-potential board 'white))

(define (black-advancement-potential board)
  (advancement-potential board 'black))

(define (analyze-movement board)
  (let* ((white-moves (possible-moves board 'white))
	 (black-moves (possible-moves board 'black))

	 (count-white-moves (length white-moves))
	 (count-black-moves (length black-moves))

	 (count-white-endangered (length (delete-duplicates
					  (filter (lambda (chain) (or (>= (length chain) 3)
								      (= 2 (num-diff (modulo (car chain) 10) (modulo (cadr chain) 10)))))
						  black-moves))))
	 (count-black-endangered (length (delete-duplicates
					  (filter (lambda (chain) (or (>= (length chain) 3)
								      (= 2 (num-diff (modulo (car chain) 10) (modulo (cadr chain) 10)))))
						  white-moves)))))
    (list count-white-moves count-black-moves count-white-endangered count-black-endangered)))

(define (pieces-ahead-of board side)
  (call-with-values
      (lambda () (cond ((eq? side 'white) (values reverse! white-piece? black-piece?))
		       ((eq? side 'black) (values (lambda (x) x) black-piece? white-piece?))
		       (else (error "Unknown side passed to pieces-ahead-of"))))
    (lambda (preprocess first-pred second-pred)
      (define (searcher board-list acc)
	(if (null? board-list)
	    acc
	    (if (vector-there-exists? (car board-list) first-pred)
		acc
		(searcher (cdr board-list) (+ acc (count (car board-list) second-pred))))))
      (searcher (preprocess (vector->list board)) 0))))
