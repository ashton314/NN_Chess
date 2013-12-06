;;; Checkers engine
;;; Ashton Wiersdorf
;;; Part of the NN_Chess project

(load-option 'format)

;; (load "../../lib/macros.scm")
;; (load "../../lib/utils.scm")
;; (load "negamax.scm")
;; (load "../../neural_network/matrix.scm")
(declare (integrate-external "macros"))
(declare (integrate-external "utils"))
(declare (integrate-external "negamax"))
(declare (integrate-external "neural_network"))

;; Global variables
(define *white-pawn* "w")
(define *white-king* "W")
(define *black-pawn* "b")
(define *black-king* "B")

;; NOTES
#|

When a square is in raw-format, it means that the numbers are as the
player would input. Converted-format is 0-index, adjusted for columns.

Board description:
  8x4 game board
  Sides: 2, white and black
  Squares:
    0  => empty
    1  => white pawn
    2  => white king
    -1 => black pawn
    -2 => black king

|#

;; Constructer function
(define (make-board)
  ;; Returns a new checkers board

  ; Internal macros
  (define-syntax getter-setter
    (syntax-rules ()
      ((_ var)
       (lambda (op . args)
	 (cond ((eq? op 'set!) (set! var (car args)))
	       ((eq? op 'get) var)
	       (else (error (format #f "Bad option to getter-setter: ~A" 'var))))))))

  ; Private values
  (let ((board #(#(1 1 1 1)
		 #(1 1 1 1)
		 #(1 1 1 1)
		 #(0 0 0 0)
		 #(0 0 0 0)
		 #(-1 -1 -1 -1)
		 #(-1 -1 -1 -1)
		 #(-1 -1 -1 -1)))
	(history '())
	(board-stack '())
	(turn 'white))

    (lambda (op . args)
      (case op
	;; Movement
	((turn) (getter-setter turn))
	((toggle-turn) (set! turn (case turn ('white 'black) ('black 'white) (else (error "something wicked happend to the turn")))))
	((move) (do-move (car args) board turn))
	((move-print) (format-board (do-move (car args) board turn)))
	((move!) (let ((data (do-move (car args) board turn)))
		   (set! board (car data))
		   (set! turn (case turn ('white 'black) ('black 'white)))))

	;; AI
	((possible-moves) (possible-moves board turn))
	((compute-move-dumb) (best-move-dumb board turn)) ; no NN
	((compute-move-smart) (best-move-smart board turn neural-net))
	((make-move-dumb!) (let ((data (do-move (best-move-dumb board turn) board turn)))
			     (set! board (car data))
			     (set! turn (case turn ('white 'black) ('black 'white)))))
	((make-move-smart!) (let ((data (do-move (best-move-smart board turn neural-net) board turn)))
			     (set! board (car data))
			     (set! turn (case turn ('white 'black) ('black 'white)))))

	;; Display/Debugging
	((print) (write-string (format-board board)))
	((set-board!) (set! board (car args)))
	((dump-board) board)

	(else (error (format #f "Bad option to board: ~A" op)))))))


;; Board utilities
(define (do-move moves board turn)
  ;; Takes a list of moves. Returns new 8x4 vector.

  (define (make-move current-square moves brd history)
    ;; Tail recursive procedure to modify the board

    ;; NOTE: current-square and moves are in raw entry-format,
    ;; split-num'd The numbers will be converted for 0-indexing by the
    ;; procedures that access the board directly

    (assert-legal current-square (car moves) brd turn)

    (let ((new-data (copy-board-with-modifications current-square (car moves) brd)))
      (let ((new-board (car new-data))
	    (new-history (cdr new-data)))
	(if (null? (cdr moves))
	    (cons new-board (cons new-history history))
	    (make-move (car moves) (cdr moves) new-board (cons new-history history))))))

  (let ((mvs (map split-num moves)))
    (if (> (length mvs) 2)
	(this-and-next (m1 m2 mvs)
		       (if (not (= (num-diff (car m1) (car m2)) 2))
			   (error "non-jump move in multi-square chain"))))
    (make-move (car mvs) (cdr mvs) board '())))

(define (copy-board-with-modifications current-square target-square board)
  (let ((new-board (vector-copy (vector-map vector-copy board)))
	(ret (list current-square target-square)))
    (sqr-set! target-square
	      (let ((piece (sqr-get current-square board)))
		(if (= (car target-square) (if (white-piece? piece) 8 1)) ; King me!
		    (if (white-piece? piece) 2 -2)
		    piece))
	      new-board)
    (sqr-set! current-square 0 new-board)
    (if (= (num-diff (car current-square) (car target-square)) 2)
	(begin
	  (set! ret (cons (sqr-get (midpoint current-square target-square) new-board) ret))
	  (sqr-set! (midpoint current-square target-square) 0 new-board)))
    (cons new-board (reverse! ret))))

(define (assert-legal current-square target-square board turn)
  ;; NOTE: current-square and target-square are in split-raw format

  ; right turn to move
  (assert (right-turn (sqr-get current-square board) turn)
	  "It is not your turn to move!")

    ; target square blank
  (assert (= 0 (sqr-get target-square board))
	  (format #f "Target square ~A is not empty!" target-square))

    ; square is on board
  (assert (for-all? (list (car current-square) (cdr current-square))
		    (lambda (num)
		      (and (< 0 num)
			   (integer? num)
			   (>= 8 num))))
	  (format #f "Square '~A' is not on the board!" current-square))
  (assert (for-all? (list (car target-square) (cdr target-square))
		    (lambda (num) (and (< 0 num)
				       (integer? num)
				       (>= 8 num))))
	  (format #f "Square '~A' is not on the board!" target-square))

    ; square is non-null
  (assert (and (there-exists? (list (car current-square) (cdr current-square))
			      (lambda (n) (or (= n 0) (even? n))))
	       (there-exists? (list (car current-square) (cdr current-square)) odd?))
	  (format #f "Square '~A' is not a legal square!" current-square))
  (assert (and (there-exists? (list (car target-square) (cdr target-square))
			      (lambda (n) (or (= n 0) (even? n))))
	       (there-exists? (list (car target-square) (cdr target-square)) odd?))
	  (format #f "Square '~A' is not a legal square!" target-square))

    ; direction good
  (if (= (abs (sqr-get current-square board)) 1)
      (assert ((if (white-piece? (sqr-get current-square board)) < >) ; functional programming, for the win
	       (car current-square) (car target-square))
	      (format #f "Piece at ~A may not move backwards!" current-square)))

    ; range good
  (assert (= (num-diff (car current-square) (car target-square))
	     (num-diff (cdr current-square) (cdr target-square)))
	  "Non-diagnal motion!")
  (case (num-diff (car current-square) (car target-square))
    ((1) #t)				; no jump
    ((2) (assert ((if (white-piece? (sqr-get current-square board)) black-piece? white-piece?) ; again, functional programming
		  (sqr-get (midpoint current-square target-square) board))
		 "You can only jump over a piece of the opposite color!"))
    (else (error "You cannot move that far!"))))
    

(define (right-turn sqr turn)
  (eq? turn (case sqr ((1 2) 'white) ((-1 -2) 'black))))

(define (king? piece)
  (= (abs piece) 2))

(define (white-piece? piece)
  (case piece ((1 2) #t) (else #f)))

(define (black-piece? piece)
  (case piece ((-1 -2) #t) (else #f)))

(define (format-board board)
  (string-append
   (format #f "
     1   2   3   4   5   6   7   8
   +---+---+---+---+---+---+---+---+~%")
   (reduce string-append
	   ""
	   (reverse!
	    (vector->list
	     (vector-mapn (lambda (row idx)
			    (string-append
			     (apply format 
				    `(#f ,(if (even? idx)
					      " ~A | ~A |   | ~A |   | ~A |   | ~A |   |~%"
					      " ~A |   | ~A |   | ~A |   | ~A |   | ~A |~%")
					 ,idx
					 ,@(map (lambda (square)
						  (case square
						    ((0) " ") ((1) *white-pawn*) ((2) *white-king*) ((-1) *black-pawn*) ((-2) *black-king*)))
						(vector->list row))))
			     (format #f "   +---+---+---+---+---+---+---+---+~%")))
			  board #(1 2 3 4 5 6 7 8)))))))


;; Helper functions
(define (num-diff a b)
  (abs (- a b)))

(define (assert condition if-err)
  (if condition
      #t
      (error if-err)))

(define (convert-coord sqr)
  ;; Converts a board coordinate into 0-index form
  (cons (- (car sqr) 1)
	(floor (remainder (cdr sqr) 2))))

(define (midpoint current target)
  ;; Given two squares on the same diagnal with one square between
  ;; them, returns that in-between square
  ;; Works on split-num'd raw squares

  ;; WORKING
  (cons (+ (car current) (/ (- (car target) (car current)) 2))
	(+ (cdr current) (/ (- (cdr target) (cdr current)) 2))))

(define (sqr-get sqr board)
  ;; Takes a square from `split-num'
  ;; sqr is in raw form
  (vector-ref (vector-ref board (- (car sqr) 1)) (truncate (/ (- (cdr sqr) 1) 2))))

(define (sqr-set! sqr obj board)
  (vector-set! (vector-ref board (- (car sqr) 1)) (truncate (/ (- (cdr sqr) 1) 2)) obj))

(define (split-num num)
  ;; Takes something like 12 and returns (1 . 2)
  (if (< num 0)
      (cons 0 num)
      (cons (truncate (/ num 10))
	    (remainder num 10))))
