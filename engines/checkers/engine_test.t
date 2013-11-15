;;; Checkers Engine Test   -*- mode: scheme -*-
;;; Ashton Wiersdorf
;;; Part of the NN_Chess project

(load "../../lib/tap.scm")
(load "engine.scm")

;; Test routines
(define (board-displays)
  (let ((board (make-board)))
    (board 'print)
    (ok (prompt-for-confirmation "Does this look right?") "board printing works")))

(define (board-routines)
  (let ((board (make-board)))

    ;; turns
    (is ((board 'turn) 'get) 'white "correct initial turn")
    (board 'toggle-turn)
    (is ((board 'turn) 'get) 'black "toggle-turn works once")
    (board 'toggle-turn)
    (is ((board 'turn) 'get) 'white "toggle-turn works again")
    ((board 'turn) 'set! 'black)
    (is ((board 'turn) 'get) 'black "turn setting! works")))
    


(define (move-making)
  (let ((board (make-board)))
    (is (board 'dump-board) #(#(1 1 1 1) #(1 1 1 1) #(1 1 1 1) #(0 0 0 0) #(0 0 0 0) #(-1 -1 -1 -1) #(-1 -1 -1 -1) #(-1 -1 -1 -1))
	"correct initial board")
    (is (car (board 'move '(32 41))) #(#(1 1 1 1) #(1 1 1 1) #(0 1 1 1) #(1 0 0 0) #(0 0 0 0) #(-1 -1 -1 -1) #(-1 -1 -1 -1) #(-1 -1 -1 -1))
	"moved for white")
    (is (board 'dump-board) #(#(1 1 1 1) #(1 1 1 1) #(1 1 1 1) #(0 0 0 0) #(0 0 0 0) #(-1 -1 -1 -1) #(-1 -1 -1 -1) #(-1 -1 -1 -1))
	"non-destructive modification")
    (board 'move! '(32 41))
    (is (board 'dump-board) #(#(1 1 1 1) #(1 1 1 1) #(0 1 1 1) #(1 0 0 0) #(0 0 0 0) #(-1 -1 -1 -1) #(-1 -1 -1 -1) #(-1 -1 -1 -1))
	"destructively moved for white")
    (is ((board 'turn) 'get) 'black "turn toggled correctly after move (to black)")
    (board 'move! '(63 52))
    (is (board 'dump-board) #(#(1 1 1 1) #(1 1 1 1) #(0 1 1 1) #(1 0 0 0) #(-1 0 0 0) #(-1 0 -1 -1) #(-1 -1 -1 -1) #(-1 -1 -1 -1))
	"destructively moved for black")
    (is ((board 'turn) 'get) 'white "turn toggled correctly after move (to white)")
    (board 'move! '(41 63))
    (is (board 'dump-board) #(#(1 1 1 1) #(1 1 1 1) #(0 1 1 1) #(0 0 0 0) #(0 0 0 0) #(-1 1 -1 -1) #(-1 -1 -1 -1) #(-1 -1 -1 -1))
	"white captures black pawn")
    (board 'move! '(74 52))
    (is (board 'dump-board) #(#(1 1 1 1) #(1 1 1 1) #(0 1 1 1) #(0 0 0 0) #(-1 0 0 0) #(-1 0 -1 -1) #(-1 0 -1 -1) #(-1 -1 -1 -1))
	"black captures white pawn")

    (board 'set-board! #(#(0 0 0 0) #(0 0 0 0) #(0 0 0 0) #(0 0 0 0) #(0 0 0 0) #(0 0 0 0) #(0 0 1 0) #(0 0 0 0)))
    (is (board 'dump-board) #(#(0 0 0 0) #(0 0 0 0) #(0 0 0 0) #(0 0 0 0) #(0 0 0 0) #(0 0 0 0) #(0 0 1 0) #(0 0 0 0))
	"set-board! works correctly")
    ((board 'turn) 'set! 'white)
    (board 'move! '(76 87))
    (is (board 'dump-board) #(#(0 0 0 0) #(0 0 0 0) #(0 0 0 0) #(0 0 0 0) #(0 0 0 0) #(0 0 0 0) #(0 0 0 0) #(0 0 0 2))
	"white piece kinged correctly")))

(define (bad-moves)
  (let ((board (make-board)))

    (define-syntax move-checker
      ;; Checks the legality of a given move using my TAP framework
      (syntax-rules ()
	((_ motion error-message)
	 (let ((condit (ignore-errors (lambda () (board 'move! motion)))))
	   (ok (condition? condit) error-message)
	   ((board 'turn) 'set! 'white)
	   (board 'set-board! #(#(1 1 1 1) #(1 1 1 1) #(1 1 1 1) #(0 0 0 0) #(0 0 0 0) #(-1 -1 -1 -1) #(-1 -1 -1 -1) #(-1 -1 -1 -1)))))))

    (move-checker '(38 48) "bad square not allowed")
    (move-checker '(38 47 58) "chained non-jump moves not allowed")
    (move-checker '(38 56) "jumping over nothing not allowed")
    (move-checker '(21 43) "jumping over same-color pawn not allowed")
    (move-checker '(21 32) "moving onto non-empty square not allowed")
    (move-checker '(32 52) "non-diagnal motion not allowed")))

(define (generation)
  (let ((board (make-board)))

    (is (board 'possible-moves)
	'((32 41) (32 43) (34 43) (34 45) (36 45) (36 47) (38 47))
	"moves generated for white")
    ;;                   1          2          3          4          5           6          7             8
    (board 'set-board! #(#(0 0 0 0) #(0 0 0 0) #(0 0 0 0) #(0 0 1 0) #(0 -1 0 0) #(0 0 0 0) #(-1 -1 -1 0) #(0 0 0 0)))
    (is (length (board 'possible-moves)) 5 "5 possible moves")))

(define (negamax-tests)
  (let ((board (make-board)))
    ;; Check `score'
    (is (score (board 'dump-board)) 0 "start board has score of 0")
    (is (score #(#(0 0 0 0) #(0 0 0 0) #(0 0 0 0) #(0 0 1 0) #(0 -1 0 0) #(0 0 0 0) #(-1 -1 -1 0) #(0 0 0 0))) -3 "score calulated correctly")
    (is (score #(#(0 0 0 0) #(0 0 0 0) #(0 0 0 0) #(0 0 0 0) #(0 0 0 0) #(0 0 0 0) #(0 0 0 0) #(1 0 0 0))) 1000 "white wins scored correctly")
    (is (score #(#(0 0 0 0) #(0 0 0 0) #(0 0 0 0) #(0 0 0 0) #(0 0 0 0) #(0 0 0 0) #(0 0 0 0) #(-1 0 0 0))) -1000 "black wins scored correctly")

    ;; Here we go, time to test out Negamax!!
    (is (best-move-dumb
	 #(#(0 0 0 0) #(1 0 0 0) #(0 0 0 0) #(0 0 1 0) #(0 -1 0 0) #(0 0 1 0) #(0 0 0 0) #(0 0 0 0)) 'white 2)
	'(-1000 (45 63))
	"white finishes the game")))

(define foo (make-board))		; test board

;; High-level chunks

;; (board-displays)
(board-routines)
(move-making)
(bad-moves)
(generation)
(negamax-tests)

(done-testing)