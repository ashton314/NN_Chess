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

    (board 'set-board! #(#(0 0 0 0) #(0 0 0 0) #(0 0 0 0) #(0 0 0 0) #(0 0 0 0) #(0 0 0 0) #(0 0 1 0) #(0 0 0 0)))
    (is (board 'dump-board) #(#(0 0 0 0) #(0 0 0 0) #(0 0 0 0) #(0 0 0 0) #(0 0 0 0) #(0 0 0 0) #(0 0 1 0) #(0 0 0 0))
	"set-board! works correctly")
    ((board 'turn) 'set! 'white)
    (board 'move! '(76 87))
    (is (board 'dump-board) #(#(0 0 0 0) #(0 0 0 0) #(0 0 0 0) #(0 0 0 0) #(0 0 0 0) #(0 0 0 0) #(0 0 0 0) #(0 0 0 2))
	"white piece kinged correctly")))

    

;; High-level chunks
(board-displays)
(board-routines)
(move-making)

(done-testing)