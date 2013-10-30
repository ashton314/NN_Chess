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
	"moved for white")))

;; High-level chunks
(board-displays)
(board-routines)
(move-making)

(done-testing)