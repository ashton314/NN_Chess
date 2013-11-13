;;; Checkers player
;;; Ashton Wiersdorf
;;; Part of the NN_Chess project

(load-option 'format)

(load "engine.scm")

(define (play-game depth)
  (let ((board (make-board)))
;    (board 'set-board! #(#(0 0 0 0) #(1 0 0 0) #(1 0 0 0) #(0 0 1 0) #(0 0 0 0) #(0 -1 -1 0) #(0 0 0 0) #(0 0 0 0)))
    (board 'set-board! #(#(0 0 0 0) #(0 0 0 0) #(0 0 0 0) #(1 1 1 1) #(0 0 0 0) #(-1 0 0 -1) #(0 0 0 0) #(0 0 0 0)))
    (define (loop)
      (board 'print)
      (format #t "~%> ")
      (let ((comm (read)))
	(cond ((list? comm) (board 'move! comm) (loop))
	      ((eq? comm 'go) (board 'move! (cadr (best-move-dumb (board 'dump-board) ((board 'turn) 'get) depth))) (loop))
	      ((eq? comm 'quit) 'bye)
	      ((eq? comm 'term) (exit))
	      (else (format #t "I don't know that command.~%") (loop)))))
    (loop)))
    