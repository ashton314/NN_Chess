;;; Checkers engine
;;; Ashton Wiersdorf
;;; Part of the NN_Chess project

(load-option 'format)

;; Macros
(define-syntax dotimes
  (syntax-rules ()
    ((_ (var times) body ...)
     (do ((var 0 (+ var 1)))
	 ((= var times) #t)
       body ...))))

(define-syntax inc!
  (syntax-rules ()
    ((_ var) (begin (set! var (+ var 1)) var))))


;; Constructer function

(define (make-board)
  ; Internal macros
  (define-syntax getter-setter
    (syntax-rules ()
      ((_ var)
       (lambda (op . args)
	 (cond ((eq? op 'set!) (set! var (car args)))
	       ((eq? op 'get) var)
	       (else (error (format #f "Bad option to getter-setter: ~A" 'var))))))))

  ; Private values
  (let ((white #((1 1 1 1)
		 (1 1 1 1)
		 (1 1 1 1)
		 (0 0 0 0)
		 (0 0 0 0)
		 (0 0 0 0)
		 (0 0 0 0)
		 (0 0 0 0)))
		   
	(black #((0 0 0 0)
		 (0 0 0 0)
		 (0 0 0 0)
		 (0 0 0 0)
		 (0 0 0 0)
		 (1 1 1 1)
		 (1 1 1 1)
		 (1 1 1 1)))
	(turn 'white))

    (lambda (op . args)
      (case op
	((turn) (getter-setter turn))
	((toggle-turn) (set! turn (case turn ('white 'black) ('black 'white))))
	((print) (write-string (format-board white black)))))))


;; Board utilities
(define (format-board whites blacks)
  (string-append
   (format #f "
     1   2   3   4   5   6   7   8
   +---+---+---+---+---+---+---+---+")
   ;; NOT FINISHED HERE))

;; Helper functions

(define (split-num num)
  ;; Takes something like 12 and returns (1 2)
  (if (< num 0)
      (list 0 num)
      (list (truncate (/ num 10))
	    (remainder num 10))))
