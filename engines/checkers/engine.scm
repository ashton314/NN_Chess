;;; Checkers engine
;;; Ashton Wiersdorf
;;; Part of the NN_Chess project

(load-option 'format)

;; Global variables
(define *white-pawn* "w")
(define *white-king* "W")
(define *black-pawn* "b")
(define *black-king* "B")

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

(define-syntax push!
  (syntax-rules ()
    ((push! datum place)
     (set! place (cons datum place)))))

(define-syntax pop!
  (syntax-rules ()
    ((pop! place)
     (let ((temp1 (car place)))
       (set! place (cdr place))
       temp1))))


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

  ;; Board description:
  ;;   8x4 game board
  ;;   Sides: 2, white and black
  ;;   Squares:
  ;;     0  => empty
  ;;     1  => white pawn
  ;;     2  => white king
  ;;     -1 => black pawn
  ;;     -2 => black king

  ; Private values
  (let ((board #(#(1 1 1 1)
		 #(1 1 1 1)
		 #(1 1 1 1)
		 #(0 0 0 0)
		 #(0 0 0 0)
		 #(-1 -1 -1 -1)
		 #(-1 -1 -1 -1)
		 #(-1 -1 -1 -1)))
	(turn 'white))

    (lambda (op . args)
      (case op
	((turn) (getter-setter turn))
	((toggle-turn) (set! turn (case turn ('white 'black) ('black 'white))))
	((print) (write-string (format-board white black)))))))


;; Board utilities
(define (format-board board)
  (reduce string-append
	  (format #f "
     1   2   3   4   5   6   7   8
   +---+---+---+---+---+---+---+---+~%")
	  (vector-mapn (lambda (row idx)
			 (format #f (if (even? idx)
					" ~A | ~A |   | ~A |   | ~A |   | ~A |   |~%"
					" ~A |   | ~A |   | ~A |   | ~A |   | ~A |~%")
				 ;;;; STILL WORKING HERE

		board #(1 2 3 4 5 6 7 8))))


;; Helper functions

(define (split-num num)
  ;; Takes something like 12 and returns (1 2)
  (if (< num 0)
      (list 0 num)
      (list (truncate (/ num 10))
	    (remainder num 10))))

(define (vector-reduce func vec)
  (let ((leng (vector-length vec)))
    (define (loop counter acc)
      (if (= (+ counter 1) leng)
	  acc
	  (loop (+ counter 1) (func acc (vector-ref vec (+ counter 1))))))
    (loop 0 (vector-ref vec 0))))

(define (vector-zip func vec1 vec2)
  (let ((end-val (apply min (map vector-length (list vec1 vec2)))))
    (define (zipper i acc)
      (if (> (+ i 1) end-val)
	  (list->vector (reverse! acc))
	  (zipper (+ i 1) (cons (func (vector-ref vec1 i) (vector-ref vec2 i)) acc))))
    (zipper 0 '())))

(define (vector-mapn func . vects)
  (let* ((end-val (apply min (map vector-length vects)))
	 (acc (make-vector end-val)))
    (do ((i 0 (+ i 1)))
	((>= i end-val) acc)
      (vector-set! acc i (apply func (map (lambda (vec) (vector-ref vec i)) vects))))))

(define (vector-clobber! target vals)
  (let ((leng (vector-length target)))
    (do ((i 0 (+ i 1)))
	((= i leng) target)
      (vector-set! target i (vector-ref vals i)))))

(define (range lower upper)
  (define (loop i acc)
    (if (< i lower)
	(reverse! acc)
	(loop (- i 1) (cons i acc))))
  (loop upper '()))