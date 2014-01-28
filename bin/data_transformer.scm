(load "trainer.com")

(define *input* (open-input-file "DATA/TRAINING_DATA/data_set01.scm"))
(define *output* (open-output-file "DATA/TRAINING_DATA/data_set02.scm"))

(define (transform)
  (do ((line (read *input*) (read *input*)))
      ((eof-object? line) (flush-output *output*))
    (let ((board (car line))
	  (side (cadr line))
	  (score (caddr line)))
      (write-line (list (cons (if (eq? side 'white) 1 0) (patterns board)) (score-pattern score)) *output*))))

(define (patterns board)
  (append
   (list (pawn-compare board)
	 (king-compare board)
	 (white-advancement-potential board)
	 (black-advancement-potential board))
   (analyze-movement board)
   (list (pieces-ahead-of board 'white)
	 (pieces-ahead-of board 'black))))

(define-syntax in-range
  (syntax-rules ()
    ((_ var (lower upper datum))
     (if (and (>= var lower) (< var upper))
	 'datum
	 #f))
    ((_ var (lower upper datum) rest ...)
     (if (and (>= var lower) (< var upper))
	 'datum
	 (in-range var rest ...)))))

(define (score-pattern score)		; Score is supposed to be relative to current side
  (in-range score
	    (-1000 -6 (0   0  0 .5  1))	; evil
	    (-6 -2    (0   0  0 .7 .7))	; meh-evil
	    (-2 0     (0   0 .5  1 .5))	; bad
	    (0 1      (0  .5  1 .5  0))	; meh
	    (1 2      (.5  1 .5  0  0))	; good
	    (2 6      (.7 .7  0  0  0))	; meh-awesome
	    (6 1001   (1  .5  0  0  0)))) ; awesome
