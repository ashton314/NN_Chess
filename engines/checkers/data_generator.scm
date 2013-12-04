;;; Network training data generator
;;; Part of the NN_Chess project
;;; Ashton Wiersdorf

(load-option 'format)

(load "engine.scm")
(load "negamax.scm")

(define-syntax call-once
  ;; Takes a thunk, and calls the thunk once. Next iteration, (or whatever) the thunk is NOT called
  (syntax-rules ()
    ((_ thunk)
     (error "not implemented"))))

(define *session-id* (- (get-universal-time) epoch))

(define *data-fh* (open-output-file (let ((now (local-decoded-time)))
				      (format #f "TRAINING_DATA/~A~A~A~A~A~A.scm"
					      (decoded-time/year now)
					      (decoded-time/month now)
					      (decoded-time/day now)
					      (decoded-time/hour now)
					      (decoded-time/minute now)
					      (decoded-time/second now)))))
(format *data-fh* ";; SESSION ID: ~A\n" *session-id*)
(format #t ";; SESSION ID: ~A\n" *session-id*)

(define *continuation-pool-size* 20)
(define *continuation-pool* (make-vector *continuation-pool-size*))
(define *slots-full* 0)
(define *score-depth* 5)

(define *root-node* #(#(1 1 1 1) #(1 1 1 1) #(1 1 1 1)
		      #(0 0 0 0) #(0 0 0 0)
		      #(-1 -1 -1 -1) #(-1 -1 -1 -1) #(-1 -1 -1 -1)))
(define *root-turn* 'white)

(format *data-fh* " ; SCORE DEPTH: ~A\n" *score-depth*)
(format *data-fh* " ; CONTINUATION POOL SIZE: ~A\n\n" *continuation-pool-size*)

(define (make-data start-node start-turn)
  (let loop ((iteration-number 0)
	     (slot 0)
	     (node start-node)
	     (turn start-turn)
	     (score (negamax start-node start-turn *score-depth* #f '()))
	     (children (map (lambda (move) (car (do-move move start-node start-turn)))
			    (possible-moves start-node start-turn))))      

    (format #t "Iteration: ~A\n" iteration-number)
    (if (= 9 (remainder iteration-number 10)) (debug))
    (if (= 0 (remainder iteration-number 20))
	(begin
	  (write-line `((iteration ,iteration-number) (slot ,slot) (turn ,turn) (score ,score)))
	  (format #t "Garbage collecting\n")
	  (gc-flip)))

    (write-line (list node turn score) *data-fh*)
    (flush-output *data-fh*)

    (if (null? children)		; terminal board state
	(begin
	  (format #t "REPACKING!!\n")
	  (vector-set! *continuation-pool* slot #f)
	  (repack-continuation-pool))
	(begin
	  (vector-set! *continuation-pool* slot (cons (pop! children) ; clobber slot where this node came from
						      (other-side turn)))
	  (if (>= *slots-full* *continuation-pool-size*)
	      (for-each (lambda (child-node)
			  (vector-set! *continuation-pool* (random *continuation-pool-size*)
				       (cons child-node (other-side turn))))
			children)
	      (for-each (lambda (child-node)
			  (if (< *slots-full* *continuation-pool-size*) (inc! *slots-full*))
			  (vector-set! *continuation-pool* (- *slots-full* 1)
				       (cons child-node (other-side turn))))
			children))))

    (if (= 0 *slots-full*)
	(begin
	  (format *data-fh* ";; PATHS EXHAUSTED\n\n")
	  (close-port *data-fh*))
	(let* ((new-slot (random *slots-full*))
	       (node-data (vector-ref *continuation-pool* new-slot)))
	  (loop (1+ iteration-number)
		new-slot
		(car node-data)
		(cdr node-data)
		(negamax (car node-data) (cdr node-data) *score-depth* #f '())
		(map (lambda (move) (car (do-move move (car node-data) (cdr node-data))))
		     (possible-moves (car node-data) (cdr node-data))))))))

(define (make-data-default)
  (make-data *root-node* *root-turn*))

(define (repack-continuation-pool)
  ;; WARNING!! THIS IS HIGHLY INEFFICIENT!!
  (format *data-fh* " ; REPACKING CONTINUATION POOL\n")
  (format *data-fh* " ; CONTINUATION POOL: ~A\n" *continuation-pool*)
  (set! *continuation-pool* (list->vector (filter (lambda (x) x) ; only true values
						  (vector->list *continuation-pool*))))
  (gc-flip)
  (set! *continuation-pool-size* (vector-length *continuation-pool*))
  (set! *slots-full* (min *continuation-pool-size* *slots-full*)))
