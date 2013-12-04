;;; Network training data generator
;;; Part of the NN_Chess project
;;; Ashton Wiersdorf

(load-option 'format)

(load "engine.scm")
(load "negamax.scm")

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

(define *continuation-pool-size* 100)
(define *continuation-pool* (make-vector *continuation-pool-size*))
(define *slots-full* 0)
(define *score-depth* 5)

(define *root-node* #(#(1 1 1 1) #(1 1 1 1) #(1 1 1 1)
		      #(0 0 0 0) #(0 0 0 0)
		      #(-1 -1 -1 -1) #(-1 -1 -1 -1) #(-1 -1 -1 -1)))
(define *root-turn* 'white)

(format *data-fh* " ; SCORE DEPTH: ~A\n" *score-depth*)
(format *data-fh* " ; CONTINUATION POOL SIZE: ~A\n\n" *continuation-pool-size*)

(gc-flip)

(define (make-data node obligitory-slot turn)
  (let ((score (negamax node turn *score-depth* #f '())))
    (write-line (list node turn score) *data-fh*)
    (flush-output *data-fh*))
  (let ((children (map (lambda (move) (car (do-move move node turn))) (possible-moves node turn))))
    (if (null? children)
	(begin
	  (vector-set! *continuation-pool* obligitory-slot #f)
	  (repack-continuation-pool))
	(begin 
	  (vector-set! *continuation-pool* obligitory-slot (cons (pop! children) (other-side turn)))
	  (if (>= *slots-full* *continuation-pool-size*)
	      (for-each (lambda (child-node)
			  (vector-set! *continuation-pool* (random *continuation-pool-size*) (cons child-node (other-side turn))))
			children)
	      (for-each (lambda (child-node)
			  (if (< *slots-full* *continuation-pool-size*) (inc! *slots-full*))
			  (vector-set! *continuation-pool* (- 1 *slots-full*) (cons child-node (other-side turn))))
		  children)))))
  (if (= 0 *slots-full*)
      (begin
	(format *data-fh* ";; PATHS EXHAUSTED\n\n")
	(close-port *data-fh*))
      (let* ((next-ref (random *slots-full*))
	     (next-node (vector-ref *continuation-pool* next-ref)))
	(make-data (car next-node) next-ref (cdr next-node)))))
    
(make-data *root-node* 0 *root-turn*)

(define (repack-continuation-pool)
  ;; WARNING!! THIS IS HIGHLY INEFFICIENT!!
  (format *data-fh* " ; REPACKING CONTINUATION POOL\n")
  (format *data-fh* " ; CONTINUATION POOL: ~A\n" *continuation-pool*)
  (set! *continuation-pool* (list->vector (filter (lambda (x) x) ; only true values
						  (vector->list *continuation-pool*))))
  (gc-flip)
  (set! *continuation-pool-size* (vector-length *continuation-pool*))
  (set! *slots-full* (min *continuation-pool-size* *slots-full*)))
