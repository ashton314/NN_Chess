;;; AI Training Routines
;;; Ashton Wiersdorf
;;; Part of the NN_Chess project

(load-option 'format)

(declare (integrate-external "engine" "feature_detector"))

(write-string "Comments: ")
(define *comments* (read-line))

;; Load neural network
 ; backup current network
(copy-file "DATA/NN_DATA/network.scm" (let ((now (local-decoded-time)))
					(format #f "DATA/NN_DATA/~A~A~A~A~A~A.scm"
						(decoded-time/year now)
						(decoded-time/month now)
						(decoded-time/day now)
						(decoded-time/hour now)
						(decoded-time/minute now)
						(decoded-time/second now))))

 ; load network
(define *network-stream* (open-input-file "DATA/NN_DATA/network.scm"))
(define *learning-rate* .7)
(define *input-nodes* (read *network-stream*))
(define *output-nodes* (read *network-stream*))
(define *hidden-layer* (read *network-stream*))
(close-port *network-stream*)

;; NOTES:
#|
INPUTS: 13
  - side (1 => white, 0 => black)
  - pawn-compare
  - king-compare
  - white-advancement-potential
  - black-advancement-potential
  - white-moves
  - black-moves
  - white-endangered-pieces
  - black-endangered-pieces
  - longest-jump-white
  - longest-jump-black
  - black-blocking-white (pieces-ahead-of board 'white)
  - white-blocking-black (ditto)

OUTPUTS: 5
  - evil
  - bad  
  - meh
  - good
  - amazing
|#

(define *training-data* '())
(call-with-input-file "DATA/TRAINING_DATA/data_set02.scm"
  (lambda (fh)
    (do ((line (read fh) (read fh)))
	((eof-object? line) #f)
      (push! (cons (car line) (cadr line)) *training-data*))))

(bkpt "check *training-data*")

(define *start-string* (let ((now (local-decoded-time)))
			 (format #f "~A ~A ~A ~A:~A:~A"
				 (decoded-time/year now)
				 (decoded-time/month now)
				 (decoded-time/day now)
				 (decoded-time/hour now)
				 (decoded-time/minute now)
				 (decoded-time/second now))))

(define *network* (define-ffn *learning-rate* *input-nodes* *output-nodes* *hidden-layer*))
(train-network *network* *training-data*
	       #t 10 .01 .95 #t #t)

(define *end-string* (let ((now (local-decoded-time)))
		       (format #f "~A ~A ~A ~A:~A:~A"
			       (decoded-time/year now)
			       (decoded-time/month now)
			       (decoded-time/day now)
			       (decoded-time/hour now)
			       (decoded-time/minute now)
			       (decoded-time/second now))))
;; Save network
(set! *network-stream* (open-output-file "DATA/NN_DATA/network.scm"))
(format *network-stream* ";; COMMENTS: ~A\n" *comments*)
(format *network-stream* ";; START:    ~A\n" *start-string*)
(format *network-stream* ";; FINISHED: ~A\n" *end-string*)
(write-line *input-nodes* *network-stream*)
(let ((data (*network* 'debug-get-layers)))
  (write-line (cadr data) *network-stream*)
  (write-line (caar data) *network-stream*))
(close-port *network-stream*)
