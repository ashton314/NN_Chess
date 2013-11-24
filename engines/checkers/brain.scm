;;; AI Routines
;;; Ashton Wiersdorf
;;; Part of the NN_Chess project

(load-option 'format)

(load "engine.scm")
(load "feature_detector.scm")

;; Load neural network
 ; backup current network
(copy-file "NN_DATA/network.scm" (let ((now (local-decoded-time)))
				   (format #f "NN_DATA/~A~A~A~A~A~A.scm"
					   (decoded-time/year now)
					   (decoded-time/month now)
					   (decoded-time/day now)
					   (decoded-time/hour now)
					   (decoded-time/minute now)
					   (decoded-time/second now))))

 ; load network
(define *network-stream* (open-input-file "NN_DATA/network.scm"))
(define *output-nodes* (read *network-stream*))
(define *hidden-layer* (read *network-stream*))
(close-port *network-stream*)

;; NOTES:
#|
Inputs: 11
  - pawn-compare
  - king-compare
  - white-advancement-potential
  - black-advancement-potential
  - movement analysis (2)
  - endanger analysis (2)
  - pieces-ahead-of-white
  - pieces-ahead-of-black
  - current-turn
Outputs: 5
  - very good for white
  - good for white
  - whatever
  - good for black
  - very good for black
|#

;; Save network
(set! *network-stream* (open-output-file "NN_DATA/network.scm"))
;; write data here
(close-port *network-stream*)