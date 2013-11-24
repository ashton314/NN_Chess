;;; AI Routines
;;; Ashton Wiersdorf
;;; Part of the NN_Chess project

(load "engine.scm")
(load "feature_detector.scm")

;; Load neural network
(define *network-stream* (open-input-file "NN_DATA/network.scm"))
(define *output-nodes* (read *network-stream*))
(define *hidden-layer* (read *network-stream*))
(close-port *network-stream*)
