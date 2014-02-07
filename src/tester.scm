;;; Test file
;;; Ashton Wiersdorf
;;; Fri Jan 31 21:39:07 MST 2014
;;; Part of the NN_Chess project

(declare (usual-integrations)
	 (integrate-external "neural_network"))
(load "tap.scm")

(load-option 'format)

(define (do-tests)
  (ok #t "tests running")
  (done-testing))