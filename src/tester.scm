;;; Test file
;;; Ashton Wiersdorf
;;; Fri Jan 31 21:39:07 MST 2014
;;; Part of the NN_Chess project

(declare (usual-integrations)
	 (integrate-external "neural_network"))
(load "tap.scm")

(load-option 'format)

(define (do-tests)
  ;; neural network tests
  (let ((test-net (define-ffn .7 2 '(3 2))))
    (is (test-net 'get-learning-rate) .7 "learning rate initilized correctly")
    (test-net 'set-learning-rate .5)
    (is (test-net 'get-learning-rate) .5 "learning rate set correctly")

    (let ((layers (test-net 'get-layers)))
      (is (length layers) 2 "two non-input layers")
      (is (length (car layers)) 3 "three nodes in hidden layers")
      (is (length (cadr layers)) 2 "two output nodes"))
    )

  (done-testing))