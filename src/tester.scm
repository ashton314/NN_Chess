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
      (is (length (cadr layers)) 2 "two output nodes")

      (ok (for-all? (car layers) (lambda (n) (= (length n) 3))) "second layer has all weights (bias included)")
      (ok (for-all? (cadr layers) (lambda (n) (= (length n) 4))) "third layer has all weights (bias included)")

      (ok (for-all? (flatten (car layers)) (lambda (n) (and (> 1 n) (< -1 n)))) "small weights in second layer")
      (ok (for-all? (flatten (cadr layers)) (lambda (n) (and (> 1 n) (< -1 n)))) "small weights in third layer")

      (ok (not (apply = (flatten (car layers)))) "random weights in second layer")
      (ok (not (apply = (flatten (cadr layers)))) "random weights in third layer"))

    (test-net 'set-layers '(((.1 .2 .3) (.4 .5 .6) (.2 .8 .4))
			    ((.5 .3 .2 .6) (.1 .3 .6 .5))))
    (is (test-net 'get-layers) '(((.1 .2 .3) (.4 .5 .6) (.2 .8 .4))
				 ((.5 .3 .2 .6) (.1 .3 .6 .5))) "layers set/get correctly")
    (let ((data (test-net 'run-with-values '(3 5))))
      (is (car data) '(.8267297672138426 .8116342150555748) "forward prop runs correctly")
      (is (cdr data) '((.8267297672138426 .8116342150555748)
		       (.9002495108803148 .9926084586557181 .9900481981330957)) "internal values on forward prop correct")))

  (done-testing))

;; Utility functions
(define (flatten lst)
  (cond ((pair? lst) (apply append (map flatten lst)))
	(else (list lst))))