;;; Forward propogation tests
;;; Ashton Wiersdorf
;;; Tue Jan 28 20:44:55 MST 2014
;;; Part of the NN_Chess project

(load "../../utils/tap.scm")
(load "../neural_network.scm")

(define *test-net* '())
(ok (set! *test-net*
	  (define-ffn .7 2 '(3 2)))
    "network created successfully")
(let ((layers (*test-net* 'layers)))
  (is (length (car layers)) 3 "correct number of hidden nodes created")
  (is (length (cadr layers)) 2 "correct number of output nodes created")

  (ok (for-all? (car layers) (lambda (node) (= (length node) 3))) "all hidden-layer nodes account for bias weight")
  (ok (for-all? (cadr layers) (lambda (node) (= (length node) 4))) "all output-layer nodes account for bias weight"))

(done-testing)
