;;; Rigorous test for matrix feed-forward networks
;;; Ashton Wiersdorf

(load "../../../utils/tap.scm")
(load "../../../utils/utils.scm")
(load "../../../utils/macros.scm")
(load "../../../utils/network_trainer.scm")

(load "../../neural_network.scm")

(load-option 'format)

(define (good val1 val2 tol)
  (< (abs (- val1 val2)) tol))

(define *training-data-stream* (open-input-file "iris_data.scm"))
(define *training-data* '())

(do ((line (read *training-data-stream*) (read *training-data-stream*)))
    ((eof-object? line) (close-port *training-data-stream*))
  (push! line *training-data*))

(define-feed-forward-net flower-net
  1
  4
  ((0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0))
  ((0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0) (0 0 0 0)))

(train-network flower-net *training-data* #t 5 0.1 0.9 #t #t)

;; final validation:
(format #t "Final validation set:\n")
(map (lambda (in out)
       (format #t "~A (should be ~A)\n" (flower-net 'run in) out))
     '((7.1 3. 5.9 2.1) (6.6 2.9 4.6 1.3) (4.6 3.2 1.4 .2))
     '((0 0 1) (0 1 0) (1 0 0)))
