;;; Rigorous test for matrix feed-forward networks
;;; Ashton Wiersdorf

(load "../../../lib/tap.scm")
(load "../../../lib/utils.scm")
(load "../../../lib/macros.scm")
(load "../../../lib/network_trainer.scm")

(load "../../matrix.scm")

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

(train-network flower-net *training-data* #t 5 0.1 0.9 #t)
