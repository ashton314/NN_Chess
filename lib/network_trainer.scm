;;; Network trainer library
;;; Ashton Wiersdorf
;;; Part of the NN_Chess project

(define (shuffle lst)
  (sort lst (lambda (a b) (< (random 10) (random 10)))))

(define (n-partitions lst num-sets)
  ;; NOTE: I could make this more exact by using the fraction part to
  ;; figure out how to partition the last subset
  (define (partition pool pool-length partition-length set-acc)
    (if (null? pool)
	(reverse! set-acc)
	(partition (list-tail pool (min pool-length partition-length)) (- pool-length partition-length) partition-length
		   (cons (list-head pool (min pool-length partition-length)) set-acc))))
  (let ((leng (length lst)))
    (partition lst leng (ceiling->exact (/ leng num-sets)) '())))

(define (train-network network-object training-data
		       shuffle-data? training-sets
		       output-error-margin validation-error-margin)
  ;; Trains a network.
  ;; Training data should be a set of pairs: car => inputs, cdr => outputs
  ;; If shuffle-data? is true, trains the network in a random order
  ;; If training-sets should be an integer; break up training-data into training-sets sets
  ;; output-error-margin is by how much a validation may be off and still get the problem "right"
  ;; validation-error-margin is a percentage of how much of the validation set must pass for the training to be complete

  (let* ((data (if shuffle-data? (shuffle training-data) training-data))
	 (training-sets (n-partitions training-data training-sets)))
    ;; stuff here...
    (error "NOT IMPLEMENTED")))