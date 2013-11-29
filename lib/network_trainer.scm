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
		       output-error-margin validation-pass-rate pass-validation?)
  ;; Trains a network.
  ;; Training data should be a set of pairs: car => inputs, cdr => outputs
  ;; If shuffle-data? is true, trains the network in a random order
  ;; If training-sets should be an integer; break up training-data into training-sets sets
  ;; output-error-margin is by how much a validation may be off and still get the problem "right"
  ;; validation-pass-rate is a percentage of how much of the validation set must pass for the training to be complete
  ;; pass-validation? if #t, loops until validation set passes

  (if (not (and (integer? training-sets) (positive? training-sets)))
      (error "fourth argument to train-network must be a positive integer"))

  (let* ((data (if shuffle-data? (shuffle training-data) training-data))
	 (sets (n-partitions data training-sets)))
    (do ((validate-set (car sets) (car train-sets))
	 (train-sets (cdr sets) (append (cdr train-sets) (list validate-set)))
	 (i 0 (1+ i)))
	((or (>= i training-sets)
	     (within-bounds network-object validate-set output-error-margin validation-pass-rate))
	 network-object)
;      (format #t "Validate set: ~A\nTraining sets: ~A\n" validate-set train-sets)
      (map (lambda (set)
	     ;; set => (((i1 i2 i3) . (o1 o2)) ((i4 i5 i6) . (o4 o5)) ..)
	     (map (lambda (data-set)
;		    (format #t "  Data set: ~A\n" data-set)
		    (network-object 'train (car data-set) (cdr data-set)))
		  set)) train-sets))))

(define (within-bounds network-object validate-set error-margin required-pass-rate)
  (define (num-diff a b)
    (abs (- a b)))

  (let ((matches (map (lambda (data-set)
			(for-all? (map (lambda (output target)
					 (num-diff output target))
				       (vector->list (network-object 'run (car data-set)))
				       (cdr data-set))
				  (lambda (x) (<= x error-margin))))
		      validate-set)))
    (let ((percentage (/ (length (filter (lambda (x) x) matches)) (length matches))))
      (format #t "Validation pass rate: ~A\n" percentage)
      (>= percentage required-pass-rate))))
