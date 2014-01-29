;;; Vector utilities
;;; Ashton Wiersdorf
;;; Tue Jan 28 20:36:10 MST 2014
;;; Part of the NN_Chess project

;; Generic sequence routines
(define-macro make-generic-application
  (syntax-rules ()
    ((_ sym vector-routine)
     (let ((old-routine sym))		; something be wrong here
       (set! sym
	     (lambda (func . args)
	       (cond ((for-all? args pair?)
	       	      (apply old-routine (cons func args)))
	       	     ((for-all? args vector?)
	       	      (apply vector-routine (cons func args)))
	       	     (else (error "Don't know how to do this for function" (quote sym))))))))))

(make-generic-application map vector-mapn)
(make-generic-application reduce vector-reduce)
(make-generic-application length vector-length)

;; Utility functions
(define (matrix-* matrix vect)
  ;; Assumes multiplying n-m matrix by m-1 matrix, which I'll call a vector here
  (vector-map (lambda (node-row) ; NOTE: There's probably room for some optimization here (e.g. eliminate calls to vector->list)
		(apply + (map * (vector->list node-row) (vector->list vect))))
	      matrix))

(define (randomize lst)
  (map (lambda (n)
	 (if (= n 0)
	     (- (random 2.0) 1) n)) lst))

(define (vector-reduce func vec)
  (let ((leng (vector-length vec)))
    (define (loop counter acc)
      (if (= (+ counter 1) leng)
	  acc
	  (loop (+ counter 1) (func acc (vector-ref vec (+ counter 1))))))
    (loop 0 (vector-ref vec 0))))

(define (vector-zip func vec1 vec2)
  (let ((end-val (apply min (map vector-length (list vec1 vec2)))))
    (define (zipper i acc)
      (if (> (+ i 1) end-val)
	  (list->vector (reverse! acc))
	  (zipper (+ i 1) (cons (func (vector-ref vec1 i) (vector-ref vec2 i)) acc))))
    (zipper 0 '())))

(define (vector-mapn func . vects)
  (let* ((end-val (apply min (map vector-length vects)))
	 (acc (make-vector end-val)))
    (do ((i 0 (+ i 1)))
	((>= i end-val) acc)
      (vector-set! acc i (apply func (map (lambda (vec) (vector-ref vec i)) vects))))))

(define (vector-clobber! target vals)
  (let ((leng (vector-length target)))
    (do ((i 0 (+ i 1)))
	((= i leng) target)
      (vector-set! target i (vector-ref vals i)))))

(define (range lower upper)
  (define (loop i acc)
    (if (< i lower)
	(reverse! acc)
	(loop (- i 1) (cons i acc))))
  (loop upper '()))
