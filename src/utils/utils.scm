;;; Utilities
;;; Ashton Wiersdorf
;;; Part of the NN_Chess project

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

(define (vector-there-exists? vect pred)
  (let ((max (vector-length vect)))
    (define (loop i)
      (if (= i max)
	  #f
	  (if (pred (vector-ref vect i))
	      #t
	      (loop (+ i 1)))))
    (loop 0)))

(define (count datum pred)
  (let ((cnt 0))
    ((cond ((vector? datum) vector-map)
	   ((list? datum) map)
	   (else (error "Don't know how to iterate over datum")))
     (lambda (x) (if (pred x) (inc! cnt))) datum) cnt))

(define (range lower upper)
  (define (loop i acc)
    (if (< i lower)
	(reverse! acc)
	(loop (- i 1) (cons i acc))))
  (loop upper '()))
