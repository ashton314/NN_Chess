;;; Vector utilities
;;; Ashton Wiersdorf
;;; Tue Jan 28 20:36:10 MST 2014
;;; Part of the NN_Chess project

(declare (usual-integrations))

;; Generic sequence routines
;; (define-syntax make-generic-application
;;   (syntax-rules ()
;;     ((_ sym vector-routine)
;;      (let ((old-routine sym))		; something be wrong here
;;        (set! sym
;; 	     (lambda (func . args)
;; 	       (cond ((for-all? args vector?)
;; 	       	      (apply vector-routine (cons func args)))
;; 	       	     (else (apply old-routine (cons func args))))))))))

;(make-generic-application map vector-mapn)      ;; bad idea to try to redefine this...
;(make-generic-application reduce vector-reduce)
;(make-generic-application length vector-length)

;; Utility functions
(define-integrable (matrix-* matrix vect)
  ;; Assumes multiplying n-m matrix by m-1 matrix, which I'll call a vector here
  (vector-map (lambda (node-row) ; NOTE: There's probably room for some optimization here (e.g. eliminate calls to vector->list)
		(apply + (map * (vector->list node-row) (vector->list vect))))
	      matrix))

(define-integrable (randomize lst)
  (map (lambda (n)
	 (if (= n 0)
	     (- (random 2.0) 1) n)) lst))

(define-integrable (vector-reduce func vec)
  (let ((leng (vector-length vec)))
    (define (loop counter acc)
      (if (= (+ counter 1) leng)
	  acc
	  (loop (+ counter 1) (func acc (vector-ref vec (+ counter 1))))))
    (loop 0 (vector-ref vec 0))))

(define-integrable (vector-zip func vec1 vec2)
  (let ((end-val (apply min (map vector-length (list vec1 vec2)))))
    (define (zipper i acc)
      (if (> (+ i 1) end-val)
	  (list->vector (reverse! acc))
	  (zipper (+ i 1) (cons (func (vector-ref vec1 i) (vector-ref vec2 i)) acc))))
    (zipper 0 '())))

(declare (integrate-operator vector-mapn))
(define vector-mapn
  (lambda (func . vects)
    (let* ((end-val (apply min (map vector-length vects)))
	   (acc (make-vector end-val)))
      (do ((i 0 (+ i 1)))
	  ((>= i end-val) acc)
	(vector-set! acc i (apply func (map (lambda (vec) (vector-ref vec i)) vects)))))))

(define-integrable (vector-clobber! target vals)
  (let ((leng (vector-length target)))
    (do ((i 0 (+ i 1)))
	((= i leng) target)
      (vector-set! target i (vector-ref vals i)))))

(define-integrable (range lower upper)
  (define (loop i acc)
    (if (< i lower)
	(reverse! acc)
	(loop (- i 1) (cons i acc))))
  (loop upper '()))
