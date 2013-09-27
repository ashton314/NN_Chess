;;; Feed-Forward Neural Network library, using matricies
;;; Ashton Wiersdorf
;;; Part of the NN_Chess project

(load-option 'format)

(define-syntax push!
  (syntax-rules ()
    ((push! datum place)
     (set! place (cons datum place)))))

(define-syntax pop!
  (syntax-rules ()
    ((pop! place)
     (let ((temp1 (car place)))
       (set! place (cdr place))
       temp1))))


(define (activation value)
  ;; Sigmoid function
  (/ 1 (+ 1 (exp (- value)))))

#|
Syntax description:
(define-feed-forward-net test
  1					; learning weight
  2					; two inputs
  ((.3 .1 .4) (.7 .2 .6))		; two outputs (weights from hidden layer)
  ((.1 .2) (.4 .3) (.8 .2)))		; hidden nodes
|#


(define-syntax define-feed-forward-net
  (syntax-rules ()
    ((_ net-name _learning-rate num-inputs output-definition hidden-layer-definition ...)
     (begin
       (format #t "Hidden layers: ~A~%" '(hidden-layer-definition ...))
       (define net-name)
       (let ((learning-rate _learning-rate)

	     (input-values (make-vector num-inputs 0))

	     ;; These are lists of 2D vectors
	     (hidden-layers (map (lambda (layer) (list->vector (map list->vector layer))) '(hidden-layer-definition ...)))
	     (hidden-layers-r '())
	     (hidden-errors (map (lambda (layer) (list->vector (map (lambda (node) (list->vector (map (lambda (nul) 0) node))) layer)))
				 '(hidden-layer-definition ...)))
	     (hidden-errors-r '())
	     (hidden-values (map (lambda (layer) (list->vector (map (lambda (node) (list->vector (map (lambda (nul) 0) node))) layer)))
				 '(hidden-layer-definition ...)))
	     (hidden-values-r '())

	     ;; This is a 2D vector
	     (output-layer (list->vector (map list->vector 'output-definition)))
	     (output-errors (list->vector (map (lambda (nul) 0) 'output-definition)))
	     (output-values (list->vector (map (lambda (node) (list->vector (map (lambda (nul) 0) node))) 'output-definition))))

	 (set! hidden-layers-r (reverse hidden-layers))
	 (set! hidden-errors-r (reverse hidden-errors))
	 (set! hidden-values-r (reverse hidden-values))

	 (set! net-name
	       (lambda (op . args)
		 (define (matrix-* matrix vect)
		   ;; Assumes multiplying n-m matrix by m-1 matrix, which I'll call a vector here
		   (vector-map (lambda (node-row) ; NOTE: There's probably room for some optimization here (e.g. eliminate calls to vector->list)
				  (apply + (map * (vector->list node-row) (vector->list vect))))
				matrix))


		 (define (feed-forward input layers values)
		   (cond
		    ((null? layers) (error "Must have some layers in the network before you can run it!"))
		    ((null? input)  (error "No input given for neural network!"))
		    (else
		     (let ((result (vector-map activation (matrix-* (car layers) input))))
		       (set-car! values result) ; will this work?
		       (if (null? (cdr layers))
			   result
			   (feed-forward result (cdr layers) (cdr values)))))))

		 (case op
		   ;; Primary methods
		   ((run)
		    ;; Runs the neural network: (net 'run (list input1 input2 ...))
		    (set! input-values (list->vector (car args)))
		    (set! output-values (feed-forward input-values `(,@hidden-layers ,output-layer) `(,@hidden-values ,output-values)))
		    output-values)
		   
		   ((train)
		    ;; Runs the backprop algorithm: (net 'train (list input1 input2 ...) (list target1 target2 ...))
		    (let ((inputs (list->vector (car args)))
			  (targets (list->vector (cadr args))))
		      (set! input-values inputs)
		      (set! output-values (feed-forward input-values `(,@hidden-layers ,output-layer) `(,@hidden-values ,output-values)))
		      (set! output-errors (vector-zip (lambda (output target) (* (- target output) (- 1 output) output)) output-values targets))
		      (set! output-layer (vector-zip (lambda (node delta) (vector-zip (lambda (weight value)
											(+ weight (* learning-rate delta value)))
										      node
										      

		   ;; Debugging methods
		   ((debug-hidden-layers)
		    (pretty-print hidden-layers)
		    (newline))

		   ((debug-layers)
		    (map (lambda (layer) (vector-map (lambda (node) (pretty-print node)) layer) (newline)) hidden-layers)
		    (pretty-print output-layer))
		   
		   (else
		    (error (format #f "Unknown option to neural net: '~A'~%" op)))))))))))

(define (vector-zip func vec1 vec2)
  (let ((end-val (apply min (map vector-length (list vec1 vec2)))))
    (define (zipper i acc)
      (if (>= (+ i 1) end-val)
	  (list->vector (reverse! acc))
	  (zipper (+ i 1) (cons (func (vector-ref vec1 i) (vector-ref vec2 i)) acc))))
    (zipper 0 '())))

(define (full-vector-set! vect obj)
  ;; This should set the vector using vector-set!
  (
  