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
       (define net-name)
       (let ((learning-rate _learning-rate)

	     (input-values (make-vector num-inputs 0))

	     ;; These are lists of 2D vectors
	     ;; ( #(#(a1 a2 a3) #(b1 b2 b3) ...) #( ... ))  ( layer layer ... ) layer => ((weights ...) (weights ...))
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
	       (format #t "Layer: ~A~%" result)
	       (vector-clobber! (car values) result)
	       (if (null? (cdr layers))
		   (begin
;; 		     (write-string "Nodes after feed-forward:<<__END__\n")
;; 		     (map (lambda (layer) (vector-map (lambda (node) (pretty-print node)) layer) (newline)) hidden-layers)
;; 		     (pretty-print output-layer)
;; 		     (newline)
;; 		     (write-string "__END__\n")
		     result)
		   (feed-forward result (cdr layers) (cdr values)))))))

	 (define (transpose matrix)	; working
	   (list->vector (reverse! (map (lambda (n) (vector-map (lambda (row) (vector-ref row n)) matrix))
					(range 0 (- (vector-length (vector-ref matrix 0)) 1))))))

	 (define (back-prop weights errors values forward-errors)
	   (format #t "Before back-prop:~%  Weights: '~A'~%  Errors: '~A'~%  Values: '~A'~%  Forward-errors: '~A'~%" weights errors values forward-errors)
	   (vector-clobber! (car errors) (vector-zip (lambda (val from-next-node)
;						       (format #t "ZIPING: val: '~A' from-next-node: '~A'~%" val from-next-node)
						       (* val (- 1 val) from-next-node))
						     (car values) forward-errors))
	   
	   (vector-clobber! (car weights) (vector-zip (lambda (current-node-in-layer this-nodes-error)
							(vector-zip (lambda (old-weight input-nodes-value)
								      (+ old-weight (* old-weight learning-rate this-nodes-error
										       input-nodes-value)))
								    current-node-in-layer input-values))
						      (car weights)
						      (car errors)))
;	   (format #t "After back-prop:~%  Weights: '~A'~%  Errors: '~A'~%~%" weights errors)
	   (if (null? (cdr weights))
	       #t
	       (back-prop (cdr weights) (cdr errors) (cdr values) (matrix-* (car weights) (car errors)))))


	 (set! net-name
	       (lambda (op . args)
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

		      ; update output deltas
;		      (format #t "Before backprop (output):~%  Weights: '~A'~%  Values: '~A'~%  Errors: '~A'~%" output-layer output-values output-errors)
		      (set! output-values (feed-forward input-values `(,@hidden-layers ,output-layer) `(,@hidden-values ,output-values)))
		      (set! output-errors (vector-zip (lambda (output target) ; vector
							(* (- target output) (- 1 output) output)) output-values targets))
		      (set! output-layer (vector-zip (lambda (node delta) (vector-zip (lambda (current-weight value)
											(+ current-weight (* learning-rate delta value)))
										      node (car hidden-values-r)))
						     output-layer output-errors))

;		      (format #t "After backprop (output):~%  Weights: '~A'~%  Values: '~A'~%  Errors: '~A'~%" output-layer output-values output-errors)

;; 		      (format #t "Hidden Values: ~A~%" hidden-values)
;; 		      (format #t "Last errors: ~A~%" output-errors)

		      ; backpropogate
		      (let ((forward-errors (apply vector-mapn (cons (lambda (#!rest weights)
								       (vector-reduce + (vector-mapn * (list->vector weights) output-errors)))
								     (vector->list output-layer)))))
			(back-prop hidden-layers-r hidden-errors-r hidden-values-r
				   forward-errors))))


		   ((last-errors)
		    output-errors)

		   ;; Debugging methods
		   ((debug-hidden-layers)
		    (pretty-print hidden-layers)
		    (newline))

		   ((debug-layers)
		    (map (lambda (layer) (vector-map (lambda (node) (pretty-print node)) layer) (newline)) hidden-layers)
		    (pretty-print output-layer)
		    (newline))
		   
		   (else
		    (error (format #f "Unknown option to neural net: '~A'~%" op)))))))))))

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