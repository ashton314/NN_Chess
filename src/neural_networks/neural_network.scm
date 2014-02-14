;;; Feed-Forward Neural Network library, using matricies
;;; Ashton Wiersdorf
;;; Part of the NN_Chess project

(load-option 'format)
(declare (usual-integrations)
	 (integrate *debug*)
	 (integrate-external "vector_utilities"))

(define *debug* #f)

(define-syntax debugging
  (syntax-rules ()
    ((_ (level) body ...)
     (if (and *debug* (if (number? *debug*) (<= level *debug*) #t))
	 (begin body ...)))
    ((_ (level flag) body ...)
     (if (or flag
	     (and *debug* (if (number? *debug*) (<= level *debug*) #t)))
	 (begin body ...)))))

(define-syntax push!
  (syntax-rules ()
    ((_ datum place)
     (set! place (cons datum place)))))

(define-syntax pop!
  (syntax-rules ()
    ((_ place)
     (let ((temp1 (car place)))
       (set! place (cdr place))
       temp1))))

(define-integrable (sigmoid val)
  ;; Sigmoid function
  (debugging (3 #f) (format #t "SIGMOID: ~A\n" val))
  (/ 1 (+ 1 (exp (- val)))))

(define-integrable (sigmoid-derivitive val)
  ;; d/dx sigmoid
  (* val (- 1 val)))

#|
Usage:
(define-ffn .7 2
  '(3 2))

(define-ffn .7 2
  '(((.2 .3 .1) (.5 .4 .7) (.2 .3 .1))
    2))
|#

(declare (integrate-operator define-ffn))
(define define-ffn
  (lambda (learning-rate input-count layers-definition . options)
    ;; returns quasi-object closure
    (let ((layers (initialize-layers input-count layers-definition)))
      (lambda (op . args)
	(case op
	  ;; primary routines
	  ((run) (car (forward-prop (car args) layers)))
	  ((run-with-values) (forward-prop (car args) layers))

	  ((genetic-code) (error "option `genetic-code' not implemented")) ; returns genetic code to network (for genetic algorithms)

	  ;; setters
	  ((set-learning-rate) (set! learning-rate (car args)))
	  ((set-layers) (set! layers (car args)))

	  ;; getters
	  ((get-learning-rate) learning-rate)
	  ((get-layers) layers)

	  ;; debugging routines
	  ((pp-layers) (map (lambda (layer) (write-string "----------\n") (pp layer)) layers)))))))


(define-integrable (forward-prop inputs all-layers)
  (define (loop input layers values-acc)  ; values-acc holds node values in REVERSE ORDER
    (debugging (2) (format #t "\n##### INPUT: ~A #####\n" input))
    (if (null? layers)			  ; forward pass finished
	(cons input values-acc)
	(let ((vals (map (lambda (node) ; add bias here ---V
			   (let* ((multiplied (map * (cons 1 input) node))
				  (summed (reduce + 0 multiplied))
				  (sigmoided (sigmoid summed)))
 			     (debugging (3) (format #t "## 0: ~A\n## 1: ~A\n## *: ~A\n## +: ~A\n## s:~A\n" (cons 1 inputs) node multiplied summed sigmoided))
			     sigmoided))
			 (car layers))))
 	  (debugging (2)
		     (format #t "##### LAYER PROCESSED: ~A #####\n" (car layers)))
	  (loop vals (cdr layers) (cons vals values-acc)))))
  (loop inputs all-layers '()))

;; utility routines
(define-integrable (initialize-layers input-count all-layers)
  (define (loop previous-input-count layers acc)
    (if (null? layers)
	(reverse! acc)
	(cond ((integer? (car layers))
	       (loop (car layers) (cdr layers)
		     (cons (map		                                          ; cons->vector mods here
			    (lambda (node)
			      (randomize (make-list (+ previous-input-count 1) 0))) ; cons->vector mods here
			    (make-list (car layers)))
			   acc)))

	      ((and (pair? (car layers))
		    (for-all? (car layers) list?)) ; weights specified
	       (loop (length (car layers)) (cdr layers)
		     (cons (map
			    (lambda (node)
			      (if (not (= (length node) (+ 1 previous-inputs)))
				  (error (format #f "too few weights specified in layer ~A (you might have forgotten to account for the bias)" node))
				  (if (for-all? node (lambda (n) (and (integer? n) (= n 0))))
				      (randomize node)                            ; cons->vector mods here
				      node)))                                     ; cons->vector mods here
			    (car layers))
			   acc)))

	      (else (error "Malformed argument list to initialize-layers (probably from a call to define-ffn)")))))
  (loop input-count all-layers '()))
