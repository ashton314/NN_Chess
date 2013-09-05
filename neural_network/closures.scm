;;; Neural Network library, using closures
;;; Ashton Wiersdorf
;;; Part of the NN_Chess project

(define *threshold* 1)
(define *learning-rate* 0.01)
(define *true-signal* 1)
(define *false-signal* 0)

(define (make-node . data)
  (let ((input-nodes  (or (assoc 'input-nodes data) '()))
	(weights      (or (assoc 'weights data) '()))
	(output-nodes (or (assoc 'output-nodes data) '()))
	(threshold    (or (assoc 'threshold data) *threshold*))
	(function     (or (assoc 'function data) (lambda (inputs weights)
						   (+ (map * inputs weights))))))
    (lambda (operation)
      (case (operation)			; This could be a symbol or a list of values as inputs
	((input-nodes) (lambda (op . rest) (if (eq? op 'get) input-nodes (set! input-nodes rest))))
	((weights) (lambda (op . rest) (if (eq? op 'get) weights (set! weights rest))))
	((output-nodes) (lambda (op . rest) (if (eq? op 'get) output-nodes (set! output-nodes rest))))
	((threshold) (lambda (op . rest) (if (eq? op 'get) threshold (set! threshold (car rest)))))
	((function) (lambda (op . rest) (if (eq? op 'get) function (set! function (car rest)))))
	(#t (if (or (not (pair? operation)) (not (= (length operation) (length weights))))
		(error "Wrong input to neural node!")
		(if (>= (function operation weights) threshold)
		    *true-signal*
		    *false-signal*)))))))



	