;;; Checkers Engine Benchmarks
;;; Ashton Wiersdorf
;;; Part of the NN_Chess project

;; (load "engine.scm")

(define *log-benchmarks* (prompt-for-confirmation "Log benchmarks? "))
(define *log-stream* (if *log-benchmarks* (open-output-file (format #f "DATA/BENCHMARK_DATA/~A_log" (- (get-universal-time) epoch)) #t) #t))

(format #t "Comment: ")
(define *comment* (read-line))

;; Helper functions
(define (disp-stats benchmark-name)
  (lambda (run-time gc-time real-time)
    (format *log-stream* "BENCHMARK: ~A
---------------------------------------------
RUN TIME:  ~A
GC TIME:   ~A
REAL TIME: ~A

Comment: ~A\n\n" (string-upcase benchmark-name) (internal-time/ticks->seconds run-time)
(internal-time/ticks->seconds gc-time) (internal-time/ticks->seconds real-time) *comment*)))

;; Benchmark routines

(define (negamax-benchmarks)
  (with-timings
   (lambda ()
     ;; do 10 3-deep negamax searches in the midgame
     (let ((board #(#(0 1 0 1) #(1 0 0 0) #(1 1 0 0) #(0 0 0 0) #(1 -1 -1 0) #(0 -1 -1 0) #(-1 0 -1 0) #(0 -1 0 -1))))
       (write-string (format-board board))
       (write-string "----------\n")
       (do ((i 1 (+ i 1)))
	   ((= i 10) #t)
	 (write-string "-")
	 (negamax board 'white 4 #f '()))
       (newline)))
   (disp-stats "midgame negamax search (depth 4, 10 iterations)")))
   

;; High-level chunks
(negamax-benchmarks)

(if (port? *log-stream*) (close-port *log-stream*))
