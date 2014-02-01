;;; Mini TAP implementation. See Perl's Test::Simple.
;;; Ashton Wiersdorf

(load-option 'format)

(declare (usual-integrations)
	 (integrate-operator plan-tests ok is is-eq done-testing))

(define plan-tests)
(define ok)
(define is)
(define is-eq)
(define done-testing)

(define *destination* #t)

(let* ((tests-planned 0)
       (tests-run 0)
       (tests-succeeded 0)
       (length-tests 2))
  (set! plan-tests (lambda (tests)
		     (set! tests-planned tests)
		     (set! length-tests (+ 1 (truncate->exact (/ (log (+ tests 1)) (log 10)))))
		     (write-string "1..")
		     (write-line tests)))
  (set! ok (lambda (test test-name)
	     (set! tests-run (+ tests-run 1))
	     (if test
		 (begin
		   (set! tests-succeeded (+ tests-succeeded 1))
		   (format *destination* "~v@A ~5A - ~A~%" length-tests tests-run "ok" test-name))
		 (begin
		   (format *destination* "~v@A ~5A - ~A~%" length-tests tests-run "fail" test-name)))))
  (set! is (lambda (two one test-name)
	     (set! tests-run (+ tests-run 1))
	     (if (equal? one two)
		 (begin
		   (set! tests-succeeded (+ tests-succeeded 1))
		   (format *destination* "~v@A ~5A - ~A~%" length-tests tests-run "is" test-name))
		 (begin
		   (format *destination* "~v@A ~5A - ~A~%Expected: ~A~%     Got: ~A~2%" length-tests tests-run "fail" test-name
			   one two)))))
  (set! is-eq (lambda (two one test-name)
		(set! tests-run (+ tests-run 1))
		(if (eq? one two)
		    (begin
		      (set! tests-succeeded (+ tests-succeeded 1))
		      (format *destination* "~v@A ~5A - ~A~%" length-tests tests-run "is" test-name))
		    (begin
		   (format *destination* "~v@A ~5A - ~A~%Expected: ~A~%     Got: ~A~2%" length-tests tests-run "fail" test-name
			   one two)))))
  (set! done-testing (lambda ()
		       (if (= tests-run tests-succeeded)
			   (if (> 1 tests-planned)
			       (write-string "All tests successful.\n") ; No tests planned
			       (if (= tests-planned tests-run)
				   (write-string "All tests successful.\n")
				   (begin
				     (write-string "Looks like you planned to run ")
				     (write tests-planned)
				     (write-string " but ran only ")
				     (write tests-run)
				     (write-string ".\n")
				     (if (= tests-planned tests-run)
					 (begin
					   (write-string "All tests run.\n")
					   (write tests-succeeded)
					   (write-string " out of ")
					   (write tests-run)
					   (write-string " tests successful.\n"))
					 (begin
					   (write tests-run)
					   (write-string " out of ")
					   (write tests-planned)
					   (write-strings " tests run.\n")
					   (write tests-succeeded)
					   (write-string " out of ")
					   (write tests-run)
					   (write-string " tests successful.\n"))))))
			   (if (> 1 tests-planned)
			       (begin
				 (write tests-succeeded)
				 (write-string " out of ")
				 (write tests-run)
				 (write-string " tests successful.\n"))
			       (if (= tests-planned tests-run)
				   (begin
				     (write-string "All tests run.\n")
				     (write tests-succeeded)
				     (write-string " out of ")
				     (write tests-run)
				     (write-string " tests successful.\n"))
				   (begin
				     (write tests-run)
				     (write-string " out of ")
				     (write tests-planned)
				     (write-strings " tests run.\n")
				     (write tests-succeeded)
				     (write-string " out of ")
				     (write tests-run)
				     (write-string " tests successful.\n"))))))))
