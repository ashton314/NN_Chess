;;; Network training data generator
;;; Part of the NN_Chess project
;;; Ashton Wiersdorf

(load "engine.scm")
(load "negamax.scm")

(define *session-id* (- (get-universal-time) epoch))

