compile:
	mit-scheme --eval "(map (lambda (file) (cf file \"bin/\")) (list \"data_generator.scm\" \"engine.scm\" \"feature_detector.scm\" \"negamax.scm\"))"

clean:
	rm *.bin

run:
	mit-scheme --load ../../lib/macros.com ../../lib/utils.com bin/engine.com bin/negamax.com bin/feature_detector.com player.scm

test:
	mit-scheme --load engine_test.t 
