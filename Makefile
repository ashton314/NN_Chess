SCHEME=mit-scheme
SCHEME_FLAGS= --eval
BLACK_HOLE=/dev/null

COMMON_UTILS= bin/macros.scm bin/utils.scm

# Suffixes
.SUFFIXES: .scm .com

.scm.com:
	cp $< bin/
	${SCHEME} ${SCHEME_FLAGS} "(begin (cf \"bin/$<\") (%exit))" > ${BLACK_HOLE} # WARNING!! MIT-SCHEME specific functions!!

# Defaults
compile:
	echo "not implemented"

clean:
	echo "not implemented"

# Files
player.com: src/player.scm bin/engine.com
	cp src/player.scm bin/
	$(SCHEME) $(SCHEME_FLAGS) "(begin (cf "bin/player.scm") (%exit))" > ${BLACK_HOLE}

bin/engine.com: src/checkers_engine/engine.scm ${COMMON_UTILS} bin/negamax.com bin/neural_network.com
	cp src/checkers_engine/engine.scm bin/
	${SCHEME} ${SCHEME_FLAGS} ## FIXME: NOT FINISHED HERE

README: README.pod
	pod2text README.pod README
