SCHEME=mit-scheme
SCHEME_FLAGS= --eval
BLACK_HOLE=/dev/null

COMMON_UTILS= bin/macros.scm bin/utils.scm

# Suffixes
.SUFFIXES: .scm .com

.scm.com:
	${SCHEME} ${SCHEME_FLAGS} "(begin (cf \"$<\" \"bin/\") (%exit))" > ${BLACK_HOLE} # WARNING!! MIT-SCHEME specific functions!!

# Defaults
compile:
	echo "not implemented"

clean:
	echo "not implemented"

# Files
player.com: bin/engine.com

bin/engine.com: src/checkers_engine/engine.scm ${COMMON_UTILS} bin/negamax.com bin/neural_network.com

README: README.pod
	pod2text README.pod README
