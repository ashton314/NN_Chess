SCHEME=mit-scheme
SCHEME_FLAGS= --eval
CF=cf
EXIT=%exit
BLACK_HOLE=/dev/null

COMMON_UTILS= bin/macros.com bin/utils.com

# Suffixes
.SUFFIXES: .scm .com

.scm.com:
	cp $< bin/
	${SCHEME} ${SCHEME_FLAGS} "(begin (${CF} \"bin/$<\") (${EXIT}))" > ${BLACK_HOLE}

# Defaults
compile: player.com

clean:
	echo "not implemented"

test:
	echo "not implemented"

doc: README

# Files
player.com: src/player.scm bin/engine.com
	cp src/player.scm bin/
	${SCHEME} ${SCHEME_FLAGS} "(begin (${CF} \"bin/player.scm\") (${EXIT}))" > ${BLACK_HOLE}

bin/engine.com: src/checkers_engine/engine.scm ${COMMON_UTILS} bin/negamax.com bin/neural_network.com
	cp src/checkers_engine/engine.scm bin/
	${SCHEME} ${SCHEME_FLAGS} "(begin (${CF} \"bin/checkers_engine.scm\") (${EXIT}))" > ${BLACK_HOLE}

bin/negamax.com: src/game_tree_search/negamax.scm
	cp $? bin/
	${SCHEME} ${SCHEME_FLAGS} "(begin (${CF} \"$@\") (${EXIT}))" > ${BLACK_HOLE}

bin/neural_network.com: src/neural_networks/neural_network.scm
	cp $? bin/
	${SCHEME} ${SCHEME_FLAGS} "(begin (${CF} \"$@\") (${EXIT}))" > ${BLACK_HOLE}

bin/macros.com: src/utils/macros.scm
	cp $? bin/
	${SCHEME} ${SCHEME_FLAGS} "(begin (${CF} \"$@\") (${EXIT}))" > ${BLACK_HOLE}

bin/utils.com: src/utils/utils.com
	cp $? bin/
	${SCHEME} ${SCHEME_FLAGS} "(begin (${CF} \"$@\") (${EXIT}))" > ${BLACK_HOLE}

bin/tap.com: src/utils/tap.scm
	cp $? bin/
	${SCHEME} ${SCHEME_FLAGS} "(begin (${CF} \"$@\") (${EXIT}))" > ${BLACK_HOLE}

README: README.pod
	pod2text README.pod README
