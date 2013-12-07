SCHEME=mit-scheme
SCHEME_FLAGS= --compiler
CF=compile-file
EXIT=%exit
BLACK_HOLE=/dev/null

COMMON_UTILS= bin/macros.com bin/utils.com

# Suffixes
.SUFFIXES: .scm .com

# .scm.com:
# 	cp $< bin/
# 	${SCHEME} ${SCHEME_FLAGS} --eval "(begin (${CF} \"bin/$<\") (${EXIT}))" > ${BLACK_HOLE}

# Defaults
compile: player.com

clean:
	rm bin/*

test:
	echo "not implemented"

doc: README

# Files
player.com: src/player.scm bin/engine.com
	cp src/player.scm bin/
	cd bin/; ${SCHEME} ${SCHEME_FLAGS} --eval "(begin (${CF} \"player.scm\") (${EXIT}))"

bin/engine.com: src/checkers_engine/engine.scm ${COMMON_UTILS} bin/negamax.com bin/neural_network.com bin/board_utils.com
	cp src/checkers_engine/engine.scm bin/
	cd bin/; ${SCHEME} ${SCHEME_FLAGS} --eval "(begin (${CF} \"engine.scm\") (${EXIT}))"

bin/negamax.com: src/game_tree_search/negamax.scm bin/board_utils.com
	cp src/game_tree_search/negamax.scm bin/
	cd bin/; ${SCHEME} ${SCHEME_FLAGS} --eval "(begin (${CF} \"negamax.scm\") (${EXIT}))"

bin/neural_network.com: src/neural_networks/neural_network.scm
	cp $? bin/
	cd bin/; ${SCHEME} ${SCHEME_FLAGS} --eval "(begin (${CF} \"neural_network.scm\") (${EXIT}))"

bin/macros.com: src/utils/macros.scm
	cp $? bin/
	cd bin/; ${SCHEME} ${SCHEME_FLAGS} --eval "(begin (${CF} \"macros.scm\") (${EXIT}))"

bin/utils.com: src/utils/utils.scm
	cp $? bin/
	cd bin/; ${SCHEME} ${SCHEME_FLAGS} --eval "(begin (${CF} \"utils.scm\") (${EXIT}))"

bin/board_utils.com: src/utils/board_utils.scm
	cp $? bin/
	cd bin/; ${SCHEME} ${SCHEME_FLAGS} --eval "(begin (${CF} \"board_utils.scm\") (${EXIT}))"

bin/tap.com: src/utils/tap.scm
	cp $? bin/
	cd bin/; ${SCHEME} ${SCHEME_FLAGS} --eval "(begin (${CF} \"tap.scm\") (${EXIT}))"

README: README.pod
	pod2text README.pod README
