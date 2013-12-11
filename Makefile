SCHEME=mit-scheme
SCHEME_FLAGS= --compiler
CF=cf
EXIT=%exit
BLACK_HOLE=/dev/null

COMMON_UTILS=src/utils/macros.scm src/utils/utils.scm

# Suffixes
.SUFFIXES: .scm .com

# Defaults
compile: player.com trainer.com generator.com

clean:
	rm bin/*

test:
	echo "not implemented"

run:
	${SCHEME} --load checkers.scm

generate:
	${SCHEME} --heap 115941 --load data_generator.scm

train:
	${SCHEME} --heap 115941 --load network_trainer.scm

doc: README

# Files
player.com: src/misc/header.scm ${COMMON_UTILS} src/player.scm src/checkers_engine/engine.scm src/game_tree_search/negamax.scm src/neural_networks/neural_network.scm
	cat src/misc/header.scm ${COMMON_UTILS} src/player.scm src/checkers_engine/engine.scm src/game_tree_search/negamax.scm src/neural_networks/neural_network.scm > bin/player.scm
	cd bin; ${SCHEME} ${SCHEME_FLAGS} --eval "(begin (${CF} \"player.scm\") (${EXIT}))"

trainer.com: src/misc/header.scm ${COMMON_UTILS} src/checkers_engine/engine.scm src/game_tree_search/negamax.scm src/neural_networks/neural_network.scm src/checkers_engine/feature_detector.scm src/training_routines/network_trainer.scm
	cat src/misc/header.scm ${COMMON_UTILS} src/checkers_engine/engine.scm src/game_tree_search/negamax.scm src/neural_networks/neural_network.scm src/checkers_engine/feature_detector.scm src/training_routines/network_trainer.scm > bin/trainer.scm
	cd bin; ${SCHEME} ${SCHEME_FLAGS} --eval "(begin (${CF} \"trainer.scm\") (${EXIT}))"

generator.com: src/misc/header.scm ${COMMON_UTILS} src/checkers_engine/engine.scm src/game_tree_search/negamax.scm src/training_routines/data_generator.scm
	cat src/misc/header.scm ${COMMON_UTILS} src/checkers_engine/engine.scm src/game_tree_search/negamax.scm src/training_routines/data_generator.scm > bin/generator.scm
	cd bin; ${SCHEME} ${SCHEME_FLAGS} --eval "(begin (${CF} \"generator.scm\") (${EXIT}))"

bin/tap.com: src/utils/tap.scm
	cp $? bin/
	cd bin/; ${SCHEME} ${SCHEME_FLAGS} --eval "(begin (${CF} \"tap.scm\") (${EXIT}))"

README: README.pod
	pod2text README.pod README
