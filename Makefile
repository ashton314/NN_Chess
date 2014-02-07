BIN=$(PWD)/bin
SRC=$(PWD)/src

SCHEME=mit-scheme
SCHEME_FLAGS= --compiler
SCHEME_EVAL=$(SCHEME) $(SCHEME_FLAGS) --eval
SCHEME_EVAL_MACROS=$(SCHEME) $(SCHEME_FLAGS) --load $(BIN)/macros.scm --eval
CF=cf
EXIT=%exit
BLACK_HOLE=/dev/null

ENGINE=$(SRC)/checkers_engine
SEARCH=$(SRC)/game_tree_search
ANN=$(SRC)/neural_networks
UTIL=$(SRC)/utils

# Suffixes
.SUFFIXES: .scm .com

.scm.com:
	cp $< $(BIN)
	cd $(BIN); $(SCHEME_EVAL) "(begin ($(CF) \"$<\") ($(EXIT)))"

# primary targets
install: $(PWD)/player.com

clean:
	rm $(BIN)/*
	rm $(PWD)/player.com

test: $(BIN)/tester.com
#	bin $(BIN); $(SCHEME_EVAL) "(begin ($(CF) \"$\") ($(EXIT)))"

play: $(PWD)/player.com
	$(SCHEME) --load player.com

train: $(BIN)/network_trainer.com

# subtargets
$(PWD)/player.com: $(BIN)/player.com
	cp $(BIN)/player.com $(PWD)

$(BIN)/player.com: $(SRC)/player.scm $(BIN)/engine.com
	cp $(SRC)/player.scm $(BIN)
	cd $(BIN); $(SCHEME_EVAL) "(begin ($(CF) \"player.scm\") ($(EXIT)))"

$(BIN)/engine.com: $(ENGINE)/engine.scm $(SRC)/game_tree_search/negamax.scm $(BIN)/macros.scm $(BIN)/utils.com $(BIN)/neural_network.com
	cp $(BIN)/macros.scm $(BIN)/engine.scm
	cat $(SRC)/game_tree_search/negamax.scm >> $(BIN)/engine.scm
	cat $(ENGINE)/engine.scm >> $(BIN)/engine.scm
	cd $(BIN); $(SCHEME_EVAL_MACROS) "(begin ($(CF) \"engine.scm\") ($(EXIT)))"

$(BIN)/tester.com: $(SRC)/tester.scm $(BIN)/neural_network.com $(BIN)/tap.com
	cp $(SRC)/tester.scm $(BIN)
	cd $(BIN); $(SCHEME_EVAL) "(begin ($(CF) \"tester.scm\") ($(EXIT)))"

$(BIN)/network_trainer.com: $(SRC)/training_routines/network_trainer.scm
	cp $? $(BIN)
	cd $(BIN); $(SCHEME_EVAL) "(begin ($(CF) \"network_trainer.scm\") ($(EXIT)))"

$(BIN)/feature_detector.com: $(ANN)/feature_detector.scm
	cp $? $(BIN)
	cd $(BIN); $(SCHEME_EVAL) "(begin ($(CF) \"feature_detector.scm\") ($(EXIT)))"

$(BIN)/neural_network.com: $(ANN)/neural_network.scm $(BIN)/vector_utilities.com
	cp $(ANN)/neural_network.scm $(BIN)
	cd $(BIN); $(SCHEME_EVAL) "(begin ($(CF) \"neural_network.scm\") ($(EXIT)))"

$(BIN)/macros.scm: $(UTIL)/macros.scm
	cp $? $(BIN)
# 	cd $(BIN); $(SCHEME_EVAL) "(begin ($(CF) \"macros.scm\") ($(EXIT)))"

$(BIN)/tap.com: $(UTIL)/tap.scm
	cp $? $(BIN)
	cd $(BIN);  $(SCHEME_EVAL) "(begin ($(CF) \"tap.scm\") ($(EXIT)))"

$(BIN)/utils.com: $(UTIL)/utils.scm
	cp $? $(BIN)
	cd $(BIN); $(SCHEME_EVAL) "(begin ($(CF) \"utils.scm\") ($(EXIT)))"

$(BIN)/vector_utilities.com: $(UTIL)/vector_utilities.scm
	cp $? $(BIN)
	cd $(BIN); $(SCHEME_EVAL) "(begin ($(CF) \"vector_utilities.scm\") ($(EXIT)))"
