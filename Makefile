SCHEME=mit-scheme
SCHEME_FLAGS= --compiler
SCHEME_EVAL=$(SCHEME) $(SCHEME_FLAGS) --eval
CF=cf
EXIT=%exit
BLACK_HOLE=/dev/null

BIN=$(PWD)/bin
SRC=$(PWD)/src

ENGINE=$(SRC)/checkers_engine
SEARCH=$(SRC)/game_tree_search
ANN=$(SRC)/neural_networks
UTIL=$(SRC)/utils

# Suffixes
.SUFFIXES: .scm .com

.scm.com:
	cp $< $(BIN)
	cd $(BIN); $(SCHEME_EVAL) "(begin ($(CF) \"$<\") ($(EXIT)))"

# top-level targets
clean:
	rm $(BIN)/*

test: $(BIN)/tester.com

# subtargets
$(BIN)/tester.com: $(SRC)/tester.scm $(BIN)/neural_network.com $(BIN)/tap.com
	cp $(SRC)/tester.scm $(BIN)
	cd $(BIN); $(SCHEME_EVAL) "(begin ($(CF) \"tester.scm\") ($(EXIT)))"

$(BIN)/neural_network.com: $(ANN)/neural_network.scm $(BIN)/vector_utilities.com
	cp $(ANN)/neural_network.scm $(BIN)
	cd $(BIN); $(SCHEME_EVAL) "(begin ($(CF) \"neural_network.scm\") ($(EXIT)))"

$(BIN)/vector_utilities.com: $(UTIL)/vector_utilities.scm
	cp $? $(BIN)
	cd $(BIN); $(SCHEME_EVAL) "(begin ($(CF) \"vector_utilities.scm\") ($(EXIT)))"

$(BIN)/tap.com: $(UTIL)/tap.scm
	cp $? $(BIN);
	cd $(BIN);  $(SCHEME_EVAL) "(begin ($(CF) \"tap.scm\") ($(EXIT)))"
