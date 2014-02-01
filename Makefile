SCHEME=mit-scheme
SCHEME_FLAGS= --compiler
CF=cf
EXIT=%exit
BLACK_HOLE=/dev/null

# Suffixes
.SUFFIXES: .scm .com

.scm.com:
	${SCHEME} ${SCHEME_FLAGS} --eval "(begin (${CF} \"$<\") (${EXIT}))"

# top-level targets

# subtargets

maketest: foo.com
	echo "Compiled foo"
