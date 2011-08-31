# makefile
#

# source and target folders
SRC=src
INC=include
BIN=ebin

# compile options
OPTS=-I "$(INC)" -o "$(BIN)"
CC=erlc
MKDIR=mkdir
RM=rm

# list of header files
HEADERS=$(INC)/parsec.hrl $(INC)/lexer.hrl

# compile parsec and all sample languages
all: parsec samples

# wipe all previously compile files
clean: 
	$(RM) -rf $(BIN)/

# build the parsec and lexeme libraries
parsec: parsec.beam lexer.beam

# build sample languages
samples: lisp

# sample language definitions
lisp: lisp_parser.beam

# compile a source
%.beam: $(SRC)/%.erl $(HEADERS)
	$(MKDIR) -p $(BIN)/
	$(CC) $(OPTS) $<
