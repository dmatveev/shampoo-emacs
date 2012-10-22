SHELL = /bin/sh
EMACS = emacs
FILES = $(wildcard shampoo*.el)

ELCFILES = $(FILES:.el=.elc)

.PHONY: all compile compile-batch clean

all: compile
compile: $(ELCFILES)

.depend: $(FILES)
	@echo Computing dependencies
	@rm -f .depend
	@for f in $(FILES); do \
	    sed -n "s/(require '\(shampoo-.*\))/$${f}c: \1.elc/p" $$f >> .depend;\
	done

-include .depend

$(ELCFILES): %.elc: %.el
	$(EMACS) --batch -Q -L . $(LIBS) -f batch-byte-compile $<

# Byte-compile all files in one batch. This is faster than
# compiling each file in isolation, but also less stringent.
compile-batch: clean
	$(EMACS) --batch -Q -L . $(LIBS) -f batch-byte-compile ${FILES}

clean:
	rm -f *~
	rm -f \#*\#
	rm -f *.elc
	rm -f .depend
