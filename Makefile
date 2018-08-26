DUNE=dune

all:
	$(DUNE) build --dev @install @DEFAULT

install:
	$(DUNE) install

uninstall:
	$(DUNE) uninstall

reinstall: uninstall install

doc:
	$(DUNE) build @doc

test:
	$(DUNE) runtest

clean:
	$(DUNE) clean

.PHONY: all install uninstall doc test clean
