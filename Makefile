all:
	jbuilder build --dev @install @DEFAULT

install:
	jbuilder install

uninstall:
	jbuilder uninstall

reinstall: uninstall install

doc:
	jbuilder build @doc

test:
	jbuilder runtest

clean:
	jbuilder clean

.PHONY: all install uninstall doc test clean
