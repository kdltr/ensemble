PREFIX ?= /usr/local

ensemble:
	./build.sh

install: ensemble ensemble.backend.matrix
	cp -v $^ ${PREFIX}/bin/
