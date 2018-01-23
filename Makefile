TARGETS=boomerang.exe

.PHONY: all build clean %.exe

all: build link

build:
	jbuilder build

link: $(TARGETS)

%.exe:
	if [ ! -f $@ ]; then ln -s _build/default/bin/$@ . ; fi

install:
	jbuilder install

clean:
	rm -rf _build *.install *.pdf $(TARGETS)

functionaltest: all
	for file in *.src ; do \
		./boomerang.exe $$file ; \
	done
	for file in examples/*.boom ; do \
		./boomerang.exe $$file ; \
	done
	for file in examples/*.src ; do \
		./boomerang.exe $$file ; \
	done
	for file in examples/*/*.boom ; do \
		./boomerang.exe $$file ; \
	done
	for file in examples/*/*/*.boom ; do \
		./boomerang.exe $$file ; \
	done

unittest:
	jbuilder runtest

test: unittest functionaltest

documentation:
	jbuilder build @docs
	if [ -f _build/default/docs/main.pdf ]; then mv _build/default/docs/main.pdf boomerang.pdf ; fi
