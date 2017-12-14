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

test:
	jbuilder runtest

documentation:
	jbuilder build @docs
	if [ -f _build/default/docs/main.pdf ]; then mv _build/default/docs/main.pdf boomerang.pdf ; fi
