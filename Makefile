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
	rm -rf _build *.install $(TARGETS)

test:
	jbuilder runtest
