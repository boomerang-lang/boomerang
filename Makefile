TARGETS=boomerang.exe

GENERATE_DATA := python generate-data.py

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
		./boomerang.exe $$file || exit ; \
	done
	for file in examples/*.boom ; do \
		./boomerang.exe $$file || exit ; \
	done
	for file in examples/*.src ; do \
		./boomerang.exe $$file || exit ; \
	done
	for file in examples/*/*.boom ; do \
		./boomerang.exe $$file || exit ; \
	done
	for file in examples/*/*/*.boom ; do \
		./boomerang.exe $$file || exit ; \
	done
	for file in examples/*/*/*/*.boom ; do \
		./boomerang.exe $$file || exit ; \
	done

unittest:
	jbuilder runtest

test: unittest functionaltest

generate-data: all
	$(GENERATE_DATA) ./boomerang.exe examples/synth_examples/bijective_optician_tests/
	$(GENERATE_DATA) ./boomerang.exe examples/synth_examples/symmetric_optician_tests/

documentation:
	jbuilder build @docs
	if [ -f _build/default/docs/main.pdf ]; then mv _build/default/docs/main.pdf boomerang.pdf ; fi
