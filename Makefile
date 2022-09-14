.PHONY: all release clean

all: release

release:
	sbcl --noinform --non-interactive --load release.lisp

clean:
	rm ./bin
