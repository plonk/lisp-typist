run:
	sbcl --load lisp-typist.lisp --eval '(lisp-typist:main)'

build:
	sbcl --load lisp-typist.lisp --eval '(sb-ext:save-lisp-and-die "lisp-typist" :toplevel #'"'"'lisp-typist:main :save-runtime-options t :executable t)'
