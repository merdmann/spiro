.SUFFIXES: .mli .cmi

%.cmi : %.mli
	ocamlc -g -c $<

%.cmo : %.ml
	ocamlc -g -c $<

src=drawables.mli files.mli  helper.mli transformation.mli script.mli main.mli \
	drawables.ml  files.ml  helper.ml  transformation.ml  script.ml main.ml


all: $(src)
	ocamlbuild main.byte -lib graphics

clean :
	rm -rf *.cmi *.cmo _build
