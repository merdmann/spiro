.SUFFIXES: .mli .cmi


target=main.byte
builddir=../build

%.cmi : %.mli
	ocamlc -g -c $<

%.cmo : %.ml
	ocamlc -g -c $<

src=drawables.mli files.mli  helper.mli transformation.mli script.mli main.mli \
	drawables.ml  files.ml  helper.ml  transformation.ml  script.ml main.ml


all: dirs $(src)
	ocamlbuild $(target) -build-dir $(builddir) -lib graphics

dirs:
	mkdir -p $(builddir)


clean :
	ocamlbuild -build-dir $(builddir) -clean $(target)
	rm -rf $(builddir)
