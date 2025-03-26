OCAMLBUILD=ocamlbuild
OCAMLFIND=ocamlfind
OBJS=$(wildcard _build/*.cm* _build/*.a _build/*.o)
# OBJS=$(wildcard _build/*.{cmi,cmo,cma,cmx,cmxa,a,o})

all: bc-lib nc-lib

install: bc-lib nc-lib
	$(OCAMLFIND) install safa META $(OBJS)

uninstall:
	$(OCAMLFIND) remove safa

nc-lib: 
	$(OCAMLBUILD) safa.cmxa

bc-lib: 
	$(OCAMLBUILD) safa.cma

doc:
	$(OCAMLBUILD) safa.docdir/index.html

clean:
	$(OCAMLBUILD) -clean 
