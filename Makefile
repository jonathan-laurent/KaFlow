KaFlow: KaFlow.native
	cp $< $@

KaFlow.native:
	ocamlbuild -use-ocamlfind $@

clean:
	rm -rf _build KaFlow KaFlow.native
