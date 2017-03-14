KaFlow: KaFlow.native
	cp $< $@

KaFlow.native:
	ocamlbuild $@

clean:
	rm -rf _build KaFlow KaFlow.native
