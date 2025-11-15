all:
	@dune build

serve:
	@dune exec src/server.exe

test:
	@dune runtest

ci:
	git ci . -m "Worked on screen recorder."
	git push

-include Makefile.local

.PHONY: test
