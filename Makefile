all:
	@dune build

serve:
	@dune exec src/server.exe

ci:
	git ci . -m "Worked on screen recorder."
	git push

-include Makefile.local
