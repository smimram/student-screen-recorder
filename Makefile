all:
	@dune build

test:
	@dune exec src/server.exe

ci:
	git ci . -m "Worked on screen recorder."
	git push
