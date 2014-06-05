all: compile

compile:
	ocamlopt -c value.ml
	ocamlopt -o valueTest value.cmx valueTest.ml
	ocamlopt -c value.cmx grid.ml
	ocamlopt -o gridTest value.cmx grid.cmx gridTest.ml
	ocamlopt -o gridRun value.cmx grid.cmx gridRun.ml
	ocamlopt -o gridRunTest value.cmx grid.cmx gridRun.cmx gridRunTest.ml

valueTest: compile
	./valueTest

gridTest: compile
	./gridTest

gridRunTest: compile
	./gridRunTest

run: compile
	./gridRun

lineCount:
	git diff --stat `git hash-object -t tree /dev/null` | tail -1

clean:
	rm *.cmi *.cmx *.o
