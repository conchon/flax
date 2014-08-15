
ECHO Compiling the graph library

ocamlc -c sig.mli
ocamlc -c util.mli
ocamlc -c util.ml
ocamlc -c bitv.mli
ocamlc -c bitv.ml
ocamlc -c blocks.ml
ocamlc -c imperative.mli
ocamlc -c imperative.ml
ocamlc -c traverse.mli
ocamlc -c traverse.ml
ocamlc -c graphviz.mli
ocamlc -c graphviz.ml
ocamlc -pack -o graph.cmo sig.cmi util.cmo bitv.cmo blocks.cmo imperative.cmo traverse.cmo graphviz.cmo
ocamlc -a -o graph.cma graph.cmo

ECHO Compiling Flax 

ocamlc -c version.ml
ocamlc -c options.mli
ocamlc -c options.ml
ocamlc -c common.mli
ocamlc -c common.ml
ocamlc -c ast.mli
ocamlyacc parser.mly
ocamllex lexer.mll
ocamlc -c parser.mli
ocamlc -c parser.ml
ocamlc -c lexer.ml
ocamlc -c fabrics.mli
ocamlc -c fabrics.ml
ocamlc -c galm.mli
ocamlc -c galm.ml
ocamlc -c compile.mli
ocamlc -c compile.ml
ocamlc -c promela.mli
ocamlc -c promela.ml
ocamlc -c cubicle.mli
ocamlc -c cubicle.ml
ocamlc -c main.ml
ocamlc -o flax.exe graph.cma version.cmo options.cmo common.cmo parser.cmo lexer.cmo fabrics.cmo galm.cmo compile.cmo promela.cmo cubicle.cmo main.cmo

ECHO You can now use flax.exe
