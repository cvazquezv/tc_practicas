
all: regexp parser lexer derive grep

regexp: regexp.mli regexp.ml
	ocamlc -c regexp.mli regexp.ml

parser: regexp_parser.mly
	ocamlyacc regexp_parser.mly
	ocamlc -c regexp_parser.mli regexp_parser.ml

lexer: regexp_lexer.mll
	ocamllex regexp_lexer.mll
	ocamlc -c regexp_lexer.ml

derive: derive.mli derive.ml
	ocamlc -c derive.mli derive.ml

grep: grep.ml
	ocamlc -o grep regexp.cmo regexp_parser.cmo regexp_lexer.cmo derive.cmo grep.ml

clean:
	rm -f *.cmo *.cmi grep regexp_lexer.ml regexp_parser.ml regexp_parser.mli

