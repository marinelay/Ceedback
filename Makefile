#load "str.cma"

TARGET = run

all: $(TARGET)

$(TARGET):  vocab.cmo imp.cmo  label_lang.cmo  labeling.cmo localize.cmo
	ocamlc -o $@ $^


vocab.cmo : vocab.ml
	ocamlc -c vocab.ml

imp.cmo : imp.ml
	ocamlc -c imp.ml

label_lang.cmo : label_lang.ml
	ocamlc -c  label_lang.ml

labeling.cmo : labeling.ml
	ocamlc -c  labeling.ml

localize.cmo : localize.ml 
	ocamlc -c  localize.ml


clean:
	rm -f *.cmx *.cmi parser.mli parser.ml lexer.ml run *.o *.cmo