#!/bin/bash

ocamllex lexer.mll
ocamlyacc -v parser.mly
ocamlc -c support.mli
ocamlc -c support.ml
ocamlc -c syntax.mli
ocamlc -c syntax.ml
ocamlc -c core.mli
ocamlc -c core.ml
ocamlc -c homework.mli
ocamlc -c homework.ml
ocamlc -c parser.mli
ocamlc -c parser.ml
ocamlc -c lexer.ml
ocamlc -c main.ml
ocamlc -o f support.cmo syntax.cmo core.cmo homework.cmo parser.cmo lexer.cmo main.cmo