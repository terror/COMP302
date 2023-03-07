set dotenv-load

export EDITOR := 'nvim'

default:
  just --list

forbid:
  ./bin/forbid

fmt:
  ocamlformat -i **/*.ml

dev-deps:
  brew install opam
  opam install ocamlformat
