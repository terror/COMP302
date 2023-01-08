set dotenv-load

export EDITOR := 'nvim'

default:
  just --list

forbid:
  ./bin/forbid

fmt:
  ocamlformat -i **/*.ml
