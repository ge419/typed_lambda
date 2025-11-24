#!/usr/bin/env bash
set -e
examples=(ex1 ex2 ex3 ex4 ex5 ex6 tyerr1 tyerr2 runerr1 runerr2 bool1 bool2 if_type_err)
for e in "${examples[@]}"; do
  f="examples/${e}.lambda"
  echo "---- ${f} ----"
  if ! dune exec typed_lambda -- "$f"; then
    echo "(non-zero exit)"
  fi
  echo
done
