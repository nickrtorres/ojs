#!/bin/sh -e

WD="$PWD"

run() {
  cd ../ && dune exec bin/ojs.exe -- "${WD}/$1"
}

tfail() {
  tcase="$1"
  actual="$2"
  expected="$3"

  printf "Test '%s' failed.\n" "$tcase"
  printf "\texpected: %s\n" "$(cat "$expected")"
  printf "\tgot: %s\n" "$actual"
}

for f in ./js/*.js
do
  expected=$(echo "${f%.js}.out" | sed "s,./js/,./out/,g")
  actual=$(run "$f")

  if ! echo "$actual" | cmp -s "$expected"
  then
    tfail "$f" "$actual" "$expected"
  fi

done
