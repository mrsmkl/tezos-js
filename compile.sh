#!/bin/sh

ocamlbuild -use-menhir -package ezjsonm -package ipaddr.unix -package lwt -package ocplib-json-typed.bson -package js_of_ocaml-lwt.logger \
    -package js_of_ocaml -package js_of_ocaml.ppx \
    -package ocplib-resto.directory -package calendar -package num -package cstruct run.byte

js_of_ocaml +nat.js +weak.js run.byte

#ocamlbuild \
# -package irmin.mem \
# -package lwt.unix \
# -package js_of_ocaml \
# -package js_of_ocaml-lwt \
# -package js_of_ocaml-lwt.logger \
# -package calendar \
# -package ipaddr.unix -package lwt \
# -package ocplib-json-typed.bson -package ocplib-resto.directory -package num main.byte

# -lflags '-I /home/sami/.opam/tezos/lib/js_of_ocaml js_of_ocaml.cma logger.cma ' \
# -cflags '-I /home/sami/.opam/tezos/lib/js_of_ocaml' \


# js_of_ocaml +bin_prot.js +nat.js +weak.js +lwt_log_js.js main.byte
