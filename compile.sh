#!/bin/sh

ocamlbuild -package irmin.mem \
 -package calendar \
 -package ipaddr.unix -package lwt \
 -package ocplib-json-typed.bson -package ocplib-resto.directory -package num main.byte

js_of_ocaml +bin_prot.js +nat.js +weak.js +lwt.js main.byte
