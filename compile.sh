#!/bin/sh

ocamlbuild -package lwt -package ocplib-json-typed.bson -package ocplib-resto.directory main.native
