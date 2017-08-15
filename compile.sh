#!/bin/sh

ocamlbuild -package irmin -package irmin.mem -package irmin.unix -package calendar\
 -package ipaddr.unix -package lwt -package ocplib-json-typed.bson -package ocplib-resto.directory -package num main.byte
