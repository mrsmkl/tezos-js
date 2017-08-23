# tezos-js
Tezos Michaelson interpreter compiled to js.

Just removed the dependencies that were problematic, so many things will not work properly yet,
for example cannot have multiple contracts, and at least none of the cryptography related things will work.

TODO:
 * Better command-line tool for node.js
 * Check that zarith wrapper works
 * Make more stubs so the original files can be used directly
 * Implement a context to store contracts
 * Cryptographic primitives

Building: (OCaml 4.04.2 suggested.)
```
opam install ezjsonm ipaddr lwt ocplib-json-typed js_of_ocaml-lwt js_of_ocaml ocplib-resto calendar num cstruct js_of_ocaml jbuilder
sudo npm install -g browserify
npm install browserify-fs
./compile.sh
```
