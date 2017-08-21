# tezos-js
Tezos Michaelson interpreter compiled to js.

Just removed the dependencies that were problematic, so many things will not work properly yet,
for example cannot have multiple contracts, and at least none of the cryptography related things will work.

TODO:
 * Better command-line tool for node.js
 * Test in browser
 * Check that zarith wrapper works
 * Make more stubs so the original files can be used directly
 * Cryptographic primitives

Building:
```
jbuilder build run.bc.js
```

Test:
```
node _build/default/run.bc.js
```
