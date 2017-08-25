#!/bin/sh

jbuilder build binding_commonjs.bc.js && browserify -r browserify-fs:fs -r ./_build/default/binding_commonjs.bc.js:michelsonjs > binding.js
