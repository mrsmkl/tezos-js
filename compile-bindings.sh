#!/bin/sh

jbuilder build binding-commonjs.bc.js && browserify -r browserify-fs:fs -r ./_build/default/binding-commonjs.bc.js > binding.js
