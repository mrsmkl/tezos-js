#!/bin/sh

jbuilder build browser.bc.js && browserify -r browserify-fs:fs -o browser.js _build/default/browser.bc.js
