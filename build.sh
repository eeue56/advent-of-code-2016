#!/bin/bash -ex

idris Main.idr -o ids.js --codegen javascript
elm-make --yes elm/Main.elm --output main.js

cat main.js ids.js > actual.js
