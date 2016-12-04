#!/bin/bash -ex
root_dir=$(pwd)
idris Main.idr -o ids.js --codegen javascript

pushd elm
elm-make --yes Main.elm --output $root_dir/main.js
popd

cat main.js ids.js > actual.js
