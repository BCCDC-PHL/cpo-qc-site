#!/bin/bash

rm -r target/deploy/cpo-qc 2> /dev/null
mkdir -p target/deploy/cpo-qc
mkdir -p target/deploy/cpo-qc/js

cp -r resources/public/css target/deploy/cpo-qc
cp -r resources/public/images target/deploy/cpo-qc
cp resources/public/favicon.ico target/deploy/cpo-qc/favicon.ico
cp resources/public/js/main.js target/deploy/cpo-qc/js/main.js
cp resources/public/index.html target/deploy/cpo-qc/index.html
pushd target/deploy > /dev/null
tar -czf cpo-qc.tar.gz cpo-qc
# rm -r cpo-qc
popd > /dev/null
