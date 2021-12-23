#!/usr/bin/env bash

rm -rf test-reports
mkdir -p test-reports/jvm test-reports/js

cp */.jvm/target/test-reports/TEST-*.xml test-reports/jvm
cp */.js/target/test-reports/TEST-*.xml test-reports/js
