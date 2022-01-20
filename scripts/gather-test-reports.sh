#!/usr/bin/env bash

rm -rf test-reports
mkdir -p test-reports

for dir in */target/jvm-* */target/js-*; do
    platform=$(basename $dir)
    mkdir -p test-reports/$platform
    [ -d $dir/test-reports ] && cp $dir/test-reports/TEST-*.xml test-reports/$platform
done