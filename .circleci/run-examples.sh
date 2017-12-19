#!/bin/bash
cd ../examples
for d in */ ; do
    testname="example-$d"
    testname="${testname%?}"  # truncate last character
    echo "Running test $testname..."
    npm run -s "$testname"
done
