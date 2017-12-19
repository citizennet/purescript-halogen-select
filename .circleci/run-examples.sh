#!/bin/bash

# In Circle CI this runs from the project root, so there is no need
# to navigate up a directory before moving into the /examples folder.
cd examples

for d in */ ; do
    testname="example-$d"
    testname="${testname%?}"  # truncate last character
    echo "Running test $testname..."
    npm run -s "$testname"
done
