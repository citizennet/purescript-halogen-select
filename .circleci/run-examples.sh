#!/bin/bash

# In Circle CI this runs from the project root, so there is no need
# to navigate up a directory before moving into the /examples folder.
cd examples

# Collect errors; I'd like to try and build all projects but register
# which ones fail.
declare -a Errors

for d in */ ; do
	testname="example-$d"
	testname="${testname%?}"  # truncate last character
	printf "\nRunning test $testname...\n"
	npm run -s "$testname"

	if [ $? -eq 0 ]
	then
		printf "\nSuccessfully built $testname\n"
	else
		# append the failed test to Errors
		Errors=("${Errors[@]}" "$testname")
		printf "\nFailed to build $testname\n"
	fi
done

if [ ${#Errors[*]} -gt 0 ] ; then
	printf "\n\n\n##########\nSome examples failed:\n##########\n"

	for item in ${Errors[*]}
	do
		printf "\n*** $item"
	done

	printf "\n\n\n"
	exit 707
fi
