#!/bin/sh

########### FOR TESTING COMPILER FLAGS ##############

# create tmp dir
tmp=`mktemp -d /tmp/chinnacc-test-XXXXXX`

# remove dir upon receiving any of the following signals
# int is when user types interrupt signal (<C-c>), term is a generic exit signal,
# hup is when term is disconnected or if controlling process is terminated (hangup),
# exit is like a normal exit from within the prog
# when any of this signals are encountered while running this script, exec the command
trap 'rm -rf $tmp' INT TERM HUP EXIT 

echo > $tmp/empty.c # create empty input file / reset it to a blank state

echo 'Testing compiler flags...'

# reports success failure based on exit code when testing compiler flags
check() {
    if [ $? -eq 0 ]; then
        echo "testing $1 ... passed"
    else
        echo "testing $1 ... failed"
        exit 1
    fi
}

# -o
rm -f $tmp/out
# compile empty file into specified output filed
./chinnacc -o $tmp/out $tmp/empty.c
[ -f $tmp/out ] # check if file exists and is a regular file
check -o

# --help
# redirect stderr to stdout then pipe the stdout to grep
# not a comprehensive test but better than nothing as we are just searching that the displayed message
# has the word chinnacc inside
./chinnacc --help 2>&1 | grep -q chinnacc
check --help

echo OK