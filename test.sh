#!/bin/bash

test_passed=0

assert() {
    expected="$1"
    input="$2"

    ./chinnacc "$input" > tmp.s || exit
    gcc -static -o tmp tmp.s
    ./tmp
    actual="$?"

    if [ "$actual" = "$expected" ]; then
        echo "$input => $actual"
    else
        echo "$input => $expected expected, but got $actual"
        exit 1
    fi
    test_passed=$(( $test_passed + 1 ))
}

# Tests
assert 0 0
assert 42 42
assert 21 '5+20-4'
assert 29 '28-2+3'

echo "Tests passed: $test_passed / 4" '\n'

echo OK
