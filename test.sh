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
assert 0 '0;'
assert 42 '42;'
assert 21 '5+20-4;'
assert 29 '28-2+3;'
assert 29 ' 28 - 1 - 1 +  3 ;'
assert 47 '5+6*7;'
assert 15 '5*(9-6);'
assert 4 '(3+5)/2;'
assert 13 '-10+23 ;'
assert 15 '--+25-10;'
assert 16 '26---10;'

assert 0 '0==1;'
assert 1 '42==42;'
assert 1 '0!=1;'
assert 0 '42!=42;'

assert 1 '0<1;'
assert 0 '1<1;'
assert 0 '2<1;'
assert 1 '0<=1;'
assert 1 '1<=1;'
assert 0 '2<=1;'

assert 1 '1>0;'
assert 0 '1>1;'
assert 0 '1>2;'
assert 1 '1>=0;'
assert 1 '1>=1;'
assert 1 '1+3>4*-5;'
assert 0 '2<=1;'

assert 3 'a=3; a;'
assert 8 'a=3; z=5; a+z;'
assert 6 'a=b=3; a+b;'
assert 3 'foo=3; foo;'
assert 8 'foo123=3; bar=5; foo123+bar;'

echo "Tests passed: $test_passed/33"

echo OK
