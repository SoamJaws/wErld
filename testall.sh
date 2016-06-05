#!/bin/bash -e
echo "======================= Compile ======================="
erl -make
echo ""
erl -pa etestbin/ -run werld_tests werld_test -run init stop -noshell
echo ""
echo "====================== Dialyzing ======================"
dialyzer -I include/ src --src
