#!/bin/bash
echo "======================= Compile ======================="
erl -make
echo ""
erl -pa etestbin/ -run werld_tests werld_test -run init stop -noshell
