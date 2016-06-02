#!/bin/bash
erl -make
erl -pa etestbin/ -run werld_tests werld_test -run init stop -noshell
