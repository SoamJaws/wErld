#!/bin/bash -e
echo "======================= Compile ======================="
mkdir -p ebin
mkdir -p etestbin
if [ ! -f wErld.plt ];
then
  dialyzer --build_plt --apps kernel stdlib erts mnesia eunit --output_plt wErld.plt
  erl -make
  dialyzer --add_to_plt ebin
else
  erl -make
fi
echo ""
erl -pa etestbin/ -run werld_tests werld_test -run init stop -noshell
echo ""
echo "====================== Dialyzing ======================"
dialyzer -I include/ src --src
