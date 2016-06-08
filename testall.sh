#!/bin/bash
echo "====================== Setup env ======================"
mkdir -p ebin
mkdir -p etestbin
echo ""

echo "==================== Setup dialyzer ==================="
if [ ! -f wErld.plt ];
then
  dialyzer --build_plt --apps kernel stdlib erts mnesia --output_plt wErld.plt
fi
echo ""

echo "======================= Compile ======================="
erl -make
echo ""

echo "================ Add wErld to dialyzer ================"
if [ ! -f wErld.plt ];
then
  dialyzer --add_to_plt ebin
fi
echo ""

erl -pa etestbin/ -run werld_tests werld_test -run init stop -noshell
echo ""

echo "====================== Dialyzing ======================"
dialyzer -I include/ src --src
echo ""
