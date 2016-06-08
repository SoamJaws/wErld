#!/bin/bash
echo "====================== Setup env ======================"
mkdir -p ebin
mkdir -p etestbin
PLT=wErld.plt
echo ""

echo "==================== Setup dialyzer ==================="
if [ ! -f $PLT ];
then
  dialyzer --build_plt --apps kernel stdlib erts mnesia compiler crypto --output_plt $PLT
fi
echo ""

echo "======================= Compile ======================="
erl -make
echo ""

echo "================ Add wErld to dialyzer ================"
if [ ! -f $PLT ];
then
  dialyzer --plt $PLT --add_to_plt ebin
fi
echo ""

erl -pa etestbin/ -run werld_tests werld_test -run init stop -noshell
echo ""

echo "====================== Dialyzing ======================"
dialyzer --plt $PLT -I include/ src --src
echo ""
