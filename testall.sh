#!/bin/bash
echo "====================== Setup env ======================"
mkdir -p ebin
mkdir -p etestbin
OTPPLT=$HOME/.dialyzer_otp.plt
DEPSPLT=deps.plt
PLT=wErld.plt
echo ""

echo "==================== Setup dialyzer ==================="
if [ ! -f $OTPPLT ];
then
  dialyzer --build_plt --apps kernel stdlib erts mnesia --output_plt $OTPPLT
fi

if [[ ! -f $DEPSPLT ]];
then
    rebar compile
    echo "Dialyzing dependencies"
    dialyzer --add_to_plt --plt $OTPPLT --output_plt $DEPSPLT -r deps/*/ebin/
fi

dialyzer --check_plt --plt $PLT -r ebin/
if [[ $? -ne 0 ]];
then
  echo "Not up to date, dialyzing"
  dialyzer --add_to_plt --plt $DEPSPLT --output_plt $PLT -r ebin/
fi
echo ""

echo "======================== Clean ========================"
rebar clean
echo ""

echo "======================= Compile ======================="
rebar compile
echo ""

echo "================ Add wErld to dialyzer ================"
if [ ! -f $PLT ];
then
  dialyzer --plt $PLT --add_to_plt ebin
fi
echo ""

echo "====================== Dialyzing ======================"
dialyzer --plt $PLT -I include/ src --src
echo ""

echo "======================== EUnit ========================"
rebar eunit
echo ""
