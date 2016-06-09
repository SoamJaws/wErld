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
  echo "No OTP PLT available, building PLT"
  dialyzer --build_plt --apps kernel stdlib erts mnesia --output_plt $OTPPLT
fi

#if [[ ! -f $DEPSPLT ]];
#then
#    rebar compile
#    echo "Dialyzing dependencies"
#    dialyzer --add_to_plt --plt $OTPPLT --output_plt $DEPSPLT -r deps/*/ebin/
#fi

if [[ ! -f $PLT ]];
then
  echo "Checking local PLT"
  dialyzer --check_plt --plt $PLT -r ebin/
  if [[ $? -ne 0 ]];
  then
    echo "Local PLT not up to date, dialyzing"
#    dialyzer --add_to_plt --plt $DEPSPLT --output_plt $PLT -r ebin/
    dialyzer --add_to_plt --plt $OTPPLT --output_plt $PLT -r ebin/
  fi
  echo ""
else
  echo "Local PLT up to date"
fi

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
EUNIT_OUTPUT=$(rebar eunit)
echo "$EUNIT_OUTPUT"
echo ""

while read -r line ; do
  IFS=':' read -a keyval <<< "$line"
  MODULE=$(echo ${keyval[0]} | tr -d ' ')
  PERCENT=$(echo ${keyval[1]} | tr -d ' ' | tr -d %)
  echo "Module $MODULE percentage $PERCENT"
done < <(echo "$EUNIT_OUTPUT" | grep "%")

echo ""
