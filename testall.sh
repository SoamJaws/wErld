#!/bin/bash
echo "====================== Setup env ======================"
mkdir -p ebin
mkdir -p etestbin
OTPPLTDIR=$HOME/.otp_plt
OTPPLT=$OTPPLTDIR/.dialyzer_otp.plt
DEPSPLT=deps.plt
PLT=wErld.plt
RESULT=0
echo ""

echo "================= Resolve dependencies ================"
rebar get-deps
echo ""

echo "==================== Setup dialyzer ==================="
mkdir -p $OTPPLTDIR
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
dialyzer --plt $PLT -Wunknown -I include/ src --src
DIALYZER_RESULT=$?

if [ "$DIALYZER_RESULT" -ne 0 ];
then
  exit $DIALYZER_RESULT
fi
echo ""

echo "======================== EUnit ========================"
EUNIT_OUTPUT=$(rebar eunit)
EUNIT_RESULT=$?
COVOK=true
PADDING=" "

if [ "$EUNIT_RESULT" -ne 0 ];
then
  echo "$EUNIT_OUTPUT"
  exit $EUNIT_RESULT
fi

while read -r line ; do
  IFS=':' read -a keyval <<< "$line"
  MODULE=${keyval[0]}
  PERCENT=$(echo ${keyval[1]} | tr -d ' ' | tr -d %)
  COLOR='\e[0;32m'

  if [[ ("$PERCENT" < 80) ]];
  then
    COVOK=false
    COLOR='\e[0;31m'
  fi

  if [[ ("$PERCENT" -lt "10") ]];
  then
    PADDING="   "
  elif [[ ("$PERCENT" -lt "100") ]];
  then
    PADDING="  "
  fi
  echo -e "$MODULE:${COLOR}$PADDING$PERCENT\e[0m%"
done < <(echo "$EUNIT_OUTPUT" | grep "%")
echo ""
if $COVOK;
then
  echo "No modules have less than 80% coverage, ok"
else
  echo "One or more modules have less than 80% coverage, not ok"
  RESULT=1
fi

exit $RESULT
