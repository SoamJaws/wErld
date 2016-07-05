#!/bin/bash
echo "====================== Setup env ======================"
echo $(which rebar)
mkdir -p ebin
mkdir -p etestbin
OTPPLTDIR=$HOME/.otp_plt
OTPPLT=$OTPPLTDIR/.dialyzer_otp.plt
DEPSPLT=deps.plt
PLT=wErld.plt
RESULT=0
VERBOSE=false
export WERLDROOT=$(git rev-parse --show-toplevel)

OPTIND=1

while getopts "vtc:" opt; do
    case "$opt" in
    v)  VERBOSE=true
        ;;
    t)  SUITE=test
        ;;
    c)  SUITE=compile
        ;;
    esac
done

shift $((OPTIND-1))
[ "$1" = "--" ] && shift

echo ""

echo "================= Resolve dependencies ================"
rebar get-deps
echo ""

echo "==================== Setup dialyzer ==================="
mkdir -p $OTPPLTDIR
if [ ! -f $OTPPLT ];
then
  echo "No OTP PLT available, building PLT"
  dialyzer --build_plt --apps kernel stdlib erts mnesia eunit --output_plt $OTPPLT
fi

#if [[ ! -f $DEPSPLT ]];
#then
#    rebar compile
#    echo "Dialyzing dependencies"
#    dialyzer --add_to_plt --plt $OTPPLT --output_plt $DEPSPLT -r deps/*/ebin/
#fi

echo "======================== Clean ========================"
rebar clean
echo ""

if [[ "$SUITE" == "compile" ]];
then

  echo "======================= Compile ======================="
  rebar compile
  echo ""

  echo "====================== Dialyzing ======================"
  dialyzer --plt $OTPPLT -Wunknown -r ebin/
  DIALYZER_RESULT=$?

  if [ "$DIALYZER_RESULT" -ne 0 ];
  then
    exit $DIALYZER_RESULT
  fi
  echo ""
fi

if [[ "$SUITE" == "test" ]];
then

  echo "======================== EUnit ========================"
  EUNIT_OUTPUT=$(rebar compile eunit)
  EUNIT_RESULT=$?
  COVOK=true
  PADDING=" "

  if [ "$EUNIT_RESULT" -ne 0 ];
  then
    echo "$EUNIT_OUTPUT"
    exit $EUNIT_RESULT
  fi

  if [ "$VERBOSE" == "true" ];
  then
    echo "$EUNIT_OUTPUT"
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

fi

echo -e "http://soamjaws.github.io/wErld/$TRAVIS_BRANCH/$TRAVIS_OTP_RELEASE\n"
exit $RESULT
