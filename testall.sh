#!/bin/bash
echo "====================== Setup env ======================"
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

while getopts "vtc" opt; do
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
rebar3 get-deps
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
#    rebar3 compile
#    echo "Dialyzing dependencies"
#    dialyzer --add_to_plt --plt $OTPPLT --output_plt $DEPSPLT -r deps/*/ebin/
#fi

echo "======================== Clean ========================"
rebar3 clean
rm -rf priv/log
echo ""

if [[ "$SUITE" == "compile" ]];
then

  echo "======================= Compile ======================="
  rebar3 compile
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
  EUNIT_OUTPUT=$(rebar3 compile eunit)
  EUNIT_RESULT=$?
  COVOK=true

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

    if [[ ("$PERCENT" -lt "80") ]];
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
    else
      PADDING=" "
    fi
    echo -e "$MODULE:${COLOR}$PADDING$PERCENT\e[0m%"
  done < <(echo "$EUNIT_OUTPUT" | egrep "%|100$")
  echo ""
  if $COVOK;
  then
    echo "No modules have less than 80% coverage, ok"
  else
    echo "One or more modules have less than 80% coverage, not ok"
    RESULT=1
  fi

  echo "===================== Common test ====================="
  rebar3 compile -DTEST
  SUITES=$(git ls-files | grep SUITE | sed -e 's/, /,/g')
  CT_OUTPUT=$(rebar3 ct --suite=$SUITES)
  CT_RESULT=$?

  if [ "$CT_RESULT" -ne 0 ];
  then
    echo "$CT_OUTPUT"
    exit $CT_RESULT
  fi

  if [ "$VERBOSE" == "true" ];
  then
    echo "$CT_OUTPUT"
  fi
fi

echo -e "http://soamjaws.github.io/wErld/$TRAVIS_BRANCH/$TRAVIS_OTP_RELEASE\n"
exit $RESULT
