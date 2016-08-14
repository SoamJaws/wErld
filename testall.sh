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
rebar get-deps
echo ""

echo "==================== Setup dialyzer ==================="
mkdir -p $OTPPLTDIR
if [ ! -f $OTPPLT ];
then
  echo "No OTP PLT available, building PLT"
  dialyzer --build_plt --apps kernel stdlib erts mnesia eunit common_test --output_plt $OTPPLT
fi

#if [[ ! -f $DEPSPLT ]];
#then
#    rebar compile
#    echo "Dialyzing dependencies"
#    dialyzer --add_to_plt --plt $OTPPLT --output_plt $DEPSPLT -r deps/*/ebin/
#fi

echo "======================== Clean ========================"
rebar clean
rm -rf priv/log
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

  echo "===================== Common test ====================="
  rebar compile -DTEST
  CT_OUTPUT=$(rebar ct)
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
