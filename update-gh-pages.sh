if [[ "$TRAVIS_PULL_REQUEST" == "false" && "$SUITE" == "test" ]]; then
  echo -e "Starting to update gh-pages\n"

  #copy data we're interested in to other place
  
  mkdir -p $HOME/ct/
  ls -l ./test/logs
  cp -R ./test/logs/* $HOME/ct/

  cd $HOME/ct/
  LOGS=$(find . -name "*_log.html")
  for FILE in $LOGS; do
    sed -i '1i<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\"><head><title>$FILE</title></head><body><pre>' $FILE
    echo "</pre>" >> $FILE
  done


  HEAD=$(echo "$LOGS" | head -1)
  CT_RUN_DIR=$(echo "$HEAD" | cut -d "/" -f2)
  cd $CT_RUN_DIR/logs
  RELATIVE_LOGS=$(find . -name "*_log.html")

  #go to home and setup git
  cd $HOME
  git config --global user.email "travis@travis-ci.org"
  git config --global user.name "Travis"

  #using token clone gh-pages branch
  git clone --quiet --branch=gh-pages https://${GH_TOKEN}@github.com/SoamJaws/wErld.git  gh-pages > /dev/null

  #go into directory and copy data we're interested in to that directory
  cd gh-pages
  git rm -rf $TRAVIS_BRANCH/$TRAVIS_OTP_RELEASE/*
  mkdir -p $TRAVIS_BRANCH/$TRAVIS_OTP_RELEASE/
  cp -Rf $HOME/ct/* $TRAVIS_BRANCH/$TRAVIS_OTP_RELEASE/

  cd $TRAVIS_BRANCH/$TRAVIS_OTP_RELEASE/$CT_RUN_DIR/logs/

  echo "Generating index.html for each testcase log"
  for CASEDIR in $(ls); do
    if [ -d "$CASEDIR" ]; then
      cd $CASEDIR
      echo "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\"> \
<head> \
<title>$CASEDIR</title> \
<link rel="stylesheet" href="../../ct_default.css" type="text/css"></link> \
</head> \
<body> \
<ul>" > index.html
      for CASELOG in $(ls); do
        echo "<li><a href=\"$CASELOG\">$CASELOG</a></li>" >> index.html
      done
      echo "</ul></body>" >> index.html
      cd ..
    fi
  done

  echo "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\"> \
<head> \
<title>App generated logs</title> \
<link rel="stylesheet" href="../../ct_default.css" type="text/css"></link> \
</head> \
<body> \
<ul>" > index.html

  for FILE in $RELATIVE_LOGS; do
    UPDATEDFILE=$(echo $FILE | sed -e 's/^.\///')
    echo "<li><a href=\"$UPDATEDFILE\">$UPDATEDFILE</a></li>" >> index.html
  done

  echo "</ul></body>" >> index.html

  cd ..
  SUITELOG=$(find . -iname suite.log.html)
  echo "$(awk '/unexpected_io.log.html/ { print; print "<li><a href=\"../../logs/index.html\">App generated logs</a></li>"; next }1' $SUITELOG)" > $SUITELOG

  #add, commit and push files
  git add -f .
  git commit -m "Travis build $TRAVIS_BUILD_NUMBER pushed to gh-pages"
  git pull -r
  git push -fq origin gh-pages > /dev/null

fi
