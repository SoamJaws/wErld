if [[ "$TRAVIS_PULL_REQUEST" == "false" && "$SUITE" == "test" ]]; then
  echo -e "Starting to update gh-pages\n"

  #copy data we're interested in to other place
  
  mkdir -p $HOME/ct/logs
  cp -R ./test/logs/* $HOME/ct/logs

  cd $HOME/ct/logs
  LOGS=$(find . -name "*_log.html")
  HEAD=$(echo "$LOGS" | head -1)
  CT_RUN_DIR=$(echo "$HEAD" | cut -d "/" -f2)

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

  echo "<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en"> \
<head> \
<title>Test line_SUITE:get_next_stop_case result</title> \
</head> \
<body> \
<ul>" > $TRAVIS_BRANCH/$TRAVIS_OTP_RELEASE/$CT_RUN_DIR/logs/index.html

  for file in $LOGS; do
    echo "<li><a href=\"$file\">$file</a></li>"
  done

  echo "</ul></body>" >> $TRAVIS_BRANCH/$TRAVIS_OTP_RELEASE/$CT_RUN_DIR/logs/index.html

  #add, commit and push files
  git add -f .
  git commit -m "Travis build $TRAVIS_BUILD_NUMBER pushed to gh-pages"
  git pull -r
  git push -fq origin gh-pages > /dev/null

fi
