if [[ "$TRAVIS_PULL_REQUEST" == "false" && "$SUITE" == "test" ]]; then
  echo -e "Starting to update gh-pages\n"

  #copy data we're interested in to other place
  
  mkdir -p $HOME/ct
  ls ./test/logs
  cp -R ./test/logs/* $HOME/ct

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

  #add, commit and push files
  git add -f .
  git commit -m "Travis build $TRAVIS_BUILD_NUMBER pushed to gh-pages"
  git pull -r
  git push -fq origin gh-pages > /dev/null

fi
