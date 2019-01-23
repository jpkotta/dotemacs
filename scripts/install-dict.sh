#!/usr/bin/env bash

#set -o xtrace
set -o errexit -o nounset -o pipefail -o errtrace
IFS=$'\n\t'

if [ -e ~/.stardict/dic/dictd_www.dict.org_web1913.dict.dz ] ; then
    echo "Already installed."
    exit
fi

tmpdir=$(mktemp -d)
cd $tmpdir

wget -O dictionary.zip "https://s3.amazonaws.com/jsomers/dictionary.zip"
unzip -o dictionary.zip dictionary/stardict-dictd-web1913-2.4.2.tar.bz2
cd dictionary
tar xf stardict-dictd-web1913-2.4.2.tar.bz2
cd stardict-dictd-web1913-2.4.2
mkdir -p ~/.stardict/dic/
mv * ~/.stardict/dic/

rm -rf $tmpdir
