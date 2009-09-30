#! /bin/bash -v
sudo zypper install -t pattern devel_basis
rm -fr sfront*
wget http://www.cs.berkeley.edu/~lazzaro/sa/sfront.tar.gz
tar xf sfront.tar.gz
cd sfront/src
make
sudo ln -sf ../bin/sfront /usr/local/bin/
which sfront
