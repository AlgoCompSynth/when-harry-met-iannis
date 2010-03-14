#! /bin/bash -v
#zypper repos -d
#sudo zypper refresh
#sudo zypper update
#sudo zypper install -t pattern devel_basis
#sudo zypper install -yl sox
rm -fr sfront sfront.tar.gz
wget http://www.cs.berkeley.edu/~lazzaro/sa/sfront.tar.gz
tar xf sfront.tar.gz
cd sfront/src
make
cd ../..
cp sfront/bin/sfront /usr/local/bin/
which sfront

# make examples
cd sfront/examples
make
cd ../..
