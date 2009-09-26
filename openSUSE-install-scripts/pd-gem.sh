#! /bin/bash -v

# pd
wget http://crca.ucsd.edu/~msp/Software/pd-0.42-5.src.tar.gz
rm -fr pd-0.42-5
tar xf pd*gz
cd pd-0.42-5/src
./configure
make
sudo make install
sudo /sbin/ldconfig
cd ../..

# gem
wget http://gem.iem.at/releases/0.92.1/gem-0.92-1.tar.gz
rm -fr gem-0.92-1
tar xf gem*gz
cd gem-0.92-1/src
./autogen.sh
./configure
make
sudo make install
sudo /sbin/ldconfig
cd ../..
