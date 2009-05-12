#! /bin/bash -v
rm -fr  pd-0.42-4*
wget http://superb-west.dl.sourceforge.net/sourceforge/pure-data/pd-0.42-4.src.tar.gz
tar xf pd-0.42-4.src.tar.gz
cd pd-0.42-4/src
  #aclocal
  #autoconf
  ./configure
  make
  sudo make install
cd ../..
rm -fr Pd-0.40.3
wget http://superb-west.dl.sourceforge.net/sourceforge/pure-data/Pd-0.40.3-extended.tar.bz2
tar xf Pd-0.40.3-extended.tar.bz2
