#! /bin/bash -v

# dependencies
sudo zypper install -y libxml2-devel # ggobi needs this

# install RPM and remove it to make sure we have all dependencies
sudo zypper install -y ggobi ggobi-devel
sudo zypper remove -y ggobi ggobi-devel

export CFLAGS="-O3 -march=native -pipe -g"
export FFLAGS="-O3 -march=native -pipe -g"
export FCFLAGS="-O3 -march=native -pipe -g"
export CXXFLAGS="-O3 -march=native -pipe -g"
export WHERE="http://www.ggobi.org/downloads"
export DIR="ggobi-2.1.8"
export WHAT="${DIR}.tar.bz2"
rm -fr ${WHAT} ${DIR}
wget ${WHERE}/${WHAT}
tar xf ${WHAT}

cd ${DIR}
./configure --with-all-plugins
make -j3
sudo make install
sudo /sbin/ldconfig
make ggobirc
sudo mkdir -p /etc/xdg/ggobi
sudo cp ggobirc /etc/xdg/ggobi/ggobirc
cd ..
sudo /sbin/ldconfig
