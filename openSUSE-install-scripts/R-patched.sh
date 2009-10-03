#! /bin/bash -v

# dependencies
sudo zypper install -y -t pattern devel_basis
sudo zypper install -y -t pattern devel_tcl
sudo zypper install -y -t pattern technical_writing
sudo zypper install -y gcc-fortran # R packages need this
sudo zypper install -y readline-devel
#sudo zypper install -y portaudio portaudio-devel

export CFLAGS='-O3 -march=native -g -pipe'
export FFLAGS='-O3 -march=native -g -pipe'
export FCFLAGS='-O3 -march=native -g -pipe'
export CXXFLAGS='-O3 -march=native -g -pipe'
export WHERE=ftp://ftp.stat.math.ethz.ch/Software/R/
export DIR=R-patched
export WHAT=R-patched.tar.bz2
rm -fr ${WHAT} ${DIR}
wget ${WHERE}/${WHAT}
tar xf ${WHAT}

cd ${DIR}
export R_PAPERSIZE='letter'
./configure --enable-threads --enable-R-profiling \
  --enable-BLAS-shlib --enable-R-shlib --enable-R-static-lib \
  --with-tcltk --with-cairo --with-libpng --with-jpeglib --with-x

# gather stats
make
make pdf
sudo make install
cd ..
sudo /sbin/ldconfig
sudo R CMD javareconf
