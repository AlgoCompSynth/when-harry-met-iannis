#! /bin/bash -v

# updates
zypper repos -d
sudo zypper refresh
sudo zypper update

# dependencies
sudo zypper install -yl acroread
sudo zypper install -y -t pattern devel_basis
sudo zypper install -y -t pattern devel_tcl
sudo zypper install -y -t pattern technical_writing
sudo zypper install -y lyx
sudo zypper install -y gcc-fortran
sudo zypper install -y readline-devel
sudo zypper install -y freeglut-devel
sudo zypper install -y portaudio portaudio-devel
sudo zypper install -y unixODBC unixODBC-devel

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

make
make pdf
sudo make install
cd ..
sudo /sbin/ldconfig
sudo R CMD javareconf

# update packages by hand
sudo R

# now make a local library ;-)
R
