#! /bin/bash -v

# dependencies
sudo zypper install -y gcc-fortran # R packages need this
sudo zypper install -y gd-devel # R packages
sudo zypper install -y freeglut-devel # R packages
sudo zypper install -y libcurl-devel # R packages
sudo zypper install -y gsl gsl-devel
sudo zypper install -y swig swig-doc swig-examples
sudo zypper install -y graphviz graphviz-devel graphviz-doc
sudo zypper install -y unixODBC-devel
sudo zypper install -y -t pattern technical_writing
sudo zypper install -y portaudio portaudio-devel
#zypper install -y yacas yacas-devel yacas-doc

# install and remove from Education repo to get any dependencies we missed
sudo zypper install -y R-base R-base-devel
sudo zypper remove -y R-base R-base-devel

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
iostat -cmdtx 2 999999 > R-compile-iostat.log &
make -j3
kill %1
make pdf
sudo make install
cd ..
sudo /sbin/ldconfig
sudo R CMD javareconf
