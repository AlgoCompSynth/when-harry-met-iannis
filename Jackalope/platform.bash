#! /bin/bash -v
cp bashrc ~/.bashrc
cp vimrc ~/.vimrc
cp gvimrc ~/.gvimrc
sudo apt-get update
sudo apt-get upgrade

# Pd Extended
rm Pd*deb
wget http://autobuild.puredata.info/auto-build/latest/Pd-0.41.4-extended-rc1-ubuntu-jaunty-i386.deb
sudo dpkg --install Pd*deb
sudo apt-get install --fix-broken

# Non-free
sudo apt-get install acroread
sudo apt-get install sun-java6-jdk
sudo apt-get install flashplugin-nonfree-extrasound
