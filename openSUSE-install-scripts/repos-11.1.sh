#! /bin/bash -v

zypper repos -d # list repos before changes

# add OSUOSL repos ... much faster
sudo zypper addrepo \
  http://ftp.osuosl.org/pub/opensuse/distribution/11.1/repo/oss/ \
  repo-oss-osuosl
sudo zypper addrepo \
  http://ftp.osuosl.org/pub/opensuse/distribution/11.1/repo/non-oss/ \
  repo-non-oss-osuosl
sudo zypper addrepo \
  http://ftp.osuosl.org/pub/opensuse/update/11.1/ \
  repo-update-osuosl

# remove the default repos from openSUSE.org
sudo zypper removerepo repo-update
sudo zypper removerepo repo-oss
sudo zypper removerepo repo-non-oss

# Community repos
sudo zypper addrepo \
  http://download.opensuse.org/repositories/home:/danielinteractive/openSUSE_11.1/ \
  'GGobi'
sudo zypper addrepo \
  http://download.opensuse.org/repositories/science/openSUSE_11.1/ \
  'Science'
sudo zypper addrepo \
  http://download.opensuse.org/repositories/GNOME:/Community/openSUSE_11.1/ \
  'openSUSE BuildService - GNOME:Community'
sudo zypper addrepo \
  http://download.opensuse.org/repositories/GNOME:/STABLE/openSUSE_11.1/ \
  'openSUSE BuildService - GNOME:STABLE'
sudo zypper addrepo \
  http://download.opensuse.org/repositories/OpenOffice.org:/STABLE/openSUSE_11.1/ \
  'openSUSE BuildService - OpenOffice.org'
sudo zypper addrepo \
  http://download.opensuse.org/repositories/X11:/Compiz/openSUSE_11.1/ \
  'openSUSE BuildService - X11:Compiz'
sudo zypper addrepo \
  http://download.opensuse.org/repositories/mozilla/openSUSE_11.1/ \
  'openSUSE BuildService - Mozilla'
sudo zypper addrepo \
  http://packman.unixheads.com/suse/11.1/ \
  'Packman Repository'
sudo zypper addrepo \
  http://download.opensuse.org/repositories/home:/anubisg1:/LXDE/openSUSE_11.1/ \
  'LXDE Repository'
sudo zypper addrepo \
  http://download.opensuse.org/repositories/server:/database:/postgresql/openSUSE_11.1/ \
  'openSUSE Build Service - Database:PostgreSQL'
sudo zypper addrepo \
  http://download.opensuse.org/repositories/Virtualization:/KVM/openSUSE_11.1/ \
  'openSUSE BuildService - Virtualization (KVM)'
sudo zypper addrepo \
  http://download.opensuse.org/repositories/Virtualization:/Qemu/openSUSE_11.1/ \
  'openSUSE BuildService - Virtualization (QEMU)'
sudo zypper addrepo \
  http://download.opensuse.org/repositories/Virtualization:/VirtualBox/openSUSE_11.1/ \
  'openSUSE BuildService - Virtualization (VirtualBox)'

# now update to latest software
sudo zypper modifyrepo --refresh --all
zypper repos -d
sudo zypper refresh
sudo zypper update
