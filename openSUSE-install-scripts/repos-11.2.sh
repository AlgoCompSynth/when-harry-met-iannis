#! /bin/bash -v

zypper repos -d # list repos before changes

sudo zypper addrepo \
  http://packman.unixheads.com/suse/11.2/ \
  'Packman 11.2'
sudo zypper addrepo \
  http://packman.unixheads.com/suse/factory/ \
  'Packman factory'
sudo zypper addrepo \
  http://packman.unixheads.com/suse/11.1/ \
  'Packman 11.1'

# now update to latest software
sudo zypper modifyrepo --refresh --all
zypper repos -d
sudo zypper refresh
sudo zypper update
