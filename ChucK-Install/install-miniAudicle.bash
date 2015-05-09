#! /bin/bash -v
#
# Copyright (C) 2012 by M. Edward (Ed) Borasky
#
# This program is licensed to you under the terms of version 3 of the
# GNU Affero General Public License. This program is distributed WITHOUT
# ANY EXPRESS OR IMPLIED WARRANTY, INCLUDING THOSE OF NON-INFRINGEMENT,
# MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. Please refer to the
# AGPL (http://www.gnu.org/licenses/agpl-3.0.txt) for more details.
#

mkdir -p /usr/local/src/
pushd /usr/local/src
rm -fr miniAudicle* chuck*
export CHUCK=chuck-1.3.5.1
export MINI=miniAudicle-1.3.3
wget -q http://audicle.cs.princeton.edu/mini/release/files/${MINI}.tgz
wget -q http://chuck.cs.princeton.edu/release/files/${CHUCK}.tgz
tar xf ${MINI}.tgz
tar xf ${CHUCK}.tgz
chown -R root:root miniAudicle* chuck*
cd ${CHUCK}/src
make linux-pulse
make install
cd ../..
cd ${MINI}/src
mv chuck chuck.bak
ln -sf /usr/local/src/${CHUCK} chuck
make linux-pulse
make install
popd
