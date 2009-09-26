#! /bin/bash -v

# now update to latest software
sudo zypper modifyrepo --no-refresh --all
sudo zypper repos -d
sudo zypper refresh
sudo zypper update

# utilities
sudo zypper install -yl command-not-found findutils-locate sudo 
sudo zypper install -yl bash-completion
sudo zypper install -yl zip unzip dos2unix unix2dos
sudo zypper install -yl flash-player
sudo zypper install -yl nspluginwrapper
sudo zypper install -yl gvim vim-enhanced vim-data
sudo zypper install -yl lynx
sudo zypper install -yl acroread
sudo zypper install -yl oprofile systemtap blktrace
sudo zypper install -yl sysstat
sudo zypper install -yl cpufrequtils
sudo zypper install -yl zenmap
sudo zypper install -yl wireshark
sudo zypper install -yl vsftpd lftp ncftp yast2-ftp-server
sudo zypper install -yl git git-gui gitk
sudo zypper install -yl R-base R-base-devel
sudo zypper install -yl portaudio portaudio-devel
sudo zypper install -yl flac flac-devel

echo 'Now set up NTP and bootloader, shut down and snapshot.'
