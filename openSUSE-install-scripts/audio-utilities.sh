#! /bin/bash -v

# now update to latest software
zypper repos -d
sudo zypper refresh
sudo zypper update

# utilities
sudo zypper install -yl portaudio portaudio-devel
sudo zypper install -yl flac flac-devel
sudo zypper install -yl timidity
sudo zypper install -yl audacity
sudo zypper install -yl rosegarden4
sudo zypper install -yl lilypond
