#! /bin/bash -v

# now update to latest software
zypper repos -d
sudo zypper refresh
sudo zypper update

# utilities
sudo zypper install portaudio portaudio-devel
sudo zypper install flac flac-devel
sudo zypper install timidity
sudo zypper install audacity
sudo zypper install rosegarden4
sudo zypper install lilypond
sudo zypper install alsa-tools-gui
