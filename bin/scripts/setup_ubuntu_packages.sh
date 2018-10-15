#!/bin/bash

# Setup packages for Ubuntu dev machine

sudo apt-get install software-properties-common aptitude synaptic compizconfig-settings-manager gnome-system-monitor \
	 `# Utils` \
	 lsof bsdtar htop xclip pv mbuffer lshw xxdiff traceroute hwinfo hardinfo socat \
	 ldnsutils `# drill` \
	 dnsutils `# dig` \
	 docker.io \
	 `# Dev` \
	 libtool pkg-config build-essential autoconf automake lshw-gtk emacs global valgrind kcachegrind manpages-dev \
	 `# Dev libraries` \
	 libncurses5-dev \
	 `# Dev libraries ibsim` \
	 libibmad-dev \
	 `# Lang` \
	 nodejs
	 `# Lang util` \
	 jq


# Spotify
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 0DF731E45CE24F27EEEB1450EFDC8610341D9410
echo deb http://repository.spotify.com stable non-free | sudo tee /etc/apt/sources.list.d/spotify.list

# Chome
wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | sudo apt-key add -
sudo sh -c 'echo "deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google.list'

# gcloud
export CLOUD_SDK_REPO="cloud-sdk-$(lsb_release -c -s)"
echo "deb http://packages.cloud.google.com/apt $CLOUD_SDK_REPO main" | sudo tee -a /etc/apt/sources.list.d/google-cloud-sdk.list
curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | sudo apt-key add -

# Skype
curl -s https://repo.skype.com/data/SKYPE-GPG-KEY | sudo apt-key add -
sudo sh -c 'echo "deb [arch=amd64] https://repo.skype.com/deb stable main" >> /etc/apt/sources.list.d/skype-stable.list'

# Virtualbox
sudo apt-get install virtualbox virtualbox-dkms virtualbox-ext-pack

sudo apt-get update
sudo apt-get install google-chrome-stable  spotify-client google-cloud-sdk

# Adding multiverse repo
sudo apt-add-repository multiverse

# Vagrant:
#
# https://www.vagrantup.com/downloads.html
#
# Install from source: https://www.vagrantup.com/docs/installation/source.html
