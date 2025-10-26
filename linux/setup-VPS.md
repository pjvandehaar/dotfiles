# Notes on setting up a VPS:

# Add nonroot user
adduser --disabled-password kpa
visudo  # append 'kpa ALL = (ALL) NOPASSWD: ALL'
su -l kpa
ssh-keygen  # and add id_rsa.pub to github/gitlab
sudo cat /root/.ssh/authorized_keys > ~/.ssh/authorized_keys # digitalocean includes my pubkey

# Allocate swap (to help npm)
sudo fallocate -l 1G /swapfile
sudo chmod 600 /swapfile
sudo mkswap /swapfile && sudo swapon /swapfile
sudo cp /etc/fstab /etc/fstab.bak && echo '/swapfile none swap sw 0 0' | sudo tee -a /etc/fstab
cat /proc/sys/vm/swappiness && sudo sysctl vm.swappiness=10 # only swap when absolutely necessary

# Firewall
sudo ufw allow OpenSSH
sudo ufw allow mosh
sudo ufw allow 'Nginx Full'
sudo ufw enable

# Follow SETUP.md (dotfiles, uv, etc)

# Setup web service
sudo apt install -y nginx nodejs npm
npm i -g nodemon
