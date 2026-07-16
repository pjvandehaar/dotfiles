## Setting up a VPS:

ssh root@<ip>
adduser --disabled-password kpa
visudo  # append 'kpa ALL = (ALL) NOPASSWD: ALL'
su -l kpa
ssh-keygen  # and add id_rsa.pub to github/gitlab
sudo cat /root/.ssh/authorized_keys > ~/.ssh/authorized_keys  # digitalocean includes my pubkey

sudo fallocate -l 1G /swapfile
sudo chmod 600 /swapfile
sudo mkswap /swapfile && sudo swapon /swapfile
sudo cp /etc/fstab /etc/fstab.bak && echo '/swapfile none swap sw 0 0' | sudo tee -a /etc/fstab
cat /proc/sys/vm/swappiness && sudo sysctl vm.swappiness=10 # only swap when absolutely necessary

sudo apt install -y mosh nginx  # get ufw profiles
sudo ufw allow OpenSSH
sudo ufw allow mosh
sudo ufw allow 'Nginx Full'
sudo ufw enable

sudo apt install -y nodejs npm
npm i -g nodemon


## Setting up my user:

git clone https://github.com/pjvandehaar/dotfiles.git && ~/dotfiles/install.py

sudo apt update && sudo apt upgrade
sudo apt install htop tmux curl mosh tig jq
sudo apt install --no-install-recommends emacs-nox
  - If emacs is slow: start emacs service/server and make sure `e` uses it.  Try `emacs -Q` (no conf).

curl -LsSf https://astral.sh/uv/install.sh | UV_NO_MODIFY_PATH=1 sh
uv venv ~/venv
uv pip install kpa requests psutil unidecode boltons ipython jupyter numpy pandas tqdm twine visidata

mkdir ~/PROJECTS/


## Setting up my webserver:

cd /srv && mkdir kpa-server-manna && git clone https://gitlab.com/pjvandehaar/kpa-server-manna.git
