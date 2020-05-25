# Notes on setting up a VPS:

# Add nonroot user
adduser --disabled-password kpa
visudo  # append 'kpa ALL = (ALL) NOPASSWD: ALL'
su -l kpa
ssh-keygen  # and add id_rsa.pub to github/gitlab
sudo cat /root/.ssh/authorized_keys > ~/.ssh/authorized_keys # digitalocean includes my pubkey

# Make comfy
git clone https://github.com/pjvandehaar/dotfiles
mv .bashrc .orig-system-bashrc && ./dotfiles/install.sh
sudo apt update && sudo apt install -y emacs tig mosh python3-pip python3-venv # & more from packages.txt
pip3 install ipython kpa # & more from packages.txt
sudo apt install -y cargo && cargo install exa

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

# Setup web service
sudo apt install -y nginx nodejs npm
npm i -g nodemon
git clone .../foo && sudo mv foo /srv/

sudo tee /etc/systemd/system/foo.service >/dev/null <<END
[Unit]
Description=...
After=network.target
[Service]
User=nobody
Group=nogroup
WorkingDirectory=/srv/foo/server
ExecStart=/usr/bin/node serve.js
[Install]
WantedBy=multi-user.target
END
sudo systemctl daemon-reload
sudo systemctl start foo
sudo systemctl enable foo

sudo tee /etc/nginx/sites-available/example.com >/dev/null <<END
server {
    listen 80;
    server_name example.com
    location / {
        include proxy_params;
        proxy_pass http://localhost:8897;
    }
}
END
sudo ln -s /etc/nginx/sites-available/example.com /etc/nginx/sites-enabled/
sudo nginx -t # check that the file is good
sudo systemctl restart nginx

#follow instructions at <https://certbot.eff.org/> and check with `systemctl list-timers`.
