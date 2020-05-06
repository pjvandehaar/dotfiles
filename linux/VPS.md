# Notes on setting up a VPS:

# System setup
adduser --disabled-password kpa
visudo # append 'kpa ALL = (ALL) NOPASSWD: ALL'
su -l kpa && ssh-keygen # add id_rsa.pub to github/gitlab
sudo cat /root/.ssh/authorized_keys > ~/.ssh/authorized_keys
git clone https://github.com/pjvandehaar/dotfiles
mv .bashrc .orig-system-bashrc
./dotfiles/install.sh
sudo apt update && sudo apt install -y emacs tig mosh python3-pip python3-venv # & more from packages.txt
pip3 install ipython kpa # & more from packages.txt
sudo apt install -y cargo && cargo install exa

# Webserver setup
sudo ufw allow OpenSSH
sudo ufw allow mosh
sudo ufw allow 'Nginx Full'
sudo ufw enable

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
