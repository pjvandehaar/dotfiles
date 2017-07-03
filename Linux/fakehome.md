firsthome=$(getent passwd $USER | cut -d: -f6) # the place where ssh-server will look for .bashrc/.bash_profile
mkdir $firsthome/.ssh
cat >> $firsthome/.ssh/authorized_keys # now paste a pubkey and then <C-d>
echo '. .bashrc' > $firsthome/.bash_profile
echo -e 'export HOME=/home/pjvh\ncd\n. .bashrc' > $firsthome/.bashrc # b/c mosh only reads .bashrc
HOME=/home/$USER && sudo mkdir $HOME && sudo chown $USER:$(groups|ptrcut 1) $HOME && cd
git clone https://github.com/Linuxbrew/brew.git ~/.linuxbrew
git clone https://github.com/pjvandehaar/dotfiles.git && ./dotfiles/Linux/install-dotfiles.sh
ssh-keygen -t rsa -b 4096 -f ~/.ssh/id_rsa
cd $firsthome && ln -s -t . $HOME/.ssh/id_rsa{,.pub} # because ssh often looks in $firsthome, not sure why. maybe the whole .ssh/ should be a symlink, or even $firsthome/?
