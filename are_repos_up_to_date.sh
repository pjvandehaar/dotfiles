#!/bin/bash

ls -F | grep '.*/' | xargs -L1 -I% bash -c "echo;echo; cd %; echo %; git status||hg status"
