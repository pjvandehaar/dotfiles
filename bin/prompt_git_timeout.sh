#!/bin/bash
# This script shows a quick summary of the current git state (if you're in a git repo).
# It's called by prompt_prompt.sh
# In theory, prompt_prompt should be able to run `timeout` itself, but in practice that didn't work for me.

if type -t timeout &>/dev/null; then
    output=$(timeout 0.2 prompt_git.sh); rs=$?
else
    output=$(prompt_git.sh); rs=0
fi

if [[ $rs = 124 ]]; then
    echo " git_timeout "
else
    echo "$output"
fi
