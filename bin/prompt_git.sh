#!/bin/bash
# This script shows a quick, colored summary of the current git state (if you're in a git repo).
# It's called by prompt_prompt.sh



show_branch() {
    local _rs=''
    # TODO: Only look for .git/ 0-3 dirs up.
    local _gitdir=$(command git rev-parse --absolute-git-dir 2>/dev/null); _rs=$?
    if [[ $_rs = 0 ]] && [[ -n $_gitdir ]]; then
        local _git_head=$(head -c500 "$_gitdir/HEAD"); _rs=$?
        if [[ $_rs = 0 ]] && [[ -n $_git_head ]]; then
            _git_head=${_git_head#ref: }
            _git_head=${_git_head#refs/heads/}
            if [[ ${#_git_head} = 40 ]]; then
                _git_head=${_git_head:0:5}
            fi
            echo " $(printf %q "$_git_head"). "
        fi
    fi
}


show_all() {
    local _rs=''

    # Find .git/ dir
    local _gitdir=$(command git rev-parse --absolute-git-dir 2>/dev/null); _rs=$?
    if [[ $_rs != 0 ]] || [[ -z $_gitdir ]]; then
        return
    fi

    # Check that the repo has a HEAD
    command git show-ref --head --quiet; _rs=$?
    if [[ $_rs != 0 ]]; then
        echo " no_HEAD "
        return
    fi

    # Get HEAD commit hash
    local _head_hash=$(command git rev-parse --short -q HEAD 2>/dev/null); _rs=$?
    if [[ $_rs != 0 ]]; then
        echo " no_commits "
        return
    fi

    # Get branch name (if we're on one, and HEAD isn't just on a commit hash)
    local _head_ref=$(command git symbolic-ref -q --short HEAD); _rs=$?
    if [[ $_rs -ge 2 ]]; then # $_rs=1 happens if we checkout a hash
        echo " status=$_rs "
        return
    fi

    # Get name of HEAD
    local _git_branch
    if [[ -n $_head_ref ]]; then
        _git_branch=$(printf %q "$_head_ref")  # Strips junk?
    else
        _git_branch=$_head_hash
    fi


    local _git_state=''

    # Check if we're in the middle of merging/rebasing/cherry-picking
    if [[ -f "${_gitdir}/MERGE_HEAD" ]]; then
        _git_state+=' MERGING '
    fi
    if [[ -d "${_gitdir}/rebase-apply" || -d "${_gitdir}/rebase-merge" ]]; then
        _git_state+=' REBASING '
    fi
    if [[ -f "${_gitdir}/CHERRY_PICK_HEAD" ]]; then
        _git_state+=' CHERRY-PICKING '
    fi

    # Check whether there are unpushed commits.
    local _git_remote=$(command git config --get "branch.${_git_branch}.remote" 2>/dev/null)
    if [[ -n "$_git_remote" ]]; then
        local _remote_branch=$(command git config --get "branch.${_git_branch}.merge")
        if ! echo "$_remote_branch" | grep -q '^refs/heads/'; then
            echo "ohnoes:${_remote_branch}"
        fi
        _remote_branch=${_remote_branch#refs/heads/}
        local _remote_branch_ref="refs/remotes/${_git_remote}/${_remote_branch}"
        local _num_unpushed_comms=$(command git rev-list --no-merges --count "${_remote_branch_ref}..HEAD" 2>/dev/null)
        if [[ -n $_num_unpushed_comms ]] && [[ _num_unpushed_comms -gt 0 ]]; then
            _git_state+=' PUSH! '
        fi
    fi

    # Check for staged changes
    command git diff-index --quiet --cached HEAD; _rs=$?
    if [[ $_rs = 1 ]]; then
        _git_state+='i'
    elif [[ $_rs != 0 ]]; then
        _git_state+=" what_is_RS_${_rs}_for_index "
    fi

    # Check for unstaged tracked changes
    command git diff-files --quiet; _rs=$?
    if [[ $_rs = 1 ]]; then
        _git_state+='w'
    elif [[ $_rs != 0 ]]; then
        _git_state+=" what_is_RS_${_rs}_for_workdir "
    fi

    if [[ -n "$_git_state" ]]; then
        _git_state=$(echo "$_git_state" | perl -ple 's{ +}{ }g; s{^ | \z}{}g')  # Strip spaces
        _git_state="($_git_state)"
    fi
    echo " ${_git_branch}${_git_state} "
}



if [[ $PWD/ = /mnt/efs* ]]; then  # Show only branch, b/c EFS is slow
    show_branch
elif [[ $PWD/ != /mnt/* ]] && [[ $PWD/ != /Volumes/* ]]; then
    show_all
fi
