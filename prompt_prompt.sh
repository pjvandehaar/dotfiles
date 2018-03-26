if [[ -z $TERM || $TERM = dumb || $- != *i* ]]; then
    unset PROMPT_COMMAND
    PS1="\w $ "
    return
fi

# tput takes dozens of ms, so run it only once, even though it pollutes the namespace
_PP_RED="\[$(tput setab 1; tput setaf 7)\]"
_PP_GRE="\[$(tput setab 2; tput setaf 7)\]"
_PP_YEL="\[$(tput setab 3; tput setaf 0)\]"
_PP_BLU="\[$(tput setab 4; tput setaf 7)\]"
_PP_PIN="\[$(tput setab 5; tput setaf 7)\]"
_PP_NONE="\[$(tput sgr0)\]"

_PP_timeout=''
if which timeout >/dev/null; then
    _PP_timeout=timeout
elif which gtimeout >/dev/null; then
    _PP_timeout=gtimeout
else
    _PP_timeout=timeout
    timeout() { shift; $*; }
fi

PROMPT_COMMAND=_PP_prompt
_PP_prompt() {
    local _err="$?"
    [[ "$_err" -eq 0 ]] && _err='' || _err="${_PP_RED} ${_err} "

    local _runtime=''
    if [[ "$_PP_runtime_seconds" -ge 5 ]]; then
        [[ "$_PP_runtime_seconds" -ge 86400 ]] && _runtime+="$((_PP_runtime_seconds / 86400))d"
        [[ "$_PP_runtime_seconds" -ge 3600 ]] && _runtime+="$((_PP_runtime_seconds % 86400 / 3600))h"
        [[ "$_PP_runtime_seconds" -ge 60 ]] && _runtime+="$((_PP_runtime_seconds % 3600 / 60))m"
        _runtime+="$((_PP_runtime_seconds % 60))s"
        _runtime="${_PP_PIN} ${_runtime} "
    fi

    local _git='' # This variable stores the result of all this.
    local _git_branch="$($_PP_timeout 0.1 \git rev-parse --is-inside-work-tree 2>/dev/null)"; local _rs=$?
    if [[ $_rs == 124 ]]; then
        _git="${_PP_RED} looking for .git timedout ${_PP_NONE}"
    elif [[ "$_git_branch" != true ]]; then
        _git=''
    elif [[ $_rs != 0 ]]; then
        _git="${_PP_RED} AAHHHH why was the return status $_rs ???? ${_PP_NONE}"
    else

        # Check that the repo has a HEAD
        $_PP_timeout 0.2 \git show-ref --head --quiet; local _rs=$?
        if [[ $_rs == 124 ]]; then
            _git="${_PP_RED} looking up HEAD timedout ${_PP_NONE}"
        elif [[ $_rs == 1 ]]; then
            _git="${_PP_RED} no HEAD ${_PP_NONE}"
        elif [[ $_rs != 0 ]]; then
            _git="${_PP_RED} AAHHHHG why was the return status $_rs ???? ${_PP_NONE}"
        else

            # Check that the repo has a commit
            $_PP_timeout 0.2 \git rev-parse --short -q HEAD &>/dev/null; local _rs=$?
            if [[ $_rs == 124 ]]; then
                _git="${_PP_RED} checking for commits timedout ${_PP_NONE}"
            elif [[ $_rs == 1 ]]; then
                _git="${_PP_RED} no commits ${_PP_NONE}"
            elif [[ $_rs != 0 ]]; then
                _git="${_PP_RED} AAHHRG why was the return status $_rs ???? ${_PP_NONE}"
            else

                # Get the current branch
                local _git_head_ref="$($_PP_timeout 0.2 \git symbolic-ref -q HEAD)"; local _rs=$?
                if [[ $_rs == 124 ]]; then
                    _git="${_PP_RED} checking the branch timedout ${_PP_NONE}"
                elif [[ $_rs -ge 1 ]]; then
                    _git=" AH why was the return status $_rs ??"
                else

                    # If HEAD is on a branch, strip junk.  If not, get HEAD's commit's hash.
                    local _git_branch
                    if [[ -n $_git_head_ref ]]; then
                        _git_branch="$(printf %q "${_git_head_ref#refs/heads/}")"
                    else
                        _git_branch="$(\git rev-parse --short -q HEAD)"
                    fi

                    local _git_state=''

                    # check if we're in the middle of merging/rebasing/cherry-picking
                    local _git_dir="$($_PP_timeout 0.1 \git rev-parse --git-dir 2>/dev/null)"; local _rs=$?
                    if [[ $_rs == 124 ]]; then
                        _git_state+=' gitdir_timedout'
                    else
                        if [[ -f "${_git_dir}/MERGE_HEAD" ]]; then
                            _git_state+=' MERGING '
                        fi
                        if [[ -d "${_git_dir}/rebase-apply" || -d "${git_dir}/rebase-merge" ]]; then
                            _git_state+=' REBASING '
                        fi
                        if [[ -f "${_git_dir}/CHERRY_PICK_HEAD" ]]; then
                            _git_state+=' CHERRY-PICKING '
                        fi

                        # check whether there are unpushed commits.
                        local _git_remote="$(git config --get branch.${_git_branch}.remote 2>/dev/null)"
                        if [[ -n "$_git_remote" ]]; then
                            local _remote_branch="$(git config --get "branch.${_git_branch}.merge")"
                            echo "$_remote_branch" | grep -q '^refs/heads/' || echo "ohnoes:${_remote_branch}"
                            _remote_branch="${_remote_branch#refs/heads/}"
                            local _remote_branch_ref="refs/remotes/${_git_remote}/${_remote_branch}"
                            local _num_unpushed_comms="$(git rev-list --no-merges --count "${_remote_branch_ref}..HEAD" 2>/dev/null)"
                            if [[ -n $_num_unpushed_comms ]] && [[ _num_unpushed_comms -gt 0 ]]; then
                                _git_state+=' PUSH! '
                            fi
                        fi

                        # check for staged & unstaged-but-tracked changes
                        $_PP_timeout 0.2 git diff-index --quiet --cached HEAD; local _rs=$?
                        if [[ $_rs == 124 ]]; then
                            _git_state+=' index_timedout '
                        else
                            if [[ $_rs == 1 ]]; then
                                _git_state+='i'
                            elif [[ $_rs != 0 ]]; then
                                _git_state+=" what_is_RS_${_rs}_for_index "
                            fi

                            $_PP_timeout 0.2 git diff-files --quiet; local _rs=$?
                            if [[ $_rs == 124 ]]; then
                                _git_state+=' workdir_timedout '
                            elif [[ $_rs == 1 ]]; then
                                _git_state+='w'
                            elif [[ $_rs != 0 ]]; then
                                _git_state+=" what_is_RS_${_rs}_for_workdir "
                            fi
                        fi
                    fi

                    [[ -n "$_git_state" ]] && _git_state="($(echo "$_git_state" | perl -ple 's{ +}{ }g; s{^ | \z}{}g'))"
                    _git="${_PP_YEL} ${_git_branch}${_git_state} ${_PP_NONE}"
                fi
            fi
        fi
    fi

    PS1="${_git}${_runtime}${_err}${_PP_NONE}"
    local _tilde=\~ # bash3 vs bash4

    if [[ -n ${VIRTUAL_ENV:-} ]]; then
        local _venv="[${VIRTUAL_ENV/#$HOME/$_tilde}]"
    else
        local _venv=" "
    fi

    # newline-or-not depends on width of the terminal to keep it consistent
    if [[ -z $COLUMNS ]] || [[ $COLUMNS -ge 115 ]]; then
        local -i _max_len=$(( ${COLUMNS:-80} / 3 ))
        local _ending=" "
    else
        local -i _max_len=$(( $COLUMNS - ${#PS1} - ${#_venv} - 5 ))
        local _ending="\n$ "
    fi

    local _pwd="${PWD/#$HOME/$_tilde}"
    (( ${#_pwd} > _max_len )) && _pwd=" … ${_pwd:${#_pwd}-${_max_len}}"
    _pwd=$(echo "$_pwd" | sed -e 's_\\_\\\\_g' -e 's_\$_\\\$_g')
    [[ -w "$PWD" ]] || _pwd="(ro) ${_pwd}" # read-only

    PS1="${_PP_NONE}${_PP_GRE} \t${_venv}${_PP_BLU} ${_pwd} ${PS1}${_ending}"
    if [[ $USER == root ]]; then
        PS1="${_PP_RED} ROOT ${PS1}"
    fi
}

_PP_runtime_last_seconds=$SECONDS
_PP_reset_runtime() {
    # I don't understand how this handles pipes.  `$BASH_COMMAND` never reveals anything but the final command.
    if [[ -n "$_PP_prompt_just_ran" ]]; then
        _PP_runtime_last_seconds=$SECONDS
        _PP_runtime_seconds=0
    else
        _PP_runtime_seconds=$((SECONDS - _PP_runtime_last_seconds))
    fi

    [[ "$BASH_COMMAND" = _PP_prompt ]] && _PP_prompt_just_ran=1 || _PP_prompt_just_ran=''
}
trap '_PP_reset_runtime' DEBUG # This gets run just before any bash command.
