if [[ -z $TERM || $TERM = dumb || $- != *i* ]]; then
    unset PROMPT_COMMAND
    export PS1="\w $ "
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

export PROMPT_COMMAND=_PP_prompt
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
        _git="${_PP_RED} _PP_timeout while checking for .git ${_PP_NONE}"
    elif [[ "$_git_branch" != true ]]; then
        _git=''
    elif [[ $_rs != 0 ]]; then
        _git="${_PP_RED} AAHHHH why was the return status $_rs ???? ${_PP_NONE}"
    else

        # Check that the repo has a HEAD
        $_PP_timeout 0.2 \git show-ref --head --quiet; local _rs=$?
        if [[ $_rs == 124 ]]; then
            _git="${_PP_RED} _PP_timeout while checking for HEAD ${_PP_NONE}"
        elif [[ $_rs == 1 ]]; then
            _git="${_PP_RED} no HEAD ${_PP_NONE}"
        elif [[ $_rs != 0 ]]; then
            _git="${_PP_RED} AAHHHHG why was the return status $_rs ???? ${_PP_NONE}"
        else

            # Check that the repo has a commit
            $_PP_timeout 0.2 \git rev-parse --short -q HEAD &>/dev/null; local _rs=$?
            if [[ $_rs == 124 ]]; then
                _git="${_PP_RED} _PP_timeout while checking for any commits ${_PP_NONE}"
            elif [[ $_rs == 1 ]]; then
                _git="${_PP_RED} no commits ${_PP_NONE}"
            elif [[ $_rs != 0 ]]; then
                _git="${_PP_RED} AAHHRG why was the return status $_rs ???? ${_PP_NONE}"
            else

                # Get the current branch
                local _git_head_ref="$($_PP_timeout 0.2 \git symbolic-ref -q HEAD)"; local _rs=$?
                if [[ $_rs == 124 ]]; then
                    _git="${_PP_RED} _PP_timeout while checking branch ${_PP_NONE}"
                elif [[ $_rs -ge 1 ]]; then
                    _git=" AH why was the return status $_rs ??"
                else

                    # If HEAD is on a branch, format it.  If not, get HEAD's commit.
                    if [[ -n $_git_head_ref ]]; then
                        local _git_branch="$(printf %q "${_git_head_ref#refs/heads/}")"
                    else
                        local _git_branch="$(\git rev-parse --short -q HEAD)"
                    fi

                    local _git_state=''
                    local _git_dir="$($_PP_timeout 0.1 \git rev-parse --git-dir 2>/dev/null)"; local _rs=$?
                    if [[ $_rs == 124 ]]; then
                        _git_state='_PP_timeout while checking for gitdir'
                    elif [[ -f "${_git_dir}/MERGE_HEAD" ]]; then
                        _git_state='MERGING'
                    elif [[ -d "${_git_dir}/rebase-apply" || -d "${git_dir}/rebase-merge" ]]; then
                        _git_state='REBASING'
                    elif [[ -f "${_git_dir}/CHERRY_PICK_HEAD" ]]; then
                        _git_state='CHERRY-PICKING'
                    fi

                    local _changes=''
                    $_PP_timeout 0.2 git diff-index --quiet --cached HEAD; local _rs=$?
                    if [[ $_rs == 1 ]]; then
                        _changes+='i'
                    elif [[ $_rs == 124 ]]; then
                        _changes+=' index_timed_out '
                    elif [[ $_rs != 0 ]]; then
                        _changes+=" what_is_RS_${_rs}_for_index "
                    fi
                    $_PP_timeout 0.2 git diff-files --quiet; local _rs=$?
                    if [[ $_rs == 1 ]]; then
                        _changes+='w'
                    elif [[ $_rs == 124 ]]; then
                        _changes+=' workdir_timed_out '
                    elif [[ $_rs != 0 ]]; then
                        _changes+=" what_is_RS_${_rs}_for_workdir "
                    fi

                    [[ -n "$_changes" ]] && _changes="($_changes)"
                    [[ -n "$_git_state" ]] && _git_state="($_git_state)"
                    _git="${_PP_YEL} ${_git_branch}${_git_state}${_changes} ${_PP_NONE}"
                fi
            fi
        fi
    fi

    local _tilde=\~ # bash3 vs bash4
    local _pwd="${PWD/#$HOME/$_tilde}"
    local -i _max_len=$(( ${COLUMNS:-80} / 3 ))
    (( ${#_pwd} > _max_len )) && _pwd=" … ${_pwd:${#_pwd}-${_max_len}}"
    _pwd=$(echo "$_pwd" | sed -e 's_\\_\\\\_g' -e 's_\$_\\\$_g')

    PS1="${_PP_NONE}${_PP_GRE} \t ${_PP_BLU} ${_pwd} ${_git}${_runtime}${_err}${_PP_NONE} "
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
