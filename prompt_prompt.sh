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

export PROMPT_COMMAND=_PP_prompt
_PP_prompt() {
    local _err="$?"
    [[ "$_err" -eq 0 ]] && _err='' || _err="${_PP_RED} ${_err} "

    # TODO: s/_pid/_git/

    local _runtime=''
    if [[ "$_PP_runtime_seconds" -ge 5 ]]; then
        [[ "$_PP_runtime_seconds" -ge 86400 ]] && _runtime+="$((_PP_runtime_seconds / 86400))d"
        [[ "$_PP_runtime_seconds" -ge 3600 ]] && _runtime+="$((_PP_runtime_seconds % 86400 / 3600))h"
        [[ "$_PP_runtime_seconds" -ge 60 ]] && _runtime+="$((_PP_runtime_seconds % 3600 / 60))m"
        _runtime+="$((_PP_runtime_seconds % 60))s"
        _runtime="${_PP_PIN} ${_runtime} "
    fi

    local timeout=''
    if which timeout >/dev/null; then
        timeout=timeout
    elif which gtimeout >/dev/null; then
        timeout=gtimeout
    else
        echo "oh noes! TODO: handle this."
    fi
    local _git_branch="$($timeout 0.1 \git rev-parse --is-inside-work-tree 2>/dev/null)"; local _pid=$?
    if [[ $_pid == 124 ]]; then
        local _git="${_PP_RED} check for .git timed out ${_PP_NONE}"
    elif [[ "$_git_branch" != true ]]; then
        local _git=''
    elif [[ $_pid != 0 ]]; then
        local _git="${_PP_RED} AAHHHH why was the PID $_pid ????"
    else
        # I think we do this to check for uninitialized repos.
        $timeout 0.2 \git show-ref --head --quiet; local _pid=$?
        if [[ $_pid == 124 ]]; then
            local _git="${_PP_RED} checking for HEAD timed out ${_PP_NONE}"
        elif [[ $_pid == 128 ]]; then
            local _git="${_PP_RED} no HEAD? ${_PP_NONE}"
        elif [[ $_pid != 0 ]]; then
            local _git="${_PP_RED} AAHHHHG why was the PID $_pid ???? ${_PP_NONE}"
        else
            local _git_head_ref="$($timeout 0.2 \git symbolic-ref -q HEAD)"; local _pid=$?
            if [[ $_pid == 124 ]]; then
                local _git="${_PP_RED} checking branch timed out ${_PP_NONE}"
            elif [[ $_pid -ge 1 ]]; then
                local _git=" AH why was the PID $_pid ??"
            else
                if [[ -n $_git_head_ref ]]; then
                    local _git_branch="$(printf %q "${_git_head_ref#refs/heads/}")"
                else
                    local _git_branch="$(\git rev-parse --short -q HEAD)"
                fi

                local _changes
                $timeout 0.2 git diff-index --quiet --cached HEAD; local _pid=$?
                if [[ $_pid == 1 ]]; then
                    _changes+=i
                elif [[ $_pid == 124 ]]; then
                    _changes+='index_timed_out '
                elif [[ $_pid != 0 ]]; then
                    _changes+="what_is_PID_${_pid}_for_index"
                fi
                $timeout 0.2 git diff-files --quiet; local _pid=$?
                if [[ $_pid == 1 ]]; then
                    _changes+=w
                elif [[ $_pid == 124 ]]; then
                    _changes+='workdir_timed_out '
                elif [[ $_pid != 0 ]]; then
                    _changes+="what_is_PID_${_pid}_for_workdir "
                fi
                _changes="${_changes## }"
                _git="${_PP_YEL} ${_git_branch}${_changes:+($_changes)} "

                # TODO: use with timeout
                # set pipefail, and use ' | grep -n1 ^ | wc -l'
                # git ls-files --other --exclude-standard --no-empty-directory | grep -q ^ || changes+=u
            fi
        fi
    fi

    local _tilde=\~ # bash3 vs bash4
    local _pwd="${PWD/#$HOME/$_tilde}"
    local -i _max_len=$(( ${COLUMNS:-80} / 3 ))
    (( ${#_pwd} > _max_len )) && _pwd=" … ${_pwd:${#_pwd}-${_max_len}}"
    _pwd=$(echo "$_pwd" | sed -e 's_\\_\\\\_g' -e 's_\$_\\\$_g')

    PS1="${_PP_NONE}${_PP_GRE} \t ${_PP_BLU} ${_pwd} $_git${_runtime}${_err}${_PP_NONE} "
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
