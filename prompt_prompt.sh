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

_last_time="$(date +%s.%N)"
_tdiff() {
    local _new_time="$(date +%s.%N)"
    local _offset="$(echo "$_new_time-$_last_time" | bc)"
    if [[ "$(echo "$_offset > 0.3" | bc)" == 1 ]]; then
        tput setab 5; tput setaf 7
        echo "$_offset $@"
        tput sgr0
    fi
    _last_time=$_new_time
}
_treset() {
    _last_time="$(date +%s.%N)"
}

export PROMPT_COMMAND=_PP_prompt
_PP_prompt() {
    local _err="$?"
    [[ "$_err" -eq 0 ]] && _err='' || _err="${_PP_RED} ${_err} "

    _tdiff 'until start of _PP_PROMPT'

    local _runtime=''
    if [[ "$_PP_runtime_seconds" -ge 5 ]]; then
        [[ "$_PP_runtime_seconds" -ge 86400 ]] && _runtime+="$((_PP_runtime_seconds / 86400))d"
        [[ "$_PP_runtime_seconds" -ge 3600 ]] && _runtime+="$((_PP_runtime_seconds % 86400 / 3600))h"
        [[ "$_PP_runtime_seconds" -ge 60 ]] && _runtime+="$((_PP_runtime_seconds % 3600 / 60))m"
        _runtime+="$((_PP_runtime_seconds % 60))s"
        _runtime="${_PP_PIN} ${_runtime} "
    fi

    _tdiff 'until just before git'
    # TODO: use "timeout 0.3 \git ..."
    local _git=''
    if [[ "$(\git rev-parse --is-inside-work-tree 2>/dev/null)" = true ]]; then
        _tdiff 'to check for .git' # 200ms
    if \git show-ref --head --quiet; then
        _tdiff 'to check for HEAD' # >1s
        _git="$(\git symbolic-ref -q HEAD)" && _git="$(printf %q "${_git#refs/heads/}")" || _git="$(\git rev-parse --short -q HEAD)"
        _tdiff 'to check  branch'
        local changes
        git diff-index --quiet --cached HEAD || changes+=i
        _tdiff 'to check index' # 700ms
        git diff-files --quiet || changes+=w
        _tdiff 'to check workingdir' # >1s
        # git ls-files --other --exclude-standard --no-empty-directory | grep -q ^ || changes+=u
        _git="${_PP_YEL} ${_git}${changes:+($changes)} "
    fi
    fi
    _tdiff 'til git is done'

    local _tilde=\~ # bash3 vs bash4
    local _pwd="${PWD/#$HOME/$_tilde}"
    local -i _max_len=$(( ${COLUMNS:-80} / 3 ))
    (( ${#_pwd} > _max_len )) && _pwd=" … ${_pwd:${#_pwd}-${_max_len}}"
    _pwd=$(echo "$_pwd" | sed -e 's_\\_\\\\_g' -e 's_\$_\\\$_g')

    _tdiff 'til end of _PP_PROMPT'
    PS1="${_PP_NONE}${_PP_GRE} \t ${_PP_BLU} ${_pwd} $_git${_runtime}${_err}${_PP_NONE} "
}

_PP_runtime_last_seconds=$SECONDS
_PP_reset_runtime() {
    # I don't understand how this handles pipes.  `$BASH_COMMAND` never reveals anything but the final command.
    if [[ -n "$_PP_prompt_just_ran" ]]; then
        _PP_runtime_last_seconds=$SECONDS
        _PP_runtime_seconds=0
        #_tdiff "from start of _PP_PROMPT until start-trap for '${BASH_COMMAND:0:80}'"
        _treset
    else
        _PP_runtime_seconds=$((SECONDS - _PP_runtime_last_seconds))
        #_tdiff "from start of last command until start-trap for '${BASH_COMMAND:0:80}'"
        _treset
    fi

    [[ "$BASH_COMMAND" = _PP_prompt ]] && _PP_prompt_just_ran=1 || _PP_prompt_just_ran=''
}
trap '_PP_reset_runtime' DEBUG # This gets run just before any bash command.
