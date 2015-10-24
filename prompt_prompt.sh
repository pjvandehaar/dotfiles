[[ "$TERM" = dumb ]] && export PS1="\w $" && exit 0

_PP_RED="\[$(tput setab 1; tput setaf 7)\]"
_PP_GRE="\[$(tput setab 2; tput setaf 7)\]"
_PP_YEL="\[$(tput setab 3; tput setaf 0)\]"
_PP_BLU="\[$(tput setab 4; tput setaf 7)\]"
_PP_PIN="\[$(tput setab 5; tput setaf 7)\]"
# _PP_CYA="\[$(tput setab 6)\]"
_NONE="\[$(tput sgr0)\]"

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

    local _git=''
    if [[ "$(git rev-parse --is-inside-work-tree 2>/dev/null)" = true ]]; then
        _git="$(\git symbolic-ref -q HEAD)" && _git="$(printf %q "${_git#refs/heads/}")" || _git="$(\git rev-parse --short -q HEAD)"
        local changes
        git diff-index --quiet --cached HEAD || changes+=i
        git diff-files --quiet || changes+=w
        # git ls-files --other --exclude-standard --no-empty-directory | grep -q ^ || changes+=u
        _git="${_PP_YEL} ${_git}${changes:+($changes)} "
    fi

    local _pwd="${PWD/#$HOME/~}"
    local -i _max_len=$(( ${COLUMNS:-80} / 3 ))
    (( ${#_pwd} > _max_len )) && _pwd=" … ${_pwd:${#_pwd}-${_max_len}}"

    PS1="${_NONE}${_PP_GRE} \t ${_PP_BLU} ${_pwd} $_git${_runtime}${_err}${_NONE} "
}

_PP_runtime_last_seconds=$SECONDS
_PP_reset_runtime() {
    if [[ -n "$_PP_prompt_just_ran" ]]; then
        _PP_runtime_last_seconds=$SECONDS
        _PP_runtime_seconds=0
    else
        _PP_runtime_seconds=$((SECONDS - _PP_runtime_last_seconds))
    fi

    [ "$BASH_COMMAND" = _PP_prompt ] && _PP_prompt_just_ran=1 || _PP_prompt_just_ran=''
}
trap '_PP_reset_runtime' DEBUG # This gets run just before any bash command.

