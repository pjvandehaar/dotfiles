# Note: This prompt has two problems:
#       (1) The git code could be faster
#       (2) It uses `trap f DEBUG`, which conflicts with other uses of `trap`, and is odd with C-x C-e


if [[ -z $TERM || $TERM = dumb || $- != *i* ]]; then
    unset PROMPT_COMMAND
    PS1="\w $ "
    return  # Ends this script
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
    timeout() { shift; "$@"; }
fi

PROMPT_COMMAND=_PP_prompt
_PP_prompt() {
    # Set _errnum first, to avoid overwriting "$?"
    local _errnum="$?"
    local _err=''
    if [[ "$_errnum" != 0 ]] && ! [[ $_PP_blank_command ]]; then
        _err="${_PP_RED} ${_errnum} "
    fi

    # Log last command to ~/.full_history like <https://www.jefftk.com/p/you-should-be-logging-shell-history>
    if [[ $_PP_prompt_has_run ]] && ! [[ $_PP_blank_command ]]; then
        local _last_cmd=$(history 1 | perl -pale 's{^\s*\d+\*?\s+}{}')
        echo "$(date +%Y-%m-%d_%H:%M:%S%z)  $(hostname)  $PWD  ${_PP_user_command_runtime}s  $_last_cmd" >> ~/.full_history
    fi
    _PP_prompt_has_run=1

    local _git=''
    _git=${_PP_YEL}$($_PP_timeout 0.2 prompt_git.sh 2>/dev/null)

    local _root=''
    if [[ $USER = root ]]; then
        _root="${_PP_RED} ROOT"
    fi

    local _machine="";
    if [[ -n ${MACHINE_LABEL:-} ]]; then
        _machine="${_PP_PIN}${MACHINE_LABEL}"
    fi

    local _time="${_PP_GRE} \t "

    local _venv=""
    local _tilde=\~ # bash3 vs bash4
    if [[ -n ${VIRTUAL_ENV:-} ]]; then
        _venv="${_PP_PIN} [${VIRTUAL_ENV/#$HOME/$_tilde}] "
    fi

    local _pwd="${PWD/#$HOME/$_tilde}"
    local -i _pwd_maxlen=$(( ${COLUMNS:-80} / 3 ))
    if (( ${#_pwd} > _pwd_maxlen )); then _pwd=" …${_pwd:${#_pwd}-${_pwd_maxlen}}"; fi
    _pwd=$(echo "$_pwd" | sed -e 's_\\_\\\\_g' -e 's_\$_\\\$_g')
    if ! [[ -w "$PWD" ]]; then _pwd="(ro) ${_pwd}"; fi # read-only
    _pwd="${_PP_BLU} ${_pwd} "

    local _runtime=''
    if [[ "$_PP_user_command_runtime" -ge 5 ]]; then
        [[ "$_PP_user_command_runtime" -ge 86400 ]] && _runtime+="$((_PP_user_command_runtime / 86400))days"
        [[ "$_PP_user_command_runtime" -ge 3600 ]] && _runtime+="$((_PP_user_command_runtime % 86400 / 3600))hours"
        [[ "$_PP_user_command_runtime" -ge 60 ]] && _runtime+="$((_PP_user_command_runtime % 3600 / 60))min"
        _runtime+="$((_PP_user_command_runtime % 60))s"
        _runtime="${_PP_PIN} ${_runtime} "
    fi

    # Set ending based only on terminal width, so that it doesn't suddenly change
    local _ending=" "
    if [[ -n $COLUMNS ]] && [[ $COLUMNS -lt 115 ]]; then
        _ending="\n$ "
    fi

    PS1="${_PP_NONE}${_root}${_machine}${_time}${_venv}${_pwd}${_git}${_runtime}${_err}${_PP_NONE}${_ending}"
}

## Use a trap to supply ${_PP_user_command_runtime} and ${_PP_blank_command}:
_PP_user_command_runtime=0
_PP_blank_command=
_PP_debug() {
    # Note: BASH_COMMAND is the command that is just starting right now.
    #       Bash updates $BASH_COMMAND the moment it starts parsing a command, including a command in a script or function.
    #       But bash doesn't modify BASH_COMMAND while inside a trap (like this function)

    if [[ "$BASH_COMMAND" != _PP_prompt ]] && [[ "$_PP_prompt_just_ran" ]]; then
        # User just typed a command and hit <enter>
        _PP_blank_command=''
        _PP_user_command_start_time=$SECONDS
    elif [[ "$BASH_COMMAND" = _PP_prompt ]] && ! [[ "$_PP_prompt_just_ran" ]]; then
        # User's command just finished (or was killed) and we're back to a command prompt
        _PP_user_command_runtime=$((SECONDS - _PP_user_command_start_time))
    elif [[ "$BASH_COMMAND" = _PP_prompt ]] && [[ "$_PP_prompt_just_ran" ]]; then
        # User just hit <enter> without typing anything.
        _PP_blank_command=1
        _PP_user_command_runtime=0
    elif [[ "$BASH_COMMAND" != _PP_prompt ]] && ! [[ "$_PP_prompt_just_ran" ]]; then
        # One of the users's commands just finished and another is starting (eg, `echo|tail;echo`)
        :
    fi

    if [[ "$BASH_COMMAND" = _PP_prompt ]]; then
        _PP_prompt_just_ran=1
    else
        _PP_prompt_just_ran=''
    fi
}
# Note: This trap runs _PP_debug everytime a top-of-stack bash command runs.
#       So if you run `echo|head;ls`, each of the three triggers this trap, and each gets its own BASH_COMMAND.
#       This trap also runs for _PP_prompt (b/c it is PROMPT_COMMAND=_PP_prompt)
trap '_PP_debug' DEBUG # This gets run just before any bash command.
