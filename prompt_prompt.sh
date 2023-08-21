# Source this script to set PS1 dynamically.

# Note: This prompt has two problems:
#       (1) It uses `trap f DEBUG`, which conflicts with other uses of `trap`, and is odd with C-x C-e.  But that's the only way to get runtime in bash.
#       (2) The git code could be faster.  But that would require a long bash script, or something compiled (and thus less portable).


if [[ -z $TERM || $TERM = dumb || $- != *i* ]]; then  # Abort if we're not in a real terminal
    unset PROMPT_COMMAND
    PS1="\w $ "
    return  # Ends this script
fi


# Run tput only once, even though it pollutes the namespace, because it takes dozens of ms
_PP_RED="\[$(tput setab 1; tput setaf 7)\]"
_PP_GRE="\[$(tput setab 2; tput setaf 7)\]"
_PP_YEL="\[$(tput setab 3; tput setaf 0)\]"
_PP_BLU="\[$(tput setab 4; tput setaf 7)\]"
_PP_PIN="\[$(tput setab 5; tput setaf 7)\]"
_PP_NONE="\[$(tput sgr0)\]"


PROMPT_COMMAND=_PP_prompt
_PP_prompt() {
    local _errnum="$?"  # Set _errnum first b/c later commands overwrite "$?"

    # Log last command to ~/.full_history like <https://www.jefftk.com/p/you-should-be-logging-shell-history>
    if [[ $_PP_prompt_has_run ]] && ! [[ $_PP_blank_command ]]; then
        local _last_cmd=$(history 1 | perl -pale 's{^\s*\d+\*?\s+}{}')
        echo "$(date +%Y-%m-%d_%H:%M:%S%z)  $(hostname)  $PWD  ${_PP_user_command_runtime}s  $_last_cmd" >> ~/.full_history
    fi
    _PP_prompt_has_run=1

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
    elif [[ -n ${CONDA_DEFAULT_ENV} ]]; then
        _venv="${_PP_PIN} [${CONDA_DEFAULT_ENV}] "
    fi

    local _pwd="${PWD/#$HOME/$_tilde}"
    local -i _pwd_maxlen=$(( ${COLUMNS:-80} / 3 ))
    if (( ${#_pwd} > _pwd_maxlen )); then _pwd=" …${_pwd:${#_pwd}-${_pwd_maxlen}}"; fi
    _pwd=$(echo "$_pwd" | sed -e 's_\\_\\\\_g' -e 's_\$_\\\$_g')
    if ! [[ -w "$PWD" ]]; then _pwd="(ro) ${_pwd}"; fi # read-only
    _pwd="${_PP_BLU} ${_pwd} "

    local _git="${_PP_YEL}$(prompt_git_timeout.sh)"

    local _runtime=''
    if [[ "$_PP_user_command_runtime" -ge 5 ]]; then
        [[ "$_PP_user_command_runtime" -ge 86400 ]] && _runtime+="$((_PP_user_command_runtime / 86400))days"
        [[ "$_PP_user_command_runtime" -ge 3600 ]] && _runtime+="$((_PP_user_command_runtime % 86400 / 3600))hours"
        [[ "$_PP_user_command_runtime" -ge 60 ]] && _runtime+="$((_PP_user_command_runtime % 3600 / 60))min"
        _runtime+="$((_PP_user_command_runtime % 60))s"
        _runtime="${_PP_PIN} ${_runtime} "
    fi

    local _err=''
    if [[ "$_errnum" != 0 ]] && ! [[ $_PP_blank_command ]]; then
        _err="${_PP_RED} ${_errnum} "
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
