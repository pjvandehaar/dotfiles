# Note: If I get tired of this, consider switching to Starship.  Use "plain text symbols".  Starship has a x86 linux binary that I could just include in dotfiles or download in `install.sh`.

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
    # Set _err first, to avoid overwriting "$?"
    local _err="$?"
    if [[ "$_err" -eq 0 ]] || [[ $_PP_blank_command ]]; then
        _err=''
    else
        _err="${_PP_RED} ${_err} "
    fi

    local _tilde=\~ # bash3 vs bash4

    # Log last command to ~/.full_history like <https://www.jefftk.com/p/you-should-be-logging-shell-history>
    if [[ $_PP_prompt_has_run ]] && ! [[ $_PP_blank_command ]]; then
        local _last_cmd=$(history 1 | perl -pale 's{^\s*\d+\*?\s+}{}')
        echo "$(date +%Y-%m-%d_%H:%M:%S%z)  $(hostname)  $PWD  ${_PP_user_command_runtime}s  $_last_cmd" >> ~/.full_history
    fi
    _PP_prompt_has_run=1

    ## Consider making $_git in its own script? Bash, C++, what?
    local _git=''
    if [[ $PWD/ = /mnt/efs* ]]; then  # Show only branch, b/c EFS is slow
        local _gitexe="$(which git)" # avoid problems with aliases/functions, in a way that always works with `timeout`
        local _rs=''
        # TODO: Only look for .git/ 3 dirs deep.
        local _gitdir=$("$_PP_timeout" 0.2 "$_gitexe" rev-parse --show-toplevel 2>/dev/null); _rs=$?
        if [[ $_rs == 124 ]]; then  # timeout
            _git="${_PP_RED} .git/ timeout "
        elif [[ $_rs -ge 2 ]]; then  # $_rs=1 happens if we checkout a tag ("non-symbolic ref")
            _git="${_PP_RED} .git/ status $_rs "
        elif [[ -n $_gitdir ]]; then
            local _git_head=$(head -c500 "$_gitdir/.git/HEAD"); _rs=$?
            if [[ $_rs == 0 ]] && [[ -n $_git_head ]]; then
                _git_head=${_git_head#ref: }
                _git_head=${_git_head#refs/heads/}
                _git="${_PP_YEL} $(printf %q "$_git_head") "
            fi
        fi

    elif [[ $PWD/ = /mnt/* ]] || [[ $PWD/ = /Volumes/* ]]; then  # Show nothing, b/c /mnt/ is slow
        :  # Do nothing

    else
    local _gitexe="$(which git)" # avoid problems with aliases/functions
    local _in_git_repo; _in_git_repo="$($_PP_timeout 0.1 "$_gitexe" rev-parse --is-inside-work-tree 2>/dev/null)"; local _rs=$?
    if [[ $_rs == 124 ]]; then
        _git="${_PP_RED} looking for .git timedout "
    elif [[ "$_in_git_repo" != true ]]; then
        :  # No .git, so show nothing
    elif [[ $_rs != 0 ]]; then
        _git="${_PP_RED} AAHHHH why was the return status $_rs ???? "
    else

        # Check that the repo has a HEAD
        $_PP_timeout 0.2 "$_gitexe" show-ref --head --quiet; local _rs=$?
        if [[ $_rs == 124 ]]; then
            _git="${_PP_RED} looking up HEAD timedout "
        elif [[ $_rs == 1 ]]; then
            _git="${_PP_RED} no HEAD "
        elif [[ $_rs != 0 ]]; then
            _git="${_PP_RED} AAHHHHG why was the return status $_rs ???? "
        else

            # Check that the repo has a commit
            $_PP_timeout 0.2 "$_gitexe" rev-parse --short -q HEAD &>/dev/null; local _rs=$?
            if [[ $_rs == 124 ]]; then
                _git="${_PP_RED} checking for commits timedout "
            elif [[ $_rs == 1 ]]; then
                _git="${_PP_RED} no commits "
            elif [[ $_rs != 0 ]]; then
                _git="${_PP_RED} AAHHRG why was the return status $_rs ???? "
            else

                # Get the current branch
                local _git_head_ref; _git_head_ref="$($_PP_timeout 0.2 "$_gitexe" symbolic-ref -q --short HEAD)"; local _rs=$?
                if [[ $_rs == 124 ]]; then
                    _git="${_PP_RED} checking the branch timedout "
                elif [[ $_rs -ge 2 ]]; then # $_rs=1 happens if we checkout a tag.
                    _git=" AH why was the return status $_rs ??"
                else

                    # If HEAD is on a branch, strip junk.  If not, get HEAD's commit's hash.
                    local _git_branch
                    if [[ -n $_git_head_ref ]]; then
                        _git_branch="$(printf %q "$_git_head_ref")"
                    else
                        _git_branch="$("$_gitexe" rev-parse --short -q HEAD)"
                    fi

                    local _git_state=''

                    # check if we're in the middle of merging/rebasing/cherry-picking
                    local _git_dir; _git_dir="$($_PP_timeout 0.1 "$_gitexe" rev-parse --git-dir 2>/dev/null)"; local _rs=$?
                    if [[ $_rs == 124 ]]; then
                        _git_state+=' gitdir_timedout'
                    else
                        if [[ -f "${_git_dir}/MERGE_HEAD" ]]; then
                            _git_state+=' MERGING '
                        fi
                        if [[ -d "${_git_dir}/rebase-apply" || -d "${_git_dir}/rebase-merge" ]]; then
                            _git_state+=' REBASING '
                        fi
                        if [[ -f "${_git_dir}/CHERRY_PICK_HEAD" ]]; then
                            _git_state+=' CHERRY-PICKING '
                        fi

                        # check whether there are unpushed commits.
                        local _git_remote; _git_remote="$(git config --get "branch.${_git_branch}.remote" 2>/dev/null)"
                        if [[ -n "$_git_remote" ]]; then
                            local _remote_branch; _remote_branch="$(git config --get "branch.${_git_branch}.merge")"
                            echo "$_remote_branch" | grep -q '^refs/heads/' || echo "ohnoes:${_remote_branch}"
                            _remote_branch="${_remote_branch#refs/heads/}"
                            local _remote_branch_ref; _remote_branch_ref="refs/remotes/${_git_remote}/${_remote_branch}"
                            local _num_unpushed_comms; _num_unpushed_comms="$(git rev-list --no-merges --count "${_remote_branch_ref}..HEAD" 2>/dev/null)"
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
                    _git="${_PP_YEL} ${_git_branch}${_git_state} "
                fi
            fi
        fi
    fi
    fi

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
