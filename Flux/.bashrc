# OTHER USEFUL FLUX COMMANDS:
# - `mdiag -a <acct>`
# - `mdiag -u $USER`
# - `qdel $(qselect -u $USER)`
# - `checkjob <jobid>`
# - `mjobctl -q diag <jobid>`
# google for keywords like MAXIJOB.

_reverse_all_but_first_n_lines() {
    perl -ne '$lines .= $_; if ($. >= '$1') { print $lines; print reverse <> }'
}

_only_print_if_gt_n_lines() {
    perl -ne '$lines .= $_; if ($. > '$1') { print $lines; print <> }'
}

_showq_with_arg() {
    # -n -v: show more job id info
    showq -n -v -u $USER $* |
    tail -n +3 | head -n -5 |
    perl -ple 's/[A-Z][a-z]{2} ([A-Z][a-z]{2}) +([0-9]+) ([:0-9]{8})/$1$2_$3/' |
    perl -ple 's{^([0-9]+)/\g1\.nyx\.arc-ts\.umich\.edu/}{$1 <clip>/}' |
    perl -ple 's{^JOBID }{JOBID SCRIPT }' |
    column -t
}

show_my_jobs() {
    clear
    (
        showq -u $USER -s
        echo
        (echo  ---- RUNNING;  _showq_with_arg -r -o STARTTIME ; echo) | _only_print_if_gt_n_lines 3
        (echo  ---- IDLE;     _showq_with_arg -i | _reverse_all_but_first_n_lines 1; echo) | _only_print_if_gt_n_lines 3
        (echo  ---- BLOCKED;  _showq_with_arg -b | _reverse_all_but_first_n_lines 1; echo) | _only_print_if_gt_n_lines 3
        (echo  ---- COMPLETE; _showq_with_arg -c | _reverse_all_but_first_n_lines 1; echo) | _only_print_if_gt_n_lines 3
    ) |
    less -XF
}

mangrep_flux() {
    # paths are from `man -w | tr : "\n"`
    find /usr/local/torque/man/ /opt/moab/man/ -type f |
    while read fn; do zgrep -iH "$1" $fn; done |
    grep -E --color=always "$1|$"
}
