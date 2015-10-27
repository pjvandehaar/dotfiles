only_print_if_gte_n_lines() {
    perl -e '$header = ""; $header .= <STDIN> foreach (1..@ARGV[0]); $printed_header=0; while ($line = <STDIN>) {print $header if ! ($printed_header++); print $line}' $1
}

showq_with_arg() {
    showq -n -v -u $USER $1 |
    tail -n +3 | head -n -5 |
    perl -ple 's/[A-Z][a-z]{2} ([A-Z][a-z]{2}) ([0-9]+) ([:0-9]{8})/$1$2_$3/' |
    column -t
}

show_my_jobs() {
    (echo  ---- BLOCKED; showq_with_arg -b; echo) | only_print_if_gte_n_lines 3
    (echo ---- IDLE; showq_with_arg -i; echo) | only_print_if_gte_n_lines 3
    (echo ---- RUNNING; showq_with_arg -r; echo) | only_print_if_gte_n_lines 3
    (echo ---- COMPLETE; showq_with_arg -c; echo) | only_print_if_gte_n_lines 3
}
