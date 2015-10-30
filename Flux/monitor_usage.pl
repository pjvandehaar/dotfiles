#!/usr/bin/env perl

use warnings; use strict;
use v5.10;
use List::Util qw{max};
use POSIX qw{strftime};

my %cpu = (); # minutes
my %walltime = (); # minutes
my %mem = (); # MB

my @jobs;

sub date { return strftime "%F %T", localtime; }

do {
    @jobs = split '\n', `qselect -s R -u pjvh`;
    die "bad return status: $?" if $? != 0;
    s/^\s+|\s+\z//g for @jobs;
    @jobs = grep {m/[0-9]/} @jobs;

    for my $psj (keys %mem) { # psj = previously seen job
        if (not grep {$psj eq $_} @jobs) {
            printf STDOUT "%s %30s: %30s   %5d min(cpu)   %5d min(walltime)   %5d MB   (%d others active)\n",
            date, 'fell out of qselect -sR', $psj, $cpu{$psj}//-1, $cpu{$psj}//-1, $walltime{$psj}//-1, scalar @jobs;
            delete $mem{$psj};
            delete $cpu{$psj};
            delete $walltime{$psj};
        }
    }

    for my $job (@jobs) {

        # if it's new, just set $mem{$job} = -1, etc, and skip it.
        # qstat caches, so a job will often not show up in `qstat` until maybe a minute after it's in `qselect`.
        if (not exists $mem{$job}) {
            printf STDERR "%s   %30s: %30s\n",
            date, 'new job', $job;
            $mem{$job} = -1;
            $cpu{$job} = -1;
            $walltime{$job} = -1;
            next;
        }

        my @lines = split '\n', `qstat -f -1 $job 2>> qstat.err | egrep 'resources_used\.(cput|mem|walltime) ='`;
        if ($? != 0) {
            printf STDERR "%s   %30s: %30s   %5d min(cpu)   %5d min(walltime)   %5d MB   (%d others active)\n",
            date, 'failed qstat', $job, $cpu{$job}//-1, $cpu{$job}//-1, $walltime{$job}//-1, scalar @jobs;
            next;
        }

        for my $line (@lines) {
            if ($line =~ m/cput/) {
                die "missing cpu line in [@lines]" if not $line =~ m/([0-9]{2}):([0-9]{2}):([0-9]{2})/;
                die "cpu must be nonnegative [$1]" if $1*60+$2 < 0;
                $cpu{$job} = $1 * 60 + $2;
            } elsif ($line =~ m/walltime/) {
                die "missing walltime line in [@lines]" if not $line =~ m/([0-9]{2}):([0-9]{2}):([0-9]{2})/;
                die "walltime must be nonnegative [$1]" if $1*60+$2 < 0;
                $walltime{$job} = $1 * 60 + $2;
            } elsif ($line =~ m/mem/) {
                die "missing mem line in [@lines]" if not $line =~ m/([0-9]+)kb/;
                die "mem must be nonnegative [$1]" if $1 < 0;
                $mem{$job} = max($mem{$job} // 0, $1 / 1000);
            }
        }
    }
} while (scalar @jobs and sleep 120); # qstat updates every minute I think.
