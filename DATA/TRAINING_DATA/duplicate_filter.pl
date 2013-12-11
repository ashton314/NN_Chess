#!/usr/bin/env perl
use strict;
use warnings;

open my $fh, '<', 'data_set00.scm'
  or die "Could not open file handle: $!\n";
open my $out, '>', 'data_set01.scm'
  or die "Could not open file handle: $!\n";

my %lines = ();
while (my $line = <$fh>) {
    if (exists $lines{$line}) {
	next;
    }
    else {
	print $out $line;
	$lines{$line} = 1;
    }
}
close $fh;
close $out;
exit;
