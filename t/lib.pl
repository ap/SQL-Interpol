# lib.pl
# Utility functions for the test suite.

use strict;
use SQL::Interpolate qw(:all);

our $fake_mysql_dbh =
    bless {Driver => {Name => 'mysql'}}, 'DBI::db';

# Return normalized string representation of SQL interpolation list.
sub sql_str
{
    my(@parts) = @_;

    @parts = sql_flatten @parts;

    my $out = Dumper(\@parts);

    # normalize
    $out =~ s/\s+/ /gs;
    $out =~ s/^.*?=\s*//;
    $out =~ s/;.*?$//;
    return $out;
}


1
