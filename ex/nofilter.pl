# one.pl
# simple tests of SQL::Interpolate.

use strict;
use Data::Dumper;
use SQL::Interpolate qw(:all);

my @colors = ('blue', 'green');
my $x = 5;
my($sql, @bind) = sql_interp qq[
    SELECT * FROM table
    WHERE color IN], \@colors, qq[
          AND y = ], \$x,
    &limit(start => 1, count => 10)
;
print "$sql\n" . Dumper(\@bind);

my @colors = ();
my($sql, @bind) = sql_interp qq[
    SELECT * FROM table
    WHERE color IN], \@colors, qq[
          AND y = ], \$x,
    &limit(start => 1, count => 10)
;
print "$sql\n" . Dumper(\@bind);

my $new_color = 'red';
my $new_shape = 'square';
my($sql, @bind) = sql_interp qq[
    INSERT INTO table ], {
        color => $new_color,
        shape => $new_shape}
;
print "$sql\n" . Dumper(\@bind);

my $color = 'yellow';
my($sql, @bind) = sql_interp qq[
    UPDATE table SET ], {
        color => $new_color,
        shape => $new_shape}, qq[
    WHERE color <> ], \$color
;
print "$sql\n" . Dumper(\@bind);

