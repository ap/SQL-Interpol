use strict;
use Test::More 'no_plan';
use Data::Dumper;
use SQL::Interpolate qw(:all);
use SQL::Interpolate::Macro qw(:all);

my $interp = new SQL::Interpolate;
my $sql_interp = $interp->make_sql_interp();
my $sql_interp2 = make_sql_interp();

my $x = 5;
my $y = 6;
my $v0 = [];
my $v = ['one', 'two'];
my $v2 = ['one', sql_literal('two')];
my $h0 = {};
my $h = {one => 1, two => 2};
my $h2 = {one => 1, two => sql_literal('three')};
my $var1 = sql_var(\$x);
my $var2 = sql_var(\$x, type => 1);

#== INSERT
interp_test(['INSERT INTO mytable', \$x],
            ['INSERT INTO mytable VALUES(?)', $x],
            'INSERT scalarref');
interp_test(['INSERT INTO mytable', sql_literal($x)],
            ["INSERT INTO mytable $x"], # invalid
            'INSERT sql_literal');
# OK in mysql
interp_test(['INSERT INTO mytable', $v0],
            ['INSERT INTO mytable VALUES()'],
            'INSERT arrayref of size = 0');
interp_test(['INSERT INTO mytable', $v],
            ['INSERT INTO mytable VALUES(?, ?)', @$v],
            'INSERT arrayref of size > 0');
interp_test(['INSERT INTO mytable', $v2],
            ['INSERT INTO mytable VALUES(?, two)', 'one'],
            'INSERT arrayref of size > 0 with sql_literal');
interp_test(['INSERT INTO mytable', [1, sql_fragment(\$x, '*', \$x)]],
            ['INSERT INTO mytable VALUES(?,  ? * ?)', 1, $x, $x],
            'INSERT arrayref of size > 0 with macro');
# OK in mysql
interp_test(['INSERT INTO mytable', $h0],
            ['INSERT INTO mytable () VALUES()'],
            'INSERT hashref of size = 0');
interp_test(['INSERT INTO mytable', $h],
            ['INSERT INTO mytable (one, two) VALUES(?, ?)', 1, 2],
            'INSERT hashref of size > 0');
interp_test(['INSERT INTO mytable', $h2],
            ['INSERT INTO mytable (one, two) VALUES(?, three)', 1],
            'INSERT hashref of size > 0 with sql_literal');
interp_test(['INSERT INTO mytable',
                {one => 1, two => $var2, three => sql_literal('3')}],
            ['INSERT INTO mytable (three, one, two) VALUES(3, ?,  ?)',
                [1, sql_var(\1)], [${$var2->{value}}, $var2]],
            'INSERT hashref of sql_var types, sql_literal');
interp_test(['INSERT INTO mytable', {one => 1, two => sql_fragment(\$x, '*', \$x)}],
            ['INSERT INTO mytable (one, two) VALUES(?,  ? * ?)', 1, $x, $x],
            'INSERT hashref with macro');
# mysql
interp_test(['INSERT HIGH_PRIORITY IGNORE INTO mytable', $v],
            ['INSERT HIGH_PRIORITY IGNORE INTO mytable VALUES(?, ?)', @$v],
            'INSERT [mod] arrayref of size > 0');

# IN
# note: 'WHERE field in ()' NOT OK in mysql.
interp_test(['WHERE field IN', \$x],
            ['WHERE field IN (?)', $x],
            'IN scalarref');
interp_test(['WHERE field IN', sql_literal($x)],
            ["WHERE field IN $x"], # invalid
            'IN sql_literal');
interp_test(['WHERE field IN', $v0],
            ['WHERE 1'],
            'IN arrayref of size = 0');
interp_test(['WHERE field IN', $v],
            ['WHERE field IN (?, ?)', @$v],
            'IN arrayref of size > 0');
interp_test(['WHERE field IN', $v2],
            ['WHERE field IN (?, two)', 'one'],
            'IN arrayref with sql_literal');
interp_test(['WHERE field IN', [1, sql_fragment(\$x, '*', \$x)]],
            ['WHERE field IN (?,  ? * ?)', 1, $x, $x],
            'IN arrayref with macro');


# SET
interp_test(['UPDATE mytable SET', $h],
            ['UPDATE mytable SET one=?, two=?', 1, 2],
            'SET hashref');
interp_test(['UPDATE mytable SET',
                {one => 1, two => $var2, three => sql_literal('3')}],
            ['UPDATE mytable SET three=3, one=?, two= ?',
                [1, sql_var(\1)], [${$var2->{value}}, $var2]],
            'SET hashref of sql_var types, sql_literal');
#FIX--what if size of hash is zero? error?

# WHERE hashref
interp_test(['WHERE', $h0],
            ['WHERE 1'],
            'WHERE hashref of size = 0');
interp_test(['WHERE', $h],
            ['WHERE (one=? AND two=?)', 1, 2],
            'WHERE hashref of size > 0');
interp_test(['WHERE', {x => 1, y=>sql_literal('2')}],
            ['WHERE (y=2 AND x=?)', 1],
            'WHERE hashref sql_literal');
interp_test(['WHERE', \$x],
            ['WHERE ?', $x],
            'WHERE scalarref');

# WHERE x=
interp_test(['WHERE x=', \$x],
            ['WHERE x= ?', $x],
            'WHERE x=scalarref');

# sql_var
interp_test(['WHERE x=', \$x, 'AND', 'y=', sql_var(\$y)],
            ['WHERE x= ? AND y= ?', $x, $y],
            'WHERE \$x, sql_var');
interp_test(['WHERE x=', \$x, 'AND', 'y=', $var2],
            ['WHERE x= ? AND y= ?', [$x, sql_var(\$x)], [${$var2->{value}}, $var2]],
            'WHERE \$x, sql_var typed');
interp_test(['WHERE', {x => $x, y => $var2}, 'AND z=', \$x],
            ['WHERE (y= ? AND x=?) AND z= ?',
                [${$var2->{value}}, $var2], [$x, sql_var(\$x)], [$x, sql_var(\$x)]],
            'WHERE hashref of \$x, sql_var typed');
interp_test(['WHERE', {x => $x, y => sql_literal('z')}],
            ['WHERE (y=z AND x=?)', $x],
            'WHERE hashref of \$x, sql_literal');

sub interp_test
{
    my($snips, $expect, $name) = @_;
#    print Dumper([sql_interp @$snips], $expect);
    is_deeply([sql_interp @$snips], $expect, $name);
    is_deeply([$interp->sql_interp(@$snips)], $expect, "$name OO");
    is_deeply([$sql_interp->(@$snips)], $expect, "$name closure");
    is_deeply([$sql_interp2->(@$snips)], $expect, "$name closure2");
}

