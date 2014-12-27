use strict;
use warnings;

use Test::More 0.88; # for done_testing
use Test::Differences;
use SQL::Interpol ':all';

my $x = 5;
my $y = 6;
my $v0 = [];
my $v = ['one', 'two'];
my $v2 = ['one', sql('two')];
my $h0 = {};

my $h = {one => 1, two => 2};

#== trivial cases
interp_test([],
            [''],
            'empty');
interp_test(['SELECT * FROM mytable'],
            ['SELECT * FROM mytable'],
            'string');
interp_test([\$x],
            [' ?', $x],
            'scalarref');

# test with sql()
interp_test([sql()],
            [''],
            'sql()');
interp_test([sql('test')],
            ['test'],
            'sql(string)');
interp_test([sql(\$x)],
            [' ?', $x],
            'sql(scalarref)');
interp_test([sql(sql(\$x))],
            [' ?', $x],
            'sql(sql(scalarref))');
interp_test([sql(sql(),sql())],
            [''],
            'sql(sql(),sql())');

#== INSERT
interp_test(['INSERT INTO mytable', \$x],
            ['INSERT INTO mytable VALUES(?)', $x],
            'INSERT scalarref');
interp_test(['REPLACE INTO mytable', \$x],
            ['REPLACE INTO mytable VALUES(?)', $x],
            'REPLACE INTO');
interp_test(['INSERT INTO mytable', sql($x)],
            ["INSERT INTO mytable $x"], # invalid
            'INSERT sql(...)');
# OK in mysql
interp_test(['INSERT INTO mytable', $v0],
            ['INSERT INTO mytable VALUES()'],
            'INSERT arrayref of size = 0');
interp_test(['INSERT INTO mytable', $v],
            ['INSERT INTO mytable VALUES(?, ?)', @$v],
            'INSERT arrayref of size > 0');
interp_test(['INSERT INTO mytable', $v2],
            ['INSERT INTO mytable VALUES(?, two)', 'one'],
            'INSERT arrayref of size > 0 with sql()');
interp_test(['INSERT INTO mytable', [1, sql(\$x, '*', \$x)]],
            ['INSERT INTO mytable VALUES(?,  ? * ?)', 1, $x, $x],
            'INSERT arrayref of size > 0 with sql()');
# OK in mysql
interp_test(['INSERT INTO mytable', $h0],
            ['INSERT INTO mytable () VALUES()'],
            'INSERT hashref of size = 0');
interp_test(['INSERT INTO mytable', $h],
            ['INSERT INTO mytable (one, two) VALUES(?, ?)', 1, 2],
            'INSERT hashref of size > 0');
interp_test(['INSERT INTO mytable', {one => 1, two => sql(\$x, '*', \$x)}],
            ['INSERT INTO mytable (one, two) VALUES(?,  ? * ?)', 1, $x, $x],
            'INSERT hashref with sql()');
# mysql
interp_test(['INSERT HIGH_PRIORITY IGNORE INTO mytable', $v],
            ['INSERT HIGH_PRIORITY IGNORE INTO mytable VALUES(?, ?)', @$v],
            'INSERT [mod] arrayref of size > 0');

# IN
# note: 'WHERE field in ()' NOT OK in mysql.
interp_test(['WHERE field IN', \$x],
            ['WHERE field IN (?)', $x],
            'IN scalarref');

my $maybe_array = [1,2];
interp_test(['WHERE field IN', \$maybe_array],
            ['WHERE field IN (?, ?)', @$maybe_array],
            'IN maybe_array turns out to be an array');

interp_test(['WHERE field IN', sql($x)],
            ["WHERE field IN $x"], # invalid
            'IN sql()');
interp_test(['WHERE field IN', $v0],
            ['WHERE 1=0'],
            'IN arrayref of size = 0');

interp_test(['WHERE field NOT IN', $v0],
            ['WHERE 1=1'],
            'NOT IN arrayref of size = 0');


interp_test(['WHERE field IN', $v],
            ['WHERE field IN (?, ?)', @$v],
            'IN arrayref of size > 0');
interp_test(['WHERE field IN', $v2],
            ['WHERE field IN (?, two)', 'one'],
            'IN arrayref with sql()');
interp_test(['WHERE field IN', [1, sql(\$x, '*', \$x)]],
            ['WHERE field IN (?,  ? * ?)', 1, $x, $x],
            'IN arrayref with sql()');
interp_test(['WHERE', {field => $v}],
            ['WHERE field IN (?, ?)', 'one', 'two'],
            'hashref with arrayref');
interp_test(['WHERE', {field => $v0}],
            ['WHERE 1=0'],
            'hashref with arrayref of size = 0');
interp_test(['WHERE', {field => [1, sql(\$x, '*', \$x)]}],
            ['WHERE field IN (?,  ? * ?)', 1, $x, $x],
            'hashref with arrayref with sql()');
interp_test(['WHERE field in', $v0],
            ['WHERE 1=0'],
            'IN lowercase');  # fails in 0.31

# SET
interp_test(['UPDATE mytable SET', $h],
            ['UPDATE mytable SET one=?, two=?', 1, 2],
            'SET hashref');
interp_test(['UPDATE mytable SET',
                {one => 1, two => $x, three => sql('3')}],
            ['UPDATE mytable SET one=?, three=3, two=?',
                1, $x],
            'SET hashref of sql_type types, sql()');
#FIX--what if size of hash is zero? error?

# WHERE hashref
interp_test(['WHERE', $h0],
            ['WHERE 1=1'],
            'WHERE hashref of size = 0');
interp_test(['WHERE', $h],
            ['WHERE (one=? AND two=?)', 1, 2],
            'WHERE hashref of size > 0');
interp_test(['WHERE', {x => 1, y => sql('2')}],
            ['WHERE (x=? AND y=2)', 1],
            'WHERE hashref sql()');
interp_test(['WHERE', {x => 1, y => undef}],
            ['WHERE (x=? AND y IS NULL)', 1],
            'WHERE hashref of NULL');

# WHERE x=
interp_test(['WHERE x=', \$x],
            ['WHERE x= ?', $x],
            'WHERE x=scalarref');

# table references
error_test(['FROM', []], qr/table reference has zero rows/, 'v 0');
error_test(['FROM', [[]]], qr/table reference has zero columns/, 'vv 1 0');
error_test(['',     [[]]], qr/table reference has zero columns/, 'vv 1 0 (resultset)');
error_test(['FROM', [{}]], qr/table reference has zero columns/, 'vh 1 0');
error_test(['',     [{}]], qr/table reference has zero columns/, 'vh 1 0 (resultset)');
interp_test(['FROM', [[1]]], ['FROM (SELECT ?) AS tbl0', 1], 'vv 1 1');
interp_test(['',     [[1]]], ['(SELECT ?)', 1], 'vv 1 1 (resultset)');
interp_test(['FROM', [{a => 1}]], ['FROM (SELECT ? AS a) AS tbl0', 1], 'vh 1 1');
interp_test(['',     [{a => 1}]], ['(SELECT ? AS a)', 1], 'vh 1 1 (resultset)');
interp_test(['FROM', [[1,2]]], ['FROM (SELECT ?, ?) AS tbl0', 1, 2], 'vv 1 2');
interp_test(['FROM', [$h]], ['FROM (SELECT ? AS one, ? AS two) AS tbl0', 1, 2], 'vh 1 2');
interp_test(['',     [$h]], ['(SELECT ? AS one, ? AS two)', 1, 2], 'vh 1 2 (resultset)');
interp_test(['FROM', [[1,2],[3,4]]],
    ['FROM (SELECT ?, ? UNION ALL SELECT ?, ?) AS tbl0', 1, 2, 3, 4], 'vv 2 2');
interp_test(['', [[1,2],[3,4]]],
    ['(SELECT ?, ? UNION ALL SELECT ?, ?)', 1, 2, 3, 4], 'vv 2 2 (resultset)');
interp_test(['FROM', [$h,$h]],
    ['FROM (SELECT ? AS one, ? AS two UNION ALL SELECT ?, ?) AS tbl0',
    1, 2, 1, 2], 'vh 2 2');
interp_test(['', [$h,$h]],
    ['(SELECT ? AS one, ? AS two UNION ALL SELECT ?, ?)',
    1, 2, 1, 2], 'vh 2 2 (resultset)');
interp_test(['FROM', [[1]], 'JOIN', [[2]]],
    ['FROM (SELECT ?) AS tbl0 JOIN (SELECT ?) AS tbl1', 1, 2], 'vv 1 1 join vv 1 1');
interp_test(['FROM', [[sql(1)]]], ['FROM (SELECT 1) AS tbl0'], 'vv 1 1 of sql(1)');
interp_test(['', [[sql(1)]]], ['(SELECT 1)'], 'vv 1 1 of sql(1) (resultset)');
interp_test(['FROM', [{a => sql(1)}]], ['FROM (SELECT 1 AS a) AS tbl0'], 'vh 1 1 of sql(1)');
interp_test(['FROM', [[sql(\1)]]], ['FROM (SELECT  ?) AS tbl0', 1], 'vv 1 1 of sql(\1)');
interp_test(['FROM', [[sql('1=', \1)]]],
    ['FROM (SELECT 1= ?) AS tbl0', 1], 'vv 1 1 of sql(s,\1)');
interp_test(['FROM', [[1]], ' AS mytable'],
    ['FROM (SELECT ?) AS mytable', 1], 'vv 1 1 with alias');
interp_test(['FROM', [[undef]]],
    ['FROM (SELECT ?) AS tbl0', undef], 'vv 1 1 of undef');
interp_test(['FROM', [{a => undef}]],
    ['FROM (SELECT ? AS a) AS tbl0', undef], 'vh 1 1 of undef');

done_testing;

sub interp_test {
    my ( $snips, $expect, $name ) = @_;
    local $Test::Builder::Level = $Test::Builder::Level + 1;
    eq_or_diff [ sql_interp @$snips ], $expect, $name;
}

sub error_test {
    my ( $list, $re, $name ) = @_;
    local $Test::Builder::Level = $Test::Builder::Level + 1;
    local $@;
    eval { sql_interp @$list };
    like $@, $re, $name;
}
