use strict;
use warnings;

use Test::More 0.88; # for done_testing
use Test::Differences;
use SQL::Interpol ':all';

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

can_ok __PACKAGE__, qw( sql_interp sql );

#== trivial cases
interp_test([],
            [''],
            'empty');
interp_test(['SELECT * FROM mytable'],
            ['SELECT * FROM mytable'],
            'string');
interp_test([\5],
            ['?', 5],
            'scalarref');

# test with sql()
interp_test([sql()],
            [''],
            'sql()');
interp_test([sql('test')],
            ['test'],
            'sql(string)');
interp_test([sql(\5)],
            ['?', 5],
            'sql(scalarref)');
interp_test([sql(sql(\5))],
            ['?', 5],
            'sql(sql(scalarref))');
interp_test([sql(sql(),sql())],
            [''],
            'sql(sql(),sql())');

#== INSERT
interp_test(['INSERT INTO mytable', \5],
            ['INSERT INTO mytable VALUES(?)', 5],
            'INSERT scalarref');
interp_test(['REPLACE INTO mytable', \5],
            ['REPLACE INTO mytable VALUES(?)', 5],
            'REPLACE INTO');
interp_test(['INSERT INTO mytable', sql(5)],
            ["INSERT INTO mytable 5"], # invalid
            'INSERT sql(...)');
# OK in mysql
interp_test(['INSERT INTO mytable', []],
            ['INSERT INTO mytable VALUES()'],
            'INSERT arrayref of size = 0');
interp_test(['INSERT INTO mytable', [ 'one', 'two' ]],
            ['INSERT INTO mytable VALUES(?, ?)', 'one', 'two'],
            'INSERT arrayref of size > 0');
interp_test(['INSERT INTO mytable', [ 'one', sql('two') ]],
            ['INSERT INTO mytable VALUES(?, two)', 'one'],
            'INSERT arrayref of size > 0 with sql()');
interp_test(['INSERT INTO mytable', [1, sql(\5, '*', \5)]],
            ['INSERT INTO mytable VALUES(?, ? * ?)', 1, 5, 5],
            'INSERT arrayref of size > 0 with sql()');
# OK in mysql
interp_test(['INSERT INTO mytable', {}],
            ['INSERT INTO mytable () VALUES()'],
            'INSERT hashref of size = 0');
interp_test(['INSERT INTO mytable', { one => 1, two => 2 }],
            ['INSERT INTO mytable (one, two) VALUES(?, ?)', 1, 2],
            'INSERT hashref of size > 0');
interp_test(['INSERT INTO mytable', {one => 1, two => sql(\5, '*', \5)}],
            ['INSERT INTO mytable (one, two) VALUES(?, ? * ?)', 1, 5, 5],
            'INSERT hashref with sql()');
# mysql
interp_test(['INSERT HIGH_PRIORITY IGNORE INTO mytable', [ 'one', 'two' ]],
            ['INSERT HIGH_PRIORITY IGNORE INTO mytable VALUES(?, ?)', 'one', 'two'],
            'INSERT [mod] arrayref of size > 0');

# IN
# note: 'WHERE field in ()' NOT OK in mysql.
interp_test(['WHERE field IN', \5],
            ['WHERE field IN (?)', 5],
            'IN scalarref');

interp_test(['WHERE field IN', \[1,2]],
            ['WHERE field IN (?, ?)', 1, 2],
            'IN maybe_array turns out to be an array');

interp_test(['WHERE field IN', sql(5)],
            ["WHERE field IN 5"], # invalid
            'IN sql()');
interp_test(['WHERE field IN', []],
            ['WHERE 1=0'],
            'IN arrayref of size = 0');

interp_test(['WHERE field NOT IN', []],
            ['WHERE 1=1'],
            'NOT IN arrayref of size = 0');


interp_test(['WHERE field IN', [ 'one', 'two' ]],
            ['WHERE field IN (?, ?)', 'one', 'two'],
            'IN arrayref of size > 0');
interp_test(['WHERE field IN', [ 'one', sql('two') ]],
            ['WHERE field IN (?, two)', 'one'],
            'IN arrayref with sql()');
interp_test(['WHERE field IN', [1, sql(\5, '*', \5)]],
            ['WHERE field IN (?, ? * ?)', 1, 5, 5],
            'IN arrayref with sql()');
interp_test(['WHERE', {field => [ 'one', 'two' ]}],
            ['WHERE field IN (?, ?)', 'one', 'two'],
            'hashref with arrayref');
interp_test(['WHERE', {field => []}],
            ['WHERE 1=0'],
            'hashref with arrayref of size = 0');
interp_test(['WHERE', {field => [1, sql(\5, '*', \5)]}],
            ['WHERE field IN (?, ? * ?)', 1, 5, 5],
            'hashref with arrayref with sql()');
interp_test(['WHERE field in', []],
            ['WHERE 1=0'],
            'IN lowercase');  # fails in 0.31

# SET
interp_test(['UPDATE mytable SET', { one => 1, two => 2 }],
            ['UPDATE mytable SET one=?, two=?', 1, 2],
            'SET hashref');
interp_test(['UPDATE mytable SET',
                {one => 1, two => 5, three => sql('3')}],
            ['UPDATE mytable SET one=?, three=3, two=?',
                1, 5],
            'SET hashref of sql_type types, sql()');
#FIX--what if size of hash is zero? error?

# WHERE hashref
interp_test(['WHERE', {}],
            ['WHERE 1=1'],
            'WHERE hashref of size = 0');
interp_test(['WHERE', { one => 1, two => 2 }],
            ['WHERE (one=? AND two=?)', 1, 2],
            'WHERE hashref of size > 0');
interp_test(['WHERE', {x => 1, y => sql('2')}],
            ['WHERE (x=? AND y=2)', 1],
            'WHERE hashref sql()');
interp_test(['WHERE', {x => 1, y => undef}],
            ['WHERE (x=? AND y IS NULL)', 1],
            'WHERE hashref of NULL');

# WHERE x=
interp_test(['WHERE x=', \5],
            ['WHERE x= ?', 5],
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
interp_test(['FROM', [{ one => 1, two => 2 }]], ['FROM (SELECT ? AS one, ? AS two) AS tbl0', 1, 2], 'vh 1 2');
interp_test(['',     [{ one => 1, two => 2 }]], ['(SELECT ? AS one, ? AS two)', 1, 2], 'vh 1 2 (resultset)');
interp_test(['FROM', [[1,2],[3,4]]],
    ['FROM (SELECT ?, ? UNION ALL SELECT ?, ?) AS tbl0', 1, 2, 3, 4], 'vv 2 2');
interp_test(['', [[1,2],[3,4]]],
    ['(SELECT ?, ? UNION ALL SELECT ?, ?)', 1, 2, 3, 4], 'vv 2 2 (resultset)');
interp_test(['FROM', [({ one => 1, two => 2 }) x 2]],
    ['FROM (SELECT ? AS one, ? AS two UNION ALL SELECT ?, ?) AS tbl0',
    1, 2, 1, 2], 'vh 2 2');
interp_test(['', [({ one => 1, two => 2 }) x 2]],
    ['(SELECT ? AS one, ? AS two UNION ALL SELECT ?, ?)',
    1, 2, 1, 2], 'vh 2 2 (resultset)');
interp_test(['FROM', [[1]], 'JOIN', [[2]]],
    ['FROM (SELECT ?) AS tbl0 JOIN (SELECT ?) AS tbl1', 1, 2], 'vv 1 1 join vv 1 1');
interp_test(['FROM', [[sql(1)]]], ['FROM (SELECT 1) AS tbl0'], 'vv 1 1 of sql(1)');
interp_test(['', [[sql(1)]]], ['(SELECT 1)'], 'vv 1 1 of sql(1) (resultset)');
interp_test(['FROM', [{a => sql(1)}]], ['FROM (SELECT 1 AS a) AS tbl0'], 'vh 1 1 of sql(1)');
interp_test(['FROM', [[sql(\1)]]], ['FROM (SELECT ?) AS tbl0', 1], 'vv 1 1 of sql(\1)');
interp_test(['FROM', [[sql('1=', \1)]]],
    ['FROM (SELECT 1= ?) AS tbl0', 1], 'vv 1 1 of sql(s,\1)');
interp_test(['FROM', [[1]], ' AS mytable'],
    ['FROM (SELECT ?) AS mytable', 1], 'vv 1 1 with alias');
interp_test(['FROM', [[undef]]],
    ['FROM (SELECT ?) AS tbl0', undef], 'vv 1 1 of undef');
interp_test(['FROM', [{a => undef}]],
    ['FROM (SELECT ? AS a) AS tbl0', undef], 'vh 1 1 of undef');

done_testing;
