# Tests of DBIx::Interpolate

use strict;
use lib 't/lib';
use DBD::Mock;
use Test::More 'no_plan';
use Data::Dumper;
use DBIx::Interpolate qw(:all);
use DBI qw(:sql_types);

my $dbh = DBI->connect('DBI:Mock:', '', '')
    or die "Cannot create handle: $DBI::errstr\n";
my $dbx = new DBIx::Interpolate($dbh);

my @data1   = (['a', 'b'], ['c', 'd']);
my @result1 = (['color', 'size'], @data1);

my $x = 5;
my $y = 6;

# selectall_arrayref
$dbh->{mock_add_resultset} = \@result1;
is_deeply(
    $dbx->selectall_arrayref("SELECT * FROM mytable WHERE x IN", [1,2]),
    \@data1,
    'selectall_arrayref'
);
is($dbh->{mock_all_history}->[0]{statement},
   'SELECT * FROM mytable WHERE x IN (?, ?)');
is_deeply($dbh->{mock_all_history}->[0]{bound_params}, [1, 2]);

# prepare
my $stx = $dbx->prepare();
is(ref($stx), 'DBIx::Interpolate::STX');

# max_sths
$stx->max_sths(2);
is($stx->max_sths(), 2);

# execute
$dbh->{mock_clear_history} = 1;
$dbh->{mock_add_resultset} = \@result1;
$stx->execute('SELECT * FROM mytable WHERE y IN', [2,3]);
is_deeply(
    $stx->fetchall_arrayref(),
    \@data1,
    'fetchall_arrayref'
);
is($dbh->{mock_all_history}->[0]{statement},
   'SELECT * FROM mytable WHERE y IN (?, ?)');
is_deeply($dbh->{mock_all_history}->[0]{bound_params}, [2, 3]);

# execute (same SQL)
$dbh->{mock_clear_history} = 1;
$dbh->{mock_add_resultset} = \@result1;
$stx->execute('SELECT * FROM mytable WHERE y IN', [4,5]);
is_deeply(
    $stx->fetchall_arrayref(),
    \@data1,
    'fetchall_arrayref'
);
is($stx->sth()->{mock_statement},
   'SELECT * FROM mytable WHERE y IN (?, ?)');
is_deeply($stx->sth()->{mock_params}, [4, 5]);

# execute (new SQL)
$dbh->{mock_clear_history} = 1;
$dbh->{mock_add_resultset} = \@result1;
$stx->execute('SELECT * FROM mytable WHERE y IN', [4,5,6]);
is_deeply(
    $stx->fetchall_arrayref(),
    \@data1,
    'fetchall_arrayref'
);
is($stx->sth()->{mock_statement},
   'SELECT * FROM mytable WHERE y IN (?, ?, ?)');
is_deeply($stx->sth()->{mock_params}, [4, 5, 6]);

is(scalar(keys %{$stx->sths()}), 2, 'two sths in stx');

# execute (new SQL)
$dbh->{mock_clear_history} = 1;
$dbh->{mock_add_resultset} = \@result1;
$stx->execute('SELECT * FROM mytable WHERE y IN', [4,5,6,7]);
is_deeply(
    $stx->fetchall_arrayref(),
    \@data1,
    'fetchall_arrayref'
);
is($stx->sth()->{mock_statement},
   'SELECT * FROM mytable WHERE y IN (?, ?, ?, ?)');
is_deeply($stx->sth()->{mock_params}, [4, 5, 6, 7]);

is(scalar(keys %{$stx->sths()}), 2, 'two sths in stx still');


# bind_param
$dbh->{mock_clear_history} = 1;
$dbh->{mock_add_resultset} = \@result1;
$dbx->selectall_arrayref("SELECT * FROM mytable WHERE x=", \$x,
    "AND y=", sql_var(\$y, type => SQL_INTEGER),
    "AND", sql_var({a => 1, b => 2}, type => SQL_DATETIME),
    "AND x IN", sql_var([4, 5], type => SQL_VARCHAR)
);
is($dbh->{mock_all_history}->[0]{statement},
    'SELECT * FROM mytable WHERE x= ? AND y= ? AND (a=? AND b=?) AND x IN (?, ?)'
);
# note: DBD::Mock doesn't save bind param type to test?

