# dbi.pl
# simple tests of DBIx::Interpolate.

use strict;
use Data::Dumper;
use DBIx::Interpolate;
use DBI;

unlink('test.sqlt');
my $dbx = DBIx::Interpolate->connect(
    "dbi:SQLite:dbname=test.sqlt", "", "",
    {RaiseError => 1, AutoCommit => 1}
);

$dbx->do("CREATE TABLE mytable(one INTEGER PRIMARY KEY, two INTEGER)");
for(my $n=0; $n<10; $n++) {
   $dbx->do("INSERT INTO mytable", {one => $n, two => $n+1});
}

my $rows = $dbx->selectall_arrayref(qq[
    SELECT * FROM mytable WHERE one > ], \3
);
print Dumper($rows);

# list context
my @rows = $dbx->selectrow_array(qq[
    SELECT * FROM mytable WHERE one = ], \3
);
print Dumper(\@rows);

# scalar context
my $rows = $dbx->selectrow_array(qq[
    SELECT one FROM mytable WHERE one = ], \3
);
print Dumper($rows);
