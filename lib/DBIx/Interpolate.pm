package DBIx::Interpolate;

our $VERSION = '0.31_01';

use strict;
use warnings;
use DBI;
use Carp;
use base 'Exporter';
use SQL::Interpolate qw(:all);

our @ISA = qw(SQL::Interpolate);
our @EXPORT;
our %EXPORT_TAGS = (all => [qw(
    attr
    dbi_interp
    key_field
    make_dbi_interp
)]);
our @EXPORT_OK = @{$EXPORT_TAGS{'all'}};

sub wrap(&);

sub import
{
    SQL::Interpolate->export_to_level(1, @_);
    __PACKAGE__->export_to_level(1, @_);
}

sub AUTOLOAD
{
    my($self, @args) = @_;
    (my $meth = our $AUTOLOAD) =~ s/.*:://;
    #print "[$meth]";
    return unless $meth =~ /[^A-Z]/; # e.g. DESTROY

    my $dbh = $self->{dbh};

    return wrap {
        return $dbh->$meth(@args);
    };
}

sub new
{
    my $class = shift;
    my $dbh;
    if(UNIVERSAL::isa($_[0], 'DBI:db')) {
        $dbh = shift;
    }
    elsif(ref($_[0]) eq 'ARRAY') {
        $dbh = DBI->connect(@{shift @_});
    }
    my $self = new SQL::Interpolate(($dbh ? $dbh : ()), @_);
    bless $self, $class;
    return $self;
}

sub connect
{
    my $class = shift;
    my $self;
    eval {
        my $dbh = DBI->connect(@_);
        return if !$dbh;
        $self = new DBIx::Interpolate($dbh);  #Q: OK?
    };
    if($@) { croak $@; }
    return $self;
}

sub dbh
{
    my $self = shift;
    return $self->{dbh};
}

sub dbi_interp
{
    my $key;
    my $attr;
    my @args = grep {
        my $save = 1;
        if(ref($_) eq 'SQL::Interpolate::Key') {
            $key = $_; $save = 0;
        }
        elsif(ref($_) eq 'SQL::Interpolate::Attr') {
            $attr = {%$_}; $save = 0;
        }
        $save;
    } @_;
    my($sql, @bind) = &sql_interp(@args);
    my @params = ($sql);
    push @params, $$key if defined $key;
    push @params, $attr, @bind;
    return @params;
}

sub make_dbi_interp
{
    my(@params) = @_;

    my $interp = sub {
        return dbi_interp(@params, @_);
    };
    return $interp;
}

sub key_field
{
    my $key = shift;
    return bless \$key, "SQL::Interpolate::Key";
}

sub attr
{
    return bless {@_}, "SQL::Interpolate::Attr";
}

sub _do_execute
{
    my($sth, @bind) = @_;
    if(ref($bind[0]) eq 'ARRAY') {
        &_bind_params($sth, @bind);
        return $sth->execute();
    }
    else {
        return $sth->execute(@bind);
    }
}

# based on function in DBI
sub _do_selectrow
{
    my ($method, $dbh, $stmt, $attr, @bind) = @_;
    my $sth = $dbh->prepare($stmt, $attr) or return;
    _do_execute($sth, @bind) or return;
    my $row = $sth->$method() and $sth->finish;
    return $row;
}

sub _bind_params
{
    my($sth, @bind) = @_;
    my $num = 1;
    for my $val (@bind) {
        $sth->bind_param($num++, $val->[0], $val->[1]->{type});
    }
}

sub prepare
{
    my($self) = @_;
    return DBIx::Interpolate::STX->new($self);
}

sub do
{
    my($self, @list) = @_;
    return wrap {
        my($sql, $attr, @bind) = $self->dbi_interp(@list);
        my $dbh = $self->{dbh};
        if(ref($bind[0]) eq 'ARRAY') {
            # based on DBI::do
            my $sth = $dbh->prepare($sql, $attr) or return undef;
            _do_execute(@bind) or return undef;
            my $rows = $sth->rows;
            return ($rows == 0) ? "0E0" : $rows;
        }
        else {
            return $dbh->do($sql, $attr, @bind);
        }
    };
}

sub selectrow_array
{
    my($self, @list) = @_;
    my $want = wantarray;
    return wrap {
        my($sql, $attr, @bind) = $self->dbi_interp(@list);
        my $dbh = $self->{dbh};
        if(ref($bind[0]) eq 'ARRAY') {
            # based on DBI::selectrow_array
            my $row = _do_selectrow('fetchrow_arrayref',
                                    $dbh, $sql, $attr, @bind)
                or return;
            return $row->[0] unless $want;
            return @$row;
        }
        else {
            return $dbh->selectrow_array($sql, $attr, @bind);
        }
    };
}

sub selectrow_arrayref
{
    my($self, @list) = @_;
    return wrap {
        my($sql, $attr, @bind) = $self->dbi_interp(@list);
        my $dbh = $self->{dbh};
        if(ref($bind[0]) eq 'ARRAY') {
            # based on DBI::selectrow_arrayref
            return _do_selectrow('fetchrow_arrayref',
                                 $dbh, $sql, $attr, @bind);
        }
        else {
            return $dbh->selectrow_arrayref($sql, $attr, @bind);
        }
    };
}

sub selectrow_hashref
{
    my($self, @list) = @_;
    return wrap {
        my($sql, $attr, @bind) = $self->dbi_interp(@list);
        my $dbh = $self->{dbh};
        if(ref($bind[0]) eq 'ARRAY') {
            # based on DBI::selectrow_hashref
            _do_selectrow('fetchrow_hashref',  $dbh, $sql, $attr, @bind);
        }
        else {
            return $dbh->selectrow_hashref($sql, $attr, @bind);
        }
    };
}

sub selectall_arrayref
{
    my($self, @list) = @_;
    return wrap {
        my($sql, $attr, @bind) = $self->dbi_interp(@list);
        my $dbh = $self->{dbh};
        if(ref($bind[0]) eq 'ARRAY') {
            # based on DBI::selectall_arrayref
            my $sth = $dbh->prepare($sql, $attr) or return;
            _do_execute($sth, @bind) or return;
            # typically undef, else hash or array ref
            my $slice = $attr->{Slice};
            if (!$slice and $slice=$attr->{Columns}) {
                if (ref $slice eq 'ARRAY') {
                    $slice = [ @{$attr->{Columns}} ];
                    for (@$slice) { $_-- }
                }
            }
            my $rows = $sth->fetchall_arrayref(
                $slice, my $MaxRows = $attr->{MaxRows});
            $sth->finish if defined $MaxRows;
	    return $rows;
        }
        else {
            return $dbh->selectall_arrayref($sql, $attr, @bind);
        }
    };
}

sub selectall_hashref
{
    my($self, @list) = @_;
    return wrap {
        my($sql, $key_field, $attr, @bind) = $self->dbi_interp(@list);
        my $dbh = $self->{dbh};
        if(ref($bind[0]) eq 'ARRAY') {
            # based on DBI::selectall_hashref
            my $sth = $dbh->prepare($sql, $attr);
            return unless $sth;
            _do_execute($sth, @bind) or return;
            return $sth->fetchall_hashref($key_field);
        }
        else {
            return $dbh->selectall_hashref($sql, $key_field, $attr, @bind);
        }
    };
}

sub selectcol_arrayref
{
    my($self, @list) = @_;
    return wrap {
        my($sql, $attr, @bind) = $self->dbi_interp(@list);
        my $dbh = $self->{dbh};
        if(ref($bind[0]) eq 'ARRAY') {
            # based on DBI::selectcol_arrayref
            my $sth = $dbh->prepare($sql, $attr);
            return unless $sth;
            _do_execute($sth, @bind) or return;
            my @columns = ($attr->{Columns}) ? @{$attr->{Columns}} : (1);
            my @values  = (undef) x @columns;
            my $idx = 0;
            for (@columns) {
                $sth->bind_col($_, \$values[$idx++]) || return;
            }
            my @col;
            if (my $max = $attr->{MaxRows}) {
                push @col, @values while @col<$max && $sth->fetch;
            }
            else {
                push @col, @values while $sth->fetch;
            }
            return \@col;
        }
        else {
            return $dbh->selectcol_arrayref($sql, $attr, @bind);
        }
    };
}

sub wrap(&) {
    my $code = shift;
    my $x;
    my @x;
    my $want = wantarray();
    eval {
        if($want) { @x = $code->(); }
        else      { $x = $code->(); }
    };
    if($@) { croak $@; }
    return $want ? @x : $x;
}

1;

package DBIx::Interpolate::STX;
use strict;

sub new
{
    my($class, $dbx) = @_;
    my $self = bless {
        # active sth
        sth => undef,
        # map: SQL --> sth (sth cache)
        sths => {},
        # queue of SQL. used to select sth to delete if cache is full
        sql_queue => [],
        # DBIx::Interpolate
        dbx => $dbx,
        # max sths allowed in the cache
        max_sths => 1
    }, $class;
    return $self;
}

sub max_sths
{
    my($self, $max_sths) = @_;
    if(defined $max_sths) {
        $self->{max_sths} = $max_sths;
    }
    else {
        return $self->{max_sths};
    }
}

sub sth
{
    my $self = shift;
    return $self->{sth};
}

sub sths
{
    my $self = shift;
    return {%{$self->{sths}}};
}

sub execute
{
    my($self, @list) = @_;
    my $dbx = $self->{dbx};
    return DBIx::Interpolate::wrap {
        my($sql, $attr, @bind) = $dbx->dbi_interp(@list);
        my $sth = $self->{sths}->{$sql};
        if(! defined $sth) {
            my $dbx = $self->{dbx};
            $sth = $dbx->dbh()->prepare($sql, $attr) or return;
            if(@{$self->{sql_queue}} + 1 > $self->{max_sths}) {
                my $sql_remove = shift @{$self->{sql_queue}};
                delete $self->{sths}->{$sql_remove};
            }
            $self->{sths}->{$sql} = $sth;
            push @{$self->{sql_queue}}, $sql;
        }
        $self->{sth} = $sth;
        &_bind_params($sth, @bind);
        return $sth->execute();
    };
}

sub _bind_params
{
    my($sth, @bind) = @_;
    my $num = 1;
    return DBIx::Interpolate::wrap {
        if(ref($bind[0]) eq 'ARRAY') {
            for my $val (@bind) {
                $sth->bind_param($num++, $val->{value}, $val->{type});
            }
        }
        else {
            for my $val (@bind) {
                $sth->bind_param($num++, $val);
            }
        }
    };
}

sub fetchrow_arrayref
{
    my $self = shift;
    return DBIx::Interpolate::wrap {
        return $self->{sth}->fetchrow_arrayref();
    };
}

sub fetchrow_array
{
    my $self = shift;
    return DBIx::Interpolate::wrap {
        return $self->{sth}->fetchrow_array();
    };
}

sub fetchrow_hashref
{
    my($self, @params) = @_;
    return DBIx::Interpolate::wrap {
        return $self->{sth}->fetchrow_arrayref(@params);
    };
}

sub fetchall_arrayref
{
    my($self, @params) = @_;
    return DBIx::Interpolate::wrap {
        return $self->{sth}->fetchall_arrayref(@params);
    };
}

sub fetchall_hashref
{
    my($self, @params) = @_;
    return DBIx::Interpolate::wrap {
        return $self->{sth}->fetchall_hashref(@params);
    };
}


1;

__END__

=head1 NAME

DBIx::Interpolate - Integrate SQL::Interpolate into DBI

=head1 SYNOPSIS

  use DBI;
  use DBIx::Interpolate qw(:all);

  # simple usage
  my $dbx = new DBIx::Interpolate($dbh);
  $dbx->selectall_arrayref(
      q[SELECT * FROM table WHERE color IN], \@colors,
      q[AND y =], \$x
  );

  # caching statement handles (for performance)
  my $stx = $dbx->prepare();
      # note: $stx represents a set of statement handles ($sth) for a class
      # of queries.
  for my $colors (@colorlists) {
      $stx->execute(q[SELECT * FROM table WHERE color IN], $colors);
          # note: this will transparently prepare a new $sth whenever
          # one compatible with the given query invocation is not cached.
      my $ary_ref = $stx->fetchall_arrayref();
  }

  # using the DBI adapter (dbi_interp) directly
  $dbh->selectall_arrayref(dbi_interp
      q[SELECT * FROM mytable WHERE color IN], \@colors,
      q[AND y =], \$x, q[OR], {z => 3, w => 2}
  );
  # note: dbi_interp typically returns ($sql, \%attr, @bind)

=head1 DESCRIPTION

DBIx::Interpolate wraps L<DBI|DBI> and inherits from
L<SQL::Interpolate|SQL::Interpolate>.  It does nothing more than bring
SQL::Interpolate behavior into DBI.  The DBIx::Interpolate interface
is very close to that of DBI.  All DBI-derived methods look and behave
identically or analogously to their DBI counterparts and mainly differ
in that certain methods, such as do and select.* expect an
interpolation list as input:

  $dbx->selectall_arrayref(
      qq[SELECT * from mytable WHERE height > ], \$x);

rather than the typical ($statement, \%attr, @bind_values) of
DBI:

  $dbx->selectall_arrayref(
      qq[SELECT * from mytable WHERE height > ?], undef, $x);

DBIx::Interpolate also supports I<statement handle sets>.  A statement
handle set is an abstraction of a statement handle and represents an
entire I<set of statement handles> for a given I<class of SQL queries>.
This abstraction is useful because a single interpolation list may
interpolate into any number of SQL queries (depending on variable input),
so multiple statement handles may need to be managed and cached.

=head2 Implementation

The parameters for DBIx::Interpolate methods are internally passed to
L</dbi_interp>, which is a thin wrapper around
L<SQL::Interpolate::sql_interp|SQL::Interpolate/item_sql_interp>.
dbi_interp accepts a few additional types of parameters and typically
returns ($statement, \%attr, @bind_values), which is passed directed
the respective DBI method.  Therefore, the above is equivalent to

  $dbh->select_arrayref(dbi_interp
      qq[SELECT * from mytable WHERE height > ], \$x
  );

which in this case is equivalent to

  my($sql, @bind) = sql_interp
      qq[SELECT * from mytable WHERE height > ], \$x
  );
  $dbh->selectall_arrayref($sql, undef, @bind);

Therefore, DBIx::Interpolate is typically quite thin and follows a
design goal of otherwise maintaining as much compatibility with DBI as
possible.

=head2 Special Cases

dbi_interp can convert some DBI-specific objects into additional return
values expected by certain DBI methods.  For example, selectall_hashref
accepts an additional $key_field parameter:

  $dbh->selectall_hashref($statement, $key_field, \%attr, @bind_values);

dbi_interp can generate the $key_field parameter (as well as \%attr)
as follows:

  my($sql, $key_field, $attr, @bind) = dbi_interp
      "SELECT * FROM mytable WHERE x=", \$x,
      key_field("y"), attr(myatt => 1)
  # Sets
  #   ($sql, $key_field, $attr, @bind) =
  #       ("SELECT * FROM mytable WHERE x=?", 'y', {myatt=>1}, $x)

Therefore, one may do

  $dbx->selectall_hashref(
      "SELECT * FROM mytable WHERE x=", \$x,
      key_field("y"), attr(myatt => 1));

=head2 FUNCTIONS

=over 4

=item C<dbi_interp>

This is a wrapper function around sql_interp().  It serves as an
adapter that returns also the \%attr value (and sometimes $key_field
value) so that the result can be passed directly to the DBI functions.

  ($sql, $attr, @bind) = dbi_interp(@interp_list);
  ($sql, $key_field, $attr, @bind) = dbi_interp(@interp_list);

In addition to the parameters accepted by
SQL::Interpolate::sql_interp, @interp_list may contain the macros
returned by C<attr> and C<key_field> functions.  These respectively
affect the \%attr and $key_field values returned.

 $dbh->selectall_hashref(dbi_interp
    "SELECT * FROM mytable WHERE x=", \$x, key_field("y")
 );

dbi_interp is typically unnecessary to use directly since it is called
internally by the DBI wrapper methods:

 $dbx->selectall_hashref("SELECT * FROM mytable WHERE x=",
     \$x, key_field("y"));
 # same as
 # $dbh->selectall_hashref(dbi_interp "SELECT * FROM mytable WHERE x=",
 #   \$x, key_field("y"));

=item C<make_dbi_interp>

  $dbi_interp = make_dbi_interp(@params);          # functional
  $dbi_interp = $interp->make_dbi_interp(@params); # OO

This is similar in make_sql_interp except that is generates a closure
around the dbi_interp function or method rather than sql_interp.

=item C<key_field>

  $keyobj = key_field($key_field);

Creates and returns an SQL::Interpolate::Key macro object, which if
processed by dbi_interp will cause dbi_interp to return an extra
$key_field value in the result so that it is suitable for passing into
$dbh->fetchrow_hashref and related methods.

  my($sql, $key, $attr, @bind) =
  my @params = dbi_interp q[SELECT * FROM mytable], key_field('itemid');
  $dbh->selectall_hashref(@params);

=item C<attr>

  $attrobj = attr(%attr);

Creates and returns an SQL::Interpolate::Attr macro object, which if
processed by dbi_interp will cause dbi_interp to add the provided
key-value pair to the $attr hashref used by DBI methods.

  my($sql, $attr, @bind) =
  my @params =
    dbi_interp q[SELECT a, b FROM mytable], attr(Columns=>[1,2]);
  $dbh->selectcol_arrayref(@params);

=back

=head2 DATABASE OBJECT (DBX) METHODS

=over 4

=item C<new> (static method)

Creates a new object and optionally creates or attached a DBI handle.

 my $dbx = new DBX::Interpolate($db, %params);

$db [optional] is either a DBI database handle or an ARRAYREF
containing parameters that will be passed to DBI::connect, e.g.
[$data_source, $username, $auth, \%attr].  This parameter may be
omitted.

Any additional %params are passed onto
L<SQL::Interpolate::new|SQL::Interpolate/item_new>.

=item C<connect> (static method)

Connects to a database.

 $dbx = DBIx::Interpolate->connect($data_source, $username, $auth, \%attr);

This is identical to DBI::connect except that it returns at
DBIx::Interpolate object.  An alternate way to connect or attach an
existing DBI handle is via the C<new> method.

=item C<dbh>

 $dbh = $dbx->dbh();

Returns the underlying DBI handle $dbh.  The is useful if you need to
pass the DBI handle to code that does not use SQL::Interpolate.

 $dbx->dbh()->selectall_arrayref(
     "SELECT * FROM mytable WHERE x = ?", undef, $x);

=item do | select.*

These methods are identical to those in DBI except that it takes a parameter
list identical to C<dbi_interp>.

 my $res = $dbx->selectall_hashref("SELECT * FROM mytable WHERE x=", \$x);

=item prepare

 $stx = $dbx->prepare();

Creates a new statement handle set ($stx of type
SQL::Interpolate::STX) associated with $dbx.  There are no parameters.

A statement handle set represents a set of statement handles for a
class of queries.  Up to one statement handle is considered I<active>.
Other operations performed on the statement handle set are passed to
the active statement handle so that the statement handle set often
looks and feels like a regular statement handle.

=back

=head2 STATEMENT HANDLE SET (STX) METHODS

=over 4

=item C<new>

  $stx = new SQL::Interpolate::STX($dbx);

Creates a new statement handle set.  Typically this is not
called directly but rather is invoked through C<prepare>.

=item C<max_sths>

  $max_sths = $stx->max_sths(); # get
  $stx->max_sths($max_sths);    # set

Gets or sets the maximum number of statement handles to cache
in the statement handle set.  The default and minimum value is 1.

=item C<sth>

  $sth = $stx->sth();

Gets the current active statement handle (e.g. the only that was
just executed).  Returns undef on none.

=item C<sths>

  $sths = $stx->sths();

Return a hashref of contained statement handles (map: $sql -> $sth).

=item C<execute>

  $rv = $stx->execute(@list);

Executes the query in the given interpolation list against a statement handle.
If no statement matching statement handle exists, a new one is prepared.
The used statement handle is made the active statement handle.
Return an error behavior is similar to DBI's execute.

@list is an interpolation list (suitable for passing to dbi_interp).

=item C<fetch...>

  $ary_ref = $stx->fetchrow_arrayref();

Various fetch.* methods analogous to those in DBIx::Interpolate are
available.  The fetch will be performed against the active statement
handle in the set.

=back

=head1 ADDITIONAL EXAMPLES

=head2 Preparing and reusing statement handles

  # preparing and reusing statement handles
  my $stx = $dbx->prepare();
      # note: $stx represents a set of statement handles ($sth) for a class
      # of queries.
  $stx->max_sths(3);
  for my $colors (@colorlists) {
      $stx->execute(q[SELECT * FROM table WHERE color IN], $colors);
          # note: this will transparently prepare a new $sth whenever
          # one compatible with the given query is not cached.
      my $ary_ref = $stx->fetchall_arrayref();
  }

The statement handle set transparently prepare statement handles if
ever and whenever the underlying SQL string (and number of bind
values) changes.  The size of the statement handle cache (3) may be
configured to optimize performance on given data sets.  Compare this
simpler and more flexible code to L<the example in
SQL::Interpolate|SQL::Interpolate/additional_examples>.

=head2 Binding variable types (DBI bind_param)

  $dbx->selectall_arrayref(
      "SELECT * FROM mytable WHERE",
      "x=", \$x, "AND y=", sql_var(\$y, SQL_VARCHAR), "AND z IN",
      sql_var([1, 2], SQL_INTEGER)
  );

Compare this much simpler code to L<the example in
SQL::Interpolate|SQL::Interpolate/additional_examples>.

=head1 LIMITATIONS

=over 4

=item DBI attributes

DBI database handle and statement handle attributes are not currently
exposed from the wrapper except via $dbx->dbh()->{...}.  Maybe a Tie
can be used. e.g. $dbx->{mysql_insert_id}

=item bind_param_array

DBI's bind_param_array is not currently supported.
A syntax as follows might be used

  "INSERT INTO mytable", [[...], [...], ...]

=back

=head2 Possible Enhancements

Support might be added for something analogous to DBI's
bind_param_inout.

Passing identified variables:

  my $x = {one => 'two'};
  my $stx = $dbx->prepare("SELECT * FROM mytable WHERE", \$x);
  $stx->execute_vars();
  ...
  $x->{two} = 'three';
  $stx->execute_vars();
  ...

  my $x = {one => 'two'};
  my $y = {one => 'three', two => 'four'};
  my $stx = $dbx->prepare("SELECT * FROM mytable WHERE", sql_var($x, 'x'));
  $stx->execute_vars();
  ...
  $stx->execute_vars(sql_var($x, 'x'); # or?
  $stx->execute_vars(x => $x); # or?
  ...

Conditional macros: (made possible by late expansion of macros)

  $blue = 1;
  $z = 123;
  $stx = $dbx->prepare(
      q[SELECT * FROM mytable WHERE],
      sql_and( sql_if(\$blue,  q[color = "blue"]),
              sql_if(\$shape, sql_fragment(q[shape =], \$shape)),
              'z=', \$z),
      q[LIMIT 10]
  );
  $stx->execute_vars();
  $stx->selectall_arrayref();
  $z = 234;
  $stx->execute_vars();  # note: $sth unchanged
  $stx->selectall_arrayref();
  $blue = 0;
  $stx->execute_vars();  # note: $sth changed
  $stx->selectall_arrayref();

=head1 DESIGN NOTES

DBIx::Interpolate is designed to look an feel like DBI even when the
DBI interface is not entirely user friendly (e.g. the
(fetch|select)(all|row)?_(array|hash)(ref)? and do methods).  Still,
the approach lowers the learning code and could simplify the process
of converting existing DBI code over to SQL::Interpolate.

The use of statement handle sets (STX) is not strictly necessary but is
rather designed to mimic DBI's statement handles more than anything
else.  An alternate solution is for the DBX object to itself be the
statement handle set (i.e. cache statement handles like in
DBIx::Simple's keep_statements).  This latter solution would be simple
to add to DBIx::Interpolate as well.

A real solution would probably be to B<integrate SQL::Interpolate
into DBIx::Simple>.

=head1 CONTRIBUTORS

David Manura (http://math2.org/david)--author.

The existence and original design of this module as an AUTOLOAD
wrapper around DBI was suggested by Jim Cromie.

Bug reports and comments on the design are most welcome.

=head1 LEGAL

Copyright (c) 2004-2005, David Manura.
This module is free software. It may be used, redistributed
and/or modified under the same terms as Perl itself.
See L<http://www.perl.com/perl/misc/Artistic.html>.

=head1 SEE ALSO

=head2 Other modules in this distribution

L<SQL::Interpolate|SQL::Interpolate>,
L<SQL::Interpolate::Filter|SQL::Interpolate::Filter>,
L<SQL::Interpolate::Macro|SQL::Interpolate::Macro>.

Dependencies: L<DBI|DBI>.

Related:
L<DBIx::Simple|DBIx::Simple>,
L<SQL::Abstract|SQL::Abstract>,
L<DBIx::Abstract|DBIx::Abstract>,
L<Class::DBI|Class::DBI>.

=cut
