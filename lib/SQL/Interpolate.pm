package SQL::Interpolate;

our $VERSION = "0.31";

use strict;
use Carp;
use base qw(Exporter);

our @EXPORT;
our %EXPORT_TAGS = (all => [qw(
    make_sql_interp
    sql_flatten
    sql_interp
    sql_literal
    sql_var
)]);
our @EXPORT_OK = @{$EXPORT_TAGS{'all'}};

# supported 'use' param names (useful to DBIx:Interpolate)
our @USE_PARAMS = qw(FILTER TRACE_SQL TRACE_FILTER);

# whether TRACE_SQL is enabled
our $trace_sql = 0;

# whether TRACE_FILTER is enabled
our $trace_filter = 0;

sub sql_flatten;

# This import function processes any "use" parameters.
sub import
{
    my $class = shift;

    my $do_filter = 0;   # whether TRACE_FILTER is enabled
    my $inherit = 0;     # whether __INHERIT is enabled
    my @list = ($class); # unprocessed params
    while(@_) {
        my $first = shift;
        if($first eq 'FILTER') {
            $do_filter = shift;
        }
        elsif($first eq 'TRACE_SQL') {
            $trace_sql = shift;
            print STDERR "TRACE_SQL enabled\n" if $trace_sql;
        }
        elsif($first eq 'TRACE_FILTER') {
            $trace_filter = shift;
            print STDERR "TRACE_FILTER enabled\n" if $trace_filter;
        }
        elsif($first eq '__INHERIT') {
            # indicates that caller inherits from SQL::Interpolate
            # (useful to DBIx::Interpolate)
            $inherit = shift;
        }
        else { push @list, $first }
    }
    @_ = @list;
    my $level = $inherit ? 2 : 1;
    __PACKAGE__->export_to_level($level, @_);

    if($do_filter) {
        require SQL::Interpolate::Filter;
        goto &SQL::Interpolate::Filter::import; # @_
    }
}

sub new
{
    my $class = shift;

    my $dbh;
    my $filters = [];
    while(ref $_[0] ne '') {
        if(UNIVERSAL::isa($_[0], 'DBI::db')) {
            $dbh = shift;
        }
        elsif(UNIVERSAL::isa($_[0], 'SQL::Interpolate::SQLFilter')) {
            push @$filters, shift;
        }
    }
    my(%params) = @_;

    # build indicies on $filters for quick access
    my $filters_hash = {};
    my $text_filters = [];
    my $inits = [];
    my $text_fragment_filters = [];
    for my $filter (@$filters) {
       for my $name ($filter->macro_names()) {
           $filters_hash->{$name} = $filter; # note: up to one filter per macro
       }
       push @$text_filters, $filter
           if $filter->can("filter_text");
       push @$inits, $filter
           if $filter->can("init");
       push @$text_fragment_filters, $filter
           if $filter->can("filter_text_fragment");
    }

    my $self = bless {
        dbh                   => $dbh,
        filters               => $filters,
        filters_hash          => $filters_hash,
        text_filters          => $text_filters,
        inits                 => $inits,
        text_fragment_filters => $text_fragment_filters
    }, $class;
    return $self;
}

# sql_interp helper function.
# Interpolate data element in aggregate variable (hashref or arrayref).
# $self - (may be undef)
# $bindref - \@bind (is modified--appended to)
# $varobj_usedref - \$varobj_used (is modified)
# $ele - raw input element from aggregate.
# returns $sql
sub _sql_interp_data
{
    my($self, $bindref, $varobj_usedref, $ele) = @_;
    if(ref $ele) {
        my($sql2, @bind2) = sql_interp(($self ? $self : ()), $ele);
        push @$bindref, @bind2;
        $$varobj_usedref = 1 if ref $bind2[0];
        return $sql2;
    }
    else {
        push @$bindref, $ele;
        return '?';
    }
}

# sql_interp helper function to interpolate "key IN list",
# assuming context ("WHERE", {key => $list, ...}).
sub _in_list
{
    my($self, $bindref, $varobj_usedref, $key, $list) = @_;
    if(@$list == 0) {
        return "1=1";
    }
    else {
        my @sqle;
        for my $ele (@$list) {
            my $sqle = _sql_interp_data($self, $bindref, $varobj_usedref, $ele);
            push @sqle, $sqle;
        }
        my $sql2 = $key . " IN (" . join(', ', @sqle) . ")";
        return $sql2;
    }
}


sub sql_interp
{
    my $self;
    if(UNIVERSAL::isa($_[0], 'SQL::Interpolate')) {
        $self = $_[0];
        $_->init() for @{$self->{inits}}; # Q: ok if sql_interp recursive?
    }

    my @parts = sql_flatten @_;
    my $sql = '';
    my $id = qr/[a-zA-Z_\.]+/;
    my @bind;
    my $varobj_used = 0; # whether typed sql_var() ever used (if so,
                         # format of @bind result is more complicated)
    my $idx = 0;
    foreach my $part (@parts) {
        my $varobj;
        my $bind_size = @bind;
        if(ref $part eq 'SQL::Interpolate::Variable') {
            unless(keys %$part == 1 && defined($part->{value})) {
                $varobj = $part;
                $varobj_used = 1;
            }
            $part = $part->{value};
        }
        elsif(ref $part eq 'SQL::Interpolate::Literal') {
            $part = $$part;
        }

        if(ref $part) {
            if($sql =~ /\bIN\s*$/si) {
                $part = [ $$part ] if ref $part eq 'SCALAR';
                if(ref $part eq 'ARRAY') {
                    if(@$part == 0) {
                        $sql =~ s/$id\s+IN\s*$/1=1/s or croak 'ASSERT';
                    }
                    else {
                        $sql .= " (" . join(', ', map {
                            _sql_interp_data($self, \@bind, \$varobj_used, $_);
                        } @$part) . ")";
                    }
                }
                else {
                    _error_item($idx, \@parts);
                }
            }
            elsif($sql =~ /\bSET\s*$/si && ref $part eq 'HASH') {
                _error('Hash has zero elements.') if keys %$part == 0;
                $sql .= " " . join(', ', map {
                    my $key = $_;
                    my $val = $part->{$key};
                    "$key=" . _sql_interp_data($self, \@bind, \$varobj_used, $val);
                } keys %$part);
            }
            elsif($sql =~ /\bINSERT[\w\s]*\sINTO\s*$id\s*$/si)
            {
                $part = [ $$part ] if ref $part eq 'SCALAR';
                if(ref $part eq 'ARRAY') {
                    $sql .= " VALUES(" . join(', ', map {
                        _sql_interp_data($self, \@bind, \$varobj_used, $_);
                    } @$part) . ")";
                }
                elsif(ref $part eq 'HASH') {
                    $sql .=
                        " (" . join(', ', keys %$part) . ")" .
                        " VALUES(" . join(', ', map {
                            _sql_interp_data($self, \@bind, \$varobj_used, $_);
                        } values %$part) . ")";
                }
                else { _error_item($idx, \@parts); }
            }
            elsif(ref $part eq 'SCALAR') {
                push @bind, $$part;
                $sql .= ' ?';
            }
            elsif(ref $part eq 'HASH') { # e.g. WHERE {x = 3, y = 4}
                if(keys %$part == 0) {
                    $sql .= ' 1=1';
                }
                else {
                    my $s = join ' AND ', map {
                        my $key = $_;
                        my $val = $part->{$key};
                        if(ref $val eq 'ARRAY') {
                            _in_list($self, \@bind, \$varobj_used, $key, $val);
                        }
                        else {
                            "$key=" . _sql_interp_data($self, \@bind, \$varobj_used, $val);
                        }
                    } keys %$part;
                    $s = "($s)" if keys %$part > 1;
                    $s = " $s";
                    $sql .= $s;
                }
            }
            else { _error_item($idx, \@parts); }
        }
        else {
            $sql .= ' ' unless $sql =~ /(^|\s)$/ || $part =~ /^\s/; # style
            $sql .= $part;
        }

        if($varobj) {
            my $num_pushed = @bind - $bind_size;
            for my $val (@bind[-$num_pushed..-1]) {
                $val = [$val, $varobj];
            }
        }
        $idx++;
    }
    if($varobj_used) {
        for my $val (@bind) {
            my $valcopy = $val;
            $val = [$val, sql_var(\$valcopy)] if ! ref $val;
        }
    }

    if($self) {
        $sql = $_->filter_text($sql) for @{$self->{text_filters}};
    }

    print STDERR "DEBUG:interp[sql=$sql,bind=".join(':',@bind)."]\n"
        if $trace_sql;
    return($sql, @bind);
}

sub sql_flatten
{
    my(@parts) = @_;

    my $dbh;
    my $self;
    if(ref $parts[0] eq 'DBI::db') {
        $dbh = shift(@parts);
    } elsif(UNIVERSAL::isa($parts[0], 'SQL::Interpolate')) {
        $self = shift(@parts);
        $dbh = $self->{dbh};
    }

    my $continue;
    do {
        # Q: is the order of expansion ok?
        $continue = 0;
        @parts = map {
            if(UNIVERSAL::isa($_, 'SQL::Interpolate::Macro')) {
                $continue = 1;
                my @params = ($dbh);
                my $filter = $self ? $self->{filters_hash}->{ref $_} : undef;
                push @params, $filter if $filter;
                $_->expand(@params);
            }
            elsif(ref $_ eq '') { # SQL string or variable ref
                if($self && @{$self->{text_fragment_filters}} != 0) {
                    my @out;
                    for my $filter (@{$self->{text_fragment_filters}}) {
                        @out = $filter->filter_text_fragment($_, \$continue);
                        last unless $continue;
                    }
                    @out
                }
                else {
                    $_
                }
            }
            else { $_ }
        } @parts;
        #DEBUG: print Data::Dumper::Dumper(\@parts);
    }
    while($continue);

    return @parts;
}

sub sql
{
    return SQL::Interpolate::SQL->new(@_);
}


sub make_sql_interp
{
    my(@params) = @_;
    my $interp = sub {
        return sql_interp(@params, @_);
    };
    return $interp;
}

sub sql_literal
{
    return SQL::Interpolate::Literal->new(@_);
}

sub sql_var
{
    return SQL::Interpolate::Variable->new(@_);
}

# helper function to throw error
sub _error_item
{
   my($idx, $partsref) = @_;
   my $prev = $partsref->[$idx-1];
   my $cur  = $partsref->[$idx];
   _error("SQL::Interpolate error: Unrecognized '$cur' following '$prev' in interpolation list.");
}
sub _error
{
   croak "SQL::Interpolate error: $_[0]";
}

1;

package SQL::Interpolate::Variable;
use strict;
use Carp;

sub new
{
    my($class, $value, %params) = @_;
    SQL::Interpolate::_error(
        "Value '$value' in sql_var constructor is not a reference")
        if ! ref $value;
    my $self = bless {value => $value, %params}, $class;
    return $self;
}

1;

package SQL::Interpolate::Literal;
use strict;
use Carp;

sub new
{
    my($class, $value) = @_;
    my $self = bless \$value, $class;
    return $self;
}

1;

__END__

=head1 NAME

SQL::Interpolate - Simplified interpolation of Perl variables into SQL statements

=head1 SYNOPSIS

  use SQL::Interpolate qw(:all);

  my $s = "blue"; my @v = (5, 6);  # sample data

  # basic scalarref syntax and IN arrayref syntax
  my($sql, @bind) = sql_interp
      "SELECT * FROM mytable WHERE color = ", \$s, "AND y IN", \@v;
  # OUTPUT: $sql = "SELECT * FROM mytable WHERE color = ? AND y IN (?, ?)"
  #         @bind = ("blue", 5, 6);

  # shortcut hashref syntax (logical AND construct).
  my($sql, @bind) = sql_interp
      "SELECT * FROM mytable WHERE", {color => $s, y => \@v};
  # OUTPUT: $sql = "SELECT * FROM mytable WHERE (color = ? AND y IN (?, ?))";
  #         @bind = ("blue", 5, 6);

  # shortcut hashref syntax (tuple in INSERT construct)
  ($sql, @bind) = sql_interp
      "INSERT INTO mytable", {color => $s, y => 1} ;
  # OUTPUT: $sql = "INSERT INTO mytable (color, y) VALUES(?, ?)";
  #         @bind = ("blue", 1);

  # shortcut hashref syntax (tuple in UPDATE construct)
  ($sql, @bind) = sql_interp
      "UPDATE mytable SET", {color => $s, y => 1}, "WHERE y <> ", \2 ;
  # OUTPUT: $sql = "UPDATE mytable SET color = ?, y = ? WHERE y <> ?";
  #         @bind = ("blue", 1, 2);

  # sample DBI call
  my $res = $dbh->selectall_arrayref($sql, undef, @bind);

=head1 DESCRIPTION

=head2 Purpose

SQL::Interpolate interpolates Perl variables into SQL statements in a
simplified manner.  It converts a list of intermixed SQL
fragments and variable references into a conventional SQL string and
I<list of bind values>, which can passed onto DBI or used directly.

When using plain DBI, one traditionally interpolates reliably with
bind values, which can become unwieldy:

  $dbh->do(qq(
      INSERT INTO table (color, shape, width, height, length)
                  VALUES(?,     ?,     ?,     ?,      ?     )
  ), undef, $c, $s, $w, $h, $l);

This may be ameliorated with "SQL building techniques,"
but SQL::Interpolate eliminates much of this need with a terse
Perl-like syntax:

  my($sql, @bind) = sql_interp
      "INSERT INTO table",
      {color => $c, shape => $s, width => $w, height => $h, length => $l};
  $dbh->do($sql, undef, @bind);

Besides the simple techniques shown in the SYNOPSIS, SQL-Interpolate
includes a number of optional modules to further integrate
SQL::Interpolate with DBI and streamline the syntax with source
filtering and macros (see the L</SEE ALSO> section):

  my $rows = $dbx->selectall_arrayref(sql[
      SELECT thid, date, title, subject
      FROM threads
      WHERE date > $x AND subject IN @subjects
  ]);

=head2 Security notes

SQL::Interpolate properly binds or escapes variables.  This
recommended practice safeguards against "SQL injection" attacks. The
L<DBI|DBI> documentation has several links on the topic.

=head1 API

The central function of this module is C<sql_interp>, which when
provided a list of elements to interpolate, returns an SQL string and a
list of bind values suitable for passing to DBI.  The rest
of this module provides variants of this theme (e.g. functional and OO
approaches) and supporting functionality, and some of the related
modules wrap or extend C<sql_interp>.

=head2 C<sql_interp>

  ($sql, @bind) = sql_interp @params;            # functional interface
  ($sql, @bind) = $interp->sql_interp(@params);  # OO interface

C<sql_interp()> strings together the given list of elements to
interpolate (the @params "interpolation list") and returns a properly
formatted SQL string ($sql) containing bind "?" parameters and a the
corresponding list of bind values (@bind).  The results are suitable
for passing to DBI.

The "interpolation list" can contain elements of these types:

* SQL literals - strings or L</sql_literal> objects
containing raw SQL fragments.

* variable references - scalarrefs, arrayrefs, hashrefs, or
L</sql_var> objects referring to data to interpolate between
the SQL fragments

* macros - objects (or certain string representing objects)
that may be further expanded into previous two types of elements.  Macros
are explained in L<SQL::Interpolate::Macro|SQL::Interpolate::Macro>.

In addition, the first element in the interpolation list may
optionally be a database handle or (for the OO interface) an instance
of SQL::Interpolate.  sql_interp and macros may use this data to customize
the output such as for a dialect of SQL.

The basic interpolation process is as follows. Strings are appended to
the output SQL ($sql), possibly with some context-dependent tweaking.
For each scalar variable reference, a corresponding placeholder ("?")
and other SQL is appended to $sql, and the de-referenced value is
pushed onto @bind.

B<Interpolation Examples>

 # sample data
 my $s  = 3;                    # scalar
 my $v  = [1, 2];               # arrayref (vector)
 my $h  = {m => 1, n => 2};     # hashref
 my $hv = {v = > $v, s => $s};  # hashref containing listref
 Let $x stand for $s, $h, or $v.

B<Default scalar behavior>

 INPUT:  "foo", \$s, "bar"
 OUTPUT: "foo ? bar", $s

B<Default hashref behavior>

 INPUT:  "WHERE", $x
 OUTPUT: "WHERE (m=? AND n=?)", $h->{m}, $h->{n}    # $x = $h
 OUTPUT: "WHERE (v IN (?, ?) AND s = ?)", @$v, $s   # $x = $hv

B<IN clause>

 INPUT:  "WHERE x IN", $x
 OUTPUT: "WHERE x IN (?)", $s                       # $x = $s
 OUTPUT: "WHERE x IN (?, ?)", @$v                   # $x = $v
 OUTPUT: "WHERE 1=1", @$x                           # @x = ()
 # Note: An arrayref of length 0 is handled specially
 # because "WHERE x in ()" can be invalid SQL (e.g. MySQL).

B<INSERT statements>

 INPUT:  "INSERT INTO mytable", $x
 OUTPUT: "INSERT INTO mytable VALUES(?)", $s;       # $x = $s
 OUTPUT: "INSERT INTO mytable VALUES(?, ?)", @$v;   # $x = $v
 OUTPUT: "INSERT INTO mytable (m, n) VALUES(?, ?)", # $x = $h
         $h->{m}, $h->{n}

B<UPDATE statements>

 INPUT:  "UPDATE mytable SET", $h
 OUTPUT: "UPDATE mytable SET m = ?, n = ?", $h->{m}, $h->{n}

B<Other rules>

Whitespace is automatically added between parameters:

 INPUT:  "UPDATE", "mytable SET", {x => 2}, "WHERE y IN", \@colors;
 OUTPUT: "UPDATE mytable SET x = ? WHERE y in (?, ?)", 2, @colors

Variables must be passed as references such as \$x (or using the
sql// operator when source filtering is enabled); otherwise, they will
processed as SQL fragments and interpolated verbatim into the
result SQL string, negating the security and performance benefits
of binding values.

Note that any scalar values I<inside> an arrayref or hashref are by
default treated as binding variables, not SQL fragments.  Still, the
contained values may be sql_var, sql_literal, or macro objects.  See
the L</Advanced INSERT> for examples.

sql_interp will Do The Right Thing(TM) on trivial cases:

  INPUT: "SELECT * FROM table WHERE color IN", []
  OUTPUT: "SELECT * FROM table WHERE 1=1"
  # invalid to MySQL: SELECT * FROM table WHERE color IN ()

SQL::Interpolate does not attempt to further optimize away such
expressions.  Databases are designed to do query optimization, without
loss of generality.

Variable interpolation is context-sensitive.  The same variable
references can generate different SQL sub-expressions depending on
context:

  INPUT:  "INSERT INTO mytable", $h
  OUTPUT: "INSERT INTO mytable (m, n) VALUES(?, ?)", ...

  INPUT:  "UPDATE mytable SET", $h
  OUTPUT: "UPDATE mytable SET m = ?, n = ?", ...

B<Error handling:> On error, sql_interp will croak with a string message.

=head2 C<sql_literal>

  my $sqlliteral = sql_literal($sql);

C<sql_literal> creates an I<sql_literal> object (of type
SQL::Interpolate::Literal), which provides a general way to represent
an SQL literal--i.e. a fragment of SQL text.

$sql - string containing SQL fragment

sql_literal objects are rarely needed because plain strings are often
sufficient to represent SQL literals in an interpolation list.
However, if you want insert an SQL literal as a value in an arrayref
or hashref, it cannot be done with a plain string because any scalar
value in an arrayref or hashref is interpreted as a binding variable.
An sql_literal objects must be used explicitly:

  sql_interp "INSERT INTO mytable",
      {x => $x, y => sql_literal("CURRENT_TIMESTAMP")};
  # OUTPUT: "INSERT INTO mytable (x, y) VALUES(?, CURRENT_TIMESTAMP)", $x
  
  sql_interp "INSERT INTO mytable", [$x, sql_literal("CURRENT_TIMESTAMP")];
  # OUTPUT: "INSERT INTO mytable VALUES(?, CURRENT_TIMESTAMP)", $x

=head2 C<sql_var>

  my $sqlvar = sql_var($value_ref, type => $sql_type, %params);

C<sql_var()> create an I<sql_var> object (of type
SQL::Interpolate::Variable), which provides a general way to represent
a binding variable and any additional information about it.

$value_ref - variable reference contained

$sql_type - any DBI SQL_DATA_TYPE (e.g. SQL_INTEGER).  Optional.
Default is undef.

Any other named parameters (%params) passed in will be saved into the
object as attributes.

sql_var objects are rarely needed because variable references can be
placed directly into an interpolation list.  sql_var objects primarily
exist to address cases where additional information should be tagged
onto the variable.  For example, DBI allows bind variables to be given
an explicit type:

  my($sql, @bind) = sql_interp "SELECT * FROM mytable WHERE",
      "x=", \$x, "AND y=", sql_var(\$y, SQL_VARCHAR), "AND z IN",
      sql_var([1, 2], SQL_INTEGER);
  # OUTPUT: @bind =
  #   ([$x, sql_var(\$x)], [$y, sql_var(\$y, type => SQL_VARCHAR)],
  #    [1, sql_var([1, 2], type => SQL_INTEGER)],
  #    [2, sql_var([1, 2], type => SQL_INTEGER)]);

If the interpolation list contains at least one sql_var object, then
all the variable references are transparently converted into sql_var
objects, and the elements of @bind take a special form: an arrayref
consisting of the bind value and the sql_var object that generated the
bind value.  Note that a single sql_var holding an aggregate (arrayref
or hashref) may generate multiple bind values.  See L</ADDITIONAL
EXAMPLES> for example usage.

Note that sql_var and sql_literal are duals in a sense.

=head2 Additional public functions/methods

=over 4

=item C<make_sql_interp>

  my $sql_interp = make_sql_interp(@params);          # functional
  my $sql_interp = $interp->make_sql_interp(@params); # OO

Creates a closure that wraps the sql_interp function such that the
parameters passed to the sql_interp consist of @params following by
the parameters passed to the closure.  This function is typically used
to eliminate a need to always pass in a database handle into
sql_interp:

  my $interp = make_sql_interp($dbh);

  my($sql, @bind) = $interp->(...);

=item C<new()>

 my $interp = new SQL::Interpolate([$dbh|$filter]...);
 @result = $interp->sql_interp([$dbh|$filter]...);

Creates a new SQL::Interpolate object, which can configure the
interpolation process.

The arguments can be

- $dbh - zero or one DBI database handle.

- $filter - zero or more SQL filters (derived from
L<SQL::Interpolate::SQLFilter|SQL::Interpolate::Macro>).

The OO interface often not needed, but it is useful if you need to
configure the behavior of many calls to sql_interp, such as when using
some macros.

=back

=head2 Private functions/methods

=over 

=item C<sql_flatten>

 @list_out = sql_flatten(@list_in);          # functional
 @list_out = $interp->sql_flatten(@list_in); # OO

Fully expands all macros in an interpolation list such that
only strings and variables references (no macros) are left.

 my @list = sql_flatten sql/SELECT * FROM mytable where x=$x/;
 # OUTPUT: @list = ('SELECT * FROM mytable where x=', \$x);

This function takes the same type of input as sql_interp, and, in
fact, sql_interp uses it to preprocess input.  Typically, this
function is only used internally by SQL::Interpolate.

=back

=head2 Exports and use parameters

=over 4

=item TRACE_SQL

To enable tracing on C<sql_interp>, do

 use SQL::Interpolate TRACE_SQL => 1;

The generated SQL statements and bind values of all
C<sql_interp> calls will be sent to STDERR.

 DEBUG:interp[sql=INSERT INTO mytable VALUES(?),bind=5]

=item EXPORTS

 use SQL::Interpolate qw(:all);

':all' exports these functions: make_sql_interp,
sql_flatten, and sql_interp, sql_var.

=back

=head1 ADDITIONAL EXAMPLES

These are more advanced examples.

=head2 Preparing and reusing a statement handle

  my $sth;
  for my $href (@array_of_hashrefs) {
     my @list = ("SELECT * FROM mytable WHERE", $href);
     my($sql, @bind) = sql_interp @list;
     die 'ASSERT' if $sth && $sth->{Statement} ne $sql;
     $sth = $dbh->prepare($sql) unless $sth;
     $sth->execute(@list);
     $sth->fetchall_arrayref();
  }

The above code requires that $sql never changes.  If $sql does change,
you would have to prepare a new statement handle.  DBIx::Interpolate
implements a streamlined solution that caches statement
handles.

=head2 Binding variables types (DBI bind_param)

  my($sql, @bind) = sql_interp "SELECT * FROM mytable WHERE",
      "x=", \$x, "AND y=", sql_var(\$y, SQL_VARCHAR), "AND z IN",
      sql_var([1, 2], SQL_INTEGER);
  # OUTPUT:
  #   @bind = ([$x, sql_var(\$x)], [$y, sql_var(\$y, type => SQL_VARCHAR)],
  #            [1, sql_var([1, 2], type => SQL_INTEGER)],
  #            [2, sql_var([1, 2], type => SQL_INTEGER)]);
  die 'ASSERT' if ref $bind[0] ne 'ARRAY';
  my $sth = $dbh->prepare($sql);
  my $idx = 1;
  for my $var (@bind) {
      $sth->bind_param($idx++, $var->[0], $var->[1]->{type});
  }
  $sth->execute();
  my $ret = $sth->selectall_arrayref();

This kludge is similar to the approach in L<SQL::Abstract's
bindtype|SQL::Abstract/bind_type>.
DBIx::Interpolate provides a simpler way of handling bind_type.

=head2 Advanced INSERT

  use SQL::Interpolate::Macro qw(sql_fragment);
  ($sql, @bind) = sql_interp "INSERT INTO mytable", {
      density => $density,
      time => sql_literal("CURRENT_TIMESTAMP()"),
      mass => sql_fragment('density *', \$volume, '+', \$extra)
      # note: sql_var(...) supported too
  };
  # OUTPUT:
  #   $sql = 'INSERT INTO mytable (density, time, mass) ' .
  #          'VALUES(?, CURRENT_TIMESTAMP(), density * ? + ?';
  #   @bind = ($density, $volume, $extra);

Note that scalars inside a hashref or arrayref are treated as binding
variables not SQL literals.  The latter can be expressed using
sql_literal().
L<sql_fragment|SQL::Interpolate::Macro/sql_fragment> is a macro.

=head1 DESIGN NOTES

The section covers the design choices used in this module.

=head2 Philosophy and requirements

These principles have guided the design of SQL-Interpolate.

B<The core module (SQL::Interpolate) should be simple and not try to
do too much>.  (Mark Stosberg) SQL-Interpolate has one central
function, sql_interp, which is relatively simple and reusable in
itself, and all other functionality is built upon it.  Complicated
(macro) and less robust (source filtering) capabilities have been
extracted out of the core module and into optional modules.  Source
filtering, for example, is optional and off by default since many
distrust source filtering and fear it will cause bugs that are
especially difficult to debug because "it's not Perl anymore."

B<The bottleneck is the database rather than Perl>.  This module
necessarily imposes I<some> overhead, largely due to the added string
and regex processing.  The author has not quantified this overhead but
expects it to be low compared to database concerns such as disk access
and query processing and network concerns such as latency.  It may be
possible to avoid rerunning C<sql_interp> when only the binding
variables change (e.g. my($sql, $bindobj) = sql_interp(...); @bind =
$bindobj->(x => 1); @bind = $bindobj->(x => 2)), but this is probably
does not provide much benefit.

B<The query language is SQL>.  There are other modules (such as
SQL::Abstract) that abstract the SQL language behind either
object-oriented (OO) method calls and/or Perl data structures (hashes
and arrays).  The former may be undesirable since it replaces one
language with another and hides the full capabilities and
expressiveness of your database's native SQL language.  The latter may
load too much meaning into the syntax of "{, "[" and "\" thereby
rendering the meaning less clear:

  SQL::Abstract example:
  %where = (lname => {like => '%son%'},
            age   => [-and => {'>=', 10}, {'<=', 20}])
  Pure SQL:
  "lname LIKE '%son' AND (age >= 10 AND age <= 20)"

In contrast, SQL::Interpolate does not abstract away your SQL but
rather makes it easier to interpolate Perl variables into your SQL.
Now, SQL::Interpolate I<does> load some meaning into "{, "[" and "\",
but we try to limit its use to obvious cases as justified below.
Since your raw SQL is exposed, you can use your particular dialect of
SQL, assuming the variable interpolation rules are relatively normal.
Database independence is a worthy goal, but it can be quite difficult
to achieve and is beyond the scope of SQL::Interpolate (though you
might wish to build such features on-top-of SQL::Interpolate).

B<Do-what-I-mean (DWIM) and satisfy the most common case.>  The syntax is
intended to be natural and terse for the most common cases.  This is
demonstrated in the examples.

Now, it may be a bit inconsistent that a hashref has two meanings. The
hashref in ("WHERE", \%hash) represents a logical AND-equal
construction, whereas the hashref in ("INSERT INTO mytable", \%hash)
and ("UPDATE mytable SET", \%hash) represents a tuple or portion of
it.  However, there is little ambiguity since a swap of the two
meanings results in illogical statements.  There is a limited number
of plausible meanings and these constructions, and these two are the
most common and useful ones in practice.  Admittedly, the former case
might alternately be understood as an logical OR (rather than AND)
construction, but it the AND construction is more common in a WHERE
clause and a natural "Do What I Mean." (Similarly, a query "Perl
MySQL" posed to a search engine usually implies "Perl AND MySQL" not
"Perl OR MySQL.)  In the latter interpretation of \%hash, the hashref
very well models a tuple that is more named rather than ordered.

Using an arrayref [x => $x, y => $y] rather than a hashref for the
AND'ed construction could work just as well, and it allows duplicate
keys and non-scalar keys.  However, SQL::Interpolate reserves [...]
for future use.  SQL::Interpolate interprets an arrayref
inside a hashref such as {x => \@y, ...} as an "x IN y" construction.
This was independently suggested by a number of people and unlikely
to be confused with the "x = y" since and x and y have different
dimensions (one is scalar and the other is a vector).

It may be a bit inconsistent that scalars inside hashrefs and
arrayrefs are interpreted as binding variables rather than SQL
literals as is the case outside.

  "WHERE", "x = ", \$x  # variables outside must be referenced
  "WHERE", {x => $x}    # variables inside should not be referenced
  "WHERE", [$x,$y]      # variables inside should not be referenced

However, this not too much a stretch of logicality, and the
alternatives are not pretty and do not satisfy the commonest case.
Consider:

  "WHERE", {x => \$x, y => \$y, z => 'CURRENT_TIMESTAMP'}
  "WHERE x IN", [\1, \2, \3]
  "WHERE x IN", \\@colors ("double referencing")

Exceptions from the commonest case require C<sql_literal()>, which is
the dual of C<sql_var()>.

=head2 Limitations /  characteristics

This module is still a bit under development, so interfaces could
change some, particularly in the more esoteric features.  Still, it
is expected you will find this module quite stable, robust, tested,
simple, flexible, and well documented.

If your new to this module, it's a good idea to examine the generated
SQL (e.g. the TRACE_SQL option) to ensure you're getting what you
think you're getting.  Be careful to reference the variables you
interpolate to prevent SQL injection (see discussion in
L</sql_interp>).

This module does not parse your SQL fragments except to the extent
required for variable interpolation, so it does not guarantee that the
generated SQL is valid but leaves that responsibility to your
database.  This is intentional so that it will works well even with
non-standard SQL dialects.  Some types of interpolation are
context-sensitive and involve examination of your SQL fragments.  The
examination could fail on obscure syntax, but it is often simple and
robust.  Look at the examples to see the types of interpolation that
are accepted, and if doubt, examine the SQL output yourself (use
Data::Dumper to examine the output of sql_interp or enable the
TRACE_SQL option) or look at the source code of sql_interp.  If
needed, you can disable context sensitivity by inserting a null-string
before a variable.

A few things are just not possible with the ("WHERE, \%hashref)
syntax, so in such case, use a more direct syntax:

  # ok--direct syntax
  sql_interp "...WHERE", {x => $x, y => $y}, 'AND y = z';
  # bad--trying to impose a hashref but keys must be scalars and be unique
  sql_interp "...WHERE",
      {sql_var(\$x) => sql_literal('x'), y => $y, y => sql_literal('z')};

=head2 Proposed enhancements

The following enhancements to SQL::Interpolate have been proposed.
The most important suggestions are listed at top, and some
suggestions could be rejected.

A tutorial could be useful. (Wojciech)

sql_interp({bla => undef}) should probably return
("bla is NULL", ()) (slaven).  http://rt.cpan.org/NoAuth/Bug.html?id=11810

The following additional type of insert might be supported (markt):

  sql_interp("INSERT INTO temp (id,val) VALUES", [1,2])

It might be useful to interpret hashrefs as tuples when
following a SELECT (markt):

  'INSERT INTO mytable SELECT', {name => sql_literal('table.col'), me => 24}
  ==> 'INSERT INTO mytable SELECT table.col as name, ? as me', 24
similar to
  'INSERT INTO mytable', {name => sql_literal('CURRENT_TIMESTAMP()'), me => 24}
Also 'SELECT', [$x, $y, $z]).

Multiple row inserts were suggested (markt) as supported on MySQL
and DB2 (any other databases?).  It is not certain that
the syntax is sufficiently clear or common:

  "INSERT INTO mytable", [[1,2], [3,4]] (and other variations)
  "INSERT INTO mytable", {x => 2}, [2]  (?)

"AND"s might be made implicit such that the following statements
become equivalent.  This may not be necessary.

  sql_interp "...WHERE", {x => 5}, 'AND', sql_or(...)
  sql_interp "...WHERE", {x => 5}, sql_or(...)

Named placeholders might be supported (possibly via sql_var()):

  "SELECT * FROM mytable WHERE",
    "x=", sql_var("color"), "and", {val => sql_var("weight")},
    sql_val(weight => 100), sql_val(color => 'blue')

sql_and and sql_or macros might support hashrefs as well (the former
for clarity and the latter for terseness):

  "SELECT * FROM mytable WHERE", {x => 2, y => 5}
  "SELECT * FROM mytable WHERE", sql_and {x => 2, y => 5} # same as above
  "SELECT * FROM mytable WHERE", sql_or {x => 2, y => 5}

Logical operations might be supported as follows.  This might be rejected
due to unintuitive syntax.

  "SELECT * FROM mytable WHERE", {
    x => sql_not(2),   # x <> 2
    y => 5
  }
 "SELECT * FROM mytable WHERE", sql_or { # might be difficult to read
   x => sql_or(2, 3, sql_and(4, 5)),
   y => 5
 }

Support for tuples (e.g. MySQL) might be added, but this is probably too
uncommon to be worthwhile to implement:

  SELECT * FROM edge WHERE", {"(sid, did)" => [5, 1]}  # equals
  SELECT * FROM edge WHERE", {"(sid, did)" => [[5, 1], [2, 3]]} # IN

Can recursive sql_interp cause problems?

=head2 Implementation notes

Oracle (unlike MySQL) does not allow 'WHERE id = 5 and 1' nor
'WHERE id = 5 or 0'.  SQL::Interpolate therefore generates the more
portable 'WHERE id = 5 and 1=1' and 'WHERE id = 5 or 1=0'.

=head1 CONTRIBUTORS

David Manura (L<http://math2.org/david/contact>) (author).
Feedback incorporated from Mark Stosberg
(L<http://mark.stosberg.com/>) (recommended simplifying the code
module, simplified the docs, and provided a bunch of other highly
useful feedback), Mark Tiefenbruck, Wojciech Pietron (Oracle compat),
Jim Chromie (DBIx::Interpolate idea), Juerd Waalboer, Terrence Brannon
(early feedback), and others.

=head1 FEEDBACK

Bug reports and comments on the design are most welcome.  rt.cpan.org
can be used for specific bug reporting, but you may also use the
discussion list (L<http://www.cpanforum.com/dist/SQL-Interpolate>) or
contact me directly (L<http://math2.org/david/contact>).  See also the
project page at L<http://math2.org/sql_interpolate>.

=head1 LEGAL

Copyright (c) 2003-2005, David Manura.
This module is free software. It may be used, redistributed
and/or modified under the same terms as Perl itself.
See L<http://www.perl.com/perl/misc/Artistic.html>.

=head1 SEE ALSO

=head2 Other modules in this distribution

L<DBIx::Interpolate|DBIx::Interpolate> extends this module slightly,
allowing certain DBI methods to accept an C<sql_interp>-like
interpolation list rather than the traditional ($statement, \%attr,
@bind_values)-like parameter list.

L<SQL::Interpolate::Filter|SQL::Interpolate::Filter> streamlines the
the SQL::Interpolate syntax with direct SQL interpolation analogous to
Perl string interpolation.

L<SQL::Interpolate::Macro|SQL::Interpolate::Macro> provides an
assortment of macros and SQL filters, which allow you to write
simpler, more robust, and possibly more portable queries.

=head2 Related modules

L<SQL::Abstract|SQL::Abstract> shares with C<SQL::Interpolate> the
purpose of making SQL generation easier. SQL::Abstract differs in that
it expresses queries in terms of OO method calls. It's syntax may
impair readability because it uses the subtle difference between a
brace and bracket to denote the difference between AND and OR in a
query (the user can change whether a bracket implies "AND" or
"OR"). Some complex where clauses are difficult or impossible with
L<SQL::Abstract|SQL::Abstract>.  SQL::Interpolate gives the author
more direct access to the underlying SQL.  This permits using the full
expressivity of the database query language.

L<DBIx::Simple|DBIx::Simple> strives to simplify SQL generation as
well as the data structures returned from
L<DBI|DBI>. C<SQL::Interpolate> and the related modules don't try to
handle C<DBI>'s results at all. C<DBIx::Simple> currently can use
L<SQL::Abstract|SQL::Abstract> to help generate SQL, and it may be
possible in the future for the same to be done with
C<SQL::Interpolate>.

L<Class::DBI|Class::DBI> is a popular "complete" solution for abstract
database access through an OO interface. It currently has a plugin
called L<Class::DBI::AbstractSearch|Class::DBI::AbstractSearch> that
allows it to use C<SQL::Abstract> to generate SQL. It's possible that
C<SQL::Interpolate> could be integrated with it as well.

=head2 Related resources

SQL Interpolate Project Page: L<http://math2.org/sql_interpolate> .

Full example code - Meset::MessageBoard in Meset
(L<http://math2.org/meset>).

=cut

