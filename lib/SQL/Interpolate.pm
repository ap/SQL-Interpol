package SQL::Interpolate;

our $VERSION = "0.31_01";

use strict;
use base qw(Exporter);
use Carp;

our @EXPORT;
our %EXPORT_TAGS = (all => [qw(
    make_sql_interp
    sql_flatten
    sql_interp
    sql_literal
    sql_var
)]);
our @EXPORT_OK = @{$EXPORT_TAGS{'all'}};

our $trace_sql = 0;
our $trace_filter = 0;

sub sql_flatten;

sub import
{
    my $class = shift;

    my $do_filter = 0; # source filtering option
    while(1) {
        if(defined($_[0]) && $_[0] eq 'FILTER') {
            shift;
            $do_filter = shift;
        }
        elsif(defined($_[0]) && $_[0] eq 'TRACE_SQL') {
            shift;
            $trace_sql = shift;
       }
        elsif(defined($_[0]) && $_[0] eq 'TRACE_FILTER') {
            shift;
            $trace_filter = shift;
        }
        else { last; }
     }
    unshift @_, $class;

    __PACKAGE__->export_to_level(1, @_);
    if($do_filter) {
        require SQL::Interpolate::Filter;
        SQL::Interpolate::SQL->export_to_level(1);
        goto &SQL::Interpolate::Filter::import;
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

# Helper function for sql_interp.
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
    my $varobj_used = 0;
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
                        $sql =~ s/$id\s+IN\s*$/1/s or croak 'ASSERT';
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
            elsif(ref $part eq 'HASH') # e.g. WHERE {x = 3, y = 4}
            {
                if(keys %$part == 0) {
                    $sql .= ' 1';
                }
                else {
                    my $s = join ' AND ', map {
                        my $key = $_;
                        my $val = $part->{$key};
                        "$key=" . _sql_interp_data($self, \@bind, \$varobj_used, $val);
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
        #print Data::Dumper::Dumper(\@parts);
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
  
  my @colors = ('blue', 'green');
  my($sql, @bind) = sql_interp
      q[SELECT * FROM mytable WHERE color IN], \@colors,
      q[AND y =], \$x, q[OR], {z => 3, w => 2} ;
  # Result:
  #   $sql = "SELECT * FROM mytable WHERE color IN (?, ?) " .
  #          "AND y = ? OR (z = ? AND w = ?)";
  #   @bind = ('blue', 'green', $x, 3, 2);

  ($sql, @bind) = sql_interp
      "INSERT INTO table", {color => $color, shape => $shape} ;
  # Result:
  #   $sql = "INSERT INTO table (color, shape) VALUES(?, ?)";
  #   @bind = ($color, $shape);

  ($sql, @bind) = sql_interp
      q[UPDATE table SET], {color => $color, shape => $shape},
      q[WHERE color <> ], \$skip_color ;
  # Result:
  #   $sql = "UPDATE table SET color = ?, shape = ?" .
  #          "WHERE color <> ?";
  #   @bind = ($color, $shape, $skip_color);

  # Passing any of the above results to DBI
  $dbh->selectall_arrayref($sql, undef, @bind);

=head1 DESCRIPTION

=head2 Purpose

SQL::Interpolate makes it easy to interpolate Perl variables into SQL
statements.  It does so in a manner that is often more natural
and less redundant, error-prone and restrictive than existing
methods.  SQL::Interpolate converts a list of intermixed SQL fragments
and variables into a conventional SQL string and I<list of bind
values>, which can be used directly or passed onto DBI.

When using DBI, one traditionally interpolates reliably using
bind values, which can become unwieldy:

  $dbh->do(qq(
      INSERT INTO table (color, shape, width, height, length)
                  VALUES(?,     ?,     ?,     ?,      ?     )
  ), undef, $new_color, $new_shape, $width, $height, $length);

SQL::Interpolate eliminates the need for many SQL building techniques
and enables you to achieve the same effect in a more Perl-like manner:

  my($bind, @sql) = sql_interp "INSERT INTO table",
      {color => $color, shape  => $shape,
       width => $width, height => $height, length => $length} ;
  $dbh->do($sql, undef, @bind);

Besides the simple techniques shown in the SYNOPSIS, SQL::Interpolate
can further integrate with DBI and streamline the syntax
with source filtering and macros:

  my $rows = $dbx->selectall_arrayref(sql[
      SELECT thid, date_modified, title, suid, state
      FROM threads
      WHERE state <> $state AND suid IN @suids
      ORDER BY t.date_modified DESC
  ]);

Refer to the L</SEE ALSO> section for more details about these related
modules.

=head2 Security notes

SQL::Interpolate properly escapes or binds variables.  This
recommended practice safeguards against an "SQL injection" attacks. The
L<DBI|DBI> documentation has several links on the topic.

=head2 Structure of this module

The central function of this module is C<sql_interp>, which when
provided a list of items to interpolate, returns an SQL string and a
list of bind values, which are typically then passed to DBI.  Other
functions provide variants of this theme (e.g. OO and functional
approaches) or supporting functionality, and some of the related
modules wrap or extend C<sql_interp>.

=head1 API

=head2 C<sql_interp>

  ($sql, @bind) = sql_interp @params;            # functional interface
  ($sql, @bind) = $interp->sql_interp(@params);  # OO interface

C<sql_interp()> strings together the given list of elements to
interpolate and returns a properly formatted SQL string containing
bind ("?") parameters along with the corresponding list of bind values.
The results are suitable for passing to DBI.

The "interpolation list" can contain

* SQL literals - strings or L</sql_literal> objects
containing raw SQL fragments.

* variable references - scalarrefs, arrayrefs, hashrefs, or
L</sql_var> objects referring to data to interpolate between
the SQL fragments

* macros - strings or objects that are further expanded into
other interpolation list items.  They are explained later.

In addition, the first element in the interpolation list may
optionally be a database handle of an instance of SQL::Interpolate.
sql_interp and macros may use these to customize the output (e.g. for
a dialect of SQL).

The basic interpolation process is as follows. Strings are appended to
the output SQL ($sql), possibly with some content-dependent tweaking.
Variable references are dereferenced, corresponding placeholders ("?")
and other SQL are appended to $sql, and the corresponding values are
pushed onto @bind.

B<Interpolation Examples>

B<Default behavior>

 # Default behavior for scalarref
 INPUT:  "WHERE one=", $x
 OUTPUT: # if $x is a scalarref, e.g. \1
         "WHERE one=?", $$x

 # Default behavior for hashref
 INPUT:  "WHERE", $x
 OUTPUT: # if $x is a hashref, e.g. {one => 1, two => 2}
         "WHERE x=? AND y=?", $x->{one}, $x->{two}

B<INSERT statements>

 INPUT:  "INSERT INTO mytable", $x
 OUTPUT: # if $x is a hashref, e.g. {one => 1, two => 2}
         "INSERT INTO mytable (one, two) VALUES(?, ?)",
             $x->{one}, $x->{two}
 OUTPUT: # if $x is an arrayref, e.g. [1, 2]
         "INSERT INTO mytable VALUES(?, ?)", @$x;
 OUTPUT: # if $x is a scalarref, e.g. \1
         "INSERT INTO mytable VALUES(?)", $$x;

B<IN clause>

 INPUT:  "WHERE x IN", $x
 OUTPUT: # if $x is an arrayref, e.g. [1, 2]
         "WHERE x IN (?, ?)", @$x
 OUTPUT: # if $x is an arrayref of length 0, e.g. []
         "WHERE 1", @$x
 OUTPUT: # if $x is a scalarref, e.g. \1
         "WHERE x in (?)", $$x
 # Note: Special handling for arrayref of length 0 is required
 # because "WHERE x in ()" can be invalid SQL (e.g. MySQL).

B<UPDATE statements>

 INPUT:  "UPDATE mytable SET", $x
 OUTPUT: # if $x is a hashref, e.g. {one => 1, two => 2}
         "UPDATE mytable SET one = ?, two = ?", 1, 2

Whitespace is automatically added between parameters:

  sql_interp "UPDATE", "mytable SET", {x => 2}, "WHERE y IN", \@colors;
  # Result SQL: "UPDATE mytable SET x = ? WHERE y in (?, ?)"

Note that variables must be passed as references (e.g. \$x or using
using the sql// operator from source filtering), otherwise they will
recognized as SQL fragments to be interpolated verbatim into the
result SQL string, which negates the security and performance benefits
of binding values.

sql_interp will Do The Right Thing(TM) on trivial cases:

  INPUT: "SELECT * FROM table WHERE color in", []
  OUTPUT: "SELECT * FROM table WHERE 1"

because the following mechanical answer is not generally valid SQL:

  OUTPUT: SELECT * FROM table WHERE color IN ()

SQL::Interpolate does not attempt to further optimize away such
expressions since we can just as well let the database do this.
Databases are designed to do query optimization, without loss of
generality.

Furthermore, variable interpolation is context-sensitive.  So,

  sql_interp qq[INSERT INTO mytable ], {color => 'blue', shape => 'square'};
  # Result SQL: INSERT INTO mytable (color, shape) VALUES(?, ?)

  sql_interp qq[UPDATE mytable SET ], {color => 'blue', shape => 'square'};
  # Result SQL: UPDATE mytable SET color = ?, shape = ?

The two generate SQL containing a different SQL subexpressions for
the same variable references.

Note that any scalar values inside an arrayref or hashref are by
default treated as binding variables, not SQL fragments.  Still, the
contained values may be sql_var, sql_literal, or macro objects.  See
the L</Advanced INSERT> for examples.

B<Error handling:> On error, sql_interp will croak with a string message.

=head3 Macros

Macros can be objects derived from
L<SQL::Interpolate::Macro|SQL::Interpolate::Macro>, or then can be
"stringified macros" existing as a convenience inside SQL strings.
Stringified macros will be converted into real macro objects before
processing.  Note that source filtering internally converts sql//
quotes into macro objects.  A macro object can contain nested
interpolation elements, and it can expand to these or others by
macro expansion.

Any macros in the interpolation list are flattened into simple
strings and variable references (via the C<sql_flatten> function).
This is performed I<before> interpolation so that only simple only
strings and variable references need be interpolated.  Flattening
involves converting any stringified macros into real macro objects and
expanding macro objects into other interpolation elements (strings,
variable references, and macro objects).  The process can be iterative
since any macros generated by macro expansion (e.g. nested macros or
otherwise) must be flattened themselves.

The transformation process also contains a number of extension hooks,
where code can be inserted to transform the interpolation list and/or
the final $sql string.  Refer to
L<SQL::Interpolate::Macro|SQL::Interpolate::Macro> for details on
macro expansion and filtering.

=head2 C<sql_literal>

  my $sqlliteral = sql_literal($sql);

C<sql_literal> creates an I<sql_literal> object (of type
SQL::Interpolate::Literal), which provides a general way to represent
an SQL literal, i.e. a fragment of SQL text.

$sql - string containing SQL fragment

sql_literal objects are usually not needed because plain strings can
instead represent SQL literals in an interpolation list.  However, if
you want insert an SQL literal into an arrayref or hashref, it can not
be done with a plain string because any scalar value in an arrayref or
hashref is interpreted as a binding variable.  sql_literal objects
must be used instead as shown:

  sql_interp "INSERT INTO mytable",
      {x => $x, y => sql_literal("CURRENT_TIMESTAMP")};
  # Result: "INSERT INTO mytable (x, y) VALUES(?, CURRENT_TIMESTAMP)"
  
  sql_interp "INSERT INTO mytable", [$x, sql_literal("CURRENT_TIMESTAMP")];
  # Result: "INSERT INTO mytable VALUES(?, CURRENT_TIMESTAMP)"
  
  sql_interp "SELECT * FROM mytable WHERE",
      {x => $x, y => sql_literal("z")};
  # Result: "SELECT * FROM mytable WHERE (x = ? AND y = z)"

=head2 C<sql_var>

  my $sqlvar = sql_var($value_ref, type => $sql_type, %params);

C<sql_var()> create an I<sql_var> object (of type
SQL::Interpolate::Variable), which provides a general way to represent
a binding variable and any additional information about it.

$value_ref - variable reference contained

$sql_type - any DBI SQL_DATA_TYPE (e.g. SQL_INTEGER).  Optional.
Default is undef.

Any other names parameters %params passed in will be saved into the
object as attributes.

sql_var objects are usually are not needed because variable references
can be placed directly into an interpolation list.  sql_var objects
primarily exist to address cases where DBI would bind the variable to
a wrong type without a hint.  Examples:

  my($sql, @bind) = sql_interp "SELECT * FROM mytable WHERE",
      "x=", \$x, "AND y=", sql_var(\$y, SQL_VARCHAR), "AND z IN",
      sql_var([1, 2], SQL_INTEGER);
  # Sets
  #   @bind = ([$x, sql_var(\$x)], [$y, sql_var(\$y, type => SQL_VARCHAR)],
  #            [1, sql_var([1, 2], type => SQL_INTEGER)],
  #            [2, sql_var([1, 2], type => SQL_INTEGER)]);

If the interpolation list contains at least one sql_var object, then
all the variable references are transparently converted into sql_var
objects, and the elements of @bind take a special form: an arrayref
consisting of the bind value and the sql_var object that generated the
bind value.  Note that a single sql_var holding an aggregate (arrayref
or hashref) may generate multiple bind values.  See L</ADDITIONAL
EXAMPLES> for example usage.

Note that sql_var and sql_literal are duals in a sense.

=head2 Additional Public Functions/Methods

=over 4

=item C<make_sql_interp>

  my $sql_interp = make_sql_interp(@params);          # functional
  my $sql_interp = $interp->make_sql_interp(@params); # OO

Creates a closure that wraps the sql_interp function such that the
parameters passed to the sql_interp consist of @params followed by the
parameters passed to the closure.  This function is typically used to
eliminate a need to always pass in a database handle into sql_interp:

  my $interp = make_sql_interp($dbh);

  my($sql, @bind) = $interp->(...);

=item C<new()>

Creates a new SQL::Interpolate object, which can configure the
interpolation process.

 my $interp = new SQL::Interpolate([$dbh|$filter]...);
 @result = $interp->sql_interp([$dbh|$filter]...);

The arguments are expected to be one of these optional values:

- $dbh - up to one DBI database handle.

- $filter - zero or more SQL filters (derived from
L<SQL::Interpolate::SQLFilter|SQL::Interpolate::Macro>).

The OO interface often not needed.  It is useful though if you need to
configure the behavior of many calls to sql_interp, such as when using
some macros.

=back

=head2 Private Functions/Methods

=over 

=item C<sql_flatten>

 @list_out = sql_flatten(@list_in);          # functional
 @list_out = $interp->sql_flatten(@list_in); # OO

Flatten out an interpolation list and expands any macros until
only strings and variables references (no macros) are left.

 my @list = sql_flatten sql/SELECT * FROM mytable where x=$x/;
 # Result: @list = ('SELECT * FROM mytable where x=', \$x);

This function takes the same type of input as sql_interp, and in
fact sql_interp uses it to preprocess input.  This function is
typically only used internally by SQL::Interpolate.

=back

=head2 Exports and Use Parameters

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
  # Sets
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
  # Result:
  #   $sql = 'INSERT INTO mytable (density, time, mass) ' .
  #          'VALUES(?, CURRENT_TIMESTAMP(), density * ? + ?';
  #   @bind = ($density, $volume, $extra);
  
  ($sql, @bind) = sql_interp "INSERT INTO mytable", [
      $density,
      sql_literal("CURRENT_TIMESTAMP()"),
      sql_fragment('density *', \$volume, '+', \$extra)
  ];
  # Result:
  #   $sql = 'INSERT INTO mytable ' .
  #          'VALUES(?, CURRENT_TIMESTAMP(), density * ? + ?';

Note that scalars inside a hashref or arrayref are treated as binding
variables not SQL literals.  The latter can be expressed using
sql_literal().
L<sql_fragment|SQL::Interpolate::Macro/item_sql_fragment> is a macro.

=head1 LIMITATIONS / STATUS

This module is still a bit under development, so interfaces could
change some, particularly with respect to macros.  Robustness, good
style, simplicity/generality, and good documentation are design goals.

Be careful to reference the variables you interpolate to prevent SQL
injection (see discussion in L</sql_interp>).

The approach of this module does not guarantee that the generated SQL
is valid for your particular database.  SQL::Interpolate does not
parse most of the SQL input but just passes it through.  It is
really the responsibility of the database to validate your SQL.  On
the positive side, this module has no problem with many non-standard
SQL constructs.  If in doubt, check the output of sql_interp yourself
(such as by enabling the TRACE_SQL option).

Some types of interpolation are context-sensitive and involve
examination of your SQL fragments.  Often, the examination is simple
and robust.  However, the examination could fail on obscure syntax.
Look at the examples to see the types of interpolation that are
accepted, and if doubt, examine the SQL output yourself or look at
this module's source code.  If for whatever reason (let us know if you
find an example), you you can disable context sensitivity by inserting
a null-string before a variable.

"AND"s could be made implicit at times, but this is not currently
supported:

  sql_interp "...WHERE", {x => 5}, 'AND', sql_or(...)
  sql_interp "...WHERE", {x => 5}, sql_or(...) # could be made equivalent

Some things are just not possible with the "WHERE \%hashref" syntax:

  # bad--keys must be scalars and be unique
  sql_interp "...WHERE",
      {sql_var(\$x) => sql_literal('x'), y => $y, y => sql_literal('z')};
  # ok
  sql_interp "...WHERE", {x => $x, y => $y}, 'AND y = z';

=head1 DESIGN NOTES

The core module (SQL::Interpolate) shouldn't try to do too much.--mark

Some distrust source filtering and fear it will cause bugs that are
especially difficult to debug because "it's not perl anymore."  Source
filtering is entirely optional, and off by default, in
SQL::Interpolate.

It may be possible to avoid rerunning C<sql_interp> when only the
binding variables change.  E.g. my($sql, $bindobj) = sql_interp(...);
@bind = $bindobj->(x => 1); @bind = $bindobj->(x => 2).
This is probably not necessary since the bottleneck is typically
with the database rather than Perl.

It may be a bit inconsistent that a hashref in ("WHERE, \%hash)
represents a logical AND-equal construction, while in ("INSERT INTO
mytable", \%hash) the hashref represents pure data.  Moreover, the
former can only represent a limited number of logical constructions,
albeit common and useful ones in practice.  However, it is a natural
"Do What I Mean" that a hashref in a WHERE clause represents a set of
"AND"'ed clauses.  The hashref represents name-value pairs, and "AND"
tends to be more common than "OR" in SQL.

Using an arrayref [x => $x, y => $y] rather than a hashref for the AND'ed
construction could work just as well, and it allows duplicate keys and
non-scalar keys.  However, SQL::Interpolate reserves [...] for future
use.

It may be a bit inconsistent that scalars inside a hashref or arrayref
are interpreted as binding variables rather than SQL literals as is
the case outside.  The alternatives, ("WHERE", {x => \$x, y => \$y, z
=> CURRENT_TIMESTAMP}), ("WHERE x IN", [\1, \2, \3]), or treating
"double referencing" \\@colors as special, aren't pretty and don't
satisfy the commonest case.  Representing embedded SQL literals via
C<sql_literal()> is a compromise, and C<sql_literal()> and
C<sql_var()> become duals.

SQL::Abstract can load too much meaning into "{, "[" and "\" so that
the interpretation of a statement may not be visually clear.

=head1 CONTRIBUTORS

David Manura (L<http://math2.org/david/contact>)--author.

Feedback incorporated from

* Mark Stosberg (L<http://mark.stosberg.com/>)
- who recommended keeping the core module simple, simplified the docs,
and provided a bunch of other highly useful feedback.

* Terrence Brannon
- early feedback

=head1 FEEDBACK

Bug reports and comments on the design are most welcome.  rt.cpan.org
can be used for specific bug reporting, but you may also contact
me directly (L<http://math2.org/david/contact>).  See also the project
page at L<http://math2.org/sql_interpolate>.

=head1 LEGAL

Copyright (c) 2003-2005, David Manura.
This module is free software. It may be used, redistributed
and/or modified under the same terms as Perl itself.
See L<http://www.perl.com/perl/misc/Artistic.html>.

=head1 SEE ALSO

=head2 Other modules in this distribution

L<DBIx::Interpolate|DBIx::Interpolate> extends this module slightly,
allowing certain DBI methods to accept an C<sql_interp>-like
parameter list rather than the traditional ($statement, \%attr,
@bind_values)-like parameter list.

L<SQL::Interpolate::Filter|SQL::Interpolate::Filter> streamlines the
the SQL::Interpolate syntax with direct SQL interpolation analogous to
Perl string interpolation.

L<SQL::Interpolate::Macro|SQL::Interpolate::Macro> provides an
assortment of macros and SQL filters, which allow you to write
simpler, more robust, and possibly more portable queries.

=head2 Related Modules

L<SQL::Abstract|SQL::Abstract> shares with C<SQL::Interpolate> the
purpose of making SQL generation easier. SQL::Abstract differs in that
it expresses queries in terms of OO method calls. It's syntax can be
hard to read because it uses the subtle difference between a brace and
bracket to denote the difference between AND and OR in a query. To
worsen the situation, the user can change whether a bracket implies
"AND" or "OR". Some complex where clauses are difficult or impossible
with L<SQL::Abstract|SQL::Abstract>.  SQL::Interpolate gives the
author more direct access to the underlying SQL.  This permits using
the full expressivity of the database query language.

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

=head2 Related Resources

SQL Interpolate Project Page: L<http://math2.org/sql_interpolate> .

Full example code - Meset::MessageBoard in Meset
(L<http://math2.org/meset>).

=cut

