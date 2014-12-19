use 5.012;
use warnings;

package SQL::Interp;

use Exporter::Tidy all => [ qw( sql_interp sql ) ];

sub sql { bless [ @_ ], __PACKAGE__ }

sub sql_interp {
    my $p = SQL::Interp::Parser->new( \@_ );
    my $sql = $p->parse( @_ );
    my $bind = $p->bind;
    return ( $sql, @$bind );
}


package SQL::Interp::Parser;

use Object::Tiny::Lvalue qw( alias_id idx items bind );

use Carp ();

sub _error { Carp::croak 'SQL::Interp error: ', @_ }

sub new {
    my $class = shift;
    my ( $items ) = @_;
    return $class->SUPER::new(
        alias_id   => 0,      # next ID to use for table alias
        idx        => 0,      # current index in interpolation list
        items      => $items, # current interpolation list
        bind       => [],     # bind elements in interpolation
    );
}

sub parse {
    my $self = shift;

    state $ident_rx = qr/[a-zA-Z_][a-zA-Z0-9_\$\.]*/;

    my $sql = '';
    my $bind = $self->bind;

    foreach my $item (@_) {
        if (not ref $item) {
            $sql .= ' ' if $sql =~ /\S/ and $item !~ /\A\s/;
            $sql .= $item;
            next;
        }

        if (ref $item eq 'SQL::Interp') {
            $sql .= ' ' if $sql ne '';
            $sql .= $self->parse(@$item);
            next;
        }

        if ( $sql =~ s/(\s*$ident_rx\s+(NOT\s+)?IN)\s*$//i ) {
            my $type = ref $item;
            my @value
                = 'SCALAR' eq $type ? $$item
                : 'ARRAY'  eq $type ? @$item
                : 'REF'    eq $type && 'ARRAY' eq ref $$item ? @$$item
                : $self->error;
            $sql .= @value
                ? $1 . ' (' . join( ', ', map { $self->bind_or_parse_value( $_ ) } @value ) . ')'
                : ( $2 ? ' 1=1' : ' 1=0' );
        }
        elsif ($sql =~ /\b(?:ON\s+DUPLICATE\s+KEY\s+UPDATE|SET)\s*$/si && ref $item eq 'HASH') {
            _error 'Hash has zero elements.' if keys %$item == 0;
            $sql .= " " . join(', ', map {
                my $key = $_;
                my $val = $item->{$key};
                "$key=" .
                    $self->bind_or_parse_value($val);
            } (sort keys %$item));
        }
        elsif ( $sql =~ /\b(REPLACE|INSERT)[\w\s]*\sINTO\s*$ident_rx\s*$/i ) {
            my $type = ref $item;
            my @value
                = 'SCALAR' eq $type ? $$item
                : 'ARRAY'  eq $type ? @$item
                : 'HASH'   eq $type ? do {
                    my @key = sort keys %$item;
                    $sql .= ' (' . join( ', ', @key ) . ')';
                    @$item{ @key };
                }
                : $self->error;
            $sql .= ' VALUES(' . join( ', ', map { $self->bind_or_parse_value( $_ ) } @value ) . ')';
        }
        elsif ($sql =~ /(?:\bFROM|JOIN)\s*$/si) {
            # table reference

            # get alias for table
            my $table_alias = undef; # alias given to table
            my $next_item = $_[$self->idx + 1];
            if(defined $next_item && ref $next_item eq '' &&
               $next_item =~ /\s*AS\b/is)
            {
                $table_alias = undef;  # provided by client
            }
            else {
                $table_alias = 'tbl' . $self->alias_id++;
            }

            $sql .= ' ' unless $sql eq '';
            $sql .= $self->parse_resultset($item);
            $sql .= " AS $table_alias" if defined $table_alias;
        }
        elsif (ref $item eq 'SCALAR') {
            push @$bind, $$item;
            $sql .= ' ?';
        }
        elsif (ref $item eq 'HASH') {  # e.g. WHERE {x = 3, y = 4}
            if (keys %$item == 0) {
                $sql .= ' 1=1';
            }
            else {
                my $s = join ' AND ', map {
                    my $key = $_;
                    my $val = $item->{$key};
                    if (! defined $val) {
                        "$key IS NULL";
                    }
                    elsif (ref $val eq 'ARRAY') {
                        @$val ? do {
                            my @v = map { $self->bind_or_parse_value($_) } @$val;
                            $key . ' IN (' . join( ', ', @v ) . ')';
                        } : '1=0';
                    }
                    else {
                        $key . '=' . $self->bind_or_parse_value($val);
                    }
                } (sort keys %$item);
                $s = "($s)" if keys %$item > 1;
                $s = " $s";
                $sql .= $s;
            }
        }
        elsif (ref $item eq 'ARRAY') {  # result set
            $sql .= ' ' unless $sql eq '';
            $sql .= $self->parse_resultset($item);
        }
        else { $self->error }
    }
    continue { $self->idx++ }

    return $sql;
}

# interpolate value from aggregate variable (hashref or arrayref)
sub bind_or_parse_value {
    my $self = shift;
    my ( $elem ) = @_;
    return $self->parse( $elem ) if ref $elem; # e.g. sql()
    push @{ $self->bind }, $elem;
    return '?';
}

# interpolate result set
#   e.g. [[1,2],[3,4]] or [{a=>1,b=>2},{a=>3,b=>4}].
sub parse_resultset {
    my $self = shift;
    my ($item) = @_;
    my $sql = '';
    if (ref $item eq 'ARRAY') {
        _error 'table reference has zero rows' # improve?
            if @$item == 0;
        my $sql2 = '';
        if(ref $item->[0] eq 'ARRAY') {
            _error 'table reference has zero columns' # improve?
                if @{ $item->[0] } == 0;
            for my $row ( @$item ) {
                my $is_first_row = ($sql2 eq '');
                $sql2 .= ' UNION ALL ' unless $is_first_row;
                $sql2 .=
                    "SELECT " .
                    join(', ', map {
                        $self->bind_or_parse_value($_)
                    } @$row);
            }
        }
        elsif(ref $item->[0] eq 'HASH') {
            _error 'table reference has zero columns' # improve?
                if keys %{ $item->[0] } == 0;
            my $first_row = $item->[0];
            for my $row ( @$item ) {
                my $is_first_row = ($sql2 eq '');
                $sql2 .= ' UNION ALL ' unless $is_first_row;
                $sql2 .=
                    "SELECT " .
                    join(', ', map {
                        my($key, $val) = ($_, $row->{$_});
                        my $sql3 = $self->bind_or_parse_value($val);
                        $sql3 .= " AS $key" if $is_first_row;
                        $sql3;
                    } (sort keys %$first_row));
             }
        }
        else {
            $self->error;
        }
        $sql .= ' ' unless $sql eq '';
        $sql .= "($sql2)";
    }
    else { $self->error }
    return $sql;
}

sub error {
    my $self = shift;
    my $idx = $self->idx;
    my $prev      = $idx > 0       ? $self->items->[$idx-1] : undef;
    my $prev_text = defined($prev) ? " following '$prev'" : "";
    my $cur  = $self->items->[$idx];
    _error "Unrecognized '$cur'$prev_text in interpolation list.";
    return;
}

1;

__END__

=head1 SYNOPSIS

  use SQL::Interp ':all';

  my ($sql, @bind) = sql_interp 'INSERT INTO table', \%item;
  my ($sql, @bind) = sql_interp 'UPDATE table SET',  \%item, 'WHERE y <> ', \2;
  my ($sql, @bind) = sql_interp 'DELETE FROM table WHERE y = ', \2;

  # These two select syntax produce the same result
  my ($sql, @bind) = sql_interp 'SELECT * FROM table WHERE x = ', \$s, 'AND y IN', \@v;
  my ($sql, @bind) = sql_interp 'SELECT * FROM table WHERE', {x => $s, y => \@v};

=head1 DESCRIPTION

This module converts SQL fragments interleaved with variable references into
one regular SQL string along with a list of bind values, suitable for passing
to DBI. This makes database code easier to read as well as easier to write,
while easily providing ready access to all SQL features.

=head1 INTERFACE

The recommended way to use SQL::Interp is via its L<DBIx::Simple> integration,
which provides an excellent alternative to plain DBI access:

  use DBIx::Simple;
  # ...
  my $rows = $db->iquery( '
      SELECT title
      FROM threads
      WHERE date >', \$x, '
      AND subject IN', \@subjects, '
  ' )->arrays;

The C<iquery> method integrates L</sql_interp> directly into L<DBIx::Simple>.

=head2 C<sql_interp>

  ($sql, @bind) = sql_interp @params;

This function rearranges the list of elements it is passed, returning it as an
SQL string with placeholders plus a corresponding list of bind values, suitable
for passing to DBI.

The interpolation list can contain elements of these types:

=over 4

=item B<SQL>

A plain string containing an SQL fragment such as C<SELECT * FROM mytable WHERE>.

=item B<Variable reference>

A scalarref, arrayref, or hashref referring to data to interpolate between the SQL.

=item B<Another interpolation list>

An interpolation list can be nested inside another interpolation list.
This is possible with the L</sql> function.

=back

=head3 Interpolation Examples

The following variable names will be used in the below examples:

 $sref  = \3;                      # scalarref
 $aref  = [1, 2];                  # arrayref
 $href  = {m => 1, n => undef};    # hashref
 $hv = {v => $v, s => $$s};        # hashref containing arrayref
 $vv = [$v, $v];                   # arrayref of arrayref
 $vh = [$h, $h];                   # arrayref of hashref

Let C<$x> stand for any of these.

=head3 Default scalarref behavior

A scalarref becomes a single bind value:

  IN:  'foo', $sref, 'bar'
  OUT: 'foo ? bar', $$sref

=head3 Default hashref behavior

A hashref becomes a logical C<AND>:

  IN:  'WHERE', $href
  OUT: 'WHERE (m=? AND n IS NULL)', $h->{m},

  IN:  'WHERE', $hv
  OUT: 'WHERE (v IN (?, ?) AND s = ?)', @$v, $$s

=head3 Default arrayref of (hashref or arrayref) behavior

I<This is not commonly used.>

  IN:  $vv
  OUT: '(SELECT ?, ? UNION ALL SELECT ?, ?)',
          map {@$_} @$v

  IN:  $vh
  OUT: '(SELECT ? as m, ? as n UNION ALL
            SELECT ?, ?)',
          $vh->[0]->{m}, $vh->[0]->{n},
          $vh->[1]->{m}, $vh->[1]->{n}

  # Typical usage:
  IN: $x
  IN: $x, 'UNION [ALL|DISTINCT]', $x
  IN: 'INSERT INTO mytable', $x
  IN: 'SELECT * FROM mytable WHERE x IN', $x

=head3 Context ('IN', $x)

A scalarref or arrayref can used to form an C<IN> clause. As a convenience,
a reference to an arrayref is also accepted. This way, you can simply provide
a reference to a value which may be a single-valued scalar or a multi-valued
arrayref:

  IN:  'WHERE x IN', $aref
  OUT: 'WHERE x IN (?, ?)', @$aref

  IN:  'WHERE x IN', $sref
  OUT: 'WHERE x IN (?)', $$sref

  IN:  'WHERE x IN', []
  OUT: 'WHERE 1=0'

  IN:  'WHERE x NOT IN', []
  OUT: 'WHERE 1=1'

=head3 Context ('INSERT INTO tablename', $x)

  IN:  'INSERT INTO mytable', $href
  OUT: 'INSERT INTO mytable (m, n) VALUES(?, ?)', $href->{m}, $href->{n}

  IN:  'INSERT INTO mytable', $aref
  OUT: 'INSERT INTO mytable VALUES(?, ?)', @$aref;

  IN:  'INSERT INTO mytable', $sref
  OUT: 'INSERT INTO mytable VALUES(?)', $$sref;

MySQL's C<REPLACE INTO> is supported the same way.

=head3 Context ('SET', $x)

  IN:  'UPDATE mytable SET', $href
  OUT: 'UPDATE mytable SET m = ?, n = ?', $href->{m}, $href->{n}

MySQL's C<ON DUPLICATE KEY UPDATE> is supported the same way.

=head3 Context ('FROM | JOIN', $x)

I<This is not commonly used.>

  IN:  'SELECT * FROM', $vv
  OUT: 'SELECT * FROM
       (SELECT ?, ? UNION ALL SELECT ?, ?) as t001',
       map {@$_} @$v

  IN:  'SELECT * FROM', $vh
  OUT: 'SELECT * FROM
       (SELECT ? as m, ? as n UNION ALL SELECT ?, ?) as temp001',
       $vh->[0]->{m}, $vh->[0]->{n},
       $vh->[1]->{m}, $vh->[1]->{n}

  IN:  'SELECT * FROM', $vv, 'AS t'
  OUT: 'SELECT * FROM
       (SELECT ?, ? UNION ALL SELECT ?, ?) AS t',
       map {@$_} @$v

  # Example usage (where $x and $y are table references):
  'SELECT * FROM', $x, 'JOIN', $y

=head3 Other Rules

Whitespace is automatically added between parameters:

 IN:  'UPDATE', 'mytable SET', {x => 2}, 'WHERE y IN', \@colors;
 OUT: 'UPDATE mytable SET x = ? WHERE y in (?, ?)', 2, @colors

Variables must be passed as references; otherwise, they will
processed as SQL fragments and interpolated verbatim into the
result SQL string, negating the security and performance benefits
of binding values.

In contrast, any scalar values I<inside> an arrayref or hashref are by
default treated as binding variables, not SQL.  The contained
elements may be also be L</sql>.

=head2 C<sql>

  sql_interp 'INSERT INTO mytable',
      {x => $x, y => sql('CURRENT_TIMESTAMP')};
  # OUT: 'INSERT INTO mytable (x, y) VALUES(?, CURRENT_TIMESTAMP)', $x

This function is useful if you want to use raw SQL as the value in an arrayref or hashref.

=head1 PHILOSOPHY

B<The query language is SQL.> There are other modules, such as
L<SQL::Abstract>, that hide SQL behind method calls and/or Perl
data structures (hashes and arrays). The former may be undesirable in some
cases since it replaces one language with another and hides the full
capabilities and expressiveness of your database's native SQL language. The
latter may load too much meaning into the syntax of C<{}>, C<[]> and C<\>, thereby
rendering the meaning less clear:

  SQL::Abstract example:
  %where = (lname => {like => '%son%'},
            age   => [-and => {'>=', 10}, {'<=', 20}])
  Plain SQL:
  "lname LIKE '%son' AND (age >= 10 AND age <= 20)"

In contrast, SQL::Interp does not abstract away your SQL but rather makes it
easier to interpolate Perl variables into it. Now, SQL::Interp I<does> overload
some meaning into C<{}>, C<[]> and C<\>, but the aim is to make common obvious
cases easier to read and write E<mdash> and leave the rest to raw SQL.

This also means SQL::Interp does not need to support every last feature of each
particular dialect of SQL: if you need one of these, just use plain SQL.

=head1 LIMITATIONS

Some types of interpolation are context-sensitive and involve examination of
your SQL fragments. The examination could fail on obscure syntax, but it is
generally robust. Look at the examples to see the types of interpolation that
are accepted. If needed, you can disable context sensitivity by inserting a
null-string before a variable.

 "SET", "", \$x

A few things are just not possible with the C<'WHERE', \%hashref>
syntax, so in such case, use a more direct syntax:

  # ok--direct syntax
  sql_interp '...WHERE', {x => $x, y => $y}, 'AND y = z';
  # bad--trying to impose a hashref but keys must be scalars and be unique
  sql_interp '...WHERE',
      {sql($x) => sql('x'), y => $y, y => sql('z')};

In the cases where this module parses or generates SQL fragments, this module
should work for many databases, but is known to work well on MySQL and
PostgreSQL.
