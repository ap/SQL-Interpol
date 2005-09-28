package SQL::Interpolate::Macro;
use strict;
use base qw(Exporter);
use SQL::Interpolate; # for $VERSION

our $VERSION = $SQL::Interpolate::VERSION;
our @EXPORT;
our %EXPORT_TAGS = (all => [qw(
    relations
    sql_and
    sql_fragment
    sql_if
    sql_link
    sql_or
    sql_paren
    sql_rel
)]);
our @EXPORT_OK = @{$EXPORT_TAGS{'all'}};

sub sql_fragment
{
    return SQL::Interpolate::Fragment->new(@_);
}

sub sql_and
{
    return SQL::Interpolate::And->new(@_);
}

sub sql_or
{
    return SQL::Interpolate::Or->new(@_);
}

sub sql_if
{
    return SQL::Interpolate::If->new(@_);
}

sub sql_rel
{
    return SQL::Interpolate::Rel->new(@_);
}

sub sql_link
{
    return SQL::Interpolate::Link->new(@_);
}

sub sql_paren
{
    return SQL::Interpolate::Paren->new(@_);
}

sub relations
{
    return SQL::Interpolate::RelProcessor->new(@_);
}

# [private]
# Given instances of two relations, generate the SQL to link them.
# For example,
#   ['Sp', ['S', 'p'], $sales_ord_line] and
#   ['p', ['p'], $part]
# gives "Sp.part_nbr = p.part_nbr".
# params:
#   $e1 - entity 1
#   $e2 - entity 2
# where each instance is a arrayref of an entity relation name, a arrayref of
#   names of contained entities, and a relation
#   specification (as passed into C<relations>).
sub _single_link_sql
{
    my($e1, $e2) = @_;
#    die Data::Dumper::Dumper($e1, $e2);
    my($left_idx, $right_idx);
    if   ($e1->[1]->[0] eq  $e2->[1]->[0])
        { $left_idx=0; $right_idx=0; }
    elsif(defined($e2->[1]->[1]) && $e1->[1]->[0] eq  $e2->[1]->[1])
        { $left_idx=0; $right_idx=1; }
    elsif(defined($e1->[1]->[1]) && $e1->[1]->[1] eq  $e2->[1]->[0])
        { $left_idx=1; $right_idx=0; }
    elsif(defined($e1->[1]->[1]) && defined($e2->[1]->[1]) &&
              $e1->[1]->[1] eq  $e2->[1]->[1])
        { $left_idx=1; $right_idx=1; }
    else {
        die "Invalid SQL link [$e1->[0] to $e2->[0]]";
    }

    my $sql = "$e1->[0].$e1->[2]->{key}->[$left_idx ]" . " = " .
              "$e2->[0].$e2->[2]->{key}->[$right_idx]";

    return $sql;
}


1;

package SQL::Interpolate::SQLFilter;
use strict;
#IMPROVE: package name?

1;

package SQL::Interpolate::RelProcessor;
our @ISA = qw(SQL::Interpolate::SQLFilter);
use strict;

sub new
{
    my($class, $relations) = @_;
    return bless {
        relations => $relations,
        keys => {}
    }, $class;
}

sub init
{
    my $self = shift;
    $self->{keys} = {};
}

sub macro_names
{
    return ('SQL::Interpolate::Rel', 'SQL::Interpolate::Link');
}

sub filter_text
{
    my($self, $sql) = @_;
    while(my($name, $key) = each %{$self->{keys}}) {
        $sql =~ s{ (?<!as\ ) \b \Q$name\E \b (?! \.) }{$key}xge;
    }
    return $sql;
}

sub filter_text_fragment
{
    my($self, $sql, $changing_ref) = @_;
    my @out;
    pos($sql) = 0;
    my $pos0 = pos($sql);
    until($sql =~ /\G$/gc) {
        my $pos1 = pos($sql);
        if($sql =~ m{\G \b REL \( (.*?) \)}xsgc) {
            push @out, substr($sql, $pos0, $pos1 - $pos0) if $pos1 != $pos0;
            $pos0 = pos($sql);
            push @out, SQL::Interpolate::Rel->new($1);
            $$changing_ref = 1;
        }
        elsif($sql =~ m{\G \b LINK \( (.*?) \)}xsgc) {
            push @out, substr($sql, $pos0, $pos1 - $pos0) if $pos1 != $pos0;
            $pos0 = pos($sql);
            my $params = $1;
            my @params = split /,/, $params;
            s{^\s*|\s*$}{}gs for @params;
            push @out, SQL::Interpolate::Link->new(@params);
            $$changing_ref = 1;
        }
        else {
            $sql =~ m{\G.[^RL]*}xsgc;
        }
    }
    my $pos1 = pos($sql);
    push @out, substr($sql, $pos0, $pos1 - $pos0) if $pos1 != $pos0;
    #die Data::Dumper::Dumper(\@out);
    return @out;
}

1;

package SQL::Interpolate::Rel;
use strict;
our @ISA = qw(SQL::Interpolate::Macro);

sub new
{
    my($class, $name) = @_;

    my $self = bless [
        $name
    ], $class;
    return $self;
}

sub expand
{
    my($self, $interp, $filter) = @_;

    my $keys = $filter->{keys};
    my $name = $self->[0];
    my $sql;
#    die Data::Dumper::Dumper($filter);
    for my $relation_name (keys %{$filter->{relations}}) {
        my $relation = $filter->{relations}->{$relation_name};
        my $name_re = $relation->{name};

        if($name =~ /($name_re)/s) {
            my($name, $name1, $name2) = ($1, $2, $3);

            $keys->{$name1} = "$name.$relation->{key}->[0]";
            $keys->{$name2} = "$name.$relation->{key}->[1]"
                if defined $relation->{key}->[1];
            $sql = "$relation_name as $name";
            last;
        }
    }
    if(! defined $sql) {
        die "Unrecognized relation REL($name).";
    }
    return $sql;
}

1;

package SQL::Interpolate::Link;
use strict;
our @ISA = qw(SQL::Interpolate::Macro);

sub new
{
    my($class, @rels) = @_;

    my $self = bless [
        @rels
    ], $class;
    return $self;
}

sub expand
{
    my($self, $interp, $filter) = @_;

    #old? my @params = split /,/, $params;  $param =~ s{^\s*|\s*$}{}gs;
    my @params = @$self;

    my $good = 1;
    my $last;
    for my $param (@params) {
        my $match = 0;
        done_param:
        for my $relation (values %{$filter->{relations}}) {
            my $name_re = $relation->{name};
            if($param =~ /($name_re)/gs) {
                my($name, $name1, $name2) = ($1, $2, $3);
                $param = [$name, [$name1, defined($name2) ? $name2 : ()], $relation];
                $match = 1;
                last done_param;
            }
        }
        if(!$match) {
            die "Invalid param [$param] in LINK macro in SQL template.";
        }
    }

    # relations touching entities.
    my %links;
    my @sql_snips;
    for my $param (@params) {
        for my $entity (@{$param->[1]}) {
            if(defined $links{$entity}) {
                #print Dumper($entity, $links{$entity}, $param), "\n";
                push @sql_snips,
                    SQL::Interpolate::Macro::_single_link_sql($links{$entity}, $param);
            }
        }
        $links{$param->[1]->[0]} = $param;
        $links{$param->[1]->[1]} = $param if defined $param->[1]->[1];
    }

    my $sql = join ' AND ', @sql_snips;

    $sql = "($sql)" if @sql_snips > 1;

    return $sql;
}

1;

package SQL::Interpolate::Paren;
use strict;
our @ISA = qw(SQL::Interpolate::Macro);

sub new
{
    my($class, @elements) = @_;

    my $self = bless [
        @elements
    ], $class;
    return $self;
}

sub expand
{
    my($self, $interp, $filter) = @_;
    return ('(', @$self, ')');
}

1;

package SQL::Interpolate::Fragment;
use strict;
our @ISA = qw(SQL::Interpolate::Macro);

sub new
{
    my($class, @elements) = @_;

    my $self = bless [
        @elements
    ], $class;
    return $self;
}

sub expand
{
    my($self, $interp, $filter) = @_;
    return @$self;
}

1;

package SQL::Interpolate::And;
use strict;
our @ISA = qw(SQL::Interpolate::Macro);

sub new
{
    my($class, @elements) = @_;

    my $self = bless [
        @elements
    ], $class;
    return $self;
}

sub expand
{
    my($self, $interp, $filter) = @_;
    if(@$self == 0) { return '1=1'; } # trivial case
    else {
        my @out = map {('AND',
            ref($_) eq '' || UNIVERSAL::isa($_, 'SQL::Interpolate::Macro') ?
            ('(', $_, ')') : $_
        )} @$self;
        shift @out;
        return '(', @out, ')';
    }
}

1;

package SQL::Interpolate::Or;
use strict;
our @ISA = qw(SQL::Interpolate::Macro);

sub new
{
    my($class, @elements) = @_;

    my $self = bless [
        @elements
    ], $class;
    return $self;
}

sub expand
{
    my($self, $interp, $filter) = @_;
    if(@$self == 0) { return '1=0'; } # trivial case
    else {
        my @out = map {('OR',
            ref($_) eq '' || UNIVERSAL::isa($_, 'SQL::Interpolate::Macro') ?
            ('(', $_, ')') : $_
        )} @$self;
        shift @out;
        return '(', @out, ')';
    }
}

1;

package SQL::Interpolate::If;
use strict;
our @ISA = qw(SQL::Interpolate::Macro);

sub new
{
    my($class, $condition, $value_if_true) = @_;

    my $self = bless [
        $condition, $value_if_true
    ], $class;
    return $self;
}

sub expand
{
    my($self, $interp, $filter) = @_;
    return $self->[0] ? $self->[1] : ();
}

1;

__END__

=head1 NAME

SQL::Interpolate::Macro - Macros and SQL filters for SQL::Interpolate

=head1 SYNOPSIS

 use SQL::Interpolate qw(:all);
 use SQL::Interpolate::Macro qw(:all);

 # Macros that assist in SQL building
 sql_interp
     q[SELECT * FROM mytable WHERE],
     sql_and( sql_if($blue,  q[color = "blue"]),
              sql_if($shape, sql_fragment(q[shape =], \$shape)) ),
     q[LIMIT 10]

 # Macros that perform automatic table joining
 # first specify database layout
 my $interp = new SQL::Interpolate(
     relations(
         sales_order      => {name => qr/([S-T])/, key => ['so_nbr']},
         part             => {name => qr/([p-r])/, key => ['part_nbr']},
         sales_order_line => {name => qr/([S-T])([p-r])/,
                              key => ['so_nbr', 'part_nbr']}
     )
 )->make_sql_interp();
 # then do queries
 ($sql, @bind) = $interp->(q[
     SELECT * FROM REL(S), REL(Sp), REL(p)
     WHERE LINK(S,Sp,p) AND S = 123
 ]);
 # Generates SQL
 #   SELECT *
 #   FROM  sales_order as S, sales_order_line as Sp, part as p
 #   WHERE S.so_nbr = Sp.so_nbr AND Sp.part_nbr = p.part_nbr AND
 #         S.so_nbr = 123
  

=head1 DESCRIPTION


=head3 Macros

This module provides an assortment of macros and SQL filters for
SQL::Interpolate.  These allow you to write simpler, more robust, and
possibly more portable queries.  You can also write your own macros
and filters.

Macros are objects derived from SQL::Interpolate::Macro, and they
expand to ordinary string and variable reference interpolation
elements before processing.  Macros may also exist as a convenience as
"stringified macros" within strings; these are converted into real
macro objects before processing (e.g. "WHERE LINK(AB,BC) AND x=y" ==>
"WHERE ", link('AB','BC'), " AND x=y").  Also, if enabled, source
filtering internally converts sql// quotes into macro objects.

Macro expansion is performed by the C<sql_flatten> function.  This
converts stringified macros into real macro objects and expands macro
objects into other interpolation elements (strings, variable
references, and macro objects).  The process can be recursive since
the expansion of a macro may contain other macros (e.g. nested
macros).

The transformation process contains a number of extension hooks where
the client may insert code to transform the interpolation list and/or
the final $sql string.  Refer to
L<SQL::Interpolate::Macro|SQL::Interpolate::Macro> for details on
macros.

An I<SQL filter> is an object derived from
SQL::Interpolate::SQLFilter.  SQL filters can be used by
sql_interp() and sql_flatten() to assist in macro expansion and/or
filtering of SQL text.  The filtering on SQL text can occur before
and/or after the SQL fragments are interpolated into a single string.

=head2 Motivation

Though a minor point, say you didn't match the parenthesis correctly
in your interpolation list:

  "WHERE thid IN", \@thids,
  "AND ( action IN", \@admin_aids,
         "OR action IN", ['post', 'reply', 'edit']

This error would be caught at runtime rather than (earlier) at
compile time as follows:

  "WHERE thid IN", \@thids,
  "AND", sql_fragment("action IN", \@admin_aids,
                      "OR action IN", ['post', 'reply', 'edit']
                      # -- syntax error by compiler

Another example may be to handle trivial cases simply, similar to
how SQL::Interpolate handle the special case where the list after an
IN is of size zero.  For example,

 my @conditions;  # list of SQL conditional statements.
 ...
 sql_interp "x=2 AND", sql_and(@conditions);

Normally, this would be expanded to

 "(", "x=2 AND", $conditions[0], 'AND', $conditions[1], 'AND', ...., ")"

Now, what if @conditions == 0?  If we define

 sql_and()  -->  "1=1"         # analogous to x^0 = 1
 sql_or()   -->  "1=0"         # analogous to x*0 = 0

then proper SQL is generated in the above case:

 "x=2 AND", "1=1"
 # equivalent to "x=2"

=head2 Built-in Macros and Filters

This module comes with various macros and SQL filters already implemented.
See the sections below.

=head2 Writing your own macros

Here is a simple example of writing your own macro.  It is based on the
sql_paren() macro provided by this module.

  sub myparen
  {
      return '(', @_, ')';
  }

Example usage:

  my($sql, @bind) = sql_interp "WHERE", my_paren("x=", \$x);
  # Sets
  #   $sql = "WHERE (x=?)";
  #   @bind = $x;

This simple expansion is not always sufficient though.  In the
following cases, macro expansion should not occur at the time Perl
calls the macro function but rather later during the execution of
C<sql_interp()>.

(1) if the macro expansion needs data that is available only to an
instance of SQL::Interpolate (e.g. a database handle that is interrogated
to generate database-dependent SQL) or

(2) to support recursive macros properly, e.g.

  sql_interp sql_and(sql_or('x=y', 'y=z'), 'z=w')
  # don't expand to
  #   sql_interp sql_and('(', 'x=y', 'y=z', ')', 'z=w')
  # which would expand to
  #   sql_interp '(', 'AND', 'x=y', 'AND', 'y=z', 'AND', ')', 'AND', 'z=w'
  # but rather to
  #   sql_interp '(', sql_or('x=y', 'y=z'), 'AND', 'z=w', ')'
  # which would expand to
  #   sql_interp '(', '(', 'x=y', 'OR', 'y=z', ')', 'AND', 'z=w', ')'

Notice how the expansion in the last example must be
done outside-to-in rather inside-to-out.

The following more complicated design pattern can be used in such cases.

  # helper function for simpler syntax
  sub myparen
  {
      return MyParen->new(@_);
  }
  
  # Macro class
  package MyParen;
  our @ISA = qw(SQL::Interpolate::Macro);
  
  sub new
  {
      my($class, @elements) = @_;
  
      my $self = bless [
          @elements
      ], $class;
      return $self;
  }
  
  sub expand
  {
      my($self, $interp, $filter) = @_;
      my $dbh = $interp->{dbh};
      if(defined($dbh) && $dbh->{Driver}->{Name} eq 'funnydatabase') {
          return ('leftparen', @$self, 'rightparen');
      }
      else {
          return ('(', @$self, ')');
      }
  }
  1;

Here, C<myparen> returns a macro object, which can be placed onto
an interpolation list.  During the flattening phase, C<sql_interp()>
will call the C<expand()> method on the macro object, and this method
must return the interpolation list that the macro will expand to.  If
the interpolation list macro contains macros itself, an sql_interp will
further expand these.  See also L<SQL::Interpolate/macros>.

=head2 Writing your own SQL filters

The following is an example that expands on the previous example
but supporting a stringified version of the macro in the SQL literals.

  # example usage:
  my $interp = new SQL::Interpolate(myparen_filter())->make_closure();
  ($sql, @bind) = $interp->("SELECT * FROM mytable WHERE MYPAREN(x=y)");
  # Equivalent to:
  #   sql_interp "SELECT * FROM mytable WHERE ", 
  
  # helper function for simpler syntax
  sub myparen_filter
  {
      return MyParenFilter->new(@_);
  }
  
  # SQL Filter class
  package MyParenFilter;
  our @ISA = qw(SQL::Interpolate::SQLFilter);
  
  # Filter a single SQL string fragment (during expansion)
  sub filter_text_fragment
  {
      my($self, $sql, $changing_ref) = @_;
      my @out;
      pos($sql) = 0;
      my $pos0 = pos($sql);
      until($sql =~ /\G$/gc) {
          my $pos1 = pos($sql);
          if($sql =~ m{\G \b MYPAREN \( (.*?) \)}xsgc) {
              push @out, substr($sql, $pos0, $pos1 - $pos0) if $pos1 != $pos0;
              $pos0 = pos($sql);
              push @out, SQL::Interpolate::Rel->new($1);
              $$changing_ref = 1;
          }
          else {
              $sql =~ m{\G.}xsgc; # more efficient: \G.[^P]*
          }
      }
      my $pos1 = pos($sql);
      push @out, substr($sql, $pos0, $pos1 - $pos0) if $pos1 != $pos0;
      return @out;
  }
  1;

Your SQL filter may optionally have a few other methods that will be called by
sql_interp if they exist.  See L</SQL::Interpolate::Macro Methods> for
details.

=head2 Related Macros and Filters

=head3 Automatic table linking

The macro processor can simplify table joins by aliasing your tables
and keys using special naming conventions and by writing the SQL
expressions that link tables together via keys.  It can be particularly
useful to represent recursive data structures in a relational database,
where the table join requirements are complicated.

This allows one to write

  my $dbx = new DBIx::Interpolate(
      relations(
          sales_order      => {name => qr/^([S-T])$/, key => ['so_nbr']},
          part             => {name => qr/^([p-r])$/, key => ['part_nbr']},
          sales_order_line => {name => qr/^([S-T])([p-r])$/,
                           key => ['so_nbr', 'part_nbr']}
  ));
  
  $dbx->selectall_arrayref(q[
      SELECT * FROM REL(S), REL(Sp), REL(p)
      WHERE LINK(S,Sp,p) AND S = 123
  ]);

instead of

  $dbh->selectall_arrayref(q[
      SELECT *
      FROM  sales_order        as S,
            sales_order_line   as Sp,
            part               as p
      WHERE S.so_nbr = Sp.so_nbr AND Sp.part_nbr = p.part_nbr AND
            S.so_nbr = 123
  ]);

or

  $dbh->selectall_arrayref(q[
      SELECT *
      FROM  sales_order      as S  JOIN
            sales_order_line as Sp USING (so_nbr) JOIN
            part             as p  USING (part_nbr)
  ]);


The above example prints part information for all the line items on
sales order #123.

The table naming convention is that the names resemble the entities
related by the tables, and this is a natural way to write queries.  In
the above example the entities are sales order (S) and part (p), which
are related by the table represented by the juxtaposition "Sp".
The LINK(S,Sp,p) macro expands by equating the keys in the given relations
that identity the same entities according to the naming convention:

 "S" and "Sp" share S --> "S.so_nbr = Sp.so_nbr"
 "Sp" and "p" share p --> "Sp.part_nbr = p.part_nbr"

Also, any entity written alone in an expression (e.g. "S" in "S = 123" above)
is considered to be shorthand for primary key in a relation representing
that entity:

 "S = 123" --> "S.so_nbr = 123"

The C<relations()> function used above generates an object
that describes how the naming conventions map to your database
schema.  The meaning in this example is as follows.  sales order
entities can be represented by the names "S" and "T".  Part entities
can be represented by the names "p", "q", and "r".  The juxtaposition
of one of each entity name represents a row in the sales_order_line
relation that relates these two entities.  The entity names in the
matches must be surrounded by capturing parenthesis ().  The names in
the key list correspond respectively to the entities captured in the
match, and these names are the names of the primary or foreign keys
(in the current relation) that identify the entities.  For example,
"Sp" represents a row in the sales_order_line table, with represents a
relationship between a sales order entity (S) and part entity (p).
The key for S is given by Sp.so_nbr, and the key for p is given by
Sp.part_nbr.

It is possible to use two entities in the same class:

 SELECT * FROM REL(Sp), REL(Tp) WHERE LINK(Sp,Tp)

Also, say the product structure is represented in the database,
where parts can contain (subcomponent) parts, which can in turn contain
other (subcomponent) parts.

 relations(...
     part_part => {
         name => qr/^([p-r])([p-r])$/, key => ['part_nbr1', 'part_nbr2']}
 )

Now, it is possible to use two entities in the same class in the same
relation:

 SELECT * FROM REL(pq), REL(qs) WHERE LINK(pq,qs)

Table linking is not limited to SELECT statements either:

 UPDATE REL(Sp), REL(p)
 SET p.color = 'blue'
 WHERE LINK(Sp,p) AND S = $sonbr

The utility of automatic table linking is probably best shown by a
real-world example.  See Meset::MessageBoard in the L</SEE ALSO>
section.

=head3 Future Changes

REL(AB,BC) could be expanded into an "x as AB JOIN y as BC on condition" or
"x as AB JOIN y as BC USING(...)"  Do all major databases support this syntax?
The juxtaposition of the JOIN and the linking condition could eliminate the
need for the separate LINK(...) macro:

  SELECT * FROM REL(AB,BC) WHERE A = ?

=head2 Functions that create SQL filter objects

=over 4

=item C<relations>

Creates an SQL filter for performing automatic table linking.
This creates an SQL::Interpolate::RelProcessor object derived from
SQL::Interpolate::SQLFilter.

Sets information on relations (tables) for this objects.

 relations({
    messageset => {name => qr/^[A-Z]$/, key => 'msid'},
    message    => {name => qr/^[m-p]$/, key => 'mid'},
    messageset_message => {foreign => {
       msid => 'messageset',
       mid  => 'message'
    }},
    messageset_messageset => {foreign => {
       msid_1 => 'messageset',
       msid_2 => 'messageset'
    }},
 });

Expands simplified SQL such as

    SELECT * FROM REL(Am), REL(m)
    WHERE  A = ? AND LINK(Am, m)

into standard SQL:

    SELECT * FROM messageset_message as Am, message as m
    WHERE  Am.msid = ? AND Am.mid = m.mid

The SQL may contain these macros:

  REL(...) - identify relation and entities on it.
     This is converted into an SQL::Interpolate::Rel object.

  LINK(X,Y,Z,...) forms a Boolean expression linking the provided
    relations.  There must be at least one parameter (typically two).
     This is converted into an SQL::Interpolate::Link object.

 $sql_in : string - input string

 $sql_out : string - output string

Possible improvements:

 - support multi-part keys?
 - support optional automatic inclusion of LINK(...).

See the L</Automatic table linking> section above.

=back

=head2 Functions that create macro objects

=over 4

=item C<sql_and>

  $macro = sql_and(@predicates);

Creates a macro object (SQL::Interpolate::And) representing
a Boolean AND over a list of predicates.

  ($sql, @bind) = sql_interp sql_and(
      'x=y', 'y=z or z=w', sql_fragment('z=', \3))

Generates

  ('((x=y) AND (y=z or z=w) AND (z=?))', 3)

If the @predicates list is empty, a '1=1' is returned.

  ($sql, @bind) = sql_interp 'x=y AND', sql_and(@predicates);
  # Result: $sql = 'x=y AND 1=1';

Predicates are surrounded by parenthesis if possibly needed.

=item C<sql_or>

  $macro = sql_or(@predicates);

Creates a macro object (SQL::Interpolate::Or) representing
a Boolean OR over a list of predicates.

  ($sql, @bind) = sql_interp sql_or('x=y', 'y=z', sql_paren('z=', \3))

Generates

  ('(x=y OR y=z OR 'z=?)', 3)

If the @predicates list is empty, a '1=0' is returned.

  ($sql, @bind) = sql_interp 'x=y OR', sql_or(@predicates);
  # Generates $sql = 'x=y OR 1=0';

Predicates are surrounded by parenthesis if possibly needed.

=item C<sql_fragment>

  $macro = sql_fragment(@objs)

Creates a macro object (SQL::Interpolate::SQL) that is container
for a list of interpolation objects.

  ($sql, @bind) = sql_interp sql_fragment('x=y AND z=', \3)
  # Generates ($sql, @bind) = ('x=y and z=?', 3);

=item C<sql_paren>

  $macro = sql_paren(@objs)

Creates a macro object (SQL::Interpolate::Paren) representing
a list of interpolation objects to be surrounded by parenthesis.

  ($sql, @bind) = sql_interp sql_paren('x=y AND z=', \3)
  # Generates ($sql, @bind) = ('(x=y and z=?)', 3);

Typically, the size of the interpolation list should not be zero.

=item C<sql_if>

  $macro = sql_if($condition, $value_if_true)

Creates a macro object (SQL::Interpolate::If) that expands to the given
value if the condition is true, else if expands to the empty list ().

  ($sql, @bind) = sql_interp
      sql_and(
          sql_if($blue,  q[color = "blue"]),
          sql_if($shape, sql_paren(q[shape =], \$shape)) )
  # Generates one of
  #   q[(color = "blue" AND shape = ?)], $shape
  #   q[(color = "blue")]
  #   q[(shape = ?)], $shape
  #   1=1, $shape

sql_if is similar to

  $condition ? $value_if_true : ()

except that it is a macro and therefore is evaluated at the time that
C<sql_interp> is processed.

=item C<sql_rel>

  $macro = sql_rel($alias)

Creates a macro object (SQL::Interpolate::Rel) that expands to a table and alias
definition based on the database description given in relations(...).
See the L</Automatic table linking> section above.

=item C<sql_link>

  $macro = sql_link(@aliases)

Creates a macro object (SQL::Interpolate::Link) that expands to a
table join condition definition based on the database description given
in relations(...).

See the L</Automatic table linking> section above.

=back

=head2 SQL::Interpolate::SQL::Filter Methods

The following methods are implemented by SQL filter objects (i.e. objects
derived from SQL::Interpolate::SQLFilter).  Many of these methods are
called by C<sql_interp> during interpolation, and many of these
methods are optional.

=over 4

=item C<init>

Initializes (or reinitializes) the macro object.
This is called by C<sql_interp> each time C<sql_interp> begins processing.
This method is optional.

  sub init
  {
      my($self) = @_;
      ... reinitialize self
  }

You may use this to reset any state.

=item C<macro_names>

Returns a list of macro types for macros that the filter object should
be passed to upon macro expansion.  This method is optional.

  sub init
  {
      my($self) = @_;
      return ('SQL::Interpolate::Rel', 'SQL::Interpolate::Link');
  }
  # now sql_interp will pass the filter object to
  # SQL::Interpolate::Rel->expand() and
  # SQL::Intrpolate::Link->expand().

This can be useful if the macro needs to read or write state in the filter.

=item C<filter_text_fragment>

Filter a single SQL string fragment (during expansion).
This method is optional.

  sub filter_text_fragment
  {
      my($self, $sql, $changing_ref) = @_;
      if($sql =~ /.../) { # match
          $$changing_ref = 1;
          ... expand and return new list of interpolation entities
          return (...);
      }
      else {
          return $sql; # return original string (unmodified)
      }
  }

One use of filter_text_fragment is to expand any macros embedded inside strings.

  "SELECT REL(AB), REL(BC) ..."

expands to

  "SELECT ", rel('AB'), ', ', rel('BC'), ' ...'

=item C<filter_text>

Filter SQL string (after it has been expanded and
joined into a single string).
This method is optional.

  sub filter_text
  {
      my($self, $sql) = @_;
      ...transform $sql...
      return $sql;
  }

Compare this to C<filter_text_fragment>, which is processed earlier.

=back

=head2 SQL::Interpolate::Macro Methods

The following methods are implemented by macro objects (i.e. objects
derived from SQL::Interpolate::Macro).  Many of these methods are
called by C<sql_interp> during interpolation.

=over 4

=item C<expand>

Expands macro to an expanded interpolation list.

  sub expand {
      my($self, $interp, $filter) = @_;
      ...expand self to interpolation list @list, possibly
         referring to $interp and $filter...
      return @list;
  }

This is called by C<sql_interp> to expand the given macro object.

=back

=head2 Exports and Use Parameters

=over 4

=item EXPORTS

 use SQL::Interpolate::Macro qw(:all);

':all' exports these functions:
relations,
sql_and,
sql_fragment,
sql_if,
sql_link,
sql_or,
sql_paren,
sql_rel
.

=back

=head1 LIMITATIONS

This macro facilities are still under development, so interfaces could
change and may particularly affect you if you are writing your own
macros.

The utility of macros over just plain SQL has been questioned.  A healthy
balance can probably be made: use macros only when they are elucidate rather
than obscure and when they improve robustness and simplicity of the syntax.

=head1 CONTRIBUTORS

David Manura (http://math2.org/david)--author.
Feedback incorporated from Mark Stosberg on table linking, SQL LIMIT,
and things.

=head1 LEGAL

Copyright (c) 2004-2005, David Manura.
This module is free software. It may be used, redistributed
and/or modified under the same terms as Perl itself.
See L<http://www.perl.com/perl/misc/Artistic.html>.

=head1 SEE ALSO

Other modules in this distribution:
L<SQL::Interpolate|SQL::Interpolate>,
L<SQL::Interpolate::Filter|SQL::Interpolate::Filter>,
L<DBIx::Interpolate|DBIx::Interpolate>.

Full example code of automatic table linking - Meset::MessageBoard in Meset
(L<http://math2.org/meset>).

Dependencies: L<DBI|DBI>.

Related modules: L<SQL::Abstract|SQL::Abstract>,
L<DBIx::Abstract|DBIx::Abstract>,
L<Class::DBI|Class::DBI>.

