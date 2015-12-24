requires 'perl', '5.006';
requires 'strict';
requires 'warnings';
requires 'constant';
requires 'Carp';

requires 'Exporter::Tidy';
requires 'Object::Tiny::Lvalue';

on test => sub {
	requires 'Data::Dumper';
	requires 'Test::Differences';
	requires 'Test::More', '0.88';
};

# vim: ft=perl
