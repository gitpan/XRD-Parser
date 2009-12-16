use strict;
use warnings;

use inc::Module::Install;

license             'perl';

name                'XRD-Parser';
author              'Toby Inkster <tobyink@cpan.org>';
perl_version_from   'lib/XRD/Parser.pm';
version_from        'lib/XRD/Parser.pm';
abstract_from       'lib/XRD/Parser.pm';

test_requires       'Test::More' => '0.61';

requires            'LWP::UserAgent' => 0;
requires            'XML::LibXML'    => 0;
requires            'RDF::Trine'     => 0;
requires            'LWP::Simple'    => 0;
requires            'URI::Escape'    => 0;
requires            'URI'            => 0;
requires            'HTTP::Link::Parser' => '0.03';

auto_set_homepage;
auto_set_bugtracker;

include 'Test::Signature';
auto_install;
WriteAll(
	'meta' => 1,
	'sign' => 1,
	);