package XRD::Parser;

=head1 NAME

XRD::Parser - Parse XRD files into RDF::Trine models

=head1 VERSION

0.01

=cut

use 5.008001;
use strict;

our $VERSION = '0.01';

use Encode qw(encode_utf8);
use LWP::Simple;
use RDF::Trine;
use URI::Escape;
use URI::URL;
use XML::LibXML qw(:all);

=head1 SYNOPSIS

  use RDF::Query;
  use XRD::Parser;
  
  my $parser = XRD::Parser->new(undef, "http://example.com/foo.xrd");
  $parser->consume;
  
  my $results = RDF::Query->new(
    "SELECT * WHERE {?who <http://spec.example.net/auth/1.0> ?auth.}")
    ->execute($parser->graph);
	
  while (my $result = $results->next)
  {
    print $result->{'auth'}->uri . "\n";
  }

=head1 DESCRIPTION

While XRD has a rather different history, it turns out it can mostly
be thought of as a serialisation format for a limited subset of
RDF.

This package ignores the order of <Link> elements, as RDF is a graph
format with no concept of statements coming in an "order". The XRD spec
says that grokking the order of <Link> elements is only a SHOULD. That
said, if you're concerned about the order of <Link> elements, the
callback routines allowed by this package may be of use.

This package aims to be roughly compatible with RDF::RDFa::Parser's
interface.

=over 8

=item $p = XRD::Parser->new($content, $uri, \%options, $store);

This method creates a new XRD::Parser object and returns it.

The $content variable may contain an XML string, or a XML::LibXML::Document.
If a string, the document is parsed using XML::LibXML::Parser, which may throw an
exception. XRD::Parser does not catch the exception.

$uri the supposed URI of the content; it is used to resolve any relative URIs found
in the XRD document. Also, if $content is empty, then XRD::Parser will attempt
to retrieve $uri using LWP::Simple.

Options [default in brackets]:

  * tdb_service     - thing-described-by.org when possible. [0] 

$storage is an RDF::Trine::Storage object. If undef, then a new
temporary store is created.

=cut

sub new
{
	my $class   = shift;
	my $content = shift || undef;
	my $baseuri = shift || undef;
	my $options = shift || undef;
	my $store   = shift || undef;
	my $domtree;
	
	unless (defined $content)
	{
		$content = &get($baseuri);
	}
	
	if (UNIVERSAL::isa($content, 'XML::LibXML::Document'))
	{
		($domtree, $content) = ($content, $content->toString);
	}
	else
	{
		my $xml_parser = XML::LibXML->new;
		$domtree = $xml_parser->parse_string($content);
	}
	
	$store = RDF::Trine::Store::DBI->temporary_store
		unless defined $store;
	
	my $self = bless {
		'content'   => $content,
		'baseuri'   => $baseuri,
		'options'   => $options,
		'DOM'       => $domtree,
		'RESULTS'   => RDF::Trine::Model->new($store),
		}, $class;
	
	return $self;
}

=item $p->uri

Returns the base URI of the document being parsed. This will usually be the
same as the base URI provided to the constructor.

Optionally it may be passed a parameter - an absolute or relative URI - in
which case it returns the same URI which it was passed as a parameter, but
as an absolute URI, resolved relative to the document's base URI.

This seems like two unrelated functions, but if you consider the consequence
of passing a relative URI consisting of a zero-length string, it in fact makes
sense.

=cut

sub uri
{
	my $this  = shift;
	my $param = shift || '';
	my $opts  = shift || {};
	
	if ((ref $opts) =~ /^XML::LibXML/)
	{
		my $x = {'element' => $opts};
		$opts = $x;
	}
	
	if ($param =~ /^([a-z][a-z0-9\+\.\-]*)\:/i)
	{
		# seems to be an absolute URI, so can safely return "as is".
		return $param;
	}
	elsif ($opts->{'require-absolute'})
	{
		return undef;
	}
	
	my $base = $this->{baseuri};
	if ($this->{'options'}->{'xml_base'})
	{
		$base = $opts->{'xml_base'} || $this->{baseuri};
	}
	
	my $url = url $param, $base;
	my $rv  = $url->abs->as_string;

	# This is needed to pass test case 0114.
	while ($rv =~ m!^(http://.*)(\.\./|\.)+(\.\.|\.)?$!i)
	{
		$rv = $1;
	}
	
	return $rv;
}

=item $p->dom

Returns the parsed XML::LibXML::Document.

=cut

sub dom
{
	my $this = shift;
	return $this->{DOM};
}

=item $p->set_callbacks(\&func1, \&func2)

Set callbacks for handling RDF triples extracted from the document. The
first function is called when a triple is generated taking the form of
(I<resource>, I<resource>, I<resource>). The second function is called when a
triple is generated taking the form of (I<resource>, I<resource>, I<literal>).

The parameters passed to the first callback function are:

=over 4

=item * A reference to the C<XRD::Parser> object

=item * A reference to the C<XML::LibXML element> being parsed

=item * Subject URI or bnode

=item * Predicate URI

=item * Object URI or bnode

=back

The parameters passed to the second callback function are:

=over 4

=item * A reference to the C<XRD::Parser> object

=item * A reference to the C<XML::LibXML element> being parsed

=item * Subject URI or bnode

=item * Predicate URI

=item * Object literal

=item * Datatype URI (possibly undef or '')

=item * Language (possibly undef or '')

=back

In place of either or both functions you can use the string C<'print'> which
sets the callback to a built-in function which prints the triples to STDOUT
as Turtle. Either or both can be set to undef, in which case, no callback
is called when a triple is found.

Beware that for literal callbacks, sometimes both a datatype *and* a language
will be passed. (This goes beyond the normal RDF data model.)

C<set_callbacks> (if used) must be used I<before> C<consume>.

=cut

sub set_callbacks
# Set callback functions for handling RDF triples.
{
	my $this = shift;

	for (my $n=0 ; $n<2 ; $n++)
	{
		if (lc($_[$n]) eq 'print')
			{ $this->{'sub'}->[$n] = ($n==0 ? \&_print0 : \&_print1); }
		elsif ('CODE' eq ref $_[$n])
			{ $this->{'sub'}->[$n] = $_[$n]; }
		else
			{ $this->{'sub'}->[$n] = undef; }
	}
}

sub _print0
# Prints a Turtle triple.
{
	my $this    = shift;
	my $element = shift;
	my $subject = shift;
	my $pred    = shift;
	my $object  = shift;
	my $graph   = shift;
	
	if ($graph)
	{
		print "# GRAPH $graph\n";
	}
	if ($element)
	{
		printf("# Triple on element %s.\n", $element->nodePath);
	}
	else
	{
		printf("# Triple.\n");
	}

	printf("%s %s %s .\n",
		($subject =~ /^_:/ ? $subject : "<$subject>"),
		"<$pred>",
		($object =~ /^_:/ ? $object : "<$object>"));
	
	return undef;
}

sub _print1
# Prints a Turtle triple.
{
	my $this    = shift;
	my $element = shift;
	my $subject = shift;
	my $pred    = shift;
	my $object  = shift;
	my $dt      = shift;
	my $lang    = shift;
	my $graph   = shift;
	
	# Clumsy, but probably works.
	$object =~ s/\\/\\\\/g;
	$object =~ s/\n/\\n/g;
	$object =~ s/\r/\\r/g;
	$object =~ s/\t/\\t/g;
	$object =~ s/\"/\\\"/g;
	
	if ($graph)
	{
		print "# GRAPH $graph\n";
	}
	if ($element)
	{
		printf("# Triple on element %s.\n", $element->nodePath);
	}
	else
	{
		printf("# Triple.\n");
	}

	no warnings;
	printf("%s %s %s%s%s .\n",
		($subject =~ /^_:/ ? $subject : "<$subject>"),
		"<$pred>",
		"\"$object\"",
		(length $dt ? "^^<$dt>" : ''),
		((length $lang && !length $dt) ? "\@$lang" : '')
		);
	use warnings;
	
	return undef;
}

=item $p->consume;

This method processes the input DOM and sends the resulting triples to 
the callback functions (if any).

=cut

sub consume
{
	my $this = shift;
	
	my @xrds = $this->{'DOM'}->getElementsByTagName('XRD')->get_nodelist;
	
	my $first = 1;
	my $only  = defined $xrds[1] ? 1 : 0;
	
	foreach my $XRD (@xrds)
	{
		$this->_consume_XRD($XRD, $first, $only);
		$first = 0
			if $first;
	}
	
	return $this;
}

sub _consume_XRD
{
	my $this  = shift;
	my $xrd   = shift;
	my $first = shift;
	my $only  = shift;
	
	my $description_uri;
	if ($xrd->hasAttributeNS(XML_XML_NS, 'id'))
	{
		$description_uri = $this->uri('#'.$xrd->getAttributeNS(XML_XML_NS, 'id'));
	}
	elsif ($only)
	{
		$description_uri = $this->uri;
	}
	else
	{
		$description_uri = $this->bnode;
	}
	
	my @aliases = map { $this->uri($this->stringify($_),{'require-absolute'=>1}) }
		$xrd->getChildrenByTagName('Alias')->get_nodelist;
		
	my $subject_node = $xrd->getChildrenByTagName('Subject')->shift;
	my $subject;
	$subject = $this->uri(
		$this->stringify($subject_node),
		{'require-absolute'=>1})
		if $subject_node;
	$subject = shift @aliases
		unless defined $subject;
	$subject = $this->bnode($xrd)
		unless defined $subject;
	
	my @subjects = ($subject, @aliases);

	$this->rdf_triple(
		$xrd,
		$description_uri,
		'http://xmlns.com/foaf/0.1/primaryTopic',
		$subject);
	foreach my $s (@subjects)
	{
		$this->rdf_triple(
			$xrd,
			$description_uri,
			'http://xmlns.com/foaf/0.1/topic',
			$s);
	}
	
	my $expires_node = $xrd->getChildrenByTagName('Expires')->shift;
	my $expires      = $this->stringify($expires_node);
	if (length $expires)
	{
		$this->rdf_triple_literal(
			$xrd,
			$description_uri,
			'http://purl.org/dc/terms/valid',
			$expires,
			'http://www.w3.org/2001/XMLSchema#dateTime');
	}
	
	foreach my $p ($xrd->getChildrenByTagName('Property')->get_nodelist)
	{
		$this->_consume_Property($p, \@subjects);
	}
	
	foreach my $l ($xrd->getChildrenByTagName('Link')->get_nodelist)
	{
		$this->_consume_Link($l, \@subjects);
	}
}

sub _consume_Property
{
	my $this = shift;
	my $p    = shift;
	my $S    = shift;
	
	my $property_uri = $this->uri(
		$p->getAttribute('type'), {'require-absolute'=>1});
	return unless $property_uri;
	
	my $value = $this->stringify($p);
	
	foreach my $subject_uri (@$S)
	{
		$this->rdf_triple_literal(
			$p,
			$subject_uri,
			$property_uri,
			$value);
	}
}

sub _consume_Link
{
	my $this = shift;
	my $l    = shift;
	my $S    = shift;
	
	return unless $l->hasAttribute('href');
	
	my $property_uri = $this->uri(
		$l->getAttribute('rel'), {'require-absolute'=>1});
	return unless $property_uri;
	
	my $value = $this->uri($l->getAttribute('href'));

	foreach my $subject_uri (@$S)
	{
		$this->rdf_triple(
			$l,
			$subject_uri,
			$property_uri,
			$value);
	}
	
	my $type = $l->getAttribute('type');
	if (defined $type)
	{
		$this->rdf_triple(
			$l,
			$value,
			'http://purl.org/dc/terms/format',
			'http://www.iana.org/assignments/media-types/'.$type);
	}
	
	foreach my $title ($l->getChildrenByTagName('Title')->get_nodelist)
	{
		my $lang = undef;
		if ($title->hasAttributeNS(XML_XML_NS, 'lang'))
		{
			$lang = $title->getAttributeNS(XML_XML_NS, 'lang');
			$lang = undef unless valid_lang($lang);
		}
		$this->rdf_triple_literal(
			$title,
			$value,
			'http://purl.org/dc/terms/title',
			$this->stringify($title),
			undef,
			$lang);
	}
	
	foreach my $lp ($l->getChildrenByTagName('Property')->get_nodelist)
	{
		$this->_consume_Link_Property($lp, $S, $property_uri, $value);
	}
}

sub _consume_Link_Property
{
	my $this = shift;
	my $lp   = shift;
	my $S    = shift;
	my $p    = shift;
	my $o    = shift;
	
	my $property_uri = $this->uri(
		$lp->getAttribute('type'), {'require-absolute'=>1});
	return unless $property_uri;
	
	my $value = $this->stringify($lp);

	foreach my $subject_uri (@$S)
	{
		my $reified_statement = $this->bnode($lp);
		
		$this->rdf_triple(
			$lp,
			$reified_statement,
			'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
			'http://www.w3.org/1999/02/22-rdf-syntax-ns#Statement');			
		$this->rdf_triple(
			$lp,
			$reified_statement,
			'http://www.w3.org/1999/02/22-rdf-syntax-ns#subject',
			$subject_uri);
		$this->rdf_triple(
			$lp,
			$reified_statement,
			'http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate',
			$p);
		$this->rdf_triple(
			$lp,
			$reified_statement,
			'http://www.w3.org/1999/02/22-rdf-syntax-ns#object',
			$o);
		$this->rdf_triple_literal(
			$lp,
			$reified_statement,
			$property_uri,
			$value);
	}
}

=item $p->graph() 

This method will return an RDF::Trine::Model object with all
statements of the full graph.

It makes sense to call C<consume> before calling C<graph>. Otherwise
you'll just get an empty graph.

=cut

sub graph
{
	my $this = shift;
	return $this->{RESULTS};
}

sub graphs
{
	my $this = shift;
	return { $this->{'baseuri'} => $this->{RESULTS} };
}

sub rdf_triple
# Function only used internally.
{
	my $this = shift;

	my $suppress_triple = 0;
	if ($this->{'sub'}->[0])
	{
		$suppress_triple = $this->{'sub'}->[0]($this, @_);
	}
	return if $suppress_triple;
	
	my $element   = shift;  # A reference to the XML::LibXML element being parsed
	my $subject   = shift;  # Subject URI or bnode
	my $predicate = shift;  # Predicate URI
	my $object    = shift;  # Resource URI or bnode
	my $graph     = shift;  # Graph URI or bnode (if named graphs feature is enabled)

	# First make sure the object node type is ok.
	my $to;
	if ($object =~ m/^_:(.*)/)
	{
		$to = RDF::Trine::Node::Blank->new($1);
	}
	else
	{
		$to = RDF::Trine::Node::Resource->new($object);
	}

	# Run the common function
	return $this->rdf_triple_common($element, $subject, $predicate, $to, $graph);
}

sub rdf_triple_literal
# Function only used internally.
{
	my $this = shift;

	my $suppress_triple = 0;
	if ($this->{'sub'}->[1])
	{
		$suppress_triple = $this->{'sub'}->[1]($this, @_);
	}
	return if $suppress_triple;

	my $element   = shift;  # A reference to the XML::LibXML element being parsed
	my $subject   = shift;  # Subject URI or bnode
	my $predicate = shift;  # Predicate URI
	my $object    = shift;  # Resource Literal
	my $datatype  = shift;  # Datatype URI (possibly undef or '')
	my $language  = shift;  # Language (possibly undef or '')
	my $graph     = shift;  # Graph URI or bnode (if named graphs feature is enabled)

	# Now we know there's a literal
	my $to;
	
	# Work around bad Unicode handling in RDF::Trine.
	$object = encode_utf8($object);

	if (defined $datatype)
	{
		if ($datatype eq 'http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral')
		{
			if ($this->{'options'}->{'use_rtnlx'})
			{
				eval
				{
					require RDF::Trine::Node::Literal::XML;
					$to = RDF::Trine::Node::Literal::XML->new($element->childNodes);
				};
			}
			
			if ( $@ || !defined $to)
			{
				my $orig = $RDF::Trine::Node::Literal::USE_XMLLITERALS;
				$RDF::Trine::Node::Literal::USE_XMLLITERALS = 0;
				$to = RDF::Trine::Node::Literal->new($object, undef, $datatype);
				$RDF::Trine::Node::Literal::USE_XMLLITERALS = $orig;
			}
		}
		else
		{
			$to = RDF::Trine::Node::Literal->new($object, undef, $datatype);
		}
	}
	else
	{
		$to = RDF::Trine::Node::Literal->new($object, $language, undef);
	}

	# Run the common function
	$this->rdf_triple_common($element, $subject, $predicate, $to, $graph);
}

sub rdf_triple_common
# Function only used internally.
{
	my $this      = shift;  # A reference to the RDF::RDFa::Parser object
	my $element   = shift;  # A reference to the XML::LibXML element being parsed
	my $subject   = shift;  # Subject URI or bnode
	my $predicate = shift;  # Predicate URI
	my $to        = shift;  # RDF::Trine::Node Resource URI or bnode
	my $graph     = shift;  # Graph URI or bnode (if named graphs feature is enabled)

	# First, make sure subject and predicates are the right kind of nodes
	my $tp = RDF::Trine::Node::Resource->new($predicate);
	my $ts;
	if ($subject =~ m/^_:(.*)/)
	{
		$ts = RDF::Trine::Node::Blank->new($1);
	}
	else
	{
		$ts = RDF::Trine::Node::Resource->new($subject);
	}

	# If we are configured for it, and graph name can be found, add it.
	if (ref($this->{'options'}->{'named_graphs'}) && ($graph))
	{
		$this->{Graphs}->{$graph}++;
		
		my $tg;
		if ($graph =~ m/^_:(.*)/)
		{
			$tg = RDF::Trine::Node::Blank->new($1);
		}
		else
		{
			$tg = RDF::Trine::Node::Resource->new($graph);
		}

		my $statement = RDF::Trine::Statement::Quad->new($ts, $tp, $to, $tg);
		$this->{RESULTS}->add_statement($statement);
	
		#if ($graph ne $this->{'options'}->{'named_graphs'}->{'default'})
		#{
		#	my $graph_statement = RDF::Trine::Statement::Quad->new($ts, $tp, $to, 
		#		$this->{'options'}->{'named_graphs'}->{'default_trine'});
		#	$this->{RESULTS}->add_statement($graph_statement,
		#		$this->{'options'}->{'named_graphs'}->{'default_trine'});
		#}
	}
	else
	{
		# If no graph name, just add triples
		my $statement = RDF::Trine::Statement->new($ts, $tp, $to);
		$this->{RESULTS}->add_statement($statement);
	}
}

sub stringify
# Function only used internally.
{
	my $this = shift;
	my $dom  = shift;
	
	if ($dom->nodeType == XML_TEXT_NODE)
	{
		return $dom->getData;
	}
	elsif ($dom->nodeType == XML_ELEMENT_NODE)
	{
		my $rv = '';
		foreach my $kid ($dom->childNodes)
			{ $rv .= $this->stringify($kid); }
		return $rv;
	}

	return '';
}

sub xmlify
# Function only used internally.
{
	my $this = shift;
	my $dom  = shift;
	my $lang = shift;
	my $rv;
	
	foreach my $kid ($dom->childNodes)
	{
		my $fakelang = 0;
		if (($kid->nodeType == XML_ELEMENT_NODE) && defined $lang)
		{
			unless ($kid->hasAttributeNS(XML_XML_NS, 'lang'))
			{
				$kid->setAttributeNS(XML_XML_NS, 'lang', $lang);
				$fakelang++;
			}
		}
		
		$rv .= $kid->toStringEC14N(1);
		
		if ($fakelang)
		{
			$kid->removeAttributeNS(XML_XML_NS, 'lang');
		}
	}
	
	return $rv;
}

sub bnode
# Function only used internally.
{
	my $this    = shift;
	my $element = shift;
	
	return sprintf('http://thing-described-by.org/?%s#%s',
		$this->uri,
		$element->getAttributeNS(XML_XML_NS, 'id'))
		if ($this->{options}->{tdb_service} && $element && length $element->getAttributeNS(XML_XML_NS, 'id'));

	return sprintf('_:RDFaAutoNode%03d', $this->{bnodes}++);
}

sub valid_lang
{
	my $value_to_test = shift;

	return 1 if (defined $value_to_test) && ($value_to_test eq '');
	return 0 unless defined $value_to_test;
	
	# Regex for recognizing RFC 4646 well-formed tags
	# http://www.rfc-editor.org/rfc/rfc4646.txt
	# http://tools.ietf.org/html/draft-ietf-ltru-4646bis-21

	# The structure requires no forward references, so it reverses the order.
	# It uses Java/Perl syntax instead of the old ABNF
	# The uppercase comments are fragments copied from RFC 4646

	# Note: the tool requires that any real "=" or "#" or ";" in the regex be escaped.

	my $alpha      = '[a-z]';      # ALPHA
	my $digit      = '[0-9]';      # DIGIT
	my $alphanum   = '[a-z0-9]';   # ALPHA / DIGIT
	my $x          = 'x';          # private use singleton
	my $singleton  = '[a-wyz]';    # other singleton
	my $s          = '[_-]';       # separator -- lenient parsers will use [_-] -- strict will use [-]

	# Now do the components. The structure is slightly different to allow for capturing the right components.
	# The notation (?:....) is a non-capturing version of (...): so the "?:" can be deleted if someone doesn't care about capturing.

	my $language   = '([a-z]{2,8}) | ([a-z]{2,3} $s [a-z]{3})';
	
	# ABNF (2*3ALPHA) / 4ALPHA / 5*8ALPHA  --- note: because of how | works in regex, don't use $alpha{2,3} | $alpha{4,8} 
	# We don't have to have the general case of extlang, because there can be only one extlang (except for zh-min-nan).

	# Note: extlang invalid in Unicode language tags

	my $script = '[a-z]{4}' ;   # 4ALPHA 

	my $region = '(?: [a-z]{2}|[0-9]{3})' ;    # 2ALPHA / 3DIGIT

	my $variant    = '(?: [a-z0-9]{5,8} | [0-9] [a-z0-9]{3} )' ;  # 5*8alphanum / (DIGIT 3alphanum)

	my $extension  = '(?: [a-wyz] (?: [_-] [a-z0-9]{2,8} )+ )' ; # singleton 1*("-" (2*8alphanum))

	my $privateUse = '(?: x (?: [_-] [a-z0-9]{1,8} )+ )' ; # "x" 1*("-" (1*8alphanum))

	# Define certain grandfathered codes, since otherwise the regex is pretty useless.
	# Since these are limited, this is safe even later changes to the registry --
	# the only oddity is that it might change the type of the tag, and thus
	# the results from the capturing groups.
	# http://www.iana.org/assignments/language-subtag-registry
	# Note that these have to be compared case insensitively, requiring (?i) below.

	my $grandfathered  = '(?:
			  (en [_-] GB [_-] oed)
			| (i [_-] (?: ami | bnn | default | enochian | hak | klingon | lux | mingo | navajo | pwn | tao | tay | tsu ))
			| (no [_-] (?: bok | nyn ))
			| (sgn [_-] (?: BE [_-] (?: fr | nl) | CH [_-] de ))
			| (zh [_-] min [_-] nan)
			)';

	# old:         | zh $s (?: cmn (?: $s Hans | $s Hant )? | gan | min (?: $s nan)? | wuu | yue );
	# For well-formedness, we don't need the ones that would otherwise pass.
	# For validity, they need to be checked.

	# $grandfatheredWellFormed = (?:
	#         art $s lojban
	#     | cel $s gaulish
	#     | zh $s (?: guoyu | hakka | xiang )
	# );

	# Unicode locales: but we are shifting to a compatible form
	# $keyvalue = (?: $alphanum+ \= $alphanum+);
	# $keywords = ($keyvalue (?: \; $keyvalue)*);

	# We separate items that we want to capture as a single group

	my $variantList   = $variant . '(?:' . $s . $variant . ')*' ;     # special for multiples
	my $extensionList = $extension . '(?:' . $s . $extension . ')*' ; # special for multiples

	my $langtag = "
			($language)
			($s ( $script ) )?
			($s ( $region ) )?
			($s ( $variantList ) )?
			($s ( $extensionList ) )?
			($s ( $privateUse ) )?
			";

	# Here is the final breakdown, with capturing groups for each of these components
	# The variants, extensions, grandfathered, and private-use may have interior '-'
	
	my $r = ($value_to_test =~ 
		/^(
			($langtag)
		 | ($privateUse)
		 | ($grandfathered)
		 )$/xi);
	return $r;
}

1;
__END__

=back

=head1 SEE ALSO

L<RDF::Trine>, L<RDF::Query>, L<RDF::RDFa::Parser>.

L<http://www.perlrdf.org/>.

=head1 AUTHOR

Toby Inkster, E<lt>tobyink@cpan.orgE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2009 by Toby Inkster

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.1 or,
at your option, any later version of Perl 5 you may have available.


=cut
