#!/usr/bin/perl -w

# Use this script to generate PowerDNS-compatible LDIF (dnsDomain2) for the
# specified reverse domain. Common usage as such:
# ./mkreverseldif.pl 3.6.0.10.in-addr.arpa maya.adjectivism.org \
#     ou=hosts,dc=adjectivism,dc=org | ldapadd -c [otheropts]
# -c is necessary to ldapadd to make sure the operation doesn't fail if some
# of the parent objects already exist (which is very likely)
#
# This script is public domain and comes with the typical disclaimer (I'm not
# liable for anything!) Examine your LDIF first!

use strict;

die "no base specified\nUsage: script arpa hostname base-dn" if !$ARGV[2];
my $base = $ARGV[2];
my $ptr = $ARGV[1];
my $arpa = $ARGV[0];

my @parts = reverse split /\./, $arpa;

my $cumm_dn = "$base";
my $cumm_domain = '';

sub parent {
    my $x = shift;
    return <<END;
dn: dc=$x,$cumm_dn
objectClass: dnsDomain
objectClass: dnsDomain2
objectClass: domain
objectClass: domainRelatedObject
objectClass: top
associatedDomain: $x$cumm_domain
dc: $x

END
}

sub ptr {
    my $x = shift;
    return <<END;
dn: dc=$x,$cumm_dn
objectClass: dnsDomain
objectClass: dnsDomain2
objectClass: domain
objectClass: domainRelatedObject
objectClass: top
associatedDomain: $x$cumm_domain
pTRRecord: $ptr
dc: $x

END
}

my $i = -1;
foreach(@parts) {
    $i++;
    if($i == $#parts) {
        print ptr($_);
    } else {
        print parent($_);
    }
    $cumm_dn = "dc=$_,$cumm_dn";
    $cumm_domain = ".$_$cumm_domain";
}
