#! /usr/bin/perl -w
#
# Copyright (C) 2006,2013 Oswald Buddenhagen <ossi@users.sf.net>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

use warnings;
use strict;
use Cwd;
use File::Path;
use File::Temp 'tempdir';

my $use_vg = $ENV{USE_VALGRIND};
my $mbsync = getcwd()."/mbsync";

if (!-d "tmp") {
  unlink "tmp";
  my $tdir = tempdir();
  symlink $tdir, "tmp" or die "Cannot symlink temp directory: $!\n";
}
chdir "tmp" or die "Cannot enter temp direcory.\n";

sub show($$$);
sub test($$$@);

################################################################################

# Format of the test defs: [ far, near, state ]
# far/near: [ maxuid, { seq, uid, flags }... ]
# state: [ MaxPulledUid, MaxExpiredFarUid, MaxPushedUid, { muid, suid, flags }... ]

use enum qw(:=1 A..Z);
sub mn($) { chr(64 + shift) }

# generic syncing tests
my @x01 = (
 [ 9,
   A, 1, "F", B, 2, "", C, 3, "FS", D, 4, "", E, 5, "T", F, 6, "F", G, 7, "FT", I, 9, "" ],
 [ 9,
   A, 1, "", B, 2, "F", C, 3, "F", D, 4, "", E, 5, "", G, 7, "", H, 8, "", J, 9, "" ],
 [ 8, 0, 0,
   1, 1, "", 2, 2, "", 3, 3, "", 4, 4, "", 5, 5, "", 6, 6, "", 7, 7, "", 8, 8, "" ],
);

my @O01 = ("", "", "");
#show("01", "01", "01");
my @X01 = (
 [ 10,
   A, 1, "F", B, 2, "F", C, 3, "FS", D, 4, "", E, 5, "T", F, 6, "FT", G, 7, "FT", I, 9, "", J, 10, "" ],
 [ 10,
   A, 1, "F", B, 2, "F", C, 3, "FS", D, 4, "", E, 5, "T", G, 7, "FT", H, 8, "T", J, 9, "", I, 10, "" ],
 [ 10, 0, 10,
   1, 1, "F", 2, 2, "F", 3, 3, "FS", 4, 4, "", 5, 5, "T", 6, 0, "", 7, 7, "FT", 0, 8, "", 10, 9, "", 9, 10, "" ],
);
test("full", \@x01, \@X01, @O01);

my @O02 = ("", "", "Expunge Both\n");
#show("01", "02", "02");
my @X02 = (
 [ 10,
   A, 1, "F", B, 2, "F", C, 3, "FS", D, 4, "", I, 9, "", J, 10, "" ],
 [ 10,
   A, 1, "F", B, 2, "F", C, 3, "FS", D, 4, "", J, 9, "", I, 10, "" ],
 [ 10, 0, 10,
   1, 1, "F", 2, 2, "F", 3, 3, "FS", 4, 4, "", 10, 9, "", 9, 10, "" ],
);
test("full + expunge both", \@x01, \@X02, @O02);

my @O03 = ("", "", "Expunge Near\n");
#show("01", "03", "03");
my @X03 = (
 [ 10,
   A, 1, "F", B, 2, "F", C, 3, "FS", D, 4, "", E, 5, "T", F, 6, "FT", G, 7, "FT", I, 9, "", J, 10, "" ],
 [ 10,
   A, 1, "F", B, 2, "F", C, 3, "FS", D, 4, "", J, 9, "", I, 10, "" ],
 [ 10, 0, 10,
   1, 1, "F", 2, 2, "F", 3, 3, "FS", 4, 4, "", 5, 0, "T", 6, 0, "", 7, 0, "T", 10, 9, "", 9, 10, "" ],
);
test("full + expunge near side", \@x01, \@X03, @O03);

my @O04 = ("", "", "Sync Pull\n");
#show("01", "04", "04");
my @X04 = (
 [ 9,
   A, 1, "F", B, 2, "", C, 3, "FS", D, 4, "", E, 5, "T", F, 6, "F", G, 7, "FT", I, 9, "" ],
 [ 10,
   A, 1, "F", B, 2, "F", C, 3, "FS", D, 4, "", E, 5, "T", G, 7, "FT", H, 8, "T", J, 9, "", I, 10, "" ],
 [ 9, 0, 0,
   1, 1, "F", 2, 2, "", 3, 3, "FS", 4, 4, "", 5, 5, "T", 6, 6, "", 7, 7, "FT", 0, 8, "", 9, 10, "" ],
);
test("pull", \@x01, \@X04, @O04);

my @O05 = ("", "", "Sync Flags\n");
#show("01", "05", "05");
my @X05 = (
 [ 9,
   A, 1, "F", B, 2, "F", C, 3, "FS", D, 4, "", E, 5, "T", F, 6, "F", G, 7, "FT", I, 9, "" ],
 [ 9,
   A, 1, "F", B, 2, "F", C, 3, "FS", D, 4, "", E, 5, "T", G, 7, "FT", H, 8, "", J, 9, "" ],
 [ 8, 0, 0,
   1, 1, "F", 2, 2, "F", 3, 3, "FS", 4, 4, "", 5, 5, "T", 6, 6, "", 7, 7, "FT", 8, 8, "" ],
);
test("flags", \@x01, \@X05, @O05);

my @O06 = ("", "", "Sync Delete\n");
#show("01", "06", "06");
my @X06 = (
 [ 9,
   A, 1, "F", B, 2, "", C, 3, "FS", D, 4, "", E, 5, "T", F, 6, "FT", G, 7, "FT", I, 9, "" ],
 [ 9,
   A, 1, "", B, 2, "F", C, 3, "F", D, 4, "", E, 5, "", G, 7, "", H, 8, "T", J, 9, "" ],
 [ 8, 0, 0,
   1, 1, "", 2, 2, "", 3, 3, "", 4, 4, "", 5, 5, "", 6, 0, "", 7, 7, "", 0, 8, "" ],
);
test("deletions", \@x01, \@X06, @O06);

my @O07 = ("", "", "Sync New\n");
#show("01", "07", "07");
my @X07 = (
 [ 10,
   A, 1, "F", B, 2, "", C, 3, "FS", D, 4, "", E, 5, "T", F, 6, "F", G, 7, "FT", I, 9, "", J, 10, "" ],
 [ 10,
   A, 1, "", B, 2, "F", C, 3, "F", D, 4, "", E, 5, "", G, 7, "", H, 8, "", J, 9, "", I, 10, "" ],
 [ 10, 0, 10,
   1, 1, "", 2, 2, "", 3, 3, "", 4, 4, "", 5, 5, "", 6, 6, "", 7, 7, "", 8, 8, "", 10, 9, "", 9, 10, "" ],
);
test("new", \@x01, \@X07, @O07);

my @O08 = ("", "", "Sync PushFlags PullDelete\n");
#show("01", "08", "08");
my @X08 = (
 [ 9,
   A, 1, "F", B, 2, "F", C, 3, "FS", D, 4, "", E, 5, "T", F, 6, "F", G, 7, "FT", I, 9, "" ],
 [ 9,
   A, 1, "", B, 2, "F", C, 3, "F", D, 4, "", E, 5, "", G, 7, "", H, 8, "T", J, 9, "" ],
 [ 8, 0, 0,
   1, 1, "", 2, 2, "F", 3, 3, "F", 4, 4, "", 5, 5, "", 6, 6, "", 7, 7, "", 0, 8, "" ],
);
test("push flags + pull deletions", \@x01, \@X08, @O08);

# size restriction tests

my @x10 = (
 [ 2,
   A, 1, "", B, 2, "*" ],
 [ 1,
   C, 1, "*" ],
 [ 0, 0, 0,
    ],
);

my @O11 = ("MaxSize 1k\n", "MaxSize 1k\n", "Expunge Near");
#show("10", "11", "11");
my @X11 = (
 [ 3,
   A, 1, "", B, 2, "*", C, 3, "?" ],
 [ 3,
   C, 1, "*", A, 2, "", B, 3, "?" ],
 [ 3, 0, 3,
   3, 1, "<", 1, 2, "", 2, 3, ">" ],
);
test("max size", \@x10, \@X11, @O11);

my @x22 = (
 [ 3,
   A, 1, "", B, 2, "*", C, 3, "?" ],
 [ 3,
   C, 1, "F*", A, 2, "", B, 3, "F?" ],
 [ 3, 0, 3,
   3, 1, "<", 1, 2, "", 2, 3, ">" ],
);

#show("22", "22", "11");
my @X22 = (
 [ 4,
   A, 1, "", B, 2, "*", C, 3, "T?", C, 4, "F*" ],
 [ 4,
   C, 1, "F*", A, 2, "", B, 4, "*" ],
 [ 4, 0, 4,
   4, 1, "F", 3, 0, "T", 1, 2, "", 2, 4, "" ],
);
test("max size + flagging", \@x22, \@X22, @O11);

my @x23 = (
 [ 2,
   A, 1, "", B, 2, "F*" ],
 [ 1,
   C, 1, "F*" ],
 [ 0, 0, 0,
    ],
);

my @X23 = (
 [ 3,
   A, 1, "", B, 2, "F*", C, 3, "F*" ],
 [ 3,
   C, 1, "F*", A, 2, "", B, 3, "F*" ],
 [ 3, 0, 3,
   3, 1, "F", 1, 2, "", 2, 3, "F" ]
);
test("max size + initial flagging", \@x23, \@X23, @O11);

my @x24 = (
 [ 3,
   A, 1, "", B, 2, "*", C, 3, "F*" ],
 [ 1,
   A, 1, "" ],
 [ 3, 0, 1,
   1, 1, "", 2, 0, "^", 3, 0, "^" ],
);

my @X24 = (
 [ 3,
   A, 1, "", B, 2, "*", C, 3, "F*" ],
 [ 3,
   A, 1, "", B, 2, "?", C, 3, "F*" ],
 [ 3, 0, 3,
   1, 1, "", 2, 2, ">", 3, 3, "F" ],
);
test("max size (pre-1.4 legacy)", \@x24, \@X24, @O11);

# expiration tests

my @x30 = (
 [ 6,
   A, 1, "F", B, 2, "", C, 3, "S", D, 4, "", E, 5, "S", F, 6, "" ],
 [ 0,
   ],
 [ 0, 0, 0,
    ],
);

my @O31 = ("", "", "MaxMessages 3\n");
#show("30", "31", "31");
my @X31 = (
 [ 6,
   A, 1, "F", B, 2, "", C, 3, "S", D, 4, "", E, 5, "S", F, 6, "" ],
 [ 5,
   A, 1, "F", B, 2, "", D, 3, "", E, 4, "S", F, 5, "" ],
 [ 6, 3, 5,
   1, 1, "F", 2, 2, "", 4, 3, "", 5, 4, "S", 6, 5, "" ],
);
test("max messages", \@x30, \@X31, @O31);

my @O32 = ("", "", "MaxMessages 3\nExpireUnread yes\n");
#show("30", "32", "32");
my @X32 = (
 [ 6,
   A, 1, "F", B, 2, "", C, 3, "S", D, 4, "", E, 5, "S", F, 6, "" ],
 [ 4,
   A, 1, "F", D, 2, "", E, 3, "S", F, 4, "" ],
 [ 6, 3, 4,
   1, 1, "F", 4, 2, "", 5, 3, "S", 6, 4, "" ],
);
test("max messages vs. unread", \@x30, \@X32, @O32);

my @x50 = (
 [ 6,
   A, 1, "FS", B, 2, "FS", C, 3, "S", D, 4, "", E, 5, "", F, 6, "" ],
 [ 6,
   A, 1, "S", B, 2, "ST", D, 4, "", E, 5, "", F, 6, "" ],
 [ 6, 3, 0,
   1, 1, "FS", 2, 2, "~S", 3, 3, "~S", 4, 4, "", 5, 5, "", 6, 6, "" ],
);

my @O51 = ("", "", "MaxMessages 3\nExpunge Both\n");
#show("50", "51", "51");
my @X51 = (
 [ 6,
   A, 1, "S", B, 2, "FS", C, 3, "S", D, 4, "", E, 5, "", F, 6, "" ],
 [ 6,
   B, 2, "FS", D, 4, "", E, 5, "", F, 6, "" ],
 [ 6, 3, 6,
   2, 2, "FS", 4, 4, "", 5, 5, "", 6, 6, "" ],
);
test("max messages + expunge", \@x50, \@X51, @O51);


################################################################################

print "OK.\n";
exit 0;


sub qm($)
{
	shift;
	s/\\/\\\\/g;
	s/\"/\\"/g;
	s/\"/\\"/g;
	s/\n/\\n/g;
	return $_;
}

# $far, $near, $channel
sub writecfg($$$)
{
	open(FILE, ">", ".mbsyncrc") or
		die "Cannot open .mbsyncrc.\n";
	print FILE
"FSync no

MaildirStore far
Path ./
Inbox ./far
".shift()."
MaildirStore near
Path ./
Inbox ./near
".shift()."
Channel test
Far :far:
Near :near:
SyncState *
".shift();
	close FILE;
}

sub killcfg()
{
	unlink $_ for (glob("*.log"));
	unlink ".mbsyncrc";
}

# $options
sub runsync($$)
{
	my ($flags, $file) = @_;

	my $cmd;
	if ($use_vg) {
		$cmd = "valgrind -q --error-exitcode=1 ";
	} else {
		$flags .= " -D";
	}
	$cmd .= "$mbsync -Tz $flags -c .mbsyncrc test";
	open FILE, "$cmd 2>&1 |";
	my @out = <FILE>;
	close FILE or push(@out, $! ? "*** error closing mbsync: $!\n" : "*** mbsync exited with signal ".($?&127).", code ".($?>>8)."\n");
	if ($file) {
		open FILE, ">$file" or die("Cannot create $file: $!\n");
		print FILE @out;
		close FILE;
	}
	return $?, @out;
}


# $path
sub readbox($)
{
	my $bn = shift;

	(-d $bn) or
		die "No mailbox '$bn'.\n";
	(-d $bn."/tmp" and -d $bn."/new" and -d $bn."/cur") or
		die "Invalid mailbox '$bn'.\n";
	open(FILE, "<", $bn."/.uidvalidity") or die "Cannot read UID validity of mailbox '$bn'.\n";
	my $dummy = <FILE>;
	chomp(my $mu = <FILE>);
	close FILE;
	my %ms = ();
	for my $d ("cur", "new") {
		opendir(DIR, $bn."/".$d) or next;
		for my $f (grep(!/^\.\.?$/, readdir(DIR))) {
			my ($uid, $flg, $ph, $num);
			if ($f =~ /^\d+\.\d+_\d+\.[-[:alnum:]]+,U=(\d+):2,(.*)$/) {
				($uid, $flg) = ($1, $2);
			} else {
				print STDERR "unrecognided file name '$f' in '$bn'.\n";
				exit 1;
			}
			open(FILE, "<", $bn."/".$d."/".$f) or die "Cannot read message '$f' in '$bn'.\n";
			my $sz = 0;
			while (<FILE>) {
				/^Subject: (\[placeholder\] )?(\d+)$/ && ($ph = defined($1), $num = $2);
				$sz += length($_);
			}
			close FILE;
			if (!defined($num)) {
				print STDERR "message '$f' in '$bn' has no identifier.\n";
				exit 1;
			}
			@{ $ms{$uid} } = ($num, $flg.($sz>1000?"*":"").($ph?"?":""));
		}
	}
	return ($mu, %ms);
}

# $boxname
# Output:
# [ maxuid,
#   serial, uid, "flags", ... ],
sub showbox($)
{
	my ($bn) = @_;

	my ($mu, %ms) = readbox($bn);
	my @MS = ($mu);
	for my $uid (sort { $a <=> $b } keys %ms) {
		push @MS, $ms{$uid}[0], $uid, $ms{$uid}[1];
	}
	printbox($bn, @MS);
}

# $filename
# Output:
# [ maxuid[F], maxxfuid, maxuid[N],
#   uid[F], uid[N], "flags", ... ],
sub showstate($)
{
	my ($fn) = @_;

	if (!open(FILE, "<", $fn)) {
		print STDERR " Cannot read sync state $fn: $!\n";
		return;
	}
	chomp(my @ls = <FILE>);
	close FILE;
	my %hdr;
	OUTER: while (1) {
		while (@ls) {
			$_ = shift(@ls);
			last OUTER if (!length($_));
			if (!/^([^ ]+) (\d+)$/) {
				print STDERR "Malformed sync state header entry: $_\n";
				close FILE;
				return;
			}
			$hdr{$1} = $2;
		}
		print STDERR "Unterminated sync state header.\n";
		close FILE;
		return;
	}
	my @T = ($hdr{'MaxPulledUid'} // "missing",
	         $hdr{'MaxExpiredFarUid'} // "0",
	         $hdr{'MaxPushedUid'} // "missing");
	for (@ls) {
		/^(\d+) (\d+) (.*)$/;
		push @T, $1, $2, $3;
	}
	printstate(@T);
}

# $filename
sub showchan($)
{
	my ($fn) = @_;

	showbox("far");
	showbox("near");
	showstate($fn);
}

# $source_state_name, $target_state_name, $configs_name
sub show($$$)
{
	my ($sx, $tx, $sfxn) = @_;
	my (@sp, @sfx);
	eval "\@sp = \@x$sx";
	eval "\@sfx = \@O$sfxn";
	mkchan($sp[0], $sp[1], @{ $sp[2] });
	print "my \@x$sx = (\n";
	showchan("near/.mbsyncstate");
	print ");\n";
	&writecfg(@sfx);
	runsync("", "");
	killcfg();
	print "my \@X$tx = (\n";
	showchan("near/.mbsyncstate");
	print ");\n";
	print "test(\"\", \\\@x$sx, \\\@X$tx, \@O$sfxn);\n\n";
	rmtree "near";
	rmtree "far";
}

# $boxname, $maxuid, @msgs
sub mkbox($$@)
{
	my ($bn, $mu, @ms) = @_;

	rmtree($bn);
	(mkdir($bn) and mkdir($bn."/tmp") and mkdir($bn."/new") and mkdir($bn."/cur")) or
		die "Cannot create mailbox $bn.\n";
	open(FILE, ">", $bn."/.uidvalidity") or die "Cannot create UID validity for mailbox $bn.\n";
	print FILE "1\n$mu\n";
	close FILE;
	while (@ms) {
		my ($num, $uid, $flg) = (shift @ms, shift @ms, shift @ms);
		my $big = $flg =~ s/\*//;
		my $ph = $flg =~ s/\?//;
		open(FILE, ">", $bn."/".($flg =~ /S/ ? "cur" : "new")."/0.1_".$num.".local,U=".$uid.":2,".$flg) or
			die "Cannot create message ".mn($num)." in mailbox $bn.\n";
		print FILE "From: foo\nTo: bar\nDate: Thu, 1 Jan 1970 00:00:00 +0000\nSubject: ".($ph?"[placeholder] ":"").$num."\n\n".(("A"x50)."\n")x($big*30);
		close FILE;
	}
}

# \@far, \@near, @syncstate
sub mkchan($$@)
{
	my ($m, $s, @t) = @_;
	&mkbox("far", @{ $m });
	&mkbox("near", @{ $s });
	open(FILE, ">", "near/.mbsyncstate") or
		die "Cannot create sync state.\n";
	print FILE "FarUidValidity 1\nMaxPulledUid ".shift(@t)."\n".
	           "NearUidValidity 1\nMaxExpiredFarUid ".shift(@t)."\nMaxPushedUid ".shift(@t)."\n\n";
	while (@t) {
		print FILE shift(@t)." ".shift(@t)." ".shift(@t)."\n";
	}
	close FILE;
}

# $boxname, $maxuid, @msgs
sub ckbox($$@)
{
	my ($bn, $MU, @MS) = @_;

	my ($mu, %ms) = readbox($bn);
	if ($mu != $MU) {
		print STDERR "MAXUID mismatch for '$bn' (got $mu, wanted $MU).\n";
		return 1;
	}
	while (@MS) {
		my ($num, $uid, $flg) = (shift @MS, shift @MS, shift @MS);
		my $m = delete $ms{$uid};
		if (!defined $m) {
			print STDERR "No message $bn:$uid.\n";
			return 1;
		}
		if ($$m[0] ne $num) {
			print STDERR "Subject mismatch for $bn:$uid.\n";
			return 1;
		}
		if ($$m[1] ne $flg) {
			print STDERR "Flag mismatch for $bn:$uid.\n";
			return 1;
		}
	}
	if (%ms) {
		print STDERR "Excess messages in '$bn': ".join(", ", sort({$a <=> $b } keys(%ms))).".\n";
		return 1;
	}
	return 0;
}

# $filename, @syncstate
sub ckstate($@)
{
	my ($fn, $fmaxuid, $maxxfuid, $nmaxuid, @T) = @_;
	my %hdr;
	$hdr{'FarUidValidity'} = "1";
	$hdr{'NearUidValidity'} = "1";
	$hdr{'MaxPulledUid'} = $fmaxuid;
	$hdr{'MaxPushedUid'} = $nmaxuid;
	$hdr{'MaxExpiredFarUid'} = $maxxfuid if ($maxxfuid ne 0);
	open(FILE, "<", $fn) or die "Cannot read sync state $fn.\n";
	chomp(my @ls = <FILE>);
	close FILE;
	OUTER: while (1) {
		while (@ls) {
			my $l = shift(@ls);
			last OUTER if (!length($l));
			if ($l !~ /^([^ ]+) (\d+)$/) {
				print STDERR "Malformed sync state header entry: $l\n";
				return 1;
			}
			my $want = delete $hdr{$1};
			if (!defined($want)) {
				print STDERR "Unexpected sync state header entry: $1\n";
				return 1;
			}
			if ($2 != $want) {
				print STDERR "Sync state header entry $1 mismatch: got $2, wanted $want\n";
				return 1;
			}
		}
		print STDERR "Unterminated sync state header.\n";
		return 1;
	}
	my @ky = keys %hdr;
	if (@ky) {
		print STDERR "Keys missing from sync state header: @ky\n";
		return 1;
	}
	for my $l (@ls) {
		if (!@T) {
			print STDERR "Excess sync state entry: '$l'.\n";
			return 1;
		}
		my $xl = shift(@T)." ".shift(@T)." ".shift(@T);
		if ($l ne $xl) {
			print STDERR "Sync state entry mismatch: '$l' instead of '$xl'.\n";
			return 1;
		}
	}
	if (@T) {
		print STDERR "Missing sync state entry: '".shift(@T)." ".shift(@T)." ".shift(@T)."'.\n";
		return 1;
	}
	return 0;
}

# $statefile, \@chan_state
sub ckchan($$)
{
	my ($F, $cs) = @_;
	my $rslt = ckstate($F, @{ $$cs[2] });
	$rslt |= &ckbox("far", @{ $$cs[0] });
	$rslt |= &ckbox("near", @{ $$cs[1] });
	return $rslt;
}

# $boxname, $maxuid, @msgs
sub printbox($$@)
{
	my ($bn, $mu, @ms) = @_;

	print " [ $mu,\n   ";
	my $frst = 1;
	while (@ms) {
		if ($frst) {
			$frst = 0;
		} else {
			print ", ";
		}
		print mn(shift(@ms)).", ".shift(@ms).", \"".shift(@ms)."\"";
	}
	print " ],\n";
}

# @syncstate
sub printstate(@)
{
	my (@t) = @_;

	print " [ ".shift(@t).", ".shift(@t).", ".shift(@t).",\n   ";
	my $frst = 1;
	while (@t) {
		if ($frst) {
			$frst = 0;
		} else {
			print ", ";
		}
		print((shift(@t) // "??").", ".(shift(@t) // "??").", \"".(shift(@t) // "??")."\"");
	}
	print " ],\n";
}

# \@chan_state
sub printchan($)
{
	my ($cs) = @_;

	&printbox("far", @{ $$cs[0] });
	&printbox("near", @{ $$cs[1] });
	printstate(@{ $$cs[2] });
}

sub readfile($)
{
	my ($file) = @_;

	open(FILE, $file) or return;
	my @nj = <FILE>;
	close FILE;
	return @nj;
}

# $title, \@source_state, \@target_state, @channel_configs
sub test($$$@)
{
	my ($ttl, $sx, $tx, @sfx) = @_;

	return 0 if (scalar(@ARGV) && !grep { $_ eq $ttl } @ARGV);
	print "Testing: ".$ttl." ...\n";
	&writecfg(@sfx);

	mkchan($$sx[0], $$sx[1], @{ $$sx[2] });

	my ($xc, @ret) = runsync("-Tj", "1-initial.log");
	if ($xc || ckchan("near/.mbsyncstate.new", $tx)) {
		print "Input:\n";
		printchan($sx);
		print "Options:\n";
		print " [ ".join(", ", map('"'.qm($_).'"', @sfx))." ]\n";
		if (!$xc) {
			print "Expected result:\n";
			printchan($tx);
			print "Actual result:\n";
			showchan("near/.mbsyncstate.new");
		}
		print "Debug output:\n";
		print @ret;
		exit 1;
	}

	my @nj = readfile("near/.mbsyncstate.journal");
	my ($jxc, @jret) = runsync("-0 --no-expunge", "2-replay.log");
	if ($jxc || ckstate("near/.mbsyncstate", @{ $$tx[2] })) {
		print "Journal replay failed.\n";
		print "Options:\n";
		print " [ ".join(", ", map('"'.qm($_).'"', @sfx))." ], [ \"-0\", \"--no-expunge\" ]\n";
		print "Old State:\n";
		printstate(@{ $$sx[2] });
		print "Journal:\n".join("", @nj)."\n";
		if (!$jxc) {
			print "Expected New State:\n";
			printstate(@{ $$tx[2] });
			print "New State:\n";
			showstate("near/.mbsyncstate");
		}
		print "Debug output:\n";
		print @jret;
		exit 1;
	}

	my ($ixc, @iret) = runsync("", "3-verify.log");
	if ($ixc || ckchan("near/.mbsyncstate", $tx)) {
		print "Idempotence verification run failed.\n";
		print "Input == Expected result:\n";
		printchan($tx);
		print "Options:\n";
		print " [ ".join(", ", map('"'.qm($_).'"', @sfx))." ]\n";
		if (!$ixc) {
			print "Actual result:\n";
			showchan("near/.mbsyncstate");
		}
		print "Debug output:\n";
		print @iret;
		exit 1;
	}

	rmtree "near";
	rmtree "far";

	my $njl = (@nj - 1) * 2;
	for (my $l = 1; $l <= $njl; $l++) {
		mkchan($$sx[0], $$sx[1], @{ $$sx[2] });

		my ($nxc, @nret) = runsync("-Tj$l", "4-interrupt.log");
		if ($nxc != (100 + ($l & 1)) << 8) {
			print "Interrupting at step $l/$njl failed.\n";
			print "Debug output:\n";
			print @nret;
			exit 1;
		}

		($nxc, @nret) = runsync("-Tj", "5-resume.log");
		if ($nxc || ckchan("near/.mbsyncstate.new", $tx)) {
			print "Resuming from step $l/$njl failed.\n";
			print "Input:\n";
			printchan($sx);
			print "Options:\n";
			print " [ ".join(", ", map('"'.qm($_).'"', @sfx))." ]\n";
			my @nnj = readfile("near/.mbsyncstate.journal");
			my $ln = int($l / 2);
			print "Journal:\n".join("", @nnj[0..$ln])."-------\n".join("", @nnj[($ln + 1)..$#nnj])."\n";
			print "Full journal:\n".join("", @nj)."\n";
			if (!$nxc) {
				print "Expected result:\n";
				printchan($tx);
				print "Actual result:\n";
				showchan("near/.mbsyncstate.new");
			}
			print "Debug output:\n";
			print @nret;
			exit 1;
		}

		rmtree "near";
		rmtree "far";
	}

	killcfg();
}
