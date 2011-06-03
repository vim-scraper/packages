#!/usr/bin/perl -w
use Win32::Clipboard;
use strict;
use warnings;
use Getopt::Std;
use LWP;
my $url = 'http://www.rafb.net/paste/paste.php'; # nopaste submission url
my $nick = '';	
my $desc = '';
my $code = '';
my $lang = '';

open(stuff_to_upload, $ARGV[0]) or 
  die("Error: cannot open file \n");
my $line;
my $lnum = 1;
while( $line = <stuff_to_upload> ){
  chomp($line);
  $code.="$line\n";
}
close stuff_to_upload;

my $agentval="Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV1; .NET CLR 2.0.50727)";
my $req = LWP::UserAgent->new('agent'=>$agentval);



my $res = $req->post ($url,
	[ 'lang' => $lang,
	  'nick' => $nick,
	  'desc' => $desc,
	  'cvt_tabs' => 'No',
	  'text' => $code,
	],
	Accept_Encoding=>'gzip, deflate',
	Cache_Control=>'no-cache',
	Accept_Language=>'en-us',
	Referer=>'http://www.rafb.net/paste/',
#e default pusa bine	Content_Type=>'',
	Accept=>'image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, application/x-shockwave-flash',
	Host=>'www.rafb.net',
	Connection=>'Keep-Alive',
	Cookie=>'uid=spx2'
);
#print $req->agent;

$res->as_string =~ m/Location: (.*)/;
my $CLIP = Win32::Clipboard();
$CLIP->Set($1);
print  $CLIP->Get();
$CLIP->WaitForChange();


print $1;



#print $res->as_string;
	exit(0);

