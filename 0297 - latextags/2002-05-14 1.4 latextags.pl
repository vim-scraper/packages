#!/usr/bin/env perl

# latextags - create a tags file for Latex code, for use by vi(m)
#             also includes bibtex bibliographies  
#
# run this script without arguments to get usage help
#
#
# Version 1.4, 14 May 2002
#
# Written by Michael Muhler <muhler@web.de>.  
# Suggestions for improvement are very welcome!
# 
# Heavily inspired by Michael Schaap's pltags.pl
# This script would not exist without Michael Schaap's work
#
# This script will not work with Perl 4 or below!
#
# Revision history:
#  1.0  Apr 2002   Original version, quickly hacked together
#  1.1  4/15/2002  added support for bibtex
#  1.2  5/08/2002  added incl option, changed some variable names
#  1.3  5/11/2002  work towards first official release
#                  clean up of code, completed incl option
#                  added verbose option
#  1.4  5/14/2002  first official release 

# TODO
# Here is a list of ideas 
# 1.) add OS support to generate OS specific files,
#     e.g. UNIX files on a Windows machine
# 2.) support for local bibitems found in latex file.    
# 3.) option if --cmds considers renewcommands
# 4.) what about the includeonly option ?

# Complain about undeclared variables
use strict;

# Used modules
use Getopt::Long;
use File::Find;

# Options with their defaults
my $do_labl = 1;    # --labl, --nolabl    include labels in tags file?
my $do_bibs = 1;    # --bibs, --nobibs    include bibliographies in tags file?
my $do_cmds = 1;    # --cmds, --nocmds    include new commands in tags file?
my $do_incl = 1;    # --incl, --noincl    create tags for included files?
                    #                     this allows to run the program on a
                    #                     master file only
my $do_exts = 0;    # --extensions, --noextensions
                    #                     include Exuberant Ctags extensions
my $do_static = 1;  # make static tags, actually fixed, not an option
my $be_verbose = 1; # no verbosity by default

# Global variables
my $VERSION = "1.4";    # latextags version
my $status = 0;         # GetOptions return value
my @infiles = ();       # List of file to be tagged
my %procfiles = ();     # List of files already tagged
my $fndvalidfile = "";  # flag which indicates if a valid included file has been found
my $ii = "";            # loop counter
my $file = "";          # File being processed
my @tags = ();          # List of produced tags
my @bibfiles = ();      # List of .bib files
my $line = "";          # Current line in file
my $stmt = "";          # Current Perl statement
my $cmdname = "";       # name of latex newcommand
my @vars = ();          # List of variables in declaration
my $var = "";           # Variable in declaration
my $tagline = "";       # Tag file line
my $inclfile = "";      # file included in latex file
my @inclfilesfull = (); # full name of included latex files
my $inclfilefull = "";  # full name of included latex file
my @TEXINPUTS = ();     # directories found in TEXINPUTS
my $TEXDIR = "";        # directory in @TEXINPUTS
my $prevtag = "";       # variable to store the previous tag
my $currtag = "";       # variable to store the current  tag
my $prevfile = "";      # variable to store the previous file
my $currfile = "";      # variable to store the current  file

# Create a tag file line and push it on the list of found tags
sub MakeTag($$$$$) {
    my ($tag,           # Tag name
        $type,          # Type of tag
        $is_static,     # Is this a static tag?
        $file,          # File in which tag appears
        $line) = @_;    # Line in which tag appears

    my $tagline = "";   # Created tag line

    # Only process tag if not empty
    if ($tag) {
        # Get rid of \n, and escape / and \ in line
        chomp $line;
        $line =~ s/\\/\\\\/g;
        $line =~ s/\//\\\//g;

        # Create a tag line
        $tagline = "$tag\t$file\t/^$line\$/";

        # If we're told to do so, add extensions
        if ($do_exts) {
            $tagline .= ";\"\t$type"
                            . ($is_static ? "\tfile:" : "");
        }

        # Push it on the stack
        push (@tags, $tagline);
    }
}


# Parse bibtex entry statement
sub BibEntry($) {
    my ($stmt) = @_;    # Statement

    if ($stmt =~ /\@\w+\{(\w+),/) {
        my $entryname = $1;
        #print "entryname: $entryname\n";
        return $entryname;
    }
    else {
        return "";
    }
}


# Parse label statement
# this function is a bit overkill
# eliminate time permitting
sub LabelName($) {
    my ($stmt) = @_;    # Statement

    # Look for the argument to "label".  Return it if found, else return ""
    # possible label declarations: \label{ex:hello1}
    if ($stmt =~ /\\label\{(\w+.*)\}/) {
        my $lablname = $1;
        return $lablname;
    }
    else {
        return "";
    }
}

# find all bib files from the current path
sub wantedbibfile {
    if (/^.*\.bib\z/s) {
        print "found bib file: $File::Find::name\n" if ($be_verbose > 2);
        push (@bibfiles, $File::Find::name);
    }
}       

# determine location of included file
# file might be somewhere in TEXINPUTS
# or just in the same directory as the parsed
# tex file
sub wantedtexfile {
    if (/$inclfile\.tex\z/s) {
        print "located included file: $File::Find::name\n" if ($be_verbose > 2);
        push (@inclfilesfull, $File::Find::name);
    }
} 

###############################################################################
#
#               Start 
#
###############################################################################

print "\nlatextags $VERSION by Michael Muhler <muhler\@web.de>\n\n";

# Get options
$status = GetOptions("labl!" => \$do_labl,
                     "bibs!" => \$do_bibs,
                     "cmds!" => \$do_cmds,
                     "incl!" => \$do_incl,
                     "verbose:i" => \$be_verbose,  # optional verbosity level
                     "extensions!" => \$do_exts);
             
# Usage if error in options or no arguments given
unless ($status && @ARGV) {
    print "\n" unless ($status);
    print "  Usage: $0 [options] filename ...\n\n";
    print "  Where options can be:\n";
    print "    --labl (--nolabl)  (don't) create tags for labels declared by \n";
    print "                       the \\label command and used in conjuction with \\ref{}.\n"; 
    print "    --bibs (--nobibs)  (don't) create tags for bibitems found in\n";
    print "                       bibliography files in the current directory\n";
    print "                       and in all directories defined in the BIBINPUTS\n";
    print "                       environment variable.\n"; 
    print "    --cmds (--nocmds)  (don't) create tags for user defined LaTeX commands.\n";
    print "                       Supports both \\newcommand and \\renewcommand.\n";
    print "    --incl (--noincl)  (don't) create tags for LaTeX filex included with\n";
    print "                       the \\include command. (Don not use \\input !)\n";
    print "    --verbose[=verbositylevel] specify verbosity level\n";  
    print "    --extensions (--noextensions)\n";
    print "                       (don't) include Exuberant Ctags/Vim style\n";
    print "                       extensions in tag file\n\n";
    print "  Default options: ";
    print ($do_labl ? "--labl " : "--nolabl ");
    print ($do_bibs ? "--bibs " : "--nobibs ");
    print ($do_cmds ? "--cmds " : "--nocmds ");
    print ($do_incl ? "--incl " : "--noincl ");
    print " --verbose=$be_verbose\n";
    print ($do_exts ? "--extensions\n\n" : "--noextensions\n\n");
    print "  Example: $0 *.tex ../include/*.tex\n\n";
    exit;
}


# preprocessing, get some information about
# environment
if ($do_incl) {
        # get directories in TEXINPUTS
        @TEXINPUTS = split(/;/,$ENV{TEXINPUTS});
        push (@TEXINPUTS, ".");
}

# Loop through files on command line - 'glob' any wildcards, since Windows
# doesn't do this for us
@infiles = map { glob } @ARGV;

foreach $file (@infiles) {
    # Skip if this is not a file we can open.  
    # Also skip tags files and backup files
    next unless ((-f $file) && (-r $file) && ($file !~ /tags$/)
                 && ($file !~ /~$/));

    if ($procfiles{$file} == 0) {
        $procfiles{$file} = 1;
    } else {
        next;
    }    
         
    print "Tagging file $file...\n" if $be_verbose;

    open (IN, $file) or die "Can't open file '$file': $!";

    # Loop through file
    foreach $line (<IN>) {
        # Statement is line with comments and whitespace trimmed
        ($stmt = $line) =~ s/%.*//;
        $stmt =~ s/^\s*//;
        $stmt =~ s/\s*$//;

        # Nothing left? Never mind.
        next unless ($stmt);

        # This is a command declaration if we find newcommand
        if (($do_cmds) && ($stmt =~/^\\(re)*newcommand\{\\(\w+)\}/)) {
            $cmdname = $2;
            print "\nFound newcommand: $cmdname \n" if ($be_verbose > 1);
            
            # Make a tag for this newcommand 
            # Default: make static tags.
            MakeTag($cmdname, "c", $do_static, $file, $line);
        }

        # This is a label declaration if the keyword label is encountered
        elsif (($do_labl) && ($stmt =~/\\label\{\w+.*\}/)) {
            print "Found label tag\n" if ($be_verbose > 1);
            
            # Make a tag for this label, Default: make static tags.
            MakeTag(LabelName($stmt), "l", $do_static, $file, $line);
        }
        # check if we found an included file
        # TODO what about files included with the input command
        elsif (($do_incl) && ($stmt =~/\\include\{(\w+.*)\}/)) {
                $inclfile = $1;
                print "\nFound an included file: $inclfile\n" if ($be_verbose > 1);
                # check if this is a tex file
                # and strip any trailing ending for this file
                if ($inclfile =~/\.tex$/) {
                        $inclfile =~ s/\.tex//;
                } elsif ($inclfile =~/\.\w+/) {
                        print "Discarding included non tex file: $inclfile\n";
                }
                if ($procfiles{$inclfile}==1) {
                        print "$inclfile was already processed!\n";
                        $fndvalidfile = 1;  # already processed
                } else {
                        $fndvalidfile = 0;
                }
                $ii = 0;
                while (($fndvalidfile == 0) && ($ii < @TEXINPUTS)) {
                        $TEXDIR = $TEXINPUTS[$ii++];
                        @inclfilesfull = ();
                        find(\&wantedtexfile, $TEXDIR);
                        foreach $inclfilefull (@inclfilesfull) {
                                # strip leading ./
                                $inclfilefull =~ s/^\.\///;
                                # skip if we cannot open file
                                next unless ((-f $inclfilefull) && (-r $inclfilefull));
                                if ($fndvalidfile++ == 0) {
                                        # push this file onto stack, if file has not
                                        # been processed
                                        push (@infiles, $inclfilefull);
                                } else {
                                        print "Found another version of $inclfile at $inclfilefull\n";
                                }
                        }
                }
        }
    }
    close (IN);
}


# process bibliographies if available
# maybe we should just
# add an tags file in the bibinputs directory
# such that we do not have to 
# run through all bibliographies each time
if ($do_bibs) {
        # get all bib files in BIBINPUTS
        my $BIBDIR;
        my $bibfile;
        # get directories in BIBINPUTS
        my @BIBINPUTS = split(/;/,$ENV{BIBINPUTS}); 
        # include local bib files
        push (@BIBINPUTS, ".");
        
        foreach $BIBDIR (@BIBINPUTS) {
                print "current bibdir: $BIBDIR\n" if ($be_verbose > 2);
                # check if we have to recurse the
                # bib file, use find ?

                find(\&wantedbibfile, $BIBDIR);
                # Loop through bib files 
                foreach $bibfile (@bibfiles) {
                        # Skip if this is not a file we can open.  
                        # Also skip tags files and backup files
                        next unless ((-f $bibfile) && (-r $bibfile) 
                             && ($file !~ /~$/));

                        if ($procfiles{$bibfile} == 0) {
                            $procfiles{$bibfile} = 1;
                        } else {
                            next;
                        }

                        print "Tagging file $bibfile...\n" if $be_verbose;

                        open (IN, $bibfile) 
                                or die "Can't open file '$bibfile': $!";

                        # Loop through file
                        foreach $line (<IN>) {
                                # Statement is line with comments 
                                # and whitespace trimmed
                                ($stmt = $line) =~ s/%.*//;
                                $stmt =~ s/^\s*//;
                                $stmt =~ s/\s*$//;

                                # Nothing left? Never mind.
                                next unless ($stmt);

                                # This is a bibtex entry definition if it starts with @
                                # and one of the following keywords is found:
                                # article, book, booklet, inbook, conference, 
                                # incollection, inproceedings, manual, masterthesis, 
                                # misc, phdthesis, proceedings, techreport, unpublished
                                # Note: bibtex is not case sensitive
                                if ($stmt =~/^\@article\{\w+,/i 
                                        or ($stmt =~/^\@(in)*book(let)*\{\w+,/i)  
                                        or ($stmt =~/^\@conference\{\w+,/i) 
                                        or ($stmt =~/^\@incollection\{\w+,/i)   
                                        or ($stmt =~/^\@(in)*proceedings\{\w+,/i) 
                                        or ($stmt =~/^\@manual\{\w+,/i)   
                                        or ($stmt =~/^\@(master|phd)thesis\{\w+,/i) 
                                        or ($stmt =~/^\@misc\{\w+,/i)   
                                        or ($stmt =~/^\@techreport\{\w+,/i) 
                                        or ($stmt =~/^\@unpublished\{\w+,/i)) {
                                        # Make a tag for this variable. We
                                        # assume that a variable is always static.
                                        print "found entry: $stmt\n" if ($be_verbose > 1);
                                        MakeTag(BibEntry($stmt), "b", $do_static, $bibfile, $line);
                                }
                        }
                        close (IN);
                }
        }
}        

# Do we have any tags?  If so, write them to the tags file
if (@tags) {

    # Add some tag file extensions if we're told to
    if ($do_exts) {
        push (@tags, "!_TAG_FILE_FORMAT\t2\t/extended format/");
        push (@tags, "!_TAG_FILE_SORTED\t1\t/0=unsorted, 1=sorted/");
        push (@tags, "!_TAG_PROGRAM_AUTHOR\tMichael Muhler\t/muhler\@web.de/");
        push (@tags, "!_TAG_PROGRAM_NAME\tlatextags\t//");
        push (@tags, "!_TAG_PROGRAM_VERSION\t$VERSION\t/supports multiple tags and extended format/");
    }
    # report duplicate tags
    @tags = sort @tags;

    print "\nWriting tags file.\n";

    open (OUT, ">tags") or die "Can't open tags file: $!";

    foreach $tagline (sort @tags) {
        print OUT "$tagline\n";
        ($currtag, $currfile) = split (/\t/,$tagline);
        if ($currtag =~ /^$prevtag$/) {
                print "Warning: found duplicate tag: $currtag\n";
                print "         found in file $prevfile\n";
                print "                and in $currfile\n";
        }
        $prevtag = $currtag;  $prevfile = $currfile;
    }

    close (OUT);
}
else {
    print "\nNo tags found.\n";
}
