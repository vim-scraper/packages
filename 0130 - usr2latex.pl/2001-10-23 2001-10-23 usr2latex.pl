#!/usr/bin/perl

# Converts vim documentation to latex.
# This script is far from perfect but produce a quite readable latex file.
# Usage:
# 1)Copy this script to your vim doc directory(where files usr_*.txt are located).
# 2)Launch it. The latex file is generated on its standard output.
# 3)Don't forget to run latex three times on the generated file to get a valid toc.

# Written by Christophe Gouiran(christophe.gouiran@steria.com).
# Based on the script usr2html.pl by Dion Nicolaas(dion@erebus.demon.nl).

use strict;

my $cur_line = 0;
my $title_found = 0;
my $section_found = 0;

sub vim2latex
{
    while (@_) 
    {
        my $infile = shift @_;
        if ($section_found)
        {
            print("\\end{verbatim}\n");
        }    
        $title_found = 0;
        $section_found = 0;
        $cur_line = 0;

        open(IN, "$infile") || die "Couldn't read from $infile.\n";

        # Replace applicable parts
        while(<IN>) 
        {

            s/^\s*Next chapter: .*$//;
            s/^\s*Previous chapter: .*$//;
            s/^\s*Table of contents: .*$//;
            s/^\s*Copyright: see.*$//;
            s/^\s*\|[\d|\.]+\|.*$//;
            
            $cur_line++;

            if ($cur_line eq 5) 
            {
                $title_found = 1;

                chomp;
                s/^\s+//; # remove trailing spaces
                s/\s+$//; # remove trailing spaces
                print("\\chapter{".$_."}\n");
                print("\\begin{verbatim}\n");
                next;
            }
            
            if ($title_found)
            {
                if (m/^=+$/) # if the line is ==========
                {
                    #print("ok =====\n");
                }
                elsif (m/^\s*\*[\d|\.]+\*/) # line begins whith *num.num*
                {
                    print("\\end{verbatim}\n");
                    
                    $section_found = 1;
                    chomp;
                    s/^\s*\*[\d|\.]+\*\s*//; # remove *num.num* from the beginning of line
                    s/\*(.+?)\*//g; # remove all text like *vimtutor* 
                    print("\\section{".$_."}\n");
                    print("\\begin{verbatim}\n");
                }    
                else
                {
                    print;
                }    
            }   

        }
        close(IN);
    }
}

# main

print <<EOF;
\\documentstyle{report}
\\title{VIM 6.0 user manual}
\\author{Bram Moolenaar}
\\begin{document}
\\maketitle{}
\\tableofcontents{}
EOF

opendir(DIR, ".") || die("$!");

my @entries = readdir(DIR);

for my $entry(@entries)
{
    if ($entry =~ /^usr_\d+\.txt$/)
    {
        vim2latex($entry);
    }
}
closedir(DIR);

if ($section_found || $title_found)
{
    print("\\end{verbatim}\n");
}    

print <<EOF

\\end{document}
EOF

