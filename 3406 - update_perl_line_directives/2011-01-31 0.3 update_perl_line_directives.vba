" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
ftplugin/vim/update_perl_line_directives.vim	[[[1
142
" Vim plugin to add or update line directives to perl embedded in vim script
" Last Change: 2011 Jan 18
" Maintainer:  Britton Kerin <britton.kerin@gmail.com>
" License:     This file is placed in the public domain.

" vim:foldmethod=marker

" Starting Vim Module Boilerplate {{{1

let s:save_cpo = &cpo
set cpo&vim

if exists("loaded_update_perl_line_directives")
  finish
endif
let loaded_update_perl_line_directives = 1

" Interface {{{1

" FIXME: you have to reload vim each time when testing this, I think because
" the existing autocommand with the old routing still happens (the redefed
" version of the perl command never gets linked in).
autocmd BufWritePre *.vim,*.vimrc call s:UpdateLineDirectives()

" Implementation {{{1

function! s:UpdateLineDirectives() "{{{2
  unlet! s:retVal
  perl Updateperllinedirectives::UpdateLineDirectives_perl
  "perl UpdateLineDirectives_perl
  if exists('retVal')
    throw "unexpected return value"
  endif
endfunction

if has('perl') "{{{2
perl <<EOF
# line 39 "~/projects/libblk/vim/update_perl_line_directives/update_perl_line_directives.vim"

package Updateperllinedirectives;

use strict;
use warnings FATAL => 'all';
use warnings NONFATAL => 'redefine';
use File::Temp qw( tempfile );
use IO::Handle;

sub UpdateLineDirectives_perl #{{{3
{

    # FIXME: the die messages never make it up through to vim UI in this case,
    # I think because things are happening in an autocommand.  Is there a
    # better way?

    my $fn = VIM::Eval('expand("%:p")');

    # Perl can deal with the nice ~/ home dir abbreviation in line directives.
    $fn =~ s/^$ENV{HOME}/~/;

    my $curbuf = $main::curbuf;   # This is the vim buffer object.

    # Existing buffer contents.
    my @ebc = $curbuf->Get(1 .. $curbuf->Count());

    my @nbc = @ebc;   # New buffer contents to be set

    #open(MYLOG2, ">/tmp/dalog") or die;
    #MYLOG2->autoflush();

    for ( my $ii = 0 ; $ii < @nbc ; $ii++ ) {
        my $cl = $nbc[$ii];
        my $nl = $nbc[$ii + 1];
        if ( $cl =~ m/\s*perl <<\w+\s*$/ ) {
            # +1 (because 1-based) +1 (talking next line) +1 (because perl line
            # directives refer to the number of the next line) == +3
            my $new_nl = "# line ".($ii + 3)." \"$fn\"";   # New next line.
            if ( defined($nl) and $nl =~ m/^#\s+line\s+\d+\s+".+"\s*$/ ) {
                $nbc[$ii + 1] = $new_nl;
            }
            else {
                # FIXME: splice is slooooww compared to alternatives if we end
                # up doing it many times
                splice(@nbc, $ii + 1, 0, $new_nl);
            }
        }
    }

    # Paranoia: compare old and new files texts do make sure we haven't done
    # anything but add line directives.
    @nbc >= @ebc or die "filtered region unexpectedly got shorter";
    my ($otfh, $otfn) = tempfile();
    print $otfh join("\n", @ebc);
    close($otfh) or die;
    my ($ntfh, $ntfn) = tempfile();
    print $ntfh join("\n", @nbc);
    close($ntfh) or die;
    my $diffout = `diff $otfn $ntfn`;
    # Diff returns 1 when files differ, hence weird fail test
    if ( $? >> 8 > 1 ) {
        die "diff command failed";
    }
    my @dol = split("\n", $diffout);    # Diff output lines

    for ( my $ii = 0 ; $ii < @dol ; $ii++ ) {
        my $cl = $dol[$ii];
        my $nl = $dol[$ii + 1];
        # This wacky condition is hand-matched to what diff spits out in the
        # cases we know are ok.  Mainly we don't wnat to see any changes except
        # line directives being added, but if we end up changing the end of a
        # file then we have some additional things that can happen.
        (not $cl =~ m/^[<>]/) or
        ($cl =~ m/^[<>] # line \d+ ".*"$/) or
        $nl eq '\ No newline at end of file' or
        ($nl =~ m/^[<>] # line \d+ ".*"$/ and
             $dol[$ii + 2] eq '\ No newline at end of file')
            or die "oops, we almost made an unexpected change";
    }

    unlink($otfn, $ntfn) == 2 or die "unexpected failure to delete temp files";

    # The somewhat weird embedded perl API forces this peculiar approach,
    # because ->Set silently does nothing for lines that done exist rather than
    # adding them.  But we don't want to delete everthing and just use append,
    # because that disturbes cursor position.
    for ( my $ii = 0 ; $ii < @ebc ; $ii++ ) {
        $curbuf->Set($ii + 1, $nbc[$ii]);
    }
    my $lines_added = @nbc - @ebc;
    for ( my $ii = 0 ; $ii < $lines_added ; $ii++ ) {
        $curbuf->Append(scalar(@ebc) + $ii, $nbc[@nbc - $lines_added + $ii]);
    }
}

EOF
else "{{{2
  throw "Error: update_perl_line_directives.vim requires perl support to be " .
        \ "compiled into vim"
  finish
endif

" Ending vim Module Boilerplate {{{1
let &cpo = s:save_cpo
