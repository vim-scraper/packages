" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
ftplugin/vim/update_perl_line_directives.vim	[[[1
109
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
# line 35 "~/projects/libblk/vim/update_perl_line_directives/update_perl_line_directives.vim"

package Updateperllinedirectives;

use strict;
use warnings FATAL => 'all';
use warnings NONFATAL => 'redefine';
use Data::Dumper;
use File::Temp qw( tempfile );
use IO::Handle;
use Storable qw( nstore retrieve );

sub UpdateLineDirectives_perl #{{{3
{
    my $fn = VIM::Eval('expand("%:p")');

    # Perl can deal with the nice ~/ home dir abbreviation in line directives.
    $fn =~ s/^$ENV{HOME}/~/;

    my $curbuf = $main::curbuf;   # This is the vim buffer object.

    # Existing buffer contents.
    my @ebc = $curbuf->Get(1 .. $curbuf->Count());

    my @nbc = @ebc;   # New buffer contents to be set

    for ( my $ii = 0 ; $ii < @nbc ; $ii++ ) {
        my $cl = $nbc[$ii];
        my $nl = $nbc[$ii + 1];
        if ( $cl =~ m/\s*perl <<\w+\s*$/ ) {
            my $new_nl = "# line ".($ii + 2)." \"$fn\"";   # New next line.
            if ( $nl =~ m/^#\s+line\s+\d+\s+".+"\s*$/ ) {
                $nbc[$ii + 1] = $new_nl;
            }
            else {
                # FIXME: splice is slooooww compared to allternatives if we end
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
    foreach ( @dol ) {
        not m/^[<>]/ or m/^[<>] # line \d+ ".*"$/
            or die "oops, we almost made an unexpected change";
    }

    $curbuf->Set(1, @nbc);
}

EOF
else "{{{2
  throw "Error: update_perl_line_directives.vim requires perl support to be " .
        \ "compiled into vim"
  finish
endif

" Ending vim Module Boilerplate {{{1
let &cpo = s:save_cpo

