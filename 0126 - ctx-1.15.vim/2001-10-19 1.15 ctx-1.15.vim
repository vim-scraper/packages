" ctx.vim -- display current scope context of cursor in a C file          "
" Copyright (C) Sept-Oct 2001, Aaron Brooks and Chris Houser              "
" Distributed under the GNU General Public License                        "
" $Revision: 1.15 $                                                       "


" To use, source this file in your ~/.vimrc.  You may also want to reduce "
" your updatetime to 1000 or so.                                          "

" Note that if you source this file from within an autocommand, you may   "
" get a non-fatal error when opening a C file.                            "

" default vim-ctx settings "
let CTX_lncols=5
let CTX_reindent=2
let CTX_autohide=1
let CTX_debug=0

perl << ENDPERL
########
# CTX main package
package CTX;

BEGIN {
    $^W=1;  # turn on warnings

    # create package-global subs and vars to define machine-states
    sub buildstates {
        my (@states) = @_;
        my ($pkg) = caller;
        my $snum = 0;
        foreach my $name (@states) {
            my $mysnum = $snum;
            *{"${pkg}::$name"}    = sub { ${"${pkg}::state"} == $mysnum };
            *{"${pkg}::go$name"}  = sub { ${"${pkg}::state"} =  $mysnum };
            ${"${pkg}::snames"}[$mysnum] = $name;
            ++$snum;
        }
    }

    # import function names from this namespace
    sub importsubs {
        my (@names) = @_;
        my ($pkg) = caller;
        foreach my $name (@names) {
            *{"${pkg}::$name"} = *{"$name"}{CODE};
        }
    }
}

# input/output subroutines
{
    my @slines;
    my $sbuf = '';
    my $slnum = 0;
    sub badd {
        $sbuf = ($_[0] || '') . $sbuf;
    }
    sub bclear {
        $sbuf = '';
        $slnum = 0;
    }
    sub bln {
        $slnum = $lnum;
    }
    sub bdo($) {
        badd $_[0];
        unshift @slines, [$sbuf, $slnum || $lnum] if $sbuf;
        bclear;
    }
    sub bdopre {
        @slines  and $slines[0][0] = $_[0] . ' ' . $slines[0][0];
        !@slines and bdo $_[0];
        bclear;
    }
    sub appendmsg {
        $outbuf->Append($outbuf->Count(), $_[0]);
    }
    sub debugmsg {
        $debug and appendmsg $_[0];
    }
    sub bout {
        my $depth = -$reindent;
        foreach my $rec (@slines) {
            my $line = $rec->[0];
            $line =~ s/\s+/ /g;
            $line =~ s/^\s*//;
            ++$depth unless $line =~ /^else/;
            my $indent = " " x ($reindent * $depth);
            if ($lncols) {
                appendmsg sprintf "%${lncols}d:%s%s", $rec->[1], $indent, $line;
            }
            else {
                appendmsg $indent . $line;
            }
        }
        $outbuf->Delete(1);
        my $height = $outbuf->Count();
        $height = $height > 20 ? 20 : $height;
        $outwin->SetHeight($height);
        @slines=();
    }
}

sub init {
    # get vim-ctx settings
    $lncols   = VIM::Eval('g:CTX_lncols');
    $reindent = VIM::Eval('g:CTX_reindent');
    $debug    = VIM::Eval('g:CTX_debug');

    # find our output buffer (and output window) and clear it
    ($outbuf) = VIM::Buffers('--context--');
    foreach my $win (VIM::Windows()) {
        if ($win->Buffer() == $outbuf) {
            $outwin = $win;
            last;
        }
    }
    $outbuf->Delete(1, $outbuf->Count());

    # find our current cursor position (in rows and columns)
    my ($row, $col) = $main::curwin->Cursor();
    $lnum = $row;
}

########
# forward-parsing to find the 'while' part of 'do-while' blocks
package CTXFWD;

# define states for small forward state machine
BEGIN {
    CTX::importsubs qw( debugmsg );
    CTX::buildstates qw( BRACE WHILE );
}

sub init {
    $lnum = $_[0] - 1;
    $buf = '';
    $lvl = 0;
}

sub dowhile {
    my ($backlvl) = @_;
    goBRACE;
    my $comment  = 0;
    my $rtnbuf  = '';
    local $_ = $buf;
    debugmsg 'DoWhile...';

    while ($lnum <= $main::curbuf->Count()) {
        $_ = $main::curbuf->Get(++$lnum) or next unless $_;
        debugmsg sprintf("%-12s%s", $snames[$state].'['.$lvl.']:', $_);

        # comments
        s{^[#]          .*$}{}x;  # for now, clobber #directives
        s{    /\*.*?\*/\s* }{}gx;
        s{    //        .*$}{}x;
        s{^\s*/\*       .*$}{}x and do { $comment = 1; next };
        s{^.*       \*/\s* }{}x and do { $comment = 0       };
        $comment                and do { $_ = '';      next };

        BRACE and do {
            $lvl == $backlvl and do { goWHILE; next };
            s/^ [^{}]* }//x  and do { ++$lvl;  next };
            s/^ [^{}]* {//x  and do { --$lvl;  next };
            $_=''; next;
        };

        WHILE and do {
            s/^\s* ;//x and do { last };
            s/^[^;]+//x and do { $rtnbuf .= $&; next };
            $rtnbuf .= $_; $_=''; next;
        };
    }

    $buf = $_;
    debugmsg 'DoWhile:'.$rtnbuf;
    return $rtnbuf;
}

########
# parse the buffer backwards, starting at the current line
package CTXBACK;

# define states for main backward state machine
BEGIN {
    CTX::importsubs qw( badd bclear bln bdo bdopre bout debugmsg appendmsg );
    CTX::buildstates qw(
        PRECTRL BRACE PREBRACE ELSE PAREN CTRL FUNC
        WANTIF EPAREN EBRACE PREELSE );
}

sub run {
    # initialize CTX for a new run
    CTX::init;
    CTXFWD::init $CTX::lnum;

    # reset flags and such
    my $braced  = 1; # brace depth. 1 == 'current', 0 == just popped up a level
    my $parend  = 0; # parenthesis depth
    my $elsed   = 1; # else depth
    my $gotbrace= 0; # indicates the ctrl statement being parsed preceded braces
    my $comment = 0; # 1 == the start of previous line was in a block comment
    my $lvl     = 0; # level of brace depth, counting up from cursor location

    # main parsing loop
    $_ = '';
    my ($oldl, $old_, $olds) = (-1, 'dmy', -1);
    goPRECTRL;
    while ($CTX::lnum > 0) {
        $_ = $main::curbuf->Get(--$CTX::lnum) or next unless $_;
        debugmsg sprintf("%-12s%s",$snames[$state].'['.$gotbrace.']:',$_);

        # safety
        if ($oldl == $CTX::lnum && $old_ eq $_ && $olds == $state) {
            appendmsg 'safety stop';
            last;
        }
        ($oldl, $old_, $olds) = ($CTX::lnum, $_, $state);

        # comments
        s{^[#]         .*$}{}x;  # for now, clobber #directives
        s{   /\*.*?\*/\s* }{}gx;
        s{   //        .*$}{}x;
        s{^.*      \*/\s*$}{}x   and do { $comment = 1; next };
        s{   /\*       .*$}{}x   and do { $comment = 0       };
        $comment                 and do { $_ = '';      next };

        # state machine
        PRECTRL and do {
            $gotbrace = 0;
            s/\bdo\s*$//        and do { bdo $&; next};
            s/\belse    \s*$//x and do { bdopre $&; goELSE; next };
            s/[;{}][^;{})]*$//x and do { $_.=$&; goBRACE; next };
            s/[)]  [^)]*   $//x and do { badd $&; $parend=1; goPAREN; next };
        };

        PREBRACE and do {
            $gotbrace = 1;
            s/\bdo\s*$// and do {bdo $& . CTXFWD::dowhile($lvl);goPRECTRL;next};
            s/\belse    \s*$//x and do { bdo $&; goELSE; next };
            s/[;{}][^;{})]*$//x and do { bdo '{'; $_.=$&; goBRACE; next };
            s/[)]  [^)]*   $//x and do { badd $&; $parend=1; goPAREN; next };
        };

        BRACE ||
        EBRACE and do {
            BRACE  && $braced==0 and do { $braced=1;++$lvl;bln;goPREBRACE;next};
            EBRACE && $braced==0 and do { $braced=1; goPREELSE;  next };
            s/}[^{}]*$//x        and do { ++$braced; next };
            s/{[^{}]*$//x        and do { --$braced; next };
        };

        ELSE and do {
            s/}[^};]*//x and do { goEBRACE; next };
            s/;[^};]*//x and do { $_.=$&; goPREELSE; next };
        };

        PAREN ||
        EPAREN and do {
            PAREN  && $parend < 1  and do { goCTRL;   next};
            EPAREN && $parend < 1  and do { goWANTIF; next};
            s/[)][^()]*$//x        and do { badd $&; ++$parend; next };
            s/[(][^()]*$//x        and do { badd $&; --$parend; next };
            badd $_.' ';
        };

        CTRL and do {
            s/\b(if|switch|while|for)\s*$// and do { bln;bdo $&;goPRECTRL;next};
            !$gotbrace && s/[^;\s]\s*$//    and do { bclear; goPRECTRL;   next};
            $gotbrace  && s/[^;\s]\s*$//    and do { bln; $_.=$&; goFUNC; next};
            badd $_.' ';
        };

        FUNC and do {
            s/[^;}]+$//x and do { badd $&.' '; next };
            s/[;}]  $//x and do { bdo ' '; last };
            badd $_.' ';
        };

        WANTIF and do {
            $elsed==1 && s/\bif\s*$// and do { bdo $&; goPRECTRL; next };
            s/\bif\s*$//x             and do { --$elsed; bclear;goPREELSE;next};
            s/\S  \s*$//x             and do { $_.=$&;   bclear;goPREELSE;next};
            badd $_.' ';
        };

        PREELSE and do {
            s/\belse   \s*$//x and do { ++$elsed; goELSE; next };
            s/[)]  [^)]*  $//x and do { badd $&; $parend=1; goEPAREN; next };
        };

        # if we've fallen through, clear input buffer and go to next line
        $_='';
    }
    &bdo;

    # output to window
    &bout;
}
ENDPERL

" automatically close --context-- buffer when last C buffer goes away. "
" frigging hard to do!  Is there a better way, I hope? "
function! CTXHide()
    if g:CTX_autohide
        let start = winnr()
        let foundc = 0
        while 1
            if bufname('%') =~ '\.[cC]'
                let foundc = foundc + 1
            endif
            wincmd w
            if winnr() == start
                break
            endif
        endwhile
        if foundc < 1
            while 1
                if bufname('%') == '--context--'
                    q
                    break
                endif
                wincmd w
                if winnr() == start
                    break
                endif
            endwhile
        endif
    endif
endfunction

" main function "
function! CTXUpdate()
    if bufnr('--context--') < 0
        topleft new --context--
        set filetype=c nowrap buftype=nofile bufhidden=delete noswapfile
        exe "normal! \<C-W>p"
    endif

    perl CTXBACK::run;
endfunction

augroup CTX
    au!
    au CursorHold  *.[cC] call CTXUpdate()
    au BufEnter    *      call CTXHide()
augroup END

" vim: filetype=perl
