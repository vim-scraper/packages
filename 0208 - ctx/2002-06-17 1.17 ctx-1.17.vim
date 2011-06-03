" ctx.vim -- display current scope context of cursor in a C file          "
" Maintainer: Chris Houser <chouser@bluweb.com>                           "
" Copyright (C) Sept 2001 - Feb 2002, Aaron Brooks and Chris Houser       "
" Distributed under the GNU General Public License                        "
" $Revision: 1.17 $                                                  "
" For updates, see http://www.bluweb.com/us/chouser/proj/ctx/             "

" To use, source this file in your ~/.vimrc. To change any of the default "
" settings below, set them *after* you source this file.                  "
"                                                                         "
" You may also want to set the updatetime in your ~/.vimrc to 1000 or so: "
"       :set updatetime 1000                                              "

" Note that if you source this file from within an autocommand, you may   "
" get a non-fatal error when opening a C file.                            "

" default vim-ctx settings "
let CTX_lncols=5        " num of cols in context win for line numbers     "
let CTX_reindent=2      " how deep each level indents (in spaces)         "
let CTX_autohide=1      " autoclose context win when last C buffer closes "
let CTX_autoupdate=1    " update the context buf on CursorHold            "
let CTX_minrows=0       " minimum num of rows for context win             "
let CTX_maxrows=20      " maximum num of rows for context win             "
let CTX_seetop=1        " always see the top of the context buf? BROKEN   "
let CTX_debug=0

" Only load once. Let loaded_ctx = 1 in your ~/.vimrc to never load at all"
if exists("loaded_ctx")
    finish
endif
let loaded_ctx = 1

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
        my $depth = -1;
        foreach my $rec (@slines) {
            my $line = $rec->[0];
            $line =~ s/\s+/ /g;
            $line =~ s/^\s*//;
            ++$depth unless $line =~ /^else|:$/;
            my $indent = " " x ($reindent * $depth);
            if ($lncols) {
                appendmsg sprintf "%${lncols}d:%s%s", $rec->[1], $indent, $line;
            }
            else {
                appendmsg $indent . $line;
            }
        }
        $outbuf->Delete(1) if $outbuf->Count();
        # trivial detection of non-function
        (@slines && $slines[0] =~ /\(.*\)/) and bclear;
        # adjust context buffer
        my $height = $outbuf->Count();
        ($height > $maxrows) and $height = $maxrows;
        ($height < $minrows) and $height = $minrows;
        $outwin and $outwin->SetHeight($height);
        if (! $seetop) {
            # how to scroll the output buffer down??
        }
        @slines=();
    }
}

sub init {
    # get vim-ctx settings
    $lncols   = VIM::Eval('g:CTX_lncols');
    $reindent = VIM::Eval('g:CTX_reindent');
    $debug    = VIM::Eval('g:CTX_debug');
    $minrows  = VIM::Eval('g:CTX_minrows');
    $maxrows  = VIM::Eval('g:CTX_maxrows');
    $seetop   = VIM::Eval('g:CTX_seetop');

    # find our output buffer and clear it
    $outwin = 0;
    ($outbuf) = VIM::Buffers('--context--');
    if (!defined $outbuf) {
        VIM::Msg("ctx: Unexpected loss of context buffer");
        $lnum = 1;
        return;
    }
    $outbuf->Delete(1, $outbuf->Count());

    # find our output window for use later
    foreach my $win (VIM::Windows()) {
        my $buf = $win->Buffer() || -1;
        if ($buf == $outbuf) {
            $outwin = $win;
            last;
        }
    }

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
    my $gotlabel= 0; # we've seen a label at this brace depth
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

        # handle line continuations by peeking at previous line
        my $prevline = $main::curbuf->Get($CTX::lnum - 1);
        if (defined $prevline && $prevline =~ s/\\$//) {
            # combine this line with the previous and go 'round again
            --$CTX::lnum;
            $_ = $prevline . $_;
            next;
        }

        # comments and strings
        s{^[#]         .*$}{}x;  # for now, clobber #directives
        s{    " .*? " \s* }{}gx;
        s{   /\*.*?\*/\s* }{}gx;
        s{   //        .*$}{}x;
        s{^.*      \*/\s*$}{}x   and do { $comment = 1; next };
        s{   /\*       .*$}{}x   and do { $comment = 0       };
        $comment                 and do { $_ = '';      next };

        # state machine
        PRECTRL and do {
            $gotbrace = 0;
            s/\bdo\s*$//x         and do { bdo $&; next};
            s/\belse      \s*$//x and do { bdopre $&; goELSE; next };
            !$gotlabel && $braced==1 && s/^\s*((case\s+)?\w+\s*:)[^:{}]*$//x and
                do { bdo $1; $gotlabel=1; next};
            s/[;{}][^:;{})] *$//x and do { $_.=$&; goBRACE; next };
            s/[)]  [^)]*     $//x and do { badd $&; $parend=1; goPAREN; next };
        };

        PREBRACE and do {
            $gotbrace = 1;
            $gotlabel = 0;
            s/\bdo\s*$// and do {bdo $& . CTXFWD::dowhile($lvl);goPRECTRL;next};
            s/\belse      \s*$//x and do { bdo $&; goELSE; next };
            s/[;:{}][^;:{})]*$//x and do { bdo '{'; $_.=$&; goBRACE; next };
            s/[)]  [^)]*     $//x and do { badd $&; $parend=1; goPAREN; next };
        };

        BRACE ||
        EBRACE and do {
            BRACE  && $braced==0 and do { $braced=1;++$lvl;bln;goPREBRACE;next};
            EBRACE && $braced==0 and do { $braced=1; goPREELSE;  next };
            !$gotlabel && $braced==1 && s/^\s*((case\s+)?\w+\s*:)[^:{}]*$//x and
                do { bdo $1; $gotlabel=1; next};
            s/}[^{}:]*$//x        and do { ++$braced; next };
            s/{[^{}:]*$//x        and do { --$braced; next };
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
function! s:autohide()
    if g:CTX_autohide && bufloaded('--context--') > 0
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
            call s:hide()
        endif
    endif
endfunction

" move the cursor to a named buffer "
function! s:gotobuf(buf)
    let start = winnr()
    while bufname('%') != a:buf
        wincmd w
        if winnr() == start
            echomsg "Couldn't find " . a:buf
            return -1
        endif
    endwhile
endfunction

" close the context buffer "
function! s:hide()
    let start = bufname('%')
    if start == '--context--'
        q!
    else
        if s:gotobuf('--context--') == 0
            q!
        endif
        call s:gotobuf(start)
    endif
endfunction

" toggle the context buffer on and off "
function! s:toggle()
    if bufloaded('--context--') > 0
        let g:CTX_autoupdate = 0
        call s:hide()
    else
        let g:CTX_autoupdate = 1
        call s:update()
    endif
endfunction

" main function "
function! s:update()
    if bufloaded('--context--') == 0
        let start = bufname('%')
        topleft new --context--
        set filetype=c nowrap buftype=nofile bufhidden=delete noswapfile
        call s:gotobuf(start)
    endif

    perl CTXBACK::run;
endfunction

" called at every CursorHold even "
function! s:cursorhold()
    if g:CTX_autoupdate
        call s:update()
    endif
endfunction

augroup CTX
    au!
    au CursorHold  *.[cC] call <SID>cursorhold()
    au BufEnter    *      call <SID>autohide()
augroup END

noremap <Plug>toggle :call <SID>toggle()<CR>
if !hasmapto ('<Plug>toggle')
    map <unique> \c <Plug>toggle
endif

" vim: filetype=perl
