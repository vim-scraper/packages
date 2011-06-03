" Perl IO 
" 
" Autor: Jidkov Serguei  mailto:jsv@gorod.bryansk.ru
" Version: 0.1
" 
" Description:
" Plugin creates two output handles: VIMOUT and VIMERROR
" You may use: 
" 
"   :perl print '2+2=', 2+2
"      -  eq :perl VIM::Msg (join $,, ('2+2=', 2+2)) 
"   :perl print VIMOUT '2+2=', 2+2
"      -  same as above
"   :perl print VIMERROR "Error: $!"
"      -  eq :perl VIM::Msg("Error: $!", 'ErrorMsg') 
"   :perl printf "%d:%d", $curwin->Cursor()  
"      -  prints cursor position 
"
"   :TestPerlIO
"       - test function: see end of file
"
" Install Details:
" Simply drop this file into your $HOME/.vim/plugin directory
" 
" To create more handles use 
"   :perl tie *HANDLE, 'VIM::Out', 'Highlight'
" where Hilight - name of hilight group
" To make HANDLE default output handle use
"   :perl select(HANDLE)
"
" TODO: 
" Handles for buffer IO?
" 

if exists('loaded_perl_io') || !has('perl')
    finish
endif
let loaded_perl_io = 1

perl << EOF

use Tie::Handle;
package VIM::Out;
@ISA = qw(Tie::Handle);

sub TIEHANDLE
{
    my ($class, $group) = @_;
    return bless(\$group, $class);
}

sub PRINT
{
    my ($group, @args) = @_;
    VIM::Msg(join($,, @args), $$group);
}

sub PRINTF
{
    my ($group, $format, @args) = @_;
    VIM::Msg(sprintf ($format, @args), $$group);
}

package main;

tie (*VIMOUT, 'VIM::Out');
tie (*VIMERR, 'VIM::Out', 'ErrorMsg');
select VIMOUT;

EOF


"finish

" Test function
"
perl << EOF
sub test_io
{
    print 1 .. 3;
    $, = ', ';
    print 'a' .. 'e';
    undef $,;
    printf '%3s' x 3, qw/aa bb cc/;
    
    tie *VS, 'VIM::Out', 'Search';
    
    print VS "User = $ENV{USER}";
    printf VS '%d=%xh=%oo=%bb', (50)x 4;
    
    untie *VS;
    print VS 'Test';
}
EOF

command TestPerlIO perl test_io()
