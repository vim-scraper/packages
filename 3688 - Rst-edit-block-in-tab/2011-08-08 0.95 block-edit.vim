
" block-edit.vim
" 
" Edit reST directive blocks in a separate tab.
" 
" This allows you to edit the contents of a block like:
" 
" .. code::
" 
"   class Mica:
"       
"       def __init__(self):
"           pass
" 
" in a separate tab, then close that tab and have the contens
" put back into the original file.

"
" Configuration
" -------------
" 
" These mappings control entering and leaving block editing.
"
nnoremap \e :call BeginBlockEdit()<CR>
let g:block_leave_key = "q"

function! BeginBlockEdit()
perl <<END

use File::Basename;

sub begin_block_edit {
    my ($line, $_) = $curwin->Cursor();
    while ($line >= 0) {
        if ($line < 1) {
            VIM::Msg("No block");
            last;
        }
        
        my $line_text = $curbuf->Get($line);
        if ($line_text =~ /^\.\./) {
            cont_block_edit($line);
            last;
        }
        
        $line--;
    }
}

sub cont_block_edit {
    my ($line) = @_;
    my $header_text = $curbuf->Get($line);

    # Look for a name for this file

    my $file_name = "block";
    if ($header_text =~ /(\S+\.\S+)/) {
        $file_name = $1;
    }

    $file_name = basename($file_name);
    $file_name = "/tmp/$file_name";

    # Find lines that are part of this block

    my $header_line = $line;
    my $start_line  = $header_line+1;
    my $end_line    = $curbuf->Count();

    my $indent = 0;

    $line++;

    while ($line < $curbuf->Count()) {
        my $line_text = $curbuf->Get($line);

        if ($line_text =~ /\S/) {
            $start_line = $line;
            last;
        }
        $line++;
    }
    
    $indent = find_indent($start_line);
    if ($indent <= 0) {
        $indent = 4;
    }

    while ($line < $curbuf->Count()) {
        my $here_indent = find_indent($line);
        if ($here_indent != -1 and $here_indent < $indent) {
            $end_line = $line-1;
            last;
        }
        $line++;
    }

    exec_block_edit($file_name, $start_line, $end_line, $indent);
}

sub exec_block_edit {
    my (
        $file_name,
        $start_line,
        $end_line,
        $indent
    ) = @_;
    
    my @lines = ();
    for (my $i=$start_line; $i<=$end_line; $i++) {
        push(@lines, $curbuf->Get($i));
    }
    map {s/\t/    /g} @lines;
    map {s/^( ){$indent}//} @lines;
    
    my $parent_name = $curbuf->Name();
    VIM::DoCommand("setlocal nomodifiable");
    VIM::DoCommand("tabe $file_name");
    
    my $key = VIM::Eval("g:block_leave_key");
    VIM::DoCommand("nnoremap <buffer> $key :call "
        . "RestoreBlockEdit("
        . "\"$parent_name\", $start_line, $end_line, $indent)<CR>"
    );
    
    $curbuf->Delete(1, $curbuf->Count());
    if (scalar(@lines) > 0) {
        $curbuf->Append(1, @lines);
    }
}

sub find_indent {
    my ($line) = @_;
    my $line_text = $curbuf->Get($line);
    
    if (! ($line_text =~ /\S/)) {
        return -1;
    }

    $line_text =~ /^(\s*)/;
    
    my $spaces = $1;
    $spaces =~ s/\t/    /g;
    
    return length($spaces);
}

begin_block_edit();

END
endfunction

function! RestoreBlockEdit(parent, start_line, end_line, indent)
perl <<END

sub restore_block_edit {
    my $parent_name = VIM::Eval("a:parent");
    my $start_line  = VIM::Eval("a:start_line");
    my $end_line    = VIM::Eval("a:end_line");
    my $indent      = VIM::Eval("a:indent");
    
    my @lines = $curbuf->Get(1 .. $curbuf->Count());
    my @lines = strip_edge_blanks(@lines);
    
    my $spaces = " " x $indent;
    map {s/^/$spaces/} @lines;
    
    VIM::DoCommand("setlocal swapfile");
    VIM::DoCommand("w");
    VIM::DoCommand("close");
    VIM::DoCommand("setlocal swb=usetab");
    VIM::DoCommand("sbuf $parent_name");

    VIM::DoCommand("setlocal modifiable");
    $curbuf->Delete($start_line, $end_line-1);
    $curbuf->Append($start_line-1, @lines);
}

sub strip_edge_blanks {
    my @lines = @_;

    if (scalar(@lines) == 0) {
        return @lines;
    }

    my $start = 0;
    my $end = $#lines;

    while ($start <= $end) {
        last if ($lines[$start] =~ /\S/);
        $start++;
    }
    while ($end >= $start) {
        last if ($lines[$end] =~ /\S/);
        $end--;
    }
        
    return @lines[$start .. $end];
}

restore_block_edit();

END
endfunction

