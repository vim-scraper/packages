" Random utility functions
" Last modified: 		22-08-03  
" Maintainer: 			Sapan Bhatia 	<sapan@corewars.org>
" 
" 1.
" Count no. of occurrences of a PERL regex in a range of lines 
" Cmd- 	:<range>Count <regex>
"
" 2.
" Insert timestamp at current cursor position in command mode
" Cmd- <Leader>t

com -range=% -nargs=1 Count <line1>,<line2>call s:Count(<f-args>) 
com TS call s:Timestamp()
map <unique> <Leader>t :TS<CR>

perl <<done
sub count
{
	$expr=shift;
	$l1=shift;
	$l2=shift;
	VIM::Msg($out);
	@buf=$curbuf->Get($l1 .. $l2);
	$count=0;
	for(@buf) {@c = m/$expr/g; $count+=@c};
	$ans="Count: ".$count;
	VIM::Msg($ans);
}

sub timestamp() {
	($row,$col)=$curwin->Cursor();
	$line=$curbuf->Get($row);
	$dstring=`date +"%d-%m-%y "`;
	chomp($dstring);
	substr($line,$col,0)=$dstring;
	$curbuf->Set($row,$line);
}
done

function s:Count(expr) range
	exec "perl count " . a:expr . ", " . a:firstline . ", " . a:lastline
endfunction

function s:Timestamp()
	perl timestamp
endfunction
