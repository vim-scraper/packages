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
"
" 3.
" Comment a block of C code, replacing internal comments with {{{ and }}}
" to cope with nesting
" Cmd- <Leader>c

com -range=% -nargs=1 Count <line1>,<line2>call s:Count(<f-args>) 
com -range -nargs=0 Comment <line1>,<line2>call s:Comment(<f-args>) 
com TS call s:Timestamp()
map <unique> <Leader>t :TS<CR>
map <unique> <Leader>c :Comment<CR>

perl <<done
sub comment {
	$l1=shift;$l2=shift;
	$c1=VIM::Eval("col(\"'<\")");
	$c2=VIM::Eval("col(\"'>\")");
	
	if ($l1 == $l2) {
		$line   	= $curbuf->Get($l1);
		$left   	= substr($line,0,$c1-1);
		$center 	= substr($line,$c1-1,$c2-$c1+1);
		$right  	= substr($line,$c2);
		
		$center =~ s/\/\*/{{{/g;
		$center =~ s/\*\//}}}/g;
		$line = $left."/*".$center."*/".$right;
		$curbuf->Set($l1,$line);
	}
	else {
		@lines = $curbuf->Get($l1 .. $l2);
		$left1 = substr($lines[0],0,$c1-1);
		$lines[0] = substr($lines[0],$c1-1);
		$right2= substr($lines[-1],$c2);
		$lines[-1] = substr($lines[-1],0,$c2);
		for(@lines) {
			s/\/\*/{{{/g;
			s/\*\//}}}/g;
		}
		$lines[0] = $left1."/*".$lines[0];
		$lines[-1]= $lines[-1]."*/".$right2;
		$curbuf->Set($l1,@lines);
	}
}

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

function s:Comment() range
	exec "perl comment ".a:firstline.", ".a:lastline
endfunction
