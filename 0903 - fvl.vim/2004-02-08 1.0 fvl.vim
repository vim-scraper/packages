" fvl.vim Copyright 2003 Ray Wallace III
"
" This program is free software; you can redistribute it and/or
" modify it under the terms of the GNU General Public
" License as published by the Free Software Foundation; either
" version 2 of the License, or (at your option) any later version.
"
" This program is distributed in the hope that it will be useful,
" but WITHOUT ANY WARRANTY; without even the implied warranty of
" MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
" General Public License for more details.
"
" You should have received a copy of the GNU Library General Public
" License along with this library; if not, write to the
" Free Software Foundation, Inc., 59 Temple Place - Suite 330,
" Boston, MA 02111-1307, USA.

" What we do:
" Use the Expect module to spawn the interpreter indicated by $LispInterp and
" create pipes for it. When the user wants to run some lisp (via LispDo,
" LispCount, or LispRange), write it out the pipe and the use expect() to read
" output from the interpreter until it prints the prompt defined by
" $LispPrompt.
"
" This plugin most likely works not just for lisp interpreters, but for any
" program that has a read-evaluate loop and prints a predictable prompt.
"
" LispOpen			Opens the Lisp Window
" LispClose			Closes same and shuts down Lisp
" LispRefresh		Try to read more from Lisp
" LispDo			Works like perldo. You MUST escape quotes. ex:
"						:LispDo (print \"hello\")
" LispCount			Evaluate [count] lines
" LispRange			Evaluate [range] lines
"

" REQUIREMENTS:
" 	perl5
"	perl Expect module (# cpan Expect)
"	vim (compiled with +perl, +windows might be needed also)
"	gcl or clisp (possibly any lisp interp)
"

"----------------------------------
" Site configurable variables follow
"----------------------------------

perl <<EOF

$LispInterp = "clisp";			# This is the name of the lisp interpreter
								#   that will be used.
$LispPrompt = "^.*\\[\\d+\\]>";	# This here's a regular expression that
								#	matches your lisp's prompt. This one matches
								#	CLisp.
$LispTimeout = 10;				# How long until an attempt to read from lisp
								#   will timeout, in seconds.
$JumpToLisp = 1;				# If this is true, then jump to the window
								#   containing the lisp interpreter after every
								#   lisp command.
$DefaultWindowSize = 15;		# The initial height of the Lisp window.

EOF

"----------------------------------
" End site configurable variables
"----------------------------------

" Globals
perl <<EOF
use Expect;

$Buffer;
$Window;
$Pipe;

EOF

" Talking to Lisp
perl <<EOF

sub readFromLisp(){
	my $done;

	if($Buffer && $Pipe){
		# read until we find a line that matches $LispPrompt, then put all the
		# text that came before the prompt into $text.
		my @ret = $Pipe->expect($LispTimeout, '-re', $LispPrompt);
		my $text = $Pipe->exp_before();

		# for whatever reason, newlines come across as null characters, so replace
		# them with the newline.
		$text =~ tr/\0x0/\n/;

		$Buffer->Append($Buffer->Count(), split(/\n/, $text));
		$Buffer->Append($Buffer->Count(), "> ")
				if !$ret[1];

		$_ = $Buffer->Count();
		VIM::Windows($Window)->Cursor($_, length($Buffer->Get($_)));
		if($JumpToLisp){
			VIM::DoCommand($Window."wincmd w");
			# Vim seems to queue up all the commands to execute at once so that
			# we only get the _net-effect_ of all of the commands. So, if we do
			# the command below, then the command above doesn't get done, so the
			# lisp window doesn't scroll. 
#			VIM::DoCommand("wincmd p");
		}
	}
	else{
		VIM::Msg("Lisp buffer or pipe isn't open!", "Error");
	}
}

sub writeToLisp(@){
	my $text = "@_";

	if(!$Pipe){
		initLispBuffer();
	}

	if($Buffer && $Pipe){
		$text = $text."\n" if($text !~ /^.*\n$/);

		$Pipe->send($text);
		$_ = $Buffer->Count();
		$Buffer->Append($Buffer->Count(), split(/\n/, $text));

		# Catch up with what lisp has to say
		readFromLisp();
	}
	else{
		VIM::Msg("Lisp buffer isn't open!", "Error");
	}
}

sub writeCountToLisp($){
	my @cursor = $curwin->Cursor();

	writeToLisp($curBuf->Get($cursor[0] .. $cursor[0] + $_[0]));
}

sub writeRangeToLisp($$){
	my ($first, $second) = @_;

#	die "$first .. $second \n";
	writeToLisp($curbuf->Get($first .. $second));
}

EOF

" Starting up Lisp
perl <<EOF

sub setupWindow(){
	my $winNum = 0;

	# make a new window with the file "Lisp.Interpreter", then disable saving
	# the file, disable its swap file, and turn on lisp syntax highlighting.
	VIM::DoCommand($DefaultWindowSize."split +e Lisp.Interpreter");
	VIM::DoCommand("setlocal buftype=nofile noswapfile filetype=lisp");

	$Buffer = VIM::Buffers("Lisp.Interpreter")
			or die "Couldn't Create the Lisp buffer!\n";
	$Buffer->Append(0, "Faboo's Lisp Wrapper");

	# figure out what window Vim put our buffer in.
	foreach (VIM::Windows()){
		++$winNum;
		$Window = $winNum if $_->Buffer()->Name() eq "Lisp.Interpreter";
	}

#	VIM::DoCommand("let g:window = $Window");
}

sub startLisp($){
	my $lisp = $_[0];
	my $pid;

	# create a new expect object, change its behavior to be pipe-like and
	# disable the echoing of lisp's output to the terminal, then fork/exec the
	# lisp interpreter.
	$Pipe = new Expect();
	$Pipe->raw_pty(1);
	$Pipe->log_stdout(0);
	$Pipe->spawn($lisp) or die "Couldn't run Lisp\n";

	print "Lisp is running\n";
}

sub initLispBuffer(){
	$" = "\n";

	unless($Pipe){
		setupWindow();
		startLisp($LispInterp);
		readFromLisp();
	}
}

EOF

" Tearing Lisp down
perl <<EOF

sub closeLisp(){
	print "Exiting Lisp\n";
	$Pipe->send("(exit)");

	undef $Pipe;
}

sub closeLispBuffer(){
	my $bufNum = $Buffer->Number();

	VIM::DoCommand("bd $bufNum");
}

EOF

autocmd BufDelete Lisp.Interpreter perl closeLisp()

command! -nargs=0 -bar LOpen perl initLispBuffer()
command! -nargs=0 -bar LispOpen perl initLispBuffer()
command! -nargs=0 -bar LClose perl closeLispBuffer()
command! -nargs=0 -bar LispClose perl closeLispBuffer()
command! -nargs=0 -bar LRefresh perl readFromLisp()
command! -nargs=0 -bar LispRefresh perl readFromLisp()

command! -nargs=* -bar LispDo perl writeToLisp(q!<args>!)
command! -nargs=0 -bar -count=0 LispCount perl writeCountToLisp(<count>)
command! -nargs=0 -bar -range LispRange perl writeRangeToLisp(<line1>, <line2>)

