" Style checker
" Author: (c) 2004 Mikolaj Machowski ( mikmach AT wp DOT pl )
" Version: 1.0 
" License: Vim Charityware 
" Require: +perl feature, Vim 6.1
" Thanks: Antony Scriven for help with $match 
"
" Features:
"      Repetition of words is sign of bad style in most languages. This script
"      will help to track repetition of words in LaTeX, HTML and mail files.
"
"      LaTeX: don't take into account \keywords
"      HTML: don't highlight content of tags
"      Mail: don't look in quoted messages, headers and signatures 
"
"      There is also support for any other filetype. StyleChecker will
"      highlight there only repeated words in comments.
"
"      This script requires Perl interface. Theoretically it could be done in
"      pure VimL but multiple iterations through long lists of words make it
"      practically impossible - vote for arrays in VimL!
"
" Installation:
"      Put this file into your .vim/macros directory and source it from local
"      ftplugins. Of course you can place this file in .vim/plugin directory
"      but this file will be sourced every time you open Vim.
"
" Usage:
"      Default shorcut for style checking is ,, . You can change it by placing
"      in .vimrc: 
"            map <YourShortcut> <Plug>StyleChecker
"
"      After execution of map script will highlight repeated words.
"      Next execution of map restore original highlighting of file.
"      In console Vim offending words will be highlighted with 8 colors, in GUI
"      with 16. 
"      Thanks to Perl's pragma locale, language specific letters will be
"      treated properly. 
"
" Settings:
"      Behavior of StyleChecker you can control through variables. They are
"      Vim variables, there is no need to play with Perl.
"
" 1. Sensitivity for repeating. It will highlight word only if is repeated in
"    text more than value. Default one is for Polish, English writers can
"    safely increase it.
"    Default: 1 (for Polish) 
if !exists("g:SC_sensitivity")
   let g:SC_sensitivity = 1
endif   

" 2. How long has to be string to consider it as repeated
"    Useful for languages with conjugation. Setting to 0 is discouraged :) 
"    Default: 4 or 5?
if !exists("g:SC_length")
   let g:SC_length = 4	
endif

" 3. How much than screen (from top and bottom)
"    This variable will decide how much of text will be taken for analize.
"    Vim will take lines from current screen + X lines from above the top
"    border and X from below bottom border. 
"    Default: 10 
if !exists("g:SC_morethanscreen")
   let g:SC_morethanscreen = 10
endif   

" 4. Don't look for surnames and geographical names in the middle of
"    sentence. Should be turned off for German.
"    Default: 1
"
if !exists("g:SC_names")
	let g:SC_names = 1
endif

" --------------------------------------------------------------------

if v:version < 601 || !has("perl")
	echo "Your Vim version doesn't meet StyleChecker requirements"
	finish
endif

" Make sure map to StyleChecker wasn't made in vimrc or elsewhere
"
if !hasmapto('<Plug>StyleChecker', 'n')
	map <buffer> <silent> ,, <Plug>Stylechecker
endif

map <Plug>Stylechecker :call SC_used()<cr>

" Check if SC() was called, it will make possible to use one mapping as
" a switch
function! SC_used()
	if !exists("g:SC_used") || g:SC_used == 0
		call SC()
		let g:SC_used = 1
	else
		syntax off | syntax on
		let g:SC_used = 0
	endif
endfunc
function! SC()
	" Remember start position
	let sline = line('.')
	let scol = col('.')
	" Get screen border lines
	normal! H
	let hline = line('.')
	normal! L
	let lline = line('.')

	perl << EOF

	# Destroy tables and variables
	$string = '';
	%been = '';
	@table = '';
	@matching = '';
	@repeated = '';

	# Import into perl part Vim variables
	$SC_sensitivity = VIM::Eval('g:SC_sensitivity');
	$SC_morethanscreen = VIM::Eval('g:SC_morethanscreen');
	$SC_length = VIM::Eval('g:SC_length');
	$SC_ft = VIM::Eval('&filetype');
	$SC_names = VIM::Eval('g:SC_names');
	$SC_guirun = VIM::Eval('has("gui_running")');

	# Check current screen +- some lines.
	$hline = VIM::Eval('hline') - $SC_morethanscreen;
	$lline = VIM::Eval('lline') + $SC_morethanscreen;
	@lines = $curbuf->Get($hline .. $lline);

	# create one line, easier operations
	$string = join ' ', @lines;

	# Supported: latex, html, mail
	# Remove common garbage:
	$string =~ s/;|,|\[|\]|\?|!|\.|:|%|\(|\)|\+|\*|~|\{|\}|@|=|`|'|"/ /cg;

	# Remove &ft specific garbage
	if ( $SC_ft eq "tex" ) {
		$string =~ s/<|>|=|#|\// /cg;

		# Detach word\latexword constructs
		$string =~ s/\\/ $0/cg;

	} elsif ( $SC_ft eq "html" ) {
		$string =~ s/#|\\/ /cg;

		# use HTML::Entities; ? 
		$string =~ s/&(amp|gt|lt|quot|apos|nbsp)/ /gc;

		# Be sure tags are separated or just remove tags? Latter could solve
		# highlighing of words from title and its kind arguments, but
		# hell will broke with unmatching <>.
		$string =~ s/</ $0/cg;
		$string =~ s/>/$0 /cg;

	} elsif ( $SC_ft eq "mail" ) {

		$string =~ s/\\|\/|<|>|#/ /cg;

	} else {
		$string =~ s/;|,|\[|\]|\?|!|\.|:|%|\(|\)|\+|\*|~|\{|\}|@|=|`|'|"/ /cg;
	    $string =~ s/\\|\/|<|>|#/ /cg;
	}

	# Make it lowercase for better comparison:
	$string = lc $string;

	# split it into table for processing
	@table = split / /, $string;

	foreach (@table) {
		# Only latin1 letters?
		use locale;
		if ( $SC_length != 0 ) {
			if (/\b[[:upper:][:lower:]]{$SC_length,}/) {
				push @matching, $_;
			}
		} else {
			push @matching, $_;
		}

	}

	# now we have table of elements which make can be dangerous
	# found repeating elements...
	foreach (@matching) {
		# Get only first x letters to take care about conjugation and
		# words with the same core
		if ( $SC_length != 0 ) { s/(.{$SC_length}).*/$1/gc; }

		$been{$_}++;
	}

	foreach (keys %been) {
		if ($been{$_} > $SC_sensitivity) {
			# $SC_ft dependent
			if ( $SC_ft eq "tex" ) {
				# Drop latex keywords
				if ($_ !~ /^-e/) {
					push @repeated, $_;
				}
			} elsif ( $SC_ft eq "html" ) {
				# Drop html tags and escaped chars
				if ($_ !~ /[<>]|^-e/) {
					push @repeated, $_;
				}
			} elsif ( $SC_ft eq "mail" ) {
				# Everything is mine <evil_laugh />
				push @repeated, $_;
			} else {
				# any filetype
				push @repeated, $_;
			}
		}
	}

	# OK, we have now table of repeated elements.
	# Highlight 'em!
	$rpti = 1;

	foreach (@repeated) {
		if ( $SC_guirun == 0 ) {
			if ( $rpti > 8 ) { $rpti = 1; }
		} else {
			if ( $rpti > 16 ) { $rpti = 1; }
		}

		if ($_ ne '') {

		$item = lc;
		$Item = ucfirst $item;

		if ( $SC_names == 1 ) {
		$match 
		= "/\\v\\C<".$item."\\k*|(^\\s*|(\\_s+\\k{2,}|[[:punct:]]{,3})@<=([[:punct:]]\\_s+))@<=<".$Item."\\k*/";
		} else {
		$match = "/\\c".$item."\\k*/";
		}

		# Supported &ft: latex, mail, *ml
		if ( $SC_ft eq "tex" ) {
			$containedin = "containedin=ALLBUT,texSectionName";

		} elsif ( $SC_ft eq "html" ) {
			$containedin = "containedin=ALLBUT,htmlTagName,htmlTag,".
							"htmlArg,htmlValue,htmlString";

		} elsif ( $SC_ft eq "mail" ) {
			$containedin = "containedin=ALLBUT,mailQuoted1,mailQuoted2,".
							"mailQuoted3,mailQuoted4,mailQuoted5,".
							"mailQuoted6,mailHeader,mailHeaderKey,".
							"mailSubject,mailSignature,mailEmail";

		} else {
			$containedin = "containedin=".$SC_ft."Comment"
		}

		VIM::DoCommand("syn match scError".$rpti." ".$match." ".$containedin);

		$rpti = $rpti + 1;

	}
		
	}

EOF
" Make 8 different highlight groups - easier to spot multiplications.
highlight scError1 ctermbg=brown ctermfg=white guibg=brown guifg=white
highlight scError2 ctermbg=gray  guibg=gray
highlight scError3 ctermbg=blue ctermfg=white guibg=blue guifg=white
highlight scError4 ctermbg=green guibg=green
highlight scError5 ctermbg=cyan guibg=cyan
highlight scError6 ctermbg=magenta guibg=magenta
highlight scError7 ctermbg=red ctermfg=white guibg=red guifg=white
highlight scError8 ctermbg=yellow guibg=yellow guifg=black

if has("gui_running")
	highlight scError9  guibg=orange
	highlight scError11 guibg=purple guifg=white
	highlight scError12 guibg=violet
	highlight scError13 guibg=seagreen guifg=white
	highlight scError14 guibg=slateblue guifg=white
	highlight scError15 guibg=magenta
	highlight scError16 guibg=black guifg=white
endif


" Back to start position
call cursor(sline,scol)
endfunction

