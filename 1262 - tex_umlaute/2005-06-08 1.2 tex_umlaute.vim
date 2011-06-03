" Vim tex_umlaute plugin, 
" Version: 1.2
" 
" Description:
" When loading a tex file, this plugin replaces all
" TeX-coded German Umlauts (like \"a) with the 
" normal representation (ä). It also knows {\ss}
"
" I was inspired by Timo Teifels html_umlaute script and just adjusted it a
" bit to use TeX style Umlauts instead of html style.
"
" It uses latin1 coding. If wrong char's appear in your file, edit this script
" so it knows the correct ones.
" 
" Maintainer: Frank Thieme <frank at fthieme dot net>
" 
" Usage:
" It does everything automatically. When reading a file,
" it replaces \"? with the corresponding Umlauts, and
" {\ss} with ß.
" When saving, it replaces the special Characters with the
" TeX-Code, undoing it after the write, to keep the chars
" if you keep working with the file.
"
" It recognizes {\"a}, \"a, \"{a} and "a but it writes \"a. 
"
" I changed the output from the Emacs iso-cvt like {\"a} after
" Jan Heerda <janjh at mathematik dot hu-berlin dot de> 
" pointed out, that kerning is disturbed badly by using {\"a} and it
" is not disturbed by using \"a. More in Mark Trettin's "An 
" essential guide to \LaTeXe{} usage" (p. 10) [CTAN:info/l2tabu/]
"
" Installation:
" Save this file as ~/.vim/plugin/tex_umlaute.vim or :source
" it manually when needed
"
" Licence: GPL v2.0 or any later version
"
" Changelog:
" v1.2, 2005-06-08
"  - now (n)german style is really (n)german style
" v1.1, 2005-06-04
"  - changed output to \"a instead of {\"a}
"  - detection of \"{a}
" v1.0, 2005-04-15
"  - initial release (based on html_umlaute 1.1)

" do this only once per buffer:
if exists("b:loaded_tex_umlaute")
    finish
endif
let b:loaded_tex_umlaute = 1

if has("autocmd")
augroup tex_Umlaute
    au!
    au FileType      tex             call s:Tex2Char()
    au BufWrite      *.tex		     call s:Char2Tex()
    au BufWritePost  *.tex     		 call s:Tex2Char()
augroup END
endif


" functions need to be sourced only once per session
if exists("s:loaded_tex_umlaute_functions")
  finish
endif
let s:loaded_tex_umlaute_functions = 1

function s:Tex2Char()
    " remember cursor position:
	let s:line = line(".")
	let s:column = col(".")
    " if more than 'report' substitutions have been done, vim 
    " displays it.
    let s:save_report = &report
    set report=99999
    " really nice Umlauts like Emacs iso-cvt writes
    %s/{\\"a}/ä/eIg
    %s/{\\"o}/ö/eIg
    %s/{\\"u}/ü/eIg
    %s/{\\"A}/Ä/eIg
    %s/{\\"O}/Ö/eIg
    %s/{\\"U}/Ü/eIg
    %s/{\\ss}/ß/eIg
    " normal styled Umlauts
    %s/\\"a/ä/eIg
    %s/\\"o/ö/eIg
    %s/\\"u/ü/eIg
    %s/\\"A/Ä/eIg
    %s/\\"O/Ö/eIg
    %s/\\"U/Ü/eIg
    %s/\\ss{}/ß/eIg
    %s/\\ss/ß/eIg
    " more rather normal styled Umlauts
    %s/\\"{a}/ä/eIg
    %s/\\"{o}/ö/eIg
    %s/\\"{u}/ü/eIg
    %s/\\"{A}/Ä/eIg
    %s/\\"{O}/Ö/eIg
    %s/\\"{U}/Ü/eIg
    %s/\\{ss}/ß/eIg
    " if you use package german or ngerman you can encode Umlauts like this
    %s/"a/ä/eIg
    %s/"o/ö/eIg
    %s/"u/ü/eIg
    %s/"A/Ä/eIg
    %s/"O/Ö/eIg
    %s/"U/Ü/eIg
    %s/"s/ß/eIg
    let &report=s:save_report
    unlet s:save_report
    call cursor(s:line,s:column)
    unlet s:line
    unlet s:column
endfunction

function s:Char2Tex()
	let s:line = line(".")
	let s:column = col(".")
    let s:save_report = &report
    set report=99999
    %s/ä/\\"a/eIg
    %s/ö/\\"o/eIg
    %s/ü/\\"u/eIg
    %s/Ä/\\"A/eIg
    %s/Ö/\\"O/eIg
    %s/Ü/\\"U/eIg
    %s/ß/\\ss{}/eIg
    let &report=s:save_report
    unlet s:save_report
    call cursor(s:line,s:column)
    unlet s:line
    unlet s:column
endfunction

