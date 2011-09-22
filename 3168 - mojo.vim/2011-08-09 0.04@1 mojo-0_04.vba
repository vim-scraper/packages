" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
ftdetect/epl.vim	[[[1
2
au BufRead,BufNewFile *.html.epl set filetype=html.epl
au BufRead,BufNewFile *.html.ep  set filetype=html.epl
syntax/epl.vim	[[[1
72
" html w/ Perl as a preprocessor
" Language:    Perl + html
" Maintainer:  yko <yko@cpan.org>
" Version:     0.04
" Last Change: 2011 Aug 09
" Location:    http://github.com/yko/mojo.vim
" Original version: vti <vti@cpan.org>

if exists("perl_fold")
   let b:bfold = perl_fold
   unlet perl_fold
endif

" Clear previous syntax name
unlet! b:current_syntax

" Include Perl syntax intp @Perl cluster
syntax include @Perl syntax/perl.vim

" This groups are broken when included
syn cluster Perl remove=perlFunctionName,perlElseIfError

if exists("b:bfold")
    let perl_fold = b:bfold
    unlet b:bfold
endif

" Begin and end of code blocks
syn match MojoStart /<%=\{0,2}/ contained
syn match MojoSingleStart /^\s*%=\{0,2}/  contained
syn match MojoEnd /=\{0,1}%>/ contained

syn cluster Mojo contains=MojoStart,MojoEnd

" Highlight code blocks
syn region PerlInside keepend oneline start=+<%=\{0,2}+hs=s end=+=\{0,1}%>+he=s-1,me=s-1 contains=MojoStart,@Perl nextgroup=MojoEnd
syn region PerlInside keepend oneline start=+^\s*%=\{0,2}+hs=s end=+$+ contains=MojoSingleStart,@Perl

if !exists("mojo_no_helpers")

    " Default helpers
    syn match perlStatementFiledesc  "\<\%(app\|content\|content_for\|dumper\|extends\|flash\|include\|layout\|memorize\|param\|session\|stash\|url_for\|title\)\>" nextgroup=perlGenericBlock skipwhite contained

    " Tag helpers
    syn match perlStatementFiledesc "\<\%(base_tag\|check_box\|file_field\|form_for\|hidden_field\|input_tag\|javascript\|link_to\|password_field\|radio_button\|select_field\|stylesheet\|submit_button\|tag\|text_area\|text_field\)\>" nextgroup=perlGenericBlock skipwhite contained

    " JavaScript
    syn region javaScript start="<%=\{1,2}\s\+javascript\s\+.*begin\s\+%>" end="<%\s\+end\s\+=\{0,1}%>" contains=@htmlJavaScript,PerlInside transparent keepend
    syn region javaScript start="\s*%=\{1,2}\s\+javascript\s\+.*begin\s*$" end="%\s\+end" contains=@htmlJavaScript,PerlInside transparent keepend

    " Style
    syn region CSS start="<%=\{1,2}\s\+stylesheet\s\+.*begin\s\+%>" end="<%\s\+end\s\+=\{0,1}%>" contains=@htmlCss,PerlInside transparent keepend
    syn region CSS start="%=\{1,2}\s\+stylesheet\s\+.*begin\s*$" end="%\s\+end" contains=@htmlCss,PerlInside transparent keepend

endif

" Display code blocks in tag parameters' quoted value like 
" <a href="<%= url_for 'foo' %>'>
syn cluster htmlPreproc add=PerlInside

command -nargs=+ HiLink hi def link <args>

HiLink MojoStart                perlType
HiLink MojoSingleStart          perlType
HiLink MojoEnd                  perlType
HiLink MojoFileName             perlString
HiLink MojoFileNameStart        perlSpecial
HiLink MojoError                Error

delcommand HiLink

let b:current_syntax = "html.epl"
after/syntax/perl/MojoliciousLite.vim	[[[1
49
" Vim syntax file (for including only)
" html w/ Perl as a preprocessor in __DATA__
" Language:    Mojo epl templates stored in Perl __DATA__
" Maintainer:  yko <yko@cpan.org>
" Version:     0.04
" Last Change: 2011 Aug 09
" Location:    http://github.com/yko/mojo.vim
"
" Thanks to Viacheslav Tykhanovskyi for simplified region syntax
"
" Possible configuration:
"
"  let mojo_highlight_data = 1
"  let mojo_disable_html = 1
"
" For highlight templates in __DATA__ add following line to your .vimrc:
" let mojo_highlight_data = 1

if !exists("mojo_highlight_data")
    finish
endif

if !exists("b:current_syntax")
  echoerr "MojolisiousTemplate can only be included in existing syntax"
  finish
endif

" Store current syntax name
let cs = b:current_syntax
unlet b:current_syntax

if !exists("mojo_disable_html")
  unlet! b:current_syntax
  syn include @Html syntax/html.vim
endif

syntax include @Epl syntax/epl.vim

" Set up hl of filename headers
syn match MojoFileNameStart "@@" contained

syn region MojoFileContainer start=/@@/ end=/^__END__\|@@/me=s-1 contains=@Epl,@Html,MojoFileName contained keepend fold
syn region MojoFileName start=/@@/ end="$" keepend contains=MojoFileNameStart contained keepend

" Push Template sections and HTML syntax into @perlDATA cluster
syn cluster perlDATA add=@Html,MojoFileContainer

" Revert current syntax name
let b:current_syntax = cs
snippets/epl.snippets	[[[1
56
# Perl expressions
snippet <%
	<% ${1} %>${2}
snippet <%=
	<%= ${1} %>${2}
snippet <%==
	<%== ${1} %>${2}
# Base tag
snippet base
	<%= base_tag %>
	${1}
# Checkbox input
snippet check
	<%= check_box employed => '${1}'${2} %>${3}
# File input
snippet file
	<%= file_field '${1}'${2} %>${3}
# Form
snippet f
	<%= form_for '${1}' => begin %>
	  ${2}
	<% end %>
# Hidden input
snippet hidden
	<%= hidden_field '${1}'${2} %>${3}
# Image
snippet im
	<%= image '${1}'${2} %>${3}
# Input
snippet input
	<%= input_tag '${1}', type => '${2}'${3} %>${4}
# JavaScript
snippet script
	<%= javascript ${1} %>${2:<% end %>}
# Link to
snippet l
	<%= link_to '${1}' => '${2}' %>${3}
# Password
snippet pass
	<%= password_field '${1}' %>
# Submit
snippet su
	<%= submit_button ${1}%>
	${2}
# URL
snippet url
	<%= url_for '${1}' %>
# Text field
snippet in
	<%= text_field '${1}'${2} %>${3}
## Some Perl binds
# Foreach
snippet foreach
	% foreach my $${1} (${2}) {
	    ${3}
	% }
