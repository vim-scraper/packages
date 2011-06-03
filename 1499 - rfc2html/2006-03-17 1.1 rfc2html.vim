" This VIM script convers a RFC into a html file
" By Samuel Hangouët (shangouet at laposte.net)
" Enjoy !
" Version: 1.1 (17/03/2006)
" Todo: - add DocType
"       - external links for ressources on the Internet (links to other RFC, etc.)

" Change page delimiter chars by a ruler
normal Go
%s/\(.*\[\)\(Page \d\+\)]\n\n\(.*\)/<a name="\2" \/>\1\2<br \/><hr \/><br \/>\3/
" Header improvement
%s/^\(Network Working Group\|Internet-Draft\|Expires:\|Request for Comments:\|Obsoletes:\|Category:\)/ <u>\1<\/u>/e
" Find delimitations of TOC
normal /^Table of Contentsmm/^1ml
" Put anchors everywhere and make titles bold
%s/^\(\w\S*\)\(.*\)$/<a name="\1" \/><strong>\1\2<\/strong>/
" Join 2-lines TOC items in only one line
'm,'ls/^\(\s\{3}\d.*\D\)\n\(\s\{4}.*\d\)$/\1<br \/>\2/e
" Put links in the TOC
'm,'ls/\(\s\{3}\)\(\S\+\)\(.*\s\d\+\)$/\1<a href="#\2">\2\3<\/a>/
" Build HTML header and footer
normal ggO<html> <head>  <title>
normal "%pA</title>
normal o </head> <body>  <pre>   <br /><a href="#Table">Go to Table of Content...</a><br />
normal Go  </pre> </body></html>
sav! %:r.html
