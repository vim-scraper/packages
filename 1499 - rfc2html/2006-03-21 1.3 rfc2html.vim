" This VIM script convers a RFC into a html file
" By Samuel Hangouët (shangouet at laposte.net)
" Enjoy !
" Version: 1.3 (21/03/2006)
" Todo: - add DocTypes
"       - add external links for ressources on the Internet (links to other RFC, etc.)
"       - add links for references ('<a href="[x] ">[x]</a>' and '<a name="[x] "/>[x]')

" This script creates 3 html files in the working directory
" from the current open RFC (index.html, toc.html and content.html)

" === INDEX ===
sp index.html
norm ggVGdo<html>
norm o<frameset cols="35%,*">
norm o<frame src="toc.html" />
norm o<frame src="content.html" name="content" />
norm o</frameset>
norm o</html>
wq!

" === TOC ===
" Find delimitations of TOC and put it in a navigation frame
norm /^Table of Contentsmm/^1kml
norm 'mV'l"ad
sp toc.html
norm ggVGd
norm "apddGo
" Suppress pges delimitations inside the TOC
%s/\n\+.*\n\n.*\n*/\r/
" Join 2-lines items into only one line
%s/^\(\s\{3}\d.*\D\)\n\(\s\{4}.*\d\)$/\1<br \/>\2/e
" Put links to the other frame inside the TOC
%s/\(\s\{3}\)\(\S\+\)\(.*\s\d\+\)$/\1<a href="content.html#\2" target="content">\2\3<\/a>/
" Build HTML header and footer
norm ggO<html> <head>  <title>
norm "%pA</title>
norm o </head> <body>  <pre>
norm Go  </pre> </body></html>
" Save and close
wq!

" === CONTENT ===
" Headers/Footers improvement
norm Go
%s/\(.*\)\[\(Page \d\+\)]\n\n\(.*\)/<a name="\2" \/>\1[\2]<br \/><hr \/><br \/>\3/
%s/^\(Network Working Group\|Internet-Draft\|Expires:\|Request for Comments:\|Obsoletes:\|Category:\)/ <u>\1<\/u>/e
" Put anchors everywhere and make titles bold
%s/^\(\w\S*\)\(.*\)$/<a name="\1" \/><strong>\1\2<\/strong>/
" Build HTML header and footer
norm ggO<html> <head>  <title>
norm "%pA</title>
norm o </head> <body>  <pre>
norm Go  </pre> </body></html>
sav! content.html
