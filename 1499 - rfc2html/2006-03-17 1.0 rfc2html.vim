" This VIM script convers a RFC into a html file
" By Samuel Hangouët (shangouet at laposte.net)
" Enjoy !
" Version: 1.0 (17/03/2006)

" Suppress headers and footers
%s//<hr \/>/
" Header improvement
%s/^\(Network Working Group\|Internet-Draft\|Expires:\|Request for Comments:\|Obsoletes:\|Category:\)/ <u>\1<\/u>/e
" Put anchors everywhere and make titles bold
%s/^\(Table of Contents\)/<a name="__TOC__" \/><strong>\1<\/strong>/
%s/^\([A-Z0-9]\+[A-Za-z0-9.]\+\)\(.*\)$/<a name="\1" \/><strong>\1\2<\/strong>/
" Convert table of content
while search(' \. \+[0-9]\+$', 'w') > 0
    normal 0wi<a href="#
    normal lvEyPa">
    normal A</a>
    normal <C-O>
endwhile
" Build HTML header (lack of a doctype...)
normal ggO<html> <head>  <title>
normal "%pA</title>
normal o </head> <body>  <pre>   <a href="#__TOC__">Go to Table of Content</a><br />
normal Go  </pre> </body></html>
sav! %:r.html
