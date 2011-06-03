" Vim Bugle syntax 
" Maintainer: Emmanouel Kellinis (me@cipher.org.uk) 
" Last Change: 2006 Dec 21
" Highligh possible security bugs in Java/C/PHP/C++/ASP source code, using the current Bugle list.
" To most recent bug list is at http://www.cipher.org.uk/index.php?p=projects/bugle.project 
" The list is very small at the moment, to give more bug regexp submit them at the link above or 
" sent an email to bugle@cipher.org.uk 

"C bugs"
syntax match bug "strcpy(buf \| buffer,str)"
syntax match bug "strcpy(.*,argv\[.*])"
syntax match bug "syslog(.*,.*)"
syntax match bug "char buffer\[.*\]\|strcpy(buffer, str)"
syntax match bug "char buffer\[.*\];\|sprintf(.*,\m%s, .*);" 
syntax match bug "char buffer\|buf\[.*\];\|gets(buffer\|buf);"
syntax match bug "strcat(char \*.*, const char \*.*)\|buffer\[.*\]|buf\[.*\]" 
syntax match bug "if (len\|length > size).*{\|sizeof(buffer\|buf)"
syntax match bug "vsprintf(str)"
syntax match bug "system(argv[.*])"
syntax match bug "popen(argv\[.*\], "
"C and C++"
syntax match bug "CreateFileMapping(.*,NULL,.*,.*,.*,.*)"
"C#"
syntax match bug "unsafe"
"PHP bugs"
syntax match bug "\(system\|popen\|shell_exec\|exec\)($_\(GET\|POST\|COOKIE\|REQUEST\|SESSION\).*)"   
syntax match bug "eval($_\(GET\|POST\|REQUEST\|COOKIE\).*)"
syntax match bug "mysql_query(.*$_\(GET\|POST\|COOKIE\|SESSION\|REQUEST\).*)"
syntax match bug "WHERE *. =.*$_\(GET\|POST\|COOKIE\|SESSION\|REQUEST\).*"
syntax match bug "echo\|print\(.*\)$_SERVER['PHP_SELF']"
"ASP Bugs"
syntax match bug "request.form\|request.querystring\|ADODB.Connection"
syntax match bug "option explicit off"
"Java Bugs"
syntax match bug "executequery\|request.getparameter"
"Comments"
syntax match bug "bug\|hack\|fixme\|XXX\|backdoor\|fixme\|dirty hack"
highlight link bug Todo 

