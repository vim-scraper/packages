" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
after/ftplugin/actionscript_snippets.vim
5
if !exists('loaded_snippet') || &cp
    finish
endif

Snippet dm duplicateMovieClip(<target>, <newName>, <depth>);"
after/ftplugin/asp_snippets.vim
13
if !exists('loaded_snippet') || &cp
    finish
endif

Snippet rr Response.Redirect(<to>)<>
Snippet app Application("<>")<>
Snippet forin For <var> in <array><CR><><CR>Next<CR><>
Snippet ifelse If <condition> Then<CR><><CR>Else<CR><><CR>End if<CR><>
Snippet rw Response.Write <>
Snippet sess Session("<>")<>
Snippet rf Request.Form("<>")<>
Snippet rq Request.QueryString("<>")<>
Snippet while While <NOT> <condition><CR><><CR>Wend<CR><>
after/ftplugin/c_snippets.vim
44
if !exists('loaded_snippet') || &cp
    finish
endif

function! Count(haystack, needle)
    let counter = 0
    let index = match(a:haystack, a:needle)
    while index > -1
        let counter = counter + 1
        let index = match(a:haystack, a:needle, index+1)
    endwhile
    return counter
endfunction

function! CArgList(count)
    " This returns a list of empty tags to be used as 
    " argument list placeholders for the call to printf
    if a:count == 0
        return ""
    else
        return repeat(', <>', a:count)
    endif
endfunction
	
Snippet do do<CR>{<CR><><CR>} while (<>);
Snippet readfile std::vector<uint8_t> v;<CR>if(FILE* fp = fopen("<filename>", "r"))<CR>{<CR>uint8_t buf[1024];<CR>while(size_t len = fread(buf, 1, sizeof(buf), fp))<CR>v.insert(v.end(), buf, buf + len);<CR>fclose(fp);<CR>}<CR><>
Snippet beginend <v>.begin(), <v>.end()<>
Snippet once #ifndef _<file:substitute(expand('%'),'\(.\)','\u\1','g')>_<CR><CR>#define _<file>_<CR><CR><><CR><CR>#endif /* _<file>_ */<CR><>
Snippet class class <name><CR>{<CR>public:<CR><name> (<arguments>);<CR>virtual ~<name>();<CR><CR>private:<CR><:D('/* data */')><CR>};<CR><>
" TODO This is a good one but I can't quite work out the syntax yet
Snippet printf printf("<"%s">\n" <"%s":CArgList(Count(@z, '%[^%]'))>);<CR><>
Snippet vector std::vector<<char>> v<>;
Snippet struct struct <name><CR>{<CR><:D('/* data */')><CR>};<CR><>
Snippet template template <typename <_InputIter>><CR><>
" TODO this one as well. Wish I knew more C
" Snippet namespace namespace ${1:${TM_FILENAME/(.*?)\\..*/\\L$1/}}\n{\n\t$0\n};<CR><>
Snippet namespace namespace <:substitute(expand('%'),'.','\l&', 'g')><CR>{<CR><><CR>};<CR><>
Snippet map std::map<<key>, <value>> map<>;<CR><>
Snippet mark #if 0<CR><CR><:D('#pragma mark -<CR><CR>'}#pragma mark <><CR><CR>#endif<CR><CR><>
Snippet if if(<>)<CR>{<CR><><CR>}<CR><>
Snippet main int main (int argc, char const* argv[])<CR>{<CR><><CR>return 0;<CR}<CR><>
Snippet Inc #include <<:D('.h')>><CR><>
Snippet inc #include "<>.h"
Snippet for for( <:D('unsigned int')> <i> = <:D('0')>; <i> < <count>; <i> += <:D('1')>)<CR>{<CR><><CR>}<CR><>
after/ftplugin/css_snippets.vim
26
if !exists('loaded_snippet') || &cp
    finish
endif

Snippet visibility <:D('visible/hidden/collapse')>;<>
Snippet list list-style-image: url(<>);<>
Snippet text text-shadow: rgb(<:D('255')>, <:D('255')>, <:D('255')>, <:D('10px')> <:D('10px')> <:D('2px')>;<> 
Snippet overflow overflow: <:D('visible/hidden/scroll/auto')>;<>
Snippet white white-space: <:D('normal/pre/nowrap')>;<>
Snippet clear cursor: url(<>);<>
Snippet margin padding-top: <:D('20px')>;<>
Snippet background background #<:D('DDD')> url(<>) <:D('repeat/repeat-x/repeat-y/no-repeat')> <:D('scroll/fixed')> top left/top center/top right/center left/center center/center right/bottom left/bottom center/bottom right/x% y%/x-pos y-pos')>;<>
Snippet word word-spacing: <:D('10px')>;<>
Snippet z z-index: <>;<>
Snippet vertical vertical-align: <:D('baseline/sub/super/top/text-top/middle/bottom/text-bottom/length/%')>;<>
Snippet marker marker-offset: <:D('10px')>;<>
Snippet cursor cursor: <:D('default/auto/crosshair/pointer/move/*-resize/text/wait/help')>;<>
Snippet border border-right: <:D('1')>px <:D('solid')> #<:D('999')>;<>
Snippet display display: block;<>
Snippet padding padding: <:D('20px')> <:D('0px')>;<>
Snippet letter letter-spacing: <>em;<>
Snippet color color: rgb(<:D('255')>, <:D('255')>, <:D('255')>);<>
Snippet font font-weight: <:D('normal/bold')>;<>
Snippet position position: <:D('static/relative/absolute/fixed')>;<>
Snippet direction direction: <:D('ltr|rtl')>;<>
Snippet float float: <:D('left/right/none')>;<>
after/ftplugin/django_snippets.vim
50
if !exists('loaded_snippet') || &cp
    finish
endif

function! Count(haystack, needle)
    let counter = 0
    let index = match(a:haystack, a:needle)
    while index > -1
        let counter = counter + 1
        let index = match(a:haystack, a:needle, index+1)
        exec "echom \"index: ". string(index) . "\""
    endwhile
    exec "echom \"found ". string(counter) . " needles\""
    return counter
endfunction

function! DjangoArgList(count)
    " This needs to be Python specific as print expects a
    " tuple and an empty tuple looks like this (,) so we'll need to make a
    " special case for it
    if a:count == 0
        return "(,)"
    else
        return '('.repeat('<>, ', a:count).')'
    endif
endfunction

Snippet {{ {% templatetag openvariable %}<>
Snippet }} {% templatetag closevariable %}<>
Snippet {% {% templatetag openblock %}<>
Snippet %} {% templatetag closeblock %}<>
Snippet now {% now "<>" %}<>
Snippet firstof {% firstof <> %}<>
Snippet ifequal {% ifequal <> <> %}<CR><><CR>{% endifequal %}<CR><>
Snippet ifchanged {% ifchanged %}<>{% endifchanged %}<>
Snippet regroup {% regroup <> by <> as <> %}<>
Snippet extends {% extends "<>" %}<CR><>
Snippet filter {% filter <> %}<CR><><CR><% endfilter %}
Snippet block {% block <> %}<CR><><CR>{% endblock %}<CR><>
Snippet cycle {% cycle <> as <> %}<>
Snippet if {% if <> %}<CR><><CR>{% endif %}<CR><>
Snippet debug {% debug %}<CR><>
Snippet ifnotequal {% ifnotequal <> <> %}<CR><><CR>{% endifnotequal %}<CR><>
Snippet include {% include <> %}<CR><>
Snippet comment {% comment %}<CR><><CR>{% endcomment %}<CR><>
Snippet for {% for <> in <> %}<CR><><CR>{% endfor %}<CR><>
Snippet ssi {% ssi <> <:D('parsed')> %}<>
Snippet model class <>(models.Model):<CR>"""<:D('model description')>"""<CR><> = <><CR><CR>class ADMIN:<CR>pass<CR><CR>def __str__(self):<CR>return "<"%s">" <"%s":DjangoArgList(Count(@z, '%[^%]'))><CR><>
Snippet widthratio {% widthration <:D('this_value')> <:D('max_value')> <:D('100')> %}<>
Snippet load {% load <> %}<CR><>
after/ftplugin/f-script_snippets.vim
10
if !exists('loaded_snippet') || &cp
    finish
endif

Snippet tbd to:<> by:<> do:[ <:D(':i')> |<CR><><CR>].<>
Snippet it ifTrue:[<CR><><CR>].<>
Snippet ift ifFalse:[<CR><><CR>] ifTrue:[<CR><><CR>].<>
Snippet itf ifTrue:[<CR><><CR>] ifFalse:[<CR><><CR>].<>
Snippet td to:<> do:[<> <:D(':i')> |<CR><><CR>].<>
Snippet if ifFalse:[<CR><><CR>].<>
after/ftplugin/haskell_snippets.vim
5
if !exists('loaded_snippet') || &cp
    finish
endif

Snippet mod module: <:D('Main')> where<CR><>
after/ftplugin/html_snippets.vim
43
if !exists('loaded_snippet') || &cp
    finish
endif

function! Onload()
    return 'onload="<>"'
endfunction

function! Id()
    return ' id="<>"'
endfunction

function! Cellspacing()
    return ' cellspacing="<:D('5')>"'
endfunction

function! FileNoExt()
    return substitute(expand('%'), '\(.*\)\..*$', '\1','')
endfunction

function! Target()
    return ' target="<>"'
endfunction

Snippet doctype <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Frameset//EN"<CR><Tab>"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd"><CR><>
Snippet head <head><CR><Tab><meta http-equiv="Content-type" content="text/html; charset=utf-8" /><CR><Tab><title><:substitute(expand('%'),'\(.*\)\..*$','\1','')></title><CR><Tab><><CR></head><CR><>
Snippet script <script type="text/javascript" language="javascript" charset="utf-8"><CR>// <![CDATA[<CR><Tab><><CR>// ]]><CR></script><CR><>
Snippet title <title><:substitute(expand('%'),'\(.*\)\..*$','\1','')></title>
Snippet body <body id="<:FileNoExt()>" <:Onload()><CR><><CR></body><CR><>
Snippet scriptsrc <script src="<>" type="text/javascript" language="<:D('javascript')>" charset="<:D('utf-8')>"></script><CR><>
Snippet textarea <textarea name="<:D('Name')>" rows="<:D('8')>" cols="<:D'(40')>"><></textarea><CR><>
Snippet meta <meta name="<:D('name')>" content="<:D('content')>" /><CR><>
Snippet movie <object width="<>" height="<>"<CR>classid="clsid:02BF25D5-8C17-4B23-BC80-D3488ABDDC6B"<CR>codebase="http://www.apple.com/qtactivex/qtplugin.cab"><CR><Tab><param name="src"<CR>value="<>" /><CR><Tab><param name="controller" value="<>" /><CR><param name="autoplay" value="<>" /><CR><Tab><embed src="<:D('movie.mov')>"<CR><Tab>width="<:D('320')>" height="<D('240')>"<CR><Tab>controller="<:D('true')>" autoplay="<:D('true')>"<CR><Tab><Tab>scale="tofit" cache="true"<CR><Tab><Tab>pluginspage="http://www.apple.com/quicktime/download/"<CR><Tab>/><CR></object><CR><>
Snippet div <div<:Id()>><CR><><CR></div><CR><>
Snippet mailto <a href="mailto:<>?subject=<:D('feedback')>"><:D('email me')></a><>
Snippet table <table border="<:D('0')>"<:Cellspacing()> cellpadding="<:D('5')>"><CR><Tab><tr><th><:D('Header')></th></tr><CR><Tab><tr><td><></td></tr><CR></table>
Snippet link <link rel="<:D('stylesheet')>" href="<:D('/css/master.css')>" type="text/css" media="<:D('screen')>" title="<>" charset="<:D('utf-8')>" />
Snippet form <form action="<:D(FileNoExt().'_submit')>" method="<:D('get')>"><CR><Tab><><CR><CR><Tab><p><input type="submit" value="Continue &rarr;" /></p><CR></form><CR><>
Snippet ref <a href="<>"><></a><>
Snippet h1 <h1 id="<>"><></h1><>
Snippet input <input type="<:D('text/submit/hidden/button')>" name="<>" value="<>" <>/><>
Snippet style <style type="text/css" media="screen"><CR>/* <![CDATA[ */<CR><Tab><><CR>/* ]]> */<CR></style><CR><>
Snippet base <base href="<>"<:Target()> /><>
after/ftplugin/java_snippets.vim
50
if !exists('loaded_snippet') || &cp
    finish
endif

function! UpFirst()
    return substitute(@z,'.','\u&','')
endfunction

function! JavaTestFileName(type)
    let filepath = expand('%:p')
    let filepath = substitute(filepath, '/','.','g')
    let filepath = substitute(filepath, '^.\(:\\\)\?','','')
    let filepath = substitute(filepath, '\','.','g')
    let filepath = substitute(filepath, ' ','','g')
    let filepath = substitute(filepath, '.*test.','','')
    if a:type == 1
        let filepath = substitute(filepath, '.[A-Za-z]*.java','','g')
    elseif a:type == 2
        let filepath = substitute(filepath, 'Tests.java','','')
    elseif a:type == 3
        let filepath = substitute(filepath, '.*\.\([A-Za-z]*\).java','\1','g')
    elseif a:type == 4
        let filepath = substitute(filepath, 'Tests.java','','')
        let filepath = substitute(filepath, '.*\.\([A-Za-z]*\).java','\1','g')
    elseif a:type == 5
        let filepath = substitute(filepath, 'Tests.java','','')
        let filepath = substitute(filepath, '.*\.\([A-Za-z]*\).java','\1','g')
        let filepath = substitute(filepath, '.','\l&','')
    endif

    return filepath
endfunction

Snippet method // {{{ <method><CR>/**<CR> * <><CR> */<CR>public <return> <method>() {<CR><>}<CR>// }}}<CR><>
Snippet jps private static final <string> <> = "<>";<CR><>
Snippet jtc try {<CR><><CR>} catch (<> e) {<CR><><CR>} finally {<CR><><CR>}<CR><>
Snippet jlog /** Logger for this class and subclasses. */<CR><CR>protected final Log log = LogFactory.getLog(getClass());<CR><>
Snippet jpv private <string> <>;<CR><CR><>
Snippet bean // {{{ set<fieldName:UpFirst()><CR>/**<CR> * Setter for <fieldName>.<CR> * @param new<fieldName:UpFirst()> new value for <fieldName><CR> */<CR>public void set<fieldName:UpFirst()>(<String> new<fieldName:UpFirst()>) {<CR><fieldName> = new<fieldName:UpFirst()>;<CR>}<CR>// }}}<CR><CR>// {{{ get<fieldName:UpFirst()><CR>/**<CR> * Getter for <fieldName>.<CR> * @return <fieldName> */<CR>public <String> get<fieldName:UpFirst()>() {<CR>return <fieldName>;<CR>}<CR>// }}}<CR><>
Snippet jwh while (<>) { // <><CR><CR><><CR><CR>}<CR><>
Snippet sout System.out.println("<>");<>
" The following snippet is quite complicated and I'm not quite sure what the
" syntax is supposed to be.
"Snippet jtest package <j:JavaTestFileName(1)><CR><CR>import junit.framework.TestCase;<CR>import <j:JavaTestFileName(2)>;<CR><CR>/**<CR> * <j:JavaTestFileName(3)><CR> *<CR> * @author <><CR> * @since <><CR> */<CR>public class <j:JavaTestFileName(3)> extends TestCase {<CR><CR>private <j:JavaTestFileName(4)> <j:JavaTestFileName(5)>;<CR><CR>public <j:JavaTestFileName(4)> get<j:JavaTestFileName(4)>() { return this.<j:JavaTestFileName(5)>; }<CR>public void set<j:JavaTestFileName(4)>(<j:JavaTestFileName(4)> <j:JavaTestFileName(5)>) { this.<j:JavaTestFileName(5)> = <j:JavaTestFileName(5)>; }<CR><CR>public void test<>() {<CR><><CR>}<CR>}<CR><>
Snippet jif if (<>) { // <><CR><><CR>}<CR><>
Snippet jelse if (<>) { // <><CR><CR><><CR><CR>} else { // <><CR><><CR>}<CR><>
Snippet jpm /**<CR> * <><CR> *<CR> * @param <> <><CR> * <:D('@return')> <><CR> */<CR>private <void> <>(<String> <>) {<CR><CR><><CR><CR>}<CR><>
Snippet main public main static void main(String[] ars) {<CR><"System.exit(0)">;<CR>}<CR><>
Snippet jpum /**<CR> * <><CR> *<CR> * @param <> <><CR> *<:D('@return')> <><CR> */<CR>public <void> <>(<String> <>) {<CR><CR><><CR><CR>}<CR><>
Snippet jcout <c:out value="${<>}" /><>
after/ftplugin/javascript_snippets.vim
6
if !exists('loaded_snippet') || &cp
    finish
endif

Snippet proto <className>.prototype.<methodName> = function(<>)<CR>{<CR><><CR>};<CR><>
Snippet fun function <functionName> (<>)<CR>{<CR><><CR>}<CR><>
after/ftplugin/latex_snippets.vim
9
if !exists('loaded_snippet') || &cp
    finish
endif

Snippet sub \subsection{<name>}\label{sub:<name:substitute(@z,'.','\l&','g')>}<CR><>
Snippet $$ \[<CR><><CR>\]<CR><>
Snippet ssub \subsubsection{<name>}\label{ssub:<name:substitute(@z,'.','\l&','g')>}<CR><>
Snippet itd \item[<desc>] <>
Snippet sec \section{<name>}\label{sec:<name:substitute(@z,'.','\l&','g')><CR><>
after/ftplugin/logo_snippets.vim
5
if !exists('loaded_snippet') || &cp
    finish
endif

Snippet to to <name> <argument><CR><><CR>end<CR><>
after/ftplugin/markdown_snippets.vim
6
if !exists('loaded_snippet') || &cp
    finish
endif

Snippet img ![<altText>](<SRC>)<>
Snippet link [<desc>](<HREF>)<>
after/ftplugin/movable type_snippets.vim
10
if !exists('loaded_snippet') || &cp
    finish
endif

Snippet cat <$MTCategoryDescription$><>
Snippet blog <$MTBlogName$><>
Snippet archive <$MTArchiveFile$><>
Snippet cal <MTCalendarIfEntries<CR><><CR></MTCalendarIfEntries><CR><>
Snippet entry <$MTEntryMore$><>
Snippet entries <MTEntriesHeader><CR><><CR></MTEntriesHeader><CR><>
after/ftplugin/objc_snippets.vim
49
if !exists('loaded_snippet') || &cp
    finish
endif

function! UpFirst()
    return substitute(@z,'.','\u&','')
endfunction

function! Count(haystack, needle)
    let counter = 0
    let index = match(a:haystack, a:needle)
    while index > -1
        let counter = counter + 1
        let index = match(a:haystack, a:needle, index+1)
    endwhile
    return counter
endfunction

function! ObjCArgList(count)
    " This needs to be Python specific as print expects a
    " tuple and an empty tuple looks like this (,) so we'll need to make a
    " special case for it
    if a:count == 0
        return "<>"
    else
        return '<>'.repeat(', <>', a:count)
    endif
endfunction


Snippet cat @interface <NSObject> (<Category>)<CR><CR>@end<CR><CR><CR>@implementation <NSObject> (<Category>)<CR><CR><><CR><CR>@end<CR><>
Snippet delacc - (id)delegate;<CR><CR>- (void)setDelegate:(id)delegate;<CR><>
Snippet ibo IBOutlet <NSSomeClass> *<someClass>;<CR><>
Snippet dict NSMutableDictionary *<dict> = [NSMutableDictionary dictionary];<CR><>
Snippet Imp #import <<>.h><CR><>
Snippet objc @interface <class> : <NSObject><CR>{<CR>}<CR>@end<CR><CR>@implementation <class><CR>- (id)init<CR>{<CR>self = [super init]; <CR>if (self != nil)<CR>{<CR><><CR>}<CR>return self;<CR>}<CR>@end<CR><>
Snippet imp #import "<>.h"<CR><>
Snippet bez NSBezierPath *<path> = [NSBezierPath bezierPath];<CR><>
Snippet acc - (<"unsigned int">)<thing><CR>{<CR>return <fThing>;<CR>}<CR><CR>- (void)set<thing:UpFirst()>:(<"unsigned int">)new<thing:UpFirst()><CR>{<CR><fThing> = new<thing:UpFirst()>;<CR>}<CR><>
Snippet format [NSString stringWithFormat:@"<>", <>]<>
Snippet focus [self lockFocus];<CR><CR><><CR><CR>[self unlockFocus];<CR><>
Snippet setprefs [[NSUserDefaults standardUserDefaults] setObject:<object> forKey:<key>];<CR><>
Snippet log NSLog(@"%s<s>", <s:ObjCArgList(Count(@z, '%[^%]'))>);<>
Snippet gsave [NSGraphicsContext saveGraphicsState];<CR><><CR>[NSGraphicsContext restoreGraphicsState];<CR><>
Snippet forarray for(unsigned int index = 0; index < [<array> count]; index += 1)<CR>{<CR><id>object = [<array> objectAtIndex:index];<CR><><CR>}<>
Snippet classi @interface <ClassName> : <NSObject><CR><CR>{<><CR><CR>}<CR><CR><><CR><CR>@end<CR><>
Snippet array NSMutableArray *<array> = [NSMutableArray array];<>
Snippet getprefs [[NSUserDefaults standardUserDefaults] objectForKey:<key>];<>
Snippet cati @interface <NSObject> (<Category>)<CR><CR><><CR><CR>@end<CR><>
after/ftplugin/ocaml_snippets.vim
22
if !exists('loaded_snippet') || &cp
    finish
endif

Snippet Queue Queue.fold <:D('(fun b v ->)')> <base> <q><CR><>
Snippet Nativeint Nativeint.abs <ni><>
Snippet Printexc Printexc.print <fn> <x><>
Snippet Sys Sys.Signal_ignore<>
Snippet Hashtbl Hashtbl.iter <:D('(fun k v -> )')> <h><>
Snippet Array Array.map <:D('(fun a -> )')> <arr><>
Snippet Printf Printf.fprintf <buf> "<format>" <args><>
Snippet Stream Stream.iter <:D('(fun x -> )')> <stream><>
Snippet Buffer Buffer.add_channel <buf> <ic> <len><>
Snippet Int32 Int32.abs <i32><>
Snippet List List.rev_map <:D('(fun x -> )')> <lst><>
Snippet Scanf Scanf.bscaf <sbuf> "<format>" <f><>
Snippet Int64 Int64.abs <i64><>
Snippet Map Map.Make <:D('(Ord : OrderedType)')><>
Snippet String String.iter <:D('(fun c -> )')> <str><>
Snippet Genlex Genlex.make_lexer <"tok_lst"> <"char_stream"><>
Snippet for for <i}> = <> to <> do<CR><><CR>done<CR><>
Snippet Stack Stack.iter <:D('(fun x -> )')> <stk><>
after/ftplugin/perl_snippets.vim
23
if !exists('loaded_snippet') || &cp
    finish
endif

function! MyX()
    return 'my <x> '
endfunction

Snippet sub sub <functionName> {<CR><><CR>}<CR><>
Snippet class package <ClassName>;<CR><CR><:D('use base qw(')><ParentClass>);<CR><CR>}sub new {<CR>my \$class = shift;<CR>\$class = ref \$class if ref \$class;<CR>my $self = bless {}, \$class;<CR>\$self;<CR>}<CR><CR>1;<CR><>
Snippet xfore <expression> foreach @<array>;<>
Snippet xwhile <expression> while <condition>;<>
Snippet xunless <expression> unless <condition>;<>
Snippet slurp my $<var>;<CR><CR>{ local $/ = undef; local *FILE; open FILE, "<<file>"; $<var> = <FILE>; close FILE }<>
Snippet if if (<>) {<CR><><CR>}<CR><>
Snippet unless unless (<>) {<CR><2><CR>}<CR><>
Snippet ifee if (<>) {<CR><2><CR>} elsif (<>) {<CR><><CR>} else {<CR><>}<CR>}<CR><>
Snippet ife if (<>) {<CR><><CR>} else {<CR><><CR>}<CR><>
Snippet for for (my \$<var> = 0; \$<var> < <expression>; \$<var>++) {<CR><><CR>}<CR><>
Snippet fore foreach <:MyX()>(@<array>) {<CR><><CR>}<CR><>
Snippet eval eval {<CR><><CR>};<CR>if ($@) {<CR><><CR>}<CR><>
Snippet while while (<>) {<CR><><CR>}<CR><>
Snippet xif <expression> if <condition>;<>
after/ftplugin/php_snippets.vim
26
if !exists('loaded_snippet') || &cp
    finish
endif

Snippet elseif elseif ( <condition> )<CR>{<CR><><CR>}<CR><>
Snippet do do<CR>{<CR><><CR><CR>} while ( <:D('$a <= 10')> );<CR><>
Snippet reql require_once( '<file>' );<CR><>
Snippet if? $<retVal> = ( <condition> ) ? <a> : <b> ;<CR><>
Snippet php <?php<CR><CR><><CR><CR>?>
Snippet switch switch ( <variable> )<CR>{<CR>case '<value>':<CR><><CR>break;<CR><CR><><CR><CR>default:<CR><><CR>break;<CR>}<CR><>
Snippet class #doc<CR>#classname:<ClassName><CR>#scope:<PUBLIC><CR>#<CR>#/doc<CR><CR>class <ClassName> <extendsAnotherClass><CR>{<CR>#internal variables<CR><CR>#Constructor<CR>function __construct ( <argument>)<CR>{<CR><><CR>}<CR>###<CR><CR>}<CR>###
Snippet incll include_once( '<file>' );<>
Snippet incl include( '${1:file}' );<>
Snippet foreach foreach( $<variable> as $<key> => $<value> )<CR>{<CR><><CR>}<CR><>
Snippet ifelse if ( <condition> )<CR>{<CR><><CR>}<CR>else<CR>{<CR><><CR>}<CR><>
Snippet $_ $_REQUEST['<variable>']<CR><>
Snippet case case '<variable>':<CR><><CR>break;<CR><>
Snippet print print "<string>"<>;<><CR><>
Snippet function <public>function <FunctionName> (<>)<CR>{<CR><><CR>}<CR><>
Snippet if if ( <condition> )<CR>{<CR><><CR>}<CR><>
Snippet else else<CR>{<CR><><CR>}<CR><>
Snippet array $<arrayName> = array( '<>',<> );<>
Snippet -globals $GLOBALS['<variable>']<><something><>;<CR><>
Snippet req require( '<file>' );<CR><>
Snippet for for ( $<i>=<>; $<i> < <>; $<i>++ )<CR>{ <CR><><CR>}<CR><>
Snippet while while ( <> )<CR>{<CR><><CR>}<CR><>
after/ftplugin/phpdoc_snippets.vim
15
if !exists('loaded_snippet') || &cp
    finish
endif

Snippet doc_d /**<CR>* <undocumentedConstant><CR>**/<CR>define(<>, <>);<><CR><>
Snippet doc_vp /**<CR>* <undocumentedClassVariable><CR>*<CR>* @var <string><><CR>**/<><CR>
Snippet doc_f /**<CR>* <undocumentedFunction><CR>*<CR>* @return <void><CR>* @author <><CR>**/<CR><>function <>(<>)<CR>{<><CR>}<CR><>
Snippet doc_s /**<CR>* <undocumentedFunction><CR>*<CR>* @return <void><CR>* @author <><CR>**/<CR><>function <>(<>);<CR><>
Snippet doc_h /**<CR>* <><CR>*<CR>* @author <><CR>* @version $Id$<CR>* @copyright <>, <><CR>* @package <default><CR>**/<CR><CR>/**<CR>* Define DocBlock<CR>**/<CR><CR><>
Snippet doc_fp /**<CR>* <undocumentedFunction><CR>*<CR>* @return <void><CR>* @author <><CR>**/<><CR>
Snippet doc_i /**<CR>* <undocumentedClass><CR>*<CR>* @package <default><CR>* @author <><CR>**/<CR>interface <><CR>{<><CR>} // END interface <><CR><>
Snippet doc_fp /**<CR>* <undocumentedConstant><><CR>**/<><CR><>
Snippet doc_v /**<CR>* <undocumentedClassVariable><CR>*<CR>* @var <string><CR>**/<CR><var> $<>;<><CR><>
Snippet doc_cp /**<CR>* <undocumentedClass><CR>*<CR>* @package <default><CR>* @author <><CR>**/<>
Snippet doc_c /**<CR>* <undocumentedClass><CR>*<CR>* @package <default><CR>* @author <><CR>**/<CR><class>class <a><CR>{<><CR>} // END <class>class <a><CR><>
after/ftplugin/propel_snippets.vim
10
if !exists('loaded_snippet') || &cp
    finish
endif

Snippet <i <index name="<key>_index"><CR><index-column name="<key>" /><CR></index><CR><>
Snippet <t <table name="<name>" <>><CR><><CR></table><CR><>
Snippet <u <unique name="unique_<key>"><CR><unique-column name="<key>" /><CR></unique><CR><>
Snippet <c <column name="<name>" type="<type>" <> /><CR><>
Snippet <p <column name="<id>" type="<integer>" required="true" primaryKey="true" autoincrement="true" /><CR><>
Snippet <f <foreign-key foreignTable="<table>"><CR><reference local="<table>_id" foreign="<id>"/><CR></foreign-key><CR><>
after/ftplugin/python_snippets.vim
62
if !exists('loaded_snippet') || &cp
    finish
endif

function! PyInit(text)
    if a:text != "args"
        return ', '.a:text
    else
        return ''
    endif
endfunction

function! PyInitVars(text)
    if a:text != "args"
        let text = substitute(a:text,'=.\{-},','','g')
        let text = substitute(text,'=.\{-}$','','g')
        let text = substitute(text,',','','g')
        let ret = ''
        for Arg in split(text, ' ')
            let ret = ret.'self.'.Arg.' = '.Arg.'\n\t\t'
        endfor
        return ret
    else
        return "pass"
    endif
endfunction

function! Count(haystack, needle)
    let counter = 0
    let index = match(a:haystack, a:needle)
    while index > -1
        let counter = counter + 1
        let index = match(a:haystack, a:needle, index+1)
    endwhile
    return counter
endfunction

function! PyArgList(count)
    " This needs to be Python specific as print expects a
    " tuple and an empty tuple looks like this (,) so we'll need to make a
    " special case for it
    if a:count == 0
        return "()"
    else
        return '('.repeat('<>, ', a:count).')'
    endif
endfunction

Snippet pf print "<s>" % <s:PyArgList(Count(@z, '%[^%]'))><CR><>
Snippet get def get<name>(self): return self._<name><CR><>
Snippet classi class <ClassName> (<object>):<CR><CR>def __init__(self<args:PyInit(@z)>):<CR><args:PyInitVars(@z)><CR><CR><>
Snippet set def set<name>(self, <newValue>):<CR>self._<name>$1 = <newValue><CR><>
Snippet . self.<>
Snippet def def <fname>(<self>):<CR><pass><CR><>
" Contributed by Panos
Snippet ifn if __name__ == '__main__':<CR><>
" Contributed by Kib2
Snippet bc """<description>"""<CR><>
Snippet lc # <linecomment><CR><>
Snippet sbl1 #!/usr/bin/env python<CR># -*- coding: Latin-1 -*-<CR><>
Snippet kfor for <variable> in <ensemble>:<CR><pass><CR><BS><>
Snippet cm <class> = classmethod(<class>)<CR><>
after/ftplugin/rails_snippets.vim
50
if !exists('loaded_snippet') || &cp
    finish
endif

Snippet mrnt rename_table "<oldTableName>", "<newTableName>"<>
Snippet rfu render :file => "<filepath>", :use_full_path => <false><>
Snippet rns render :nothing => <true>, :status => <:D('401')><>
Snippet ri render :inline => "<:D("<%= 'hello' %>")>"<>
Snippet rt render :text => "<>"<>
Snippet mcc t.column "<title>", :<string><>
Snippet rpl render :partial => "<item>", :locals => { :<name> => "<value>"<> }<>
Snippet rea redirect_to :action => "<index>"<>
Snippet rtlt render :text => "<>", :layout => <true><>
Snippet ft <%= form_tag :action => "<update>" %><>
Snippet forin <% for <item> in <:D('@items')> %><CR><%= <item>.<name> %><CR><% end %><CR><>
Snippet lia <%= link_to "<>", :action => "<index>" %><>
Snippet rl render :layout => "<layoutname>"<>
Snippet ra render :action => "<action>"<>
Snippet mrnc rename_column "<table>", "<oldColumnName>", "<newColumnName>"<>
Snippet mac add_column "<table>", "<column>", :<string><>
Snippet rpc render :partial => "<item>", :collection => <items><>
Snippet rec redirect_to :controller => "<items>"<>
Snippet rn render :nothing => <true><>
Snippet lic <%= link_to "<>", :controller => "<>" %><>
Snippet rpo render :partial => "<item>", :object => <object><>
Snippet rts render :text => "<>", :status => <:D('401')><>
Snippet rcea render_component :action => "<index>"<>
Snippet recai redirect_to :controller => "<items>", :action => "<show>", :id => <:D('@item')><>
Snippet mcdt create_table "<table>" do |t|<CR><><CR>end<CR><><CR><>
Snippet ral render :action => "<action>", :layout => "<layoutname>"<>
Snippet rit render :inline => "<>", :type => <:D(':rxml')><>
Snippet rceca render_component :controller => "<items>", :action => "<index>"<>
Snippet licai <%= link_to "<>", :controller => "<items>", :action => "<edit>", :id => <:D('@item')> %><>
Snippet verify verify :only => [:<>], :method => :post, :render => {:status => 500, :text => "use HTTP-POST"}<>
Snippet mdt drop_table "<table>"<>
Snippet rp render :partial => "<item>"<>
Snippet rcec render_component :controller => "<items>"<>
Snippet mrc remove_column "<table>", "<column>"<>
Snippet mct create_table "<table>" do |t|<CR><><CR>end<CR><>
Snippet flash flash[:<notice>] = "<>"<>
Snippet rf render :file => "<filepath>"<>
Snippet lica <%= link_to "<>", :controller => "<items>", :action => "<index>" %><>
Snippet liai <%= link_to "<>", :action => "<edit>", :id => <:D('@item')> %><>
Snippet reai redirect_to :action => "<show>", :id => <:D('@item')><>
Snippet logi logger.info "<>"<>
Snippet marc add_column "<table>", "<column>", :<string><CR><CR><><CR><>
Snippet rps render :partial => "<item>", :status => <:D('500')><>
Snippet ril render :inline => "<>", :locals => { <:D(':name')> => "<value>"<> }<>
Snippet rtl render :text => "<>", :layout => "<>"<>
Snippet reca redirect_to :controller => "<items>", :action => "<list>"<>
after/ftplugin/ruby_snippets.vim
28
if !exists('loaded_snippet') || &cp
    finish
endif

Snippet do do<CR><>end<CR><>
Snippet class class <className><CR><>end<CR><>
Snippet begin begin<CR><><CR>rescue <Exception> => <e><CR><>end<CR><>
Snippet each_with_index0 each_with_index do |<element>, <index>|<CR><element>.<><CR>end<CR><>
Snippet collect collect { |<element>| <element>.<> }<CR><>
Snippet forin for <element> in <collection><CR><element>.<><CR>end<CR><>
Snippet doo do |<object>|<CR><><CR>end<CR><>
Snippet : :<key> => "<value>"<><CR><>
Snippet def def <methodName><CR><><CR>end<CR><>
Snippet case case <object><CR>when <condition><CR><><CR>end<CR><>
Snippet collecto collect do |<element>|<CR><element>.<><CR>end<CR><>
Snippet each each { |<element>| <element>.<> }<CR><>
Snippet each_with_index each_with_index { |<element>, <idx>| <element>.<> }<CR><>
Snippet if if <condition><CR><><CR>end<CR><>
Snippet eacho each do |<element>|<CR><element>.<><CR>end<CR><>
Snippet unless unless <condition><CR><><CR>end<CR><>
Snippet ife if <condition><CR><><CR>else<CR><><CR>end<CR><>
Snippet when when <condition><CR><>
Snippet selecto select do |<element>|<CR><element>.<><CR>end<CR><>
Snippet injecto inject(<object>) do |<injection>, <element>| <CR><><CR>end<CR><>
Snippet reject { |<element>| <element>.<> }<CR><>
Snippet rejecto reject do |<element>| <CR><element>.<><CR>end<CR><>
Snippet inject inject(<object>) { |<injection>, <element>| <> }<CR><>
Snippet select select { |<element>| <element>.<> }<CR><>
after/ftplugin/sh_snippets.vim
8
if !exists('loaded_snippet') || &cp
    finish
endif

"Snippet !env #!/usr/bin/env ${1:${TM_SCOPE/(?:source|.*)\\.(\\w+).*/$1/}}
Snippet if if [[ <condition> ]]; then<CR><><CR>fi<>
Snippet elif elif [[ <condition> ]]; then<CR><>
Snippet for for (( <i> = <>; <i> <>; <i><> )); do<CR><><CR>done<>
after/ftplugin/slate_snippets.vim
15
if !exists('loaded_snippet') || &cp
    finish
endif

Snippet do do: [| :<each>| <>]<CR><>
Snippet proto define: #<NewName> &parents: {<parents>} &slots: {<slotSpecs>}.<CR><>
Snippet ifte <condition> ifTrue: [<>:then] ifFalse: [<>:else]<CR><>
Snippet collect collect: [| :<each>| <>]<CR><>
Snippet if <condition> ifTrue: [<>:then]<>
Snippet until [<condition>] whileFalse: [<>:body]<>
Snippet reject reject: [| :<each>| <>]<CR><>
Snippet dowith doWithIndex: [| :<each> :<index> | <>]<CR><>
Snippet select select: [| :<each>| <>]<>
Snippet while [<condition>] whileTrue: [<>:body]<>
Snippet inject inject: <object> [| :<injection>, :<each>| <>]<>
after/ftplugin/smarty_snippets.vim
31
if !exists('loaded_snippet') || &cp
    finish
endif

Snippet {cycle {cycle values="#SELSTART#<foo>,<bar>#SELEND#" name="default" print=true advance=true delimiter="," assign=varname }<CR><>
Snippet |regex_replace |regex_replace:"<regex>":"<>"<>
Snippet {counter {counter name="#INSERTION#" start=1 skip=1 direction="up" print=true<CR>assign="foo" }<CR><CR>{counter}<CR><>
Snippet {eval {eval var="#SELSTART#{template_format}#SELEND#" assign=varname} <CR><>
"Snippet |date_format |date_format:"${1:strftime() formatting}" <CR><>
Snippet |truncate |truncate:<:D('80')>:<>:<false>
Snippet {if {if <varname><><CR>"<foo>"}<CR><CR>{* $varname can also be a php call *}<CR><CR><><CR><CR>{/if}<CR><>
"Snippet |string_format |string_format:"${1:sprintf formatting}" <CR><>
Snippet {assign {assign var=<> value="<>"}<>
Snippet {foreach {foreach from=<varname> item=i [key=k name=""] }<CR><CR><><CR><CR>{/foreach}<CR><CR><>
Snippet {capture {capture name=#INSERTION#}<CR><CR>#SELECT#<CR><CR>{/capture}<CR><>
Snippet |wordwrap |wordwrap:<:D('80')>:"<>":<>
Snippet |spacify |spacify:"<>"<> 
Snippet |default |default:"<>"<>
Snippet {debug {debug output="#SELSTART#<>#SELEND#" }<>
Snippet |replace |replace:"<needle>":"<>"<>
Snippet {include {include file="<>" [assign=varname foo="bar"] }<>
Snippet |escape |escape:"<>"<>
Snippet {strip {strip}<CR><><CR>{/strip}<>
Snippet {math {math equation="<>" assign=<> <>}<>
Snippet {config_load {config_load file="#INSERTION#" [section="" scope="local|parent|global"] }<>
Snippet |cat  |cat:"<>"<>
Snippet {insert {insert name="insert_<>" [assign=varname script="foo.php" foo="bar"] }<>
Snippet {fetch {fetch file="#SELSTART#http:// or file#SELEND#" assign=varname}<>
Snippet {literal {literal}<CR><CR><><CR><CR>{/literal}<>
Snippet {include_php {include_php file="<>" [once=true]}<>
Snippet |strip |strip:["<>"]<>
after/ftplugin/symfony_snippets.vim
17
if !exists('loaded_snippet') || &cp
    finish
endif

Snippet image_tag image_tag('<imageName>'<>)<>
Snippet get public function get<> ()<CR>{<CR>return $this-><>;<CR>}<CR><CR><>
Snippet link_to link_to('<linkName>', '<moduleName>/<actionName><>')<>
Snippet sexecute public function execute<Action>()<CR>{<CR><><CR>}<CR><>
Snippet set public function set<> ($<>)<CR>{<CR>$this-><> = <>;<CR>}<CR><CR><>
Snippet execute /**<CR>* <className><CR>*<CR>*/<CR>public function execute<Action>()<CR>{<CR><><CR>}<CR><>
Snippet tforeach <?php foreach ($<variable> as $<key><>): ?><CR><><CR><?php endforeach ?><CR><>
Snippet getparam $this->getRequestParameter('<id>')<>
Snippet div <div<>><CR><><CR></div><>
Snippet tif <?php if (<condition>): ?><CR><><CR><?php endif ?><CR><>
Snippet setget public function set<var> (<arg>)<CR>{<CR>$this-><arg> = <arg>;<CR>}<CR><CR>public function get<var> ()<CR>{<CR>return $this-><var>;<CR>}<CR><CR><>
Snippet echo <?php echo <> ?><>
Snippet tfor <?php for($<i> = <>; $<i> <= <>; $<i>++): ?><CR><><CR><?php endfor ?><CR><>
after/ftplugin/tcl_snippets.vim
10
if !exists('loaded_snippet') || &cp
    finish
endif

Snippet switch switch<> -- $<var> {<CR><match> {<CR><><CR>}<CR>default<CR>{<>}<CR>}<CR><>
Snippet foreach foreach <var> $<list> {<CR><><CR>}<CR><>
Snippet proc proc <name> {<args>} <CR>{<CR><><CR>}<CR><>
Snippet if if {<condition>} {<CR><><CR>}<CR><>
Snippet for for {<i> {<>} {<>} {<CR><><CR>}<CR><>
Snippet while while {<condition>} {<CR><><CR>}<CR><>
after/ftplugin/template_toolkit_snippets.vim
9
if !exists('loaded_snippet') || &cp
    finish
endif

Snippet wrap [% WRAPPER <template> %]<CR><><CR>[% END %]<CR><>
Snippet if [% IF <condition> %]<CR><><CR>[% ELSE %]<CR><><CR>[% END %]<CR><>
Snippet unl [% UNLESS <condition> %]<CR><><CR>[% END %]<CR><>
Snippet inc [% INCLUDE <template> %]<CR><>
Snippet for  [% FOR <var> IN <set> %]<CR><><CR>[% END %]<>
after/ftplugin/tex_snippets.vim
9
if !exists('loaded_snippet') || &cp
    finish
endif

Snippet sub \subsection{<name>}\label{sub:<name:substitute(@z,'.','\l&','g')>}<CR><>
Snippet $$ \[<CR><><CR>\]<CR><>
Snippet ssub \subsubsection{<name>}\label{ssub:<name:substitute(@z,'.','\l&','g')>}<CR><>
Snippet itd \item[<desc>] <>
Snippet sec \section{<name>}\label{sec:<name:substitute(@z,'.','\l&','g')><CR><>
after/ftplugin/xhtml_snippets.vim
44
if !exists('loaded_snippet') || &cp
    finish
endif

Snippet doctype <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"<CR>"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"><CR><>
Snippet aref <a href="<>" id="<>" title="<>"><></a><>
Snippet head  <head><CR><><CR></head><>
Snippet script <script type="text/javascript" language="<javascript>" charset="<:D('utf-8')>"><CR>// <![CDATA[<CR><><CR>// ]]><CR></script><>
Snippet html <html xmlns="http://www.w3.org/1999/xhtml" xml:lang="<en>"<CR>lang="<en>"><CR><><CR></html>
Snippet h3 <h3><></h3><>
Snippet h4 <h4><></h4><>
Snippet h5 <h5><></h5><>
Snippet h6 <h6><></h6><>
Snippet fieldset <fieldset><CR><><CR></fieldset><>
Snippet noscript <noscript><CR><><CR></noscript><>
Snippet ul <ul <>><CR><><CR></ul><>
Snippet xml <?xml version="1.0" encoding="iso-8859-1"?><CR><CR><>
Snippet body <body id="<>" <>><CR><><CR></body><>
Snippet legend <legend align="<>" accesskey="<>"><CR><><CR></legend><>
Snippet title <title><PageTitle></title><>
Snippet scriptsrc <script src="<>" type="text/javascript" language="<javascript>" charset="<:D('utf-8')>"></script><>
Snippet img <img src="<>" alt="<>" class="<>" /><>
Snippet option <option label="<label>" value="<value>" <>></option> <>
Snippet optgroup <optgroup label="<Label>"><CR><><CR></optgroup><>
Snippet meta <meta name="<name>" content="<content>" /><>
Snippet td <td<>><></td><>
Snippet dt <dt><><CR></dt><CR><dd></dd><>
Snippet tfoot <tfoot><CR><><CR></tfoot><>
Snippet div <!-- begin div.<id> --><CR><div id="<id>"><CR><><CR></div><CR><!-- end div.<id> --><CR><>
Snippet ol <ol <>><CR><><CR></ol><>
Snippet txtarea <textarea id="<ID>" name="<Name>" rows="<>" cols="<>" tabindex="<>" <>><></textarea><>
Snippet mailto <a href="mailto:<>?subject=<>"><></a><>
Snippet table <table summary="<Summary>" class="<className>" width="<:D('100%')>" cellspacing="<>" cellpadding="<>" border="<>"><CR><><CR></table><>
Snippet hint <span class="hint"><></span><>
Snippet link <link rel="<stylesheet>" href="<>" type="text/css" media="<screen>" title="<>" charset="<:D('utf-8')>" /><>
Snippet form <form action="<urlToGoTo>" method="<get>" id="<formID>" name="<formName>"><CR><><CR></form><>
Snippet tr <tr <>><CR><><CR></tr><>
Snippet label <label for="<inputItem>"><></label><>
Snippet image <img src="<>" alt="<>" width="<>" height="<>" <>/>
Snippet input <input name="<>" id="<>" type="radio" value="<defaultValue>" tabindex="<>" <> /><>
Snippet select <select id="<ID>" name="<Name>" size="<>" tabindex="<>" <>><CR><><CR></select><CR><>
Snippet style <style type="text/css" media="<screen>"><CR>/* <![CDATA[ */<CR><><CR>/* ]]> */<CR></style><CR><>
Snippet div.header <!-- Begin HeaderDiv:: --><CR><div id="HeaderDiv"><CR><!--logo in background --><CR><h1><CompanyName></h1><CR></div><CR><!-- End HeaderDiv:: --><CR><>
Snippet base <base href="<>" <>/><>
doc/snippets_emu.txt
275
*snippets_emu.txt*  For Vim version 7.0. Last change: 2006 Oct 19


		  VIM REFERENCE MANUAL    by Panos Laganakos and Felix Ingram


1. SnippetsEmu Features                         |snippets_emu-features|
        Basic Snippets                              |basic-snippet|
        Named Tags                                  |named-tags|
        Tag Commands                                |snippet-commands|
        Buffer Specific Snippets                    |snip-buffer-specific|
        Filetype Specific Snippets                  |snip-ftplugin|
2. SnippetsEmu Options                          |snippets_emu-options|
        TextMate Compatible Mode                    |snip-textmate-cp|
        Tag Highlighting                            |snip-tag-highlighting|
        Start and End Tags                          |snip-start-end-tags|
3. Detailed Explanations                        |snip-detailed-explanations|
        Valid Tag Names                             |snip-tag-name-syntax|
        Advanced Tag Command Examples               |snip-advanced-tag-commands|
4. SnippetsEmu Contact Details                  |snip-contact-details|
5. SnippetsEmu Known Bugs                       |snippets_emu-bugs|

{Vi does not have any of these features}

==============================================================================
SNIPPETSEMU FEATURES					*snippets_emu-features*

SnippetsEmu attempts to emulate several of the snippets features of the OSX
editor TextMate, in particular the variable bouncing and replacement behaviour.
Simple usage is built up around the following functionality:

	Basic Snippet					|basic-snippet|
	Named Tags					|named-tags|
	Executable Snippet				|snippet-commands|
        Buffer Specific Snippets                        |snip-buffer-specific|

                                                               *basic-snippet*
Basic Snippet ~

Basic snippets build upon the ideas used by the built-in 'iabbr' command (See
|:iabbrev|). The built-in abbreviations allow short sequences to be replaced
by longer pieces of text. SnippetsEmu allows the user to define markers within
the larger piece of text which will be used to place the cursor upon
expansion.

A basic snippet can save you a lot of typing. Define a word trigger and on
insertion it will be expanded to the full snippet.

The command used to define a snippet is 'Iabbr'. Observe the capital 'I'.

Basic Syntax: >

	Iabbr trigger_name The cursor will be placed here: <> Trailing text

In insert mode typing 'trigger_name<space>' will remove 'trigger_name' and
replace it with the text: 'The cursor will be placed here:  Trailing text'.
The cursor will be placed between the two spaces before the word 'Trailing'

                                                                  *named-tags*
Named tags ~

Instead of the simple '<>' tags used for cursor placement a user can define
named tags. When the value of a named tag is changed then all other tags with
that name will be changed to the same value.

E.g. >

    Iabbr trigger My name is <forename> <surname>. Please call me <forename>.

In insert mode typing 'trigger<space>' will place the cursor inside the
'<forename>' tag. Whatever is entered inside the tag will replace the other
similarly named tag at the end of the line after the user presses 'Tab'.

If no value is entered for a named tag then the tag's name will be used
instead. This is one way of defining default values.

Using the above example, entering 'trigger<space>' and pressing 'Tab' twice
will result in the following text: >

    My name is forename surname. Please call me forename.

The rules for what constitutes a valid tag name are explained below. See
|snip-tag-name-syntax|.
                                                            *snippet-commands*
Tag commands ~

Tags can contain commands. Commands can be any Vim function, including user
defined functions.

A common example is performing substitutions.

E.g. >

    Iabbr trigger My name is <name>. I SAID: MY NAME IS <name:substitute(@z,'.','\u&','g')>!

The value entered in the <name> tag will be passed to the command in the
second <name> tag in the @z register (any value already in @z will be
preserved and restored). The substitute command will change the entered value
to be in upper case. I.e. Entering 'trigger<space>' and typing 'Tycho<Tab>'
will result in the following text: >
    
    My name is Tycho. I SAID: MY NAME IS TYCHO!
~
                                                        *snip-buffer-specific*
Buffer Specific Snippets ~

The Iabbr command can take a <buffer> option which will only set the snippet
for the buffer in which it is defined. This is the recommended option when
using filetype specific snippets.

E.g. >
    Iabbr <buffer> for for <var> in <list>:<CR><>
    Iabbr <buffer> for for ($<count> = <>; $<count> >= 1; $<count>++) {<CR><>}

The first of the above is appropriate for use with Python and the second is
appropriate for use with Perl.

                                                        *snip-snippet-command*
The command 'Snippet' can be used instead of 'Iabbr <buffer>'. Both are
functionally identical.

I.e. the above can be rewritten as below: >
    Snippet for for <var> in <list>:<CR><>
    Snippet for for ($<count> = <>; $<count> >= 1; $<count>++) {<CR><>}
~
                                                               *snip-ftplugin*
Using the buffer specific command allows filetype specific snippets to be
defined. The preferred practice is to include snippets in files named
<filetype>_snippets.vim and for these files to be placed in the
~/.vim/after/ftplugin directory (or vimfiles\after\ftplugin under Windows).
When a file of a specific type is loaded so will all of the defined snippets.
The 'after' directory is used to ensure that the plugin has been loaded. It is
also recommended that the following is included at the top of the file: >
    
    if !exists('loaded_snippet') || &cp
        finish
    endif

This will stop errors being generated if the plugin has not loaded for any
reason.

===============================================================================
SNIPPETSEMU OPTIONS                                     *snippets_emu-options*
                                                            *snip-textmate-cp*
SnippetsEmu TextMate Compatible Mode ~

SnipppetsEmu maintains two modes currently, SnippetsEmu basic mode and
TextMate compatible mode.

TextMate compatible mode only expands snippets when 'Tab' is pressed after the
trigger is entered. It also allows other functionality such as special
characters in triggers.

IMPORTANT: TextMate compatible mode will be the only mode from version 1.0
onwards; the original functionality will not be available.

To enable the TextMate compatible mode you set the snip_set_textmate_cp
variable to 1. This can be done by adding the following line in your
vimrc >

    let g:snip_set_textmate_cp = 1
~
                                                       *snip-tag-highlighting*
Tag Highlighting ~

If syntax highlighting is enabled then tags within snippets will highlighted.
To disable this option add the following line to your vimrc. >

    let g:snip_disable_highlight = 1
~
                                                         *snip-start-end-tags*
Start and End Tags ~

By default the start and end tags are set to be '<' and '>'. These can be
changed by setting the following variables in vimrc: >

    g:snip_start_tag
    g:snip_end_tag

They can be also changed for a specific buffer by setting the following: >

    b:snip_start_tag
    b:snip_end_tag
~
                                                         *snip-elem-delimiter*
Element Delimiter ~

The value of snip_elem_delim is used to separate a tag's name and its command.
By default it is set to ':' but can be set as above either globally or for a
specific buffer using the following variables: >
    
    g:snip_elem_delim
    b:snip_elem_delim

==============================================================================
DETAILED EXPLANATIONS                             *snip-detailed-explanations*
                                                        *snip-tag-name-syntax*
Valid Tag Names ~

Tag names cannot contain whitespace unless they are enclosed in quotes.

Valid Examples: >
    <validName>
    <"valid name">
    <tagName:command>
    <"Tag Name":command>

Invalid Examples: >
    <invalid name>
    <Tag Name:command>

~
                                                  *snip-advanced-tag-commands*
Advanced Tag Command Examples ~

Commands in tags can be as complex as desired. Readability is the main
limitation as the command will be placed in the document before execution.

The preferred method for defining complex commands is to hide the
functionality in a user function. >
    
    function! Count(haystack, needle)
        let counter = 0
        let index = match(a:haystack, a:needle)
        while index > -1
            let counter = counter + 1
            let index = match(a:haystack, a:needle, index+1)
        endwhile
        return counter
    endfunction

    function! PyArgList(count)
        if a:count == 0
            return "(,)"
        else
            return '('.repeat('<{}>, ', a:count).')'
        endif
    endfunction

    Snippet pf print "<{s}>" % <{s:PyArgList(Count(@z, '%[^%]'))}><CR><{}>

The above snippet will expand 'pf' to 'print "<{s}>" ...'. The user then
enters a format string. Once the string is entered the Count and PyArgList
functions are used to generate a number of empty tags.

                                                            *snip-limitations*
The above represents once of the limitations of the plugin. Due to the way
tags are identified it is not possible to include empty tags in another tag's
command. The only way to generate empty tags is to return them from a function
as in the above example. For other examples see the included bundles.

==============================================================================
SNIPPETSEMU CONTACT DETAILS                             *snip-contact-details*

To contact the author please email:

F <dot> Ingram <dot> lists <at> gmail <dot> com

The author welcomes corrections to this documentation, example snippets and
bug reports

The plugin is also currently hosted at Google Code:
    http://code.google.com/p/snippetsemu

Bug reports can also be posted on the hosting site:
    http://code.google.com/p/snippetsemu/issues/list

==============================================================================
SNIPPETSEMU KNOWN BUGS                  		   *snippets_emu-bugs*

Bugs are currently tracked on Google Code. Please post any you find on the
issue tracker:
    http://code.google.com/p/snippetsemu/issues/list

vim:tw=78:sw=4:ts=8:ft=help:norl:
plugin/snippetsEmu.vim
827
"        File: snippetsEmu.vim
"      Author: Felix Ingram
"              ( f.ingram.lists@gmail.com )
" Description: An attempt to implement TextMate style Snippets. Features include
"              automatic cursor placement and command execution.
" Last Change: Monday 23rd October 2006
" Version:     0.6
"
" This file contains some simple functions that attempt to emulate some of the 
" behaviour of 'Snippets' from the OS X editor TextMate, in particular the
" variable bouncing and replacement behaviour.
"
" {{{ USAGE:
"
" Place the file in your plugin directory.
" Define snippets using the Iabbr command which takes similar arguments to the
" built in iabbr command.
" Snippets are best defined in the 'after' subdirectory of your Vim home
" directory ('~/.vim/after' on Unix). Filetype specific snippets can be defined
" in '~/.vim/after/ftplugin/<filetype>_snippets.vim. Using the <buffer> argument will
" restrict the abbreviations to the filetype only.
"
" Example One:
" Iabbr <buffer> fori for <datum> in <data>:<CR><datum>.<>
"
" The above will expand to the following (indenting may differ):
" 
" for <datum> in <data>:
"   <datum>.<>
" 
" The cursor will be placed after the first < in insert mode.
" Pressing <S-Del> will 'tab' to the next place marker (<data>) in
" insert mode.  Adding text between < and > and then hitting <S-Del> will
" remove the angle brackets and replace all markers with a similar identifier.
"
" Example Two:
" With the cursor at the pipe, hitting <S-Del> will replace:
" for <MyVariableName|datum> in <data>:
"   <datum>.<>
"
" with (the pipe shows the cursor placement):
"
" for MyVariableName in <data>:
"   MyVariableName.<>
" 
" Enjoy.
"
" Additional Features:
"
" Commands in tags. Anything after a ':' in a tag will be run with Vim's
" 'execute' command. The value entered by the user (or the tag name if no change
" has been made) is passed in the @z register (the original contents of the
" register are restored once the command has been run).
"
" Named Tags. Naming a tag (the <datum> tag in the example above) and changing
" the value will cause all other tags with the same name to be changed to the
" same value (as illustrated in the above example). Not changing the value and
" hitting <S-Del> will cause the tag's name to be used as the default value.
" 
" TextMate 'compatibility'. New since 0.5. Setting the variable
" 'g:snip_set_textmate_cp' to '1' will change how the plugin operates. Snippets
" will not be expanded automatically, instead the user must hit <Tab>. If the
" previous word was defined as a snippet then it's expanded. If there are still
" active tags then the cursor is moved with replacements and commands being
" performed as usual. If there is no snippet to expand and no active tags then a
" <Tab> is inserted.
" The variable can be set in vimrc with the following command:
" let g:snip_set_textmate_cp = 1
"
" Multi-character start and end tags. Start and end tags can now be more than
" one character in length. The main advantage of this is that a single set of
" tags can be defined which will work on the majority of filetype. For example,
" the default settings of '<' and '>' work for most languages except HTML
" (and Visual Basic: do you know why?). You can now define tags such as '<{' and
" '}>' (which probably break in some random web page template language but you
" get the idea).
"
" Known Bugs:
"
" FIXED If the abbreviation starts with a tag and is inserted at the start of the line
" then the cursor will not be placed in the correct tag.
"
" FIXED Empty tag replacement.  Changing an empty tag will change all remaining
" empty tags
"
" FIXED Short variable names.  Having a single character in the tags will mess up
" the insert point.
"
" FIXED Autoindentation breaks and too much whitespace can be swallowed.
" Caused by using 'i' instead of 'a' in the redefined command.
"
" FIXED Search_str not defined
" Sourcing the script but defining no snippets will generate an error
"
" FIXED Search_str not buffer specific.
" Using different tags in different buffers will break the searching in all but
" the most recently opened (assuming different tags for all buffers)
"
" FIXED Multiple commands on the same line
" Having two tags with commands defined on the same line presents unexpected
" behaviour
"
" FIXED Escape strings for matching in replacements
" Special characters in commands or tag names can cause problems when replacing.
" Infinite loops are very likely.
"
" FIXED Default value function doesn't work for named tags
" Having a named tag will fill @z which in turn will cause the default function
" to fail. One possible fix will be to remove the possibility for tag names to
" act as default values. This may annoy, whoever.
" Fix details: A script variable 's:CHANGED_VAL' is set when a user changes a
" tag's value. The D() function checks for this and returns the appropriate
" response.
"
" FIXED Snippets starting with 's' don't work due to bad regex.
" Many thanks to nfowar for spotting this one.
"
" Test tags for pattern matching:
" The following are examples of valid and invalid tags. Whitespace can only be
" used in a tag name if the name is enclosed in quotes.
"
" Valid tags
" <>
" <tagName>
" <tagName:command>
" <:command>
" <"Tag Name">
" <"Tag Name":command>
" @@
" @tagName@
" @tagName:command@
" @:command@
" @"Tag Name"@
" @"Tag Name":command@
"
" Invalid tags, random text
" <:>
" <Tag Name>
" <Tag Name:command>
" <"Tag Name":>
" <Tag >
" <OpenTag
" @:@
" @Tag Name@
" @Tag Name:command@
" @"Tag Name":@
" @Tag @
" @OpenTag
"
" Here's our magic search term (assumes '<',':' and '>' as our tag delimiters:
" <\([^[:punct:] \t]\{-}\|".\{-}"\)\(:[^>]\{-1,}\)\?>
" }}}

if v:version < 700
  echomsg "snippetsEmu plugin requires Vim version 7 or later"
  finish
endif

let s:debug = 0

function! <SID>Debug(text)
  if exists('s:debug') && s:debug == 1
    echom a:text
  endif
endfunction

if (exists('loaded_snippet') || &cp) && !s:debug
  finish
endif

call <SID>Debug("Started the plugin")

let loaded_snippet=1
" {{{ Set up variables
if !exists("g:snip_start_tag")
    let g:snip_start_tag = "<"
endif

if !exists("g:snip_end_tag")
    let g:snip_end_tag = ">"
endif

if !exists("g:snip_elem_delim")
    let g:snip_elem_delim = ":"
endif

let s:just_expanded = 0

" }}}
" {{{ Map Jumper to the default key if not set already
if ( !hasmapto( '<Plug>Jumper', 'i' ) )
  if exists("g:snip_set_textmate_cp") && g:snip_set_textmate_cp == 1
    imap <unique> <Tab> <Plug>Jumper
  else
    imap <unique> <S-Del> <Plug>Jumper
  endif
endif
if exists("g:snip_set_textmate_cp") && g:snip_set_textmate_cp == 1
  imap <silent> <script> <Plug>Jumper <C-R>=<SID>Jumper()<CR>
else
  imap <silent> <script> <Plug>Jumper <ESC>:call <SID>Jumper()<CR>
endif

call <SID>Debug("Mapped keys")

" }}}
" {{{ CheckForBufferTags() - Checks to see whether buffer specific tags have
" been defined
function! <SID>CheckForBufferTags()
  if exists("b:snip_end_tag") && exists("b:snip_start_tag") && exists("b:snip_elem_delim")
    return 1
  else
    return 0
  endif
endfunction
" }}}
" {{{ SetSearchStrings() - Set the search string. Checks for buffer dependence
function! <SID>SetSearchStrings()
  if <SID>CheckForBufferTags()
    let b:search_str = b:snip_start_tag.'\([^'.
          \b:snip_start_tag.b:snip_end_tag.
          \'[:punct:] \t]\{-}\|".\{-}"\)\('.
          \b:snip_elem_delim.
          \'[^'.b:snip_end_tag.b:snip_start_tag.']\{-1,}\)\?'.b:snip_end_tag
    let b:search_commandVal = "[^".b:snip_elem_delim."]*"
    let b:search_endVal = "[^".b:snip_end_tag."]*"
  else
    let b:search_str = g:snip_start_tag.'\([^'.
          \g:snip_start_tag.g:snip_end_tag.
          \'[:punct:] \t]\{-}\|".\{-}"\)\('.
          \g:snip_elem_delim.
          \'[^'.g:snip_end_tag.g:snip_start_tag.']\{-1,}\)\?'.g:snip_end_tag
    let b:search_commandVal = "[^".g:snip_elem_delim."]*"
    let b:search_endVal = "[^".g:snip_end_tag."]*"
  endif
endfunction
" }}}
" {{{ SetCom(text) - Set command function
function! <SID>SetCom(text)
  " First we'll set up the tag highlighting using syntax highlighting

  if !exists("g:snip_disable_highlight") || g:snip_disable_highlight == 0
    " We do these commands on each snippet definition to ensure that
    " they are set properly. Supposedly some plugins or options such as
    " spell can cause highlight groups to disappear.
    " Thanks to the multiselect plugin for showing how to do this 
    "hi default SnippetHighlight gui=reverse term=reverse cterm=reverse
    hi default SnippetHighlight term=reverse cterm=bold ctermfg=7 ctermbg=1 guifg=White guibg=Red
    if <SID>CheckForBufferTags()
      let matchPat = b:snip_start_tag.'\([^[:punct:] \t]\{-}\|".\{-}"\)\('.b:snip_elem_delim.'[^'.b:snip_end_tag.']\{-1,}\)\?'.b:snip_end_tag
    else
      let matchPat = g:snip_start_tag.'\([^[:punct:] \t]\{-}\|".\{-}"\)\('.g:snip_elem_delim.'[^'.g:snip_end_tag.']\{-1,}\)\?'.g:snip_end_tag
    endif

    execute "match SnippetHighlight '".matchPat."'"
  endif

  if exists("g:snip_set_textmate_cp") && g:snip_set_textmate_cp == 1
    " When using TextMate compatibility we don't need to worry about calling
    " SetPos() or NextHop() as this will be handled when tab is hit

    let text = substitute(a:text, '<CR>\|<Esc>\|<Tab>\|<BS>\|<Space>\|<C-r>\|<Pipe>\|\"\|\\','\\&',"g")

    let text = substitute(text, "$", "","")
    if match(text,"<buffer>") == 0
      let text = substitute(text, '\s*<buffer>\s*', "","")
      let tokens = split(text, ' ')
      let lhs = tokens[0]
      let rhs = join(tokens[1:])
      call <SID>SetSearchStrings()
      for char in split(lhs, '\zs')
        if char == '@'
          exec 'setlocal iskeyword+=@-@'
        elseif char != '^'
          try
            exec 'setlocal iskeyword+='.char
          catch /474/
          endtry
        endif
      endfor
      let lhs = <SID>Hash(lhs)
      return "let b:snip_".lhs.' = "'.rhs.'"'
    else
      let text = substitute(text, '^\s*', "", "")
      let tokens = split(text, ' ')
      let lhs = tokens[0]
      let rhs = join(tokens[1:])
      call <SID>SetSearchStrings()
      for char in split(lhs, '\zs')
        if char == '@'
          exec 'setlocal iskeyword+=@-@'
        elseif char != '^'
          try
            exec 'setlocal iskeyword+='.char
          catch /474/
          endtry
        endif
      endfor
      let lhs = <SID>Hash(lhs)
      return "let g:snip_".lhs.' = "'.rhs.'"'
    endif
  else
    call <SID>SetSearchStrings()
    if match(a:text,"<buffer>") == 0
      return "iabbr <buffer> ".substitute(strpart(a:text,stridx(a:text,">")+2)," "," <ESC>:call <SID>SetPos()<CR>a","")."<ESC>:call <SID>NextHop()<CR><C-R>=Eatchar('\\s')<CR>"
    else
      return "iabbr ".substitute(a:text," "," <ESC>:call <SID>SetPos()<CR>a","")."<ESC>:call <SID>NextHop()<CR><C-R>=Eatchar('\\s')<CR>"
    endif
  endif
endfunction
" }}}
" {{{ SetPos() - Store the current cursor position
" This function also now sets up the search strings so that autocommands can be
" used to define different tag delimiters for different filetypes
function! <SID>SetPos()
  let b:curCurs = col(".")
  let b:curLine = line(".")
  let s:curCurs = col(".")
  let s:curLine = line(".")
  call <SID>SetSearchStrings()
  let s:just_expanded = 1
endfunction
" }}}
" {{{ Check for end - Check whether the cursor is at the end of the current line
function! <SID>CheckForEnd()
  " Check to see whether we're at the end of a line so we can decide on
  " how to start inserting
  if <SID>CheckForBufferTags()
    let snip_elem_delim = b:snip_elem_delim
    let snip_start_tag = b:snip_start_tag
    let snip_end_tag = b:snip_end_tag
  else
    let snip_elem_delim = g:snip_elem_delim
    let snip_start_tag = g:snip_start_tag
    let snip_end_tag = g:snip_end_tag
  endif
  if col(".") == strlen(getline("."))
    return 1
  elseif getline(".") =~ '^$'
    return 1
  elseif (getline(".")[col(".")] == snip_elem_delim) &&
      \(getline(".")[col(".") + 1] == snip_end_tag) &&
      \(col(".") + 2 ==strlen(getline(".")))
      return 1
  else
    return 0 
  endif
endfunction
" }}}
" {{{ DeleteEmptyTag 
function! <SID>DeleteEmptyTag()
  if <SID>CheckForBufferTags()
    let snip_elem_delim = b:snip_elem_delim
    let snip_start_tag = b:snip_start_tag
    let snip_end_tag = b:snip_end_tag
  else
    let snip_elem_delim = g:snip_elem_delim
    let snip_start_tag = g:snip_start_tag
    let snip_end_tag = g:snip_end_tag
  endif
  for i in range(strlen(snip_start_tag) + strlen(snip_end_tag))
    normal x
  endfor
endfunction
" }}}
" {{{ NextHop() - Jump to the next tag if one is available
function! <SID>NextHop()
  if <SID>CheckForBufferTags()
    let snip_elem_delim = b:snip_elem_delim
    let snip_start_tag = b:snip_start_tag
    let snip_end_tag = b:snip_end_tag
  else
    let snip_elem_delim = g:snip_elem_delim
    let snip_start_tag = g:snip_start_tag
    let snip_end_tag = g:snip_end_tag
  endif
  if s:just_expanded == 1
    call cursor(s:curLine, 1)
    let s:just_expanded = 0
  else
    call cursor(s:curLine, s:curCurs)
  endif
  " Check to see whether we're sitting on a tag and if not then perform a
  " search
  if match(getline("."), b:search_str,s:curCurs) != 0
    if search(b:search_str) != 0
      " Delete the tag if appropriate
      " First check whether we're sitting on an empty tag
      if (strpart(getline("."), col(".")+strlen(snip_start_tag)-1, strlen(snip_end_tag)) == snip_end_tag)
        " We are so let's check whether the tag is the final text on the
        " line.
        if match(getline("."), snip_start_tag.snip_end_tag.'$') -
              \ (col(".")+strlen(snip_start_tag))+1+strlen(snip_end_tag) == 0
          call <SID>DeleteEmptyTag()
          startinsert!
        else
          " It isn't so we'll delete it and start normally
          call <SID>DeleteEmptyTag()
          startinsert
        endif
      else
        " Not on an empty tag so it must be a normal tag, so we'll just start
        " insert as usual
        for i in range(strlen(snip_start_tag))
          normal l
        endfor
        startinsert
      endif
    else
      " No more matches so we'll jump to the next bit of whitespace
      if <SID>CheckForEnd() == 1 || match(getline("."),'\W',s:curCurs) == -1
        startinsert!
      elseif match(getline("."),'\W',s:curCurs) < match(getline("."),'$',s:curCurs)
        call search('\W')
        startinsert
      else
        startinsert!
      endif
    endif
  else
    " We're sitting on a tag so we'll delete it and start insert
    " Delete the tag as appropriate
     if (strpart(getline("."), col(".")+strlen(snip_start_tag)-1, strlen(snip_end_tag)) == snip_end_tag )
      " We are so let's check whether the tag is the final text on the
      " line.
      if match(getline("."), snip_start_tag.snip_end_tag.'$') -
            \ (col(".")+strlen(snip_start_tag))+1+strlen(snip_end_tag) == 0
        call <SID>DeleteEmptyTag()
        startinsert!
      else
        " It isn't so we'll delete it and start normally
        call <SID>DeleteEmptyTag()
        startinsert
      endif
    else
      " Not on an empty tag so it must be a normal tag, so we'll just start
      " insert as usual
      for i in range(strlen(snip_start_tag))
        normal l
      endfor
      startinsert
    endif
  endif
endfunction
" }}}
" {{{ RunCommand() - Execute commands stored in tags
function! <SID>RunCommand(command, z)
  " Escape backslashes for the matching.  Not sure what other escaping is
  " needed here
  if <SID>CheckForBufferTags()
    let snip_elem_delim = b:snip_elem_delim
    let snip_start_tag = b:snip_start_tag
    let snip_end_tag = b:snip_end_tag
  else
    let snip_elem_delim = g:snip_elem_delim
    let snip_start_tag = g:snip_start_tag
    let snip_end_tag = g:snip_end_tag
  endif
  let command = a:command
  let s:snip_temp = substitute(command, "\\", "\\\\\\\\","g")
  " Save current value of 'z'
  let s:snip_save = @z
  let @z=a:z
  " Call the command
  execute 'let @z = '. a:command
  " Replace the value
  let ret = @z
  let @z = s:snip_save
  return ret
  call setline(line("."),substitute(getline("."),snip_start_tag.s:replaceVal.s:matchVal.snip_elem_delim.s:snip_temp.snip_end_tag, @z, "g"))
endfunction
" }}}
" {{{ MakeChanges() - Search the document making all the changes required
" This function has been factored out to allow the addition of commands in tags

function! <SID>MakeChanges()
  " Make all the changes
  " Change all the tags with the same name and no commands defined
  if <SID>CheckForBufferTags()
    let snip_elem_delim = b:snip_elem_delim
    let snip_start_tag = b:snip_start_tag
    let snip_end_tag = b:snip_end_tag
  else
    let snip_elem_delim = g:snip_elem_delim
    let snip_start_tag = g:snip_start_tag
    let snip_end_tag = g:snip_end_tag
  endif

  if s:matchVal == ""
    return
  endif
  call <SID>Debug("Matching on this value: ".s:matchVal)
  call <SID>Debug("Replacing with this value: ".s:replaceVal)
  while search(snip_start_tag.s:matchVal.snip_end_tag,"w") > 0
    call setline(line("."),substitute(getline("."), snip_start_tag.s:matchVal.snip_end_tag, s:replaceVal,"g"))
  endwhile
  " Change all the tags with the same name and a command defined.
  " I.e. start tag, tag name (matchVal), element delimiter, characters not
  " whitespace and then end tag
  call <SID>Debug("Searching for this string: ".snip_start_tag.s:matchVal.snip_elem_delim)
  while search(snip_start_tag.s:matchVal.snip_elem_delim,"w") > 0
    " Grab the command
    " We need to search from the cursor position to avoid problems when the
    " snip_delimiter is on the line
    let commandText = matchstr(getline("."),snip_elem_delim.".\\{-}".snip_end_tag, col("."))
    call <SID>Debug("Found this command text: ".commandText)
    let commandToRun = strpart(commandText,1,strlen(commandText)-strlen(snip_end_tag)-1)
    call <SID>Debug("Running this command: ".commandToRun)
    let s:snip_temp = substitute(commandToRun, "\\", "\\\\\\\\","g")
    let commandResult = <SID>RunCommand(commandToRun, s:replaceVal)
    call <SID>Debug("Got this result: ".commandResult)
    let tagmatch = '\V'.snip_start_tag.s:matchVal.snip_elem_delim.s:snip_temp.snip_end_tag
    let lines = split(substitute(getline("."), tagmatch, commandResult, "g"),'\n')
    if len(lines) > 1
      call setline(".", lines[0])
      call append(".", lines[1:])
    else
      call setline(".", lines)
    endif
  endwhile
endfunction

" }}}
" {{{ ChangeVals() - Rewrite of ChangedVal and NoChangedVal avoiding code
" duplication
function! <SID>ChangeVals(changed)
  if a:changed == 1
    let s:CHANGED_VAL = 1
  else
    let s:CHANGED_VAL = 0
  endif
  if <SID>CheckForBufferTags()
    let snip_elem_delim = b:snip_elem_delim
    let snip_start_tag = b:snip_start_tag
    let snip_end_tag = b:snip_end_tag
  else
    let snip_elem_delim = g:snip_elem_delim
    let snip_start_tag = g:snip_start_tag
    let snip_end_tag = g:snip_end_tag
  endif
  let elem_match = match(s:line, snip_elem_delim, s:curCurs)
  let tagstart = strridx(getline("."), snip_start_tag,s:curCurs)+strlen(snip_start_tag)
  if elem_match != -1 && elem_match < match(s:line, snip_end_tag, s:curCurs)
    " We've got snip_elem_delim  before snip_end_tag so we have a command to
    " run. There are no longer default values for a tag, the name is used
    " instead.
    " Grab the command to run
    let commandText = matchstr(s:line, b:search_endVal, match(s:line, snip_elem_delim, s:curCurs))
    let commandToRun = strpart(commandText,1,strlen(commandText)-strlen(snip_end_tag)+1)
    let commandMatch = substitute(commandToRun, '\', '\\\\', "g")
    " matchVal is the same regardless of whether we changed the value or not.
    " It's always from the cursor to b:search_commandVal (since we've got a
    " command to run)
    let s:matchVal = matchstr(s:line, b:search_commandVal, s:curCurs)
    " replaceVal is the only thing that will differ.
    if s:CHANGED_VAL
      " The value has changed so we need to grab our current position back
      " to the start of the tag
      let replaceVal = strpart(getline("."), tagstart,s:curCurs-tagstart)
      call <SID>Debug("User entered this value: ".replaceVal)
      let tagmatch = snip_start_tag.replaceVal.s:matchVal.snip_elem_delim.commandMatch.snip_end_tag
    else
      " The value hasn't changed so it's just the matchVal (the tag name)
      " without any quotes that are around it
      let replaceVal = substitute(s:matchVal, '^\"\(.*\)\"$', "\1", "")
      call <SID>Debug("User did not enter a value. Replacing with this value: ".replaceVal)
      let tagmatch = snip_start_tag.s:matchVal.snip_elem_delim.commandMatch.snip_end_tag
    endif
    call <SID>Debug("Matching on this string: ".tagmatch)
    let tagsubsitution = <SID>RunCommand(commandToRun, replaceVal)
    let lines = split(substitute(getline("."), tagmatch, tagsubsitution, "g"),'\n')
    if len(lines) > 1
      call setline(".", lines[0])
      call append(".", lines[1:])
    else
      call setline(".", lines)
    endif
    "let s:replaceVal = tagsubsitution
    " We use replaceVal instead of tagsubsitution as otherwise the command
    " result will be passed to subsequent tags
    let s:replaceVal = replaceVal
    call <SID>MakeChanges()
  else
    " There's no command to run
    let s:matchVal = matchstr(s:line, b:search_endVal, s:curCurs)
    let firstBit = strpart(s:line,0, tagstart - strlen(snip_start_tag))
    if s:CHANGED_VAL
      " This is the value the user typed in. We pull it out as the
      " line up to the cursor less the 'firstBit' above. We also
      " chop off the start_tag as well.
      let middleBit = strpart(strpart(getline("."), strlen(firstBit), s:curCurs-strlen(firstBit)), strlen(snip_start_tag))
      let s:replaceVal = strpart(strpart(s:line, tagstart-1, s:curCurs - (tagstart - 1)), 1)
    else
      if s:matchVal[0] == '"' 
        " We have a quotes around our tag name so let's remove them
        let s:replaceVal = strpart(s:matchVal,1,strlen(s:matchVal)-2)
        let middleBit = strpart(s:line,s:curCurs+1, match(s:line, snip_end_tag, s:curCurs)-s:curCurs-2)
      else
        let s:replaceVal = strpart(s:matchVal,0,strlen(s:matchVal))
        let middleBit = strpart(s:line,s:curCurs,match(s:line,snip_end_tag,s:curCurs)-s:curCurs)
      endif
      let firstBit = strpart(s:line,0,s:curCurs - strlen(snip_start_tag))
    endif
    let lastBit = strpart(strpart(s:line,match(s:line,snip_end_tag,s:curCurs)),strlen(snip_end_tag))
    call setline(line("."),firstBit.middleBit.lastBit)
    if s:matchVal != ""
      call <SID>MakeChanges()
    endif
  endif
  unlet s:CHANGED_VAL
  call <SID>NextHop()
endfunction
" }}}
"{{{ SID() - Get the SID for the current script
function! s:SID()
  return matchstr(expand('<sfile>'), '<SNR>\zs\d\+\ze_SID$')
endfun
"}}}
"{{{ CheckForInTag() - Check whether we're in a tag
function! <SID>CheckForInTag()
  if <SID>CheckForBufferTags()
    let snip_elem_delim = b:snip_elem_delim
    let snip_start_tag = b:snip_start_tag
    let snip_end_tag = b:snip_end_tag
  else
    let snip_elem_delim = g:snip_elem_delim
    let snip_start_tag = g:snip_start_tag
    let snip_end_tag = g:snip_end_tag
  endif
  if snip_start_tag != snip_end_tag
    " The tags are different so we can check to see whether the
    " end tag comes before a start tag
    let s:endMatch = match(s:line, snip_end_tag, s:curCurs)
    let s:startMatch = match(s:line, snip_start_tag, s:curCurs)
    let s:whiteSpace = match(s:line, '\s', s:curCurs)

    if s:endMatch != -1 && ((s:endMatch < s:startMatch) || s:startMatch == -1)
      " End has come before start so we're in a tag.
      return 1
    else
      return 0
    endif
  else
    " Start and end tags are the same so we need do tag counting to see
    " whether we're in a tag.
    let s:count = 0
    let s:curSkip = s:curCurs
    while match(strpart(s:line,s:curSkip),snip_start_tag) != -1 
      if match(strpart(s:line,s:curSkip),snip_start_tag) == 0
        let s:curSkip = s:curSkip + 1
      else
        let s:curSkip = s:curSkip + 1 + match(strpart(s:line,s:curSkip),snip_start_tag)
      endif
      let s:count = s:count + 1
    endwhile
    if (s:count % 2) == 1
      " Odd number of tags implies we're inside a tag.
      return 1
    else
      " We're not inside a tag.
      return 0
    endif
  endif
endfunction
"}}}
" {{{ Jumper()
" We need to rewrite this function to reflect the new behaviour. Every jump
" will now delete the markers so we need to allow for the following conditions
" 1. Empty tags e.g. "<>".  When we land inside then we delete the tags.
"  "<:>" is now an invalid tag (use "<>" instead) so we don't need to check for
"  this
" 2. Tag with variable name.  Save the variable name for the next jump.
" 3. Tag with command. Tags no longer have default values. Everything after the
" centre delimiter until the end tag is assumed to be a command.
" 
" Jumper is performed when we want to perform a jump.  If we've landed in a
" 1. style tag then we'll be in free form text and just want to jump to the
" next tag.  If we're in a 2. or 3. style tag then we need to look for whether
" the value has changed and make all the replacements.   If we're in a 3.
" style tag then we need to replace all the occurrences with their command
" modified values.
" 
function! <SID>Jumper()
  " Set up some useful variables
  if <SID>CheckForBufferTags()
    let snip_elem_delim = b:snip_elem_delim
    let snip_start_tag = b:snip_start_tag
    let snip_end_tag = b:snip_end_tag
  else
    let snip_elem_delim = g:snip_elem_delim
    let snip_start_tag = g:snip_start_tag
    let snip_end_tag = g:snip_end_tag
  endif
  if !exists('b:search_str')
    return "\<Tab>"
  endif
  if exists('g:snip_set_textmate_cp') && g:snip_set_textmate_cp == 1
    let s:curCurs = col(".") - 1
  else
    let s:curCurs = col(".")
  endif
  let s:curLine = line(".")
  let s:line = getline(".")
  let s:matchVal = ""
  let s:replaceVal = ""
  " Check to see whether we're at the start of a tag.  
  " start then we should be assuming that we've got a 'default' value or a
  " command to run.  Otherwise the user will have pressed the jump key
  " without changing the value.
  " First we need to check that we're inside a tag i.e. the previous
  " jump didn't land us in a 1. style tag.
   
  if exists("g:snip_set_textmate_cp") && g:snip_set_textmate_cp == 1
    " First we'll check that the user hasn't just typed a snippet to expand
    "
    let origword = matchstr(strpart(getline("."), 0, s:curCurs), '\k\{-}$')
    let word = <SID>Hash(origword)
"    " The following code is lifted wholesale from the imaps.vim script - Many
"    " thanks for the inspiration to add the TextMate compatibility
"    " Unless we are at the very end of the word, we need to go back in order
"    " to find the last word typed.
    let rhs = ''
    " We don't use the FT specific variable names so we'll avoid that check. We
    " do need to check for buffer specific expansions, however (which is how we
    " achieve the same thing).
    if exists('b:snip_'.word)
      exe 'let rhs = b:snip_'.word
    elseif exists('g:snip_'.word)
    " also check for global definitions
      exe 'let rhs = g:snip_'.word
    end

    if rhs != ''
      " if this is a mapping, then erase the previous part of the map
      " by also returning a number of backspaces.
      let bkspc = substitute(origword, '.', "\<bs>", "g")

      let s:curCurs = s:curCurs - strlen(expand('<cword>')) - 1
      let s:just_expanded = 1
      return bkspc.rhs."\<Esc>:call cursor(".s:curLine.", ".s:curCurs.")\<CR>:call <SNR>".s:SID()."_NextHop()\<CR>"
    else
      " No definition so let's check to see whether we're in a tag
      if <SID>CheckForInTag()
        " We're in a tag so we need to do processing
        if strpart(s:line, s:curCurs - strlen(snip_start_tag), strlen(snip_start_tag)) == snip_start_tag
          call <SID>ChangeVals(0)
          return ''
        else
          call <SID>ChangeVals(1)
          return ''
        endif
      else
        " We're not in a tag so we'll see whether there are more tags
        if search(b:search_str, "n")
          " More tags so let's perform nexthop
          let s:replaceVal = ""
          call <SID>NextHop()
          return ''
        else
          " No more tags so let's return a Tab
          return "\<Tab>"
        endif
      endif
    end
  endif
  " We're not using TextMate style jumping so let's use the old school method

  if <SID>CheckForInTag()
    if strpart(s:line, s:curCurs - strlen(snip_start_tag), strlen(snip_start_tag)) == snip_start_tag
      call <SID>ChangeVals(0)
    else
      call <SID>ChangeVals(1)
    endif
  else
    " Not in a tag so let's jump to the next tag
    let s:replaceVal = ""
    call <SID>NextHop()
  endif
endfunction
" }}}
" {{{ Set up the 'Iabbr' and 'Snippet' commands
command! -nargs=+ Iabbr execute <SID>SetCom(<q-args>)
command! -nargs=+ Snippet execute <SID>SetCom("<buffer> ".<q-args>)
"}}}
" {{{ Utility functions
" The following two functions are from Benji Fisher's foo.vim - a very helpful file
" The built-in getchar() function returns a Number for an 8-bit character, and
" a String for any other character.  This version always returns a String.
fun! Getchar()
  let c = getchar()
  if c != 0
    let c = nr2char(c)
  endif
  return c
endfun

fun! Eatchar(pat)
   let c = Getchar()
   return (c =~ a:pat) ? '' : c
endfun

" This function will just return what's passed to it. 
" We use string() so that numbers can be passed more naturally
fun! D(text)
  if exists('s:CHANGED_VAL') && s:CHANGED_VAL == 1
    return @z
  else
    return a:text
  endif
endfun

" <SID>Hash allows the use of special characters in snippets when textmate mode
" is used.
" This function is lifted straight from the imaps.vim plugin. Please let me know
" if this is against licensing.
function! <SID>Hash(text)
	return substitute(a:text, '\([^[:alnum:]]\)',
				\ '\="_".char2nr(submatch(1))."_"', 'g')
endfunction

" }}}
" Abbreviations are set up as usual but using the Iabbr command rather
" than iabbr.  Formatting needs to be done as usual, hence the '<<'s and
" similar.  Not sure how well @ works as a delimiter but it can be changed
" BEST PRACTICE RECOMMENDATION: Store your abbeviation definitions in
" '.vim/after/plugin/' so they will get sourced once the plugin has been loaded
" vim: set tw=80 sw=2 sts=2 et foldmethod=marker :
