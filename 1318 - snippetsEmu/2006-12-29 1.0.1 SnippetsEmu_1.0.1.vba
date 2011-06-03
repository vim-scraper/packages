" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
after/ftplugin/actionscript_snippets.vim
9
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet dm duplicateMovieClip(".st."target".et.", ".st."newName".et.", ".st."depth".et.");"
after/ftplugin/asp_snippets.vim
17
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet rr Response.Redirect(".st."to".et.")".st.et
exec "Snippet app Application(\"".st.et."\")".st.et
exec "Snippet forin For ".st."var".et." in ".st."array".et."<CR>".st.et."<CR>Next<CR>".st.et
exec "Snippet ifelse If ".st."condition".et." Then<CR>".st.et."<CR>Else<CR>".st.et."<CR>End if<CR>".st.et
exec "Snippet rw Response.Write ".st.et
exec "Snippet sess Session(\"".st.et."\")".st.et
exec "Snippet rf Request.Form(\"".st.et."\")".st.et
exec "Snippet rq Request.QueryString(\"".st.et."\")".st.et
exec "Snippet while While ".st."NOT".et." ".st."condition".et."<CR>".st.et."<CR>Wend<CR>".st.et
after/ftplugin/c_snippets.vim
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
    endwhile
    return counter
endfunction

function! CArgList(count)
    " This returns a list of empty tags to be used as 
    " argument list placeholders for the call to printf
    let st = g:snip_start_tag
    let et = g:snip_end_tag
    if a:count == 0
        return ""
    else
        return repeat(', '.st.et, a:count)
    endif
endfunction
	
let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet do do<CR>{<CR>".st.et."<CR>} while (".st.et.");"
exec "Snippet readfile std::vector<uint8_t> v;<CR>if(FILE* fp = fopen(\"".st."filename".et."\", \"r\"))<CR>{<CR>uint8_t buf[1024];<CR>while(size_t len = fread(buf, 1, sizeof(buf), fp))<CR>v.insert(v.end(), buf, buf + len);<CR>fclose(fp);<CR>}<CR>".st.et
exec "Snippet beginend ".st."v".et.".begin(), ".st."v".et.".end()".st.et
exec "Snippet once #ifndef _".st."file:substitute(expand('%'),'\\(.\\)','\\u\\1','g')".et."_<CR><CR>#define _".st."file".et."_<CR><CR>".st.et."<CR><CR>#endif /* _".st."file".et."_ */<CR>".st.et
exec "Snippet class class ".st."name".et."<CR>{<CR>public:<CR>".st."name".et." (".st."arguments".et.");<CR>virtual ~".st."name".et."();<CR><CR>private:<CR>".st.":D('/* data */')".et."<CR>};<CR>".st.et
" TODO This is a good one but I can't quite work out the syntax yet
exec "Snippet printf printf(\"".st."\"%s\"".et."\\n\"".st."\"%s\":CArgList(Count(@z, '%[^%]'))".et.");<CR>".st.et
exec "Snippet vector std::vector<".st."char".et."> v".st.et.";"
exec "Snippet struct struct ".st."name".et."<CR>{<CR>".st.":D('/* data */')".et."<CR>};<CR>".st.et
exec "Snippet template template <typename ".st."_InputIter".et."><CR>".st.et
" TODO this one as well. Wish I knew more C
" Snippet namespace namespace ${1:${TM_FILENAME/(.*?)\\..*/\\L$1/}}\n{\n\t$0\n};<CR>.st.et
exec "Snippet namespace namespace ".st.":substitute(expand('%'),'.','\\l&', 'g')".et."<CR>{<CR>".st.et."<CR>};<CR>".st.et
exec "Snippet map std::map<".st."key".et.", ".st."value".et."> map".st.et.";<CR>".st.et
exec "Snippet mark #if 0<CR><CR>".st.":D('#pragma mark -<CR><CR>'}#pragma mark ".st.et."<CR><CR>#endif<CR><CR>".st.et
exec "Snippet if if(".st.et.")<CR>{<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet main int main (int argc, char const* argv[])<CR>{<CR>".st.et."<CR>return 0;<CR}<CR>".st.et
exec "Snippet Inc #include <".st.":D('.h')".et."><CR>".st.et
exec "Snippet inc #include \"".st.et.".h\""
exec "Snippet for for( ".st.":D('unsigned int')".et." ".st."i".et." = ".st.":D('0')".et."; ".st."i".et." < ".st."count".et."; ".st."i".et." += ".st.":D('1')".et.")<CR>{<CR>".st.et."<CR>}<CR>".st.et
after/ftplugin/css_snippets.vim
30
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet visibility ".st.":D('visible/hidden/collapse')".et.";".st.et
exec "Snippet list list-style-image: url(".st.et.");".st.et
exec "Snippet text text-shadow: rgb(".st.":D('255')".et.", ".st.":D('255')".et.", ".st.":D('255')".et.", ".st.":D('10px')".et." ".st.":D('10px')".et." ".st.":D('2px')".et.";".st.et." "
exec "Snippet overflow overflow: ".st.":D('visible/hidden/scroll/auto')".et.";".st.et
exec "Snippet white white-space: ".st.":D('normal/pre/nowrap')".et.";".st.et
exec "Snippet clear cursor: url(".st.et.");".st.et
exec "Snippet margin padding-top: ".st.":D('20px')".et.";".st.et
exec "Snippet background background #".st.":D('DDD')".et." url(".st.et.") ".st.":D('repeat/repeat-x/repeat-y/no-repeat')".et." ".st.":D('scroll/fixed')".et." top left/top center/top right/center left/center center/center right/bottom left/bottom center/bottom right/x% y%/x-pos y-pos')".et.";".st.et
exec "Snippet word word-spaceing: ".st.":D('10px')".et.";".st.et
exec "Snippet z z-index: ".st.et.";".st.et
exec "Snippet vertical vertical-align: ".st.":D('baseline/sub/super/top/text-top/middle/bottom/text-bottom/length/%')".et.";".st.et
exec "Snippet marker marker-offset: ".st.":D('10px')".et.";".st.et
exec "Snippet cursor cursor: ".st.":D('default/auto/crosshair/pointer/move/*-resize/text/wait/help')".et.";".st.et
exec "Snippet border border-right: ".st.":D('1')".et."px ".st.":D('solid')".et." #".st.":D('999')".et.";".st.et
exec "Snippet display display: block;".st.et
exec "Snippet padding padding: ".st.":D('20px')".et." ".st.":D('0px')".et.";".st.et
exec "Snippet letter letter-spacing: ".st.et."em;".st.et
exec "Snippet color color: rgb(".st.":D('255')".et.", ".st.":D('255')".et.", ".st.":D('255')".et.");".st.et
exec "Snippet font font-weight: ".st.":D('normal/bold')".et.";".st.et
exec "Snippet position position: ".st.":D('static/relative/absolute/fixed')".et.";".st.et
exec "Snippet direction direction: ".st.":D('ltr|rtl')".et.";".st.et
exec "Snippet float float: ".st.":D('left/right/none')".et.";".st.et
after/ftplugin/django_model_snippets.vim
61
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

function! DjangoArgList(count)
    " This needs to be Python specific as print expects a
    " tuple and an empty tuple looks like this (,) so we'll need to make a
    " special case for it
    let st = g:snip_start_tag
    let et = g:snip_end_tag
    if a:count == 0
        return "()"
    else
        return '('.repeat(st.et.', ', a:count).')'
    endif
endfunction

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet mmodel class ".st.et."(models.Model):<CR>\"\"\"".st.":D('model description')".et."\"\"\"<CR>".st.et." = ".st.et."<CR><CR>class Admin:<CR>pass<CR><CR><BS>def __str__(self):<CR>return \"".st."s".et."\" % ".st."s:DjangoArgList(Count(@z, '%[^%]'))".et."<CR>".st.et
exec "Snippet mauto models.AutoField(".st.et.")".st.et
exec "Snippet mbool models.BooleanField()".st.et
exec "Snippet mchar models.CharField(maxlength=".st."50".et.st.et.")".st.et
exec "Snippet mcsi models.CommaSeparatedIntegerField(maxlength=".st."50".et.st.et.")".st.et
exec "Snippet mdate models.DateField(".st.et.")".st.et
exec "Snippet mdatet models.DateTimeField(".st.et.")".st.et
exec "Snippet memail models.EmailField(".st.et.")".st.et
exec "Snippet mfile models.FileField(upload_to=\"".st.et."\"".st.et.")".st.et
exec "Snippet mfilep models.FilePathField(path=\"".st.et."\"".st.et.")".st.et
exec "Snippet mfloat models.FloatField(max_digits=".st.et.", decimal_places=".st.et.")".st.et
exec "Snippet mimage models.ImageField(".st.et.")".st.et
exec "Snippet mint models.IntegerField(".st.et.")".st.et
exec "Snippet mipadd models.IPAddressField(".st.et.")".st.et
exec "Snippet mnull models.NullBooleanField()".st.et
exec "Snippet mphone models.PhoneNumberField(".st.et.")".st.et
exec "Snippet mpint models.PositiveIntegerField(".st.et.")".st.et
exec "Snippet mspint models.PositiveSmallIntegerField(".st.et.")".st.et
exec "Snippet mslug models.SlugField(".st.et.")".st.et
exec "Snippet msint models.SmallIntegerField(".st.et.")".st.et
exec "Snippet mtext models.TextField(".st.et.")".st.et
exec "Snippet mtime models.TimeField(".st.et.")".st.et
exec "Snippet murl models.URLField(verifiy_exists=".st."true".et.st.et.")".st.et
exec "Snippet muss models.USStateField(".st.et.")".st.et
exec "Snippet mxml models.XMLField(schema_path=\"".st.et."\"".st.et.")".st.et
exec "Snippet mfor models.ForeignKey(".st.et.")".st.et
exec "Snippet mm2o models.ForeignKey(".st.et.")".st.et
exec "Snippet mm2m models.ManyToManyField(".st.et.")".st.et
exec "Snippet mo2o models.OneToOneField(".st.et.")".st.et
exec "Snippet mman models.Manager()".st.et
after/ftplugin/django_template_snippets.vim
32
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet {{ {% templatetag openvariable %}".st.et
exec "Snippet }} {% templatetag closevariable %}".st.et
exec "Snippet {% {% templatetag openblock %}".st.et
exec "Snippet %} {% templatetag closeblock %}".st.et
exec "Snippet now {% now \"".st.et."\" %}".st.et
exec "Snippet firstof {% firstof ".st.et." %}".st.et
exec "Snippet ifequal {% ifequal ".st.et." ".st.et." %}<CR>".st.et."<CR>{% endifequal %}<CR>".st.et
exec "Snippet ifchanged {% ifchanged %}".st.et."{% endifchanged %}".st.et
exec "Snippet regroup {% regroup ".st.et." by ".st.et." as ".st.et." %}".st.et
exec "Snippet extends {% extends \"".st.et."\" %}<CR>".st.et
exec "Snippet filter {% filter ".st.et." %}<CR>".st.et."<CR>".st."% endfilter %}"
exec "Snippet block {% block ".st.et." %}<CR>".st.et."<CR>{% endblock %}<CR>".st.et
exec "Snippet cycle {% cycle ".st.et." as ".st.et." %}".st.et
exec "Snippet if {% if ".st.et." %}<CR>".st.et."<CR>{% endif %}<CR>".st.et
exec "Snippet debug {% debug %}<CR>".st.et
exec "Snippet ifnotequal {% ifnotequal ".st.et." ".st.et." %}<CR>".st.et."<CR>{% endifnotequal %}<CR>".st.et
exec "Snippet include {% include ".st.et." %}<CR>".st.et
exec "Snippet comment {% comment %}<CR>".st.et."<CR>{% endcomment %}<CR>".st.et
exec "Snippet for {% for ".st.et." in ".st.et." %}<CR>".st.et."<CR>{% endfor %}<CR>".st.et
exec "Snippet ssi {% ssi ".st.et." ".st.":D('parsed')".et." %}".st.et
exec "Snippet widthratio {% widthration ".st.":D('this_value')".et." ".st.":D('max_value')".et." ".st.":D('100')".et." %}".st.et
exec "Snippet load {% load ".st.et." %}<CR>".st.et
" Field snippet contributed by Alex Pounds
exec "Snippet field <p><label for=\"id_".st."fieldnam}e>\">".st."fieldlabel".et.":</label> {{ form.".st."fieldname".et." }}<CR>{% if form.".st."fieldname".et.".errors %}*** {{ form.".st."fieldname".et.".errors|join:\", \" }} {% endif %}</p>".st.et
after/ftplugin/f-script_snippets.vim
14
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet tbd to:".st.et." by:".st.et." do:[ ".st.":D(':i')".et." |<CR>".st.et."<CR>].".st.et
exec "Snippet it ifTrue:[<CR>".st.et."<CR>].".st.et
exec "Snippet ift ifFalse:[<CR>".st.et."<CR>] ifTrue:[<CR>".st.et."<CR>].".st.et
exec "Snippet itf ifTrue:[<CR>".st.et."<CR>] ifFalse:[<CR>".st.et."<CR>].".st.et
exec "Snippet td to:".st.et." do:[".st.et." ".st.et." |<CR>".st.et."<CR>].".st.et
exec "Snippet if ifFalse:[<CR>".st.et."<CR>].".st.et
after/ftplugin/haskell_snippets.vim
9
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet mod module: ".st.":D('Main')".et." where<CR>".st.et
after/ftplugin/html_snippets.vim
62
if !exists('loaded_snippet') || &cp
    finish
endif

function! Onload()
    let st = g:snip_start_tag
    let et = g:snip_end_tag
    return 'onload="'.st.et.'"'
endfunction

function! Id()
    let st = g:snip_start_tag
    let et = g:snip_end_tag
    return ' id="'.st.et.'"'
endfunction

function! Cellspacing()
    let st = g:snip_start_tag
    let et = g:snip_end_tag
    let cd = g:snip_elem_delim
    return ' cellspacing="'.st.cd."D('5')".et.'"'
endfunction

function! FileNoExt()
    return substitute(expand('%'), '\(.*\)\..*$', '\1','')
endfunction

function! Target()
    let st = g:snip_start_tag
    let et = g:snip_end_tag
    return ' target="'.st.et.'"'
endfunction

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet doctype <!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\"<CR><TAB>\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\"><CR>".st.et
exec "Snippet doc4s <!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"<CR>\"http://www.w3.org/TR/html4/strict.dtd\"><CR>".st.et
exec "Snippet doc4t <!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"<CR>\"http://www.w3.org/TR/html4/loose.dtd\"><CR>".st.et
exec "Snippet doc4f <!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\"<CR>\"http://www.w3.org/TR/html4/frameset.dtd\"><CR>".st.et
exec "Snippet docxs <!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML Strict//EN\"<CR>\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\"><CR>".st.et
exec "Snippet docxt <!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML Transitional//EN\"<CR>\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\"><CR>".st.et
exec "Snippet docxf <!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML Frameset//EN\"<CR>\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\"><CR>".st.et
exec "Snippet head <head><CR><TAB><meta http-equiv=\"Content-type\" content=\"text/html; charset=utf-8\" /><CR><TAB><title>".st.":substitute(expand('%'),'\\(.*\\)\\..*$','\\1','')".et."</title><CR><TAB>".st.et."<CR></head><CR>".st.et
exec "Snippet script <script type=\"text/javascript\" language=\"javascript\" charset=\"utf-8\"><CR>// <![CDATA[<CR><TAB>".st.et."<CR>// ]]><CR></script><CR>".st.et
exec "Snippet title <title>".st.":substitute(expand('%'),'\\(.*\\)\\..*$','\\1','')".et."</title>"
exec "Snippet body <body id=\"".st.":FileNoExt()".et."\" ".st.":Onload()".et."<CR>".st.et."<CR></body><CR>".st.et
exec "Snippet scriptsrc <script src=\"".st.et."\" type=\"text/javascript\" language=\"".st.":D('javascript')".et."\" charset=\"".st.":D('utf-8')".et."\"></script><CR>".st.et
exec "Snippet textarea <textarea name=\"".st.":D('Name')".et."\" rows=\"".st.":D('8')".et."\" cols=\"".st.":D'(40')".et."\">".st.et."</textarea><CR>".st.et
exec "Snippet meta <meta name=\"".st.":D('name')".et."\" content=\"".st.":D('content')".et."\" /><CR>".st.et
exec "Snippet movie <object width=\"".st.et."\" height=\"".st.et."\"<CR>classid=\"clsid:02BF25D5-8C17-4B23-BC80-D3488ABDDC6B\"<CR>codebase=\"http://www.apple.com/qtactivex/qtplugin.cab\"><CR><TAB><param name=\"src\"<CR>value=\"".st.et."\" /><CR><TAB><param name=\"controller\" value=\"".st.et."\" /><CR><param name=\"autoplay\" value=\"".st.et."\" /><CR><TAB><embed src=\"".st.":D('movie.mov')".et."\"<CR><TAB>width=\"".st.":D('320')".et."\" height=\"".st."D('240')".et."\"<CR><TAB>controller=\"".st.":D('true')".et."\" autoplay=\"".st.":D('true')".et."\"<CR><TAB><TAB>scale=\"tofit\" cache=\"true\"<CR><TAB><TAB>pluginspage=\"http://www.apple.com/quicktime/download/\"<CR><TAB>/><CR></object><CR>".st.et
exec "Snippet div <div".st.":Id()".et."><CR>".st.et."<CR></div><CR>".st.et
exec "Snippet mailto <a href=\"mailto:".st.et."?subject=".st.":D('feedback')".et."\">".st.":D('email me')".et."</a>".st.et
exec "Snippet table <table border=\"".st.":D('0')".et."\"".st.":Cellspacing()".et." cellpadding=\"".st.":D('5')".et."\"><CR><TAB><tr><th><:D('Header')".et."</th></tr><CR><TAB><tr><td>".st.et."</td></tr><CR></table>"
exec "Snippet link <link rel=\"".st.":D('stylesheet')".et."\" href=\"".st.":D('/css/master.css')".et."\" type=\"text/css\" media=\"".st.":D('screen')".et."\" title=\"".st.et."\" charset=\"".st.":D('utf-8')".et."\" />"
exec "Snippet form <form action=\"".st.":D(FileNoExt().'_submit')".et."\" method=\"".st.":D('get')".et."\"><CR><TAB>".st.et."<CR><CR><TAB><p><input type=\"submit\" value=\"Continue &rarr;\" /></p><CR></form><CR>".st.et
exec "Snippet ref <a href=\"".st.et."\">".st.et."</a>".st.et
exec "Snippet h1 <h1 id=\"".st.et."\">".st.et."</h1>".st.et
exec "Snippet input <input type=\"".st.":D('text/submit/hidden/button')".et."\" name=\"".st.et."\" value=\"".st.et."\" ".st.et."/>".st.et
exec "Snippet style <style type=\"text/css\" media=\"screen\"><CR>/* <![CDATA[ */<CR><TAB>".st.et."<CR>/* ]]> */<CR></style><CR>".st.et
exec "Snippet base <base href=\"".st.et."\"".st.":Target()".et." />".st.et
after/ftplugin/java_snippets.vim
52
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

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet method // {{{ ".st."method".et."<CR>/**<CR> * ".st.et."<CR> */<CR>public ".st."return".et." ".st."method".et."() {<CR>".st.et."}<CR>// }}}<CR>".st.et
exec "Snippet jps private static final ".st."string".et." ".st.et." = \"".st.et."\";<CR>".st.et
exec "Snippet jtc try {<CR>".st.et."<CR>} catch (".st.et." e) {<CR>".st.et."<CR>} finally {<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet jlog /** Logger for this class and subclasses. */<CR><CR>protected final Log log = LogFactory.getLog(getClass());<CR>".st.et
exec "Snippet jpv private ".st."string".et." ".st.et.";<CR><CR>".st.et
exec "Snippet bean // {{{ set".st."fieldName:UpFirst()".et."<CR>/**<CR> * Setter for ".st."fieldName".et.".<CR> * @param new".st."fieldName:UpFirst()".et." new value for ".st."fieldName".et."<CR> */<CR>public void set".st."fieldName:UpFirst()".et."(".st."String".et." new".st."fieldName:UpFirst()".et.") {<CR>".st."fieldName".et." = new".st."fieldName:UpFirst()".et.";<CR>}<CR>// }}}<CR><CR>// {{{ get".st."fieldName:UpFirst()".et."<CR>/**<CR> * Getter for ".st."fieldName".et.".<CR> * @return ".st."fieldName".et." */<CR>public ".st."String".et." get".st."fieldName:UpFirst()".et."() {<CR>return ".st."fieldName".et.";<CR>}<CR>// }}}<CR>".st.et
exec "Snippet jwh while (".st.et.") { // ".st.et."<CR><CR>".st.et."<CR><CR>}<CR>".st.et
exec "Snippet sout System.out.println(\"".st.et."\");".st.et
exec "Snippet jtest package ".st."j:JavaTestFileName(1)".et."<CR><CR>import junit.framework.TestCase;<CR>import ".st."j:JavaTestFileName(2)".et.";<CR><CR>/**<CR> * ".st."j:JavaTestFileName(3)".et."<CR> *<CR> * @author ".st.et."<CR> * @since ".st.et."<CR> */<CR>public class ".st."j:JavaTestFileName(3)".et." extends TestCase {<CR><CR>private ".st."j:JavaTestFileName(4)".et." ".st."j:JavaTestFileName(5)".et.";<CR><CR>public ".st."j:JavaTestFileName(4)".et." get".st."j:JavaTestFileName(4)".et."() { return this.".st."j:JavaTestFileName(5)".et."; }<CR>public void set".st."j:JavaTestFileName(4)".et."(".st."j:JavaTestFileName(4)".et." ".st."j:JavaTestFileName(5)".et.") { this.".st."j:JavaTestFileName(5)".et." = ".st."j:JavaTestFileName(5)".et."; }<CR><CR>public void test".st.et."() {<CR>".st.et."<CR>}<CR>}<CR>".st.et
exec "Snippet jif if (".st.et.") { // ".st.et."<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet jelse if (".st.et.") { // ".st.et."<CR><CR>".st.et."<CR><CR>} else { // ".st.et."<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet jpm /**<CR> * ".st.et."<CR> *<CR> * @param ".st.et." ".st.et."<CR> * ".st.":D('@return')".et." ".st.et."<CR> */<CR>private ".st."void".et." ".st.et."(".st."String".et." ".st.et.") {<CR><CR>".st.et."<CR><CR>}<CR>".st.et
exec "Snippet main public main static void main(String[] ars) {<CR>".st."\"System.exit(0)\"".et.";<CR>}<CR>".st.et
exec "Snippet jpum /**<CR> * ".st.et."<CR> *<CR> * @param ".st.et." ".st.et."<CR> *".st.":D('@return')".et." ".st.et."<CR> */<CR>public ".st."void".et." ".st.et."(".st."String".et." ".st.et.") {<CR><CR>".st.et."<CR><CR>}<CR>".st.et
exec "Snippet jcout <c:out value=\"${".st.et."}\" />".st.et
after/ftplugin/javascript_snippets.vim
10
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet proto ".st."className".et.".prototype.".st."methodName".et." = function(".st.et.")<CR>{<CR>".st.et."<CR>};<CR>".st.et
exec "Snippet fun function ".st."functionName".et." (".st.et.")<CR>{<CR>".st.et."<CR>}<CR>".st.et
after/ftplugin/latex_snippets.vim
13
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet sub \\subsection{".st."name".et."}\\label{sub:".st."name:substitute(@z,'.','\\l&','g')".et."}<CR>".st.et
exec "Snippet $$ \\[<CR>".st.et."<CR>\\]<CR>".st.et
exec "Snippet ssub \\subsubsection{".st."name".et."}\\label{ssub:".st."name:substitute(@z,'.','\\l&','g')".et."}<CR>".st.et
exec "Snippet itd \\item[".st."desc".et."] ".st.et
exec "Snippet sec \\section{".st."name".et."}\\label{sec:".st."name:substitute(@z,'.','\\l&','g')".et."}<CR>".st.et
after/ftplugin/logo_snippets.vim
9
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet to to ".st."name".et." ".st."argument".et."<CR>".st.et."<CR>end<CR>".st.et
after/ftplugin/markdown_snippets.vim
10
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet img ![".st."altText".et."](".st."SRC".et.")".st.et
exec "Snippet link [".st."desc".et."](".st."HREF".et.")".st.et
after/ftplugin/movable type_snippets.vim
14
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet cat <$MTCategoryDescription$>".st.et
exec "Snippet blog <$MTBlogName$>".st.et
exec "Snippet archive <$MTArchiveFile$>".st.et
exec "Snippet cal <MTCalendarIfEntries<CR>".st.et."<CR></MTCalendarIfEntries><CR>".st.et
exec "Snippet entry <$MTEntryMore$>".st.et
exec "Snippet entries <MTEntriesHeader><CR>".st.et."<CR></MTEntriesHeader><CR>".st.et
after/ftplugin/objc_snippets.vim
56
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
    let st = g:snip_start_tag
    let et = g:snip_end_tag

    if a:count == 0
        return st.et
    else
        return st.et.repeat(', '.st.et, a:count)
    endif
endfunction


let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet cat @interface ".st."NSObject".et." (".st."Category".et.")<CR><CR>@end<CR><CR><CR>@implementation ".st."NSObject".et." (".st."Category".et.")<CR><CR>".st.et."<CR><CR>@end<CR>".st.et
exec "Snippet delacc - (id)delegate;<CR><CR>- (void)setDelegate:(id)delegate;<CR>".st.et
exec "Snippet ibo IBOutlet ".st."NSSomeClass".et." *".st."someClass".et.";<CR>".st.et
exec "Snippet dict NSMutableDictionary *".st."dict".et." = [NSMutableDictionary dictionary];<CR>".st.et
exec "Snippet Imp #import <".st.et.".h><CR>".st.et
exec "Snippet objc @interface ".st."class".et." : ".st."NSObject".et."<CR>{<CR>}<CR>@end<CR><CR>@implementation ".st."class".et."<CR>- (id)init<CR>{<CR>self = [super init]; <CR>if (self != nil)<CR>{<CR>".st.et."<CR>}<CR>return self;<CR>}<CR>@end<CR>".st.et
exec "Snippet imp #import \"".st.et.".h\"<CR>".st.et
exec "Snippet bez NSBezierPath *".st."path".et." = [NSBezierPath bezierPath];<CR>".st.et
exec "Snippet acc - (".st."\"unsigned int\"".et.")".st."thing".et."<CR>{<CR>return ".st."fThing".et.";<CR>}<CR><CR>- (void)set".st."thing:UpFirst()".et.":(".st."\"unsigned int\"".et.")new".st."thing:UpFirst()".et."<CR>{<CR>".st."fThing".et." = new".st."thing:UpFirst()".et.";<CR>}<CR>".st.et
exec "Snippet format [NSString stringWithFormat:@\"".st.et."\", ".st.et."]".st.et
exec "Snippet focus [self lockFocus];<CR><CR>".st.et."<CR><CR>[self unlockFocus];<CR>".st.et
exec "Snippet setprefs [[NSUserDefaults standardUserDefaults] setObject:".st."object".et." forKey:".st."key".et."];<CR>".st.et
exec "Snippet log NSLog(@\"%s".st."s".et."\", ".st."s:ObjCArgList(Count(@z, '%[^%]'))".et.");".st.et
exec "Snippet gsave [NSGraphicsContext saveGraphicsState];<CR>".st.et."<CR>[NSGraphicsContext restoreGraphicsState];<CR>".st.et
exec "Snippet forarray for(unsigned int index = 0; index < [".st."array".et." count]; index += 1)<CR>{<CR>".st."id".et."object = [".st."array".et." objectAtIndex:index];<CR>".st.et."<CR>}".st.et
exec "Snippet classi @interface ".st."ClassName".et." : ".st."NSObject".et."<CR><CR>{".st.et."<CR><CR>}<CR><CR>".st.et."<CR><CR>@end<CR>".st.et
exec "Snippet array NSMutableArray *".st."array".et." = [NSMutableArray array];".st.et
exec "Snippet getprefs [[NSUserDefaults standardUserDefaults] objectForKey:<key>];".st.et
exec "Snippet cati @interface ".st."NSObject".et." (".st."Category".et.")<CR><CR>".st.et."<CR><CR>@end<CR>".st.et
after/ftplugin/ocaml_snippets.vim
26
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet Queue Queue.fold ".st.":D('(fun b v ->)')".et." ".st."base".et." ".st."q".et."<CR>".st.et
exec "Snippet Nativeint Nativeint.abs ".st."ni".et.st.et
exec "Snippet Printexc Printexc.print ".st."fn".et." ".st."x".et.st.et
exec "Snippet Sys Sys.Signal_ignore".st.et
exec "Snippet Hashtbl Hashtbl.iter ".st.":D('(fun k v -> )')".et." ".st."h".et.st.et
exec "Snippet Array Array.map ".st.":D('(fun a -> )')".et." ".st."arr".et.st.et
exec "Snippet Printf Printf.fprintf ".st."buf".et." \"".st."format".et."\" ".st."args".et.st.et
exec "Snippet Stream Stream.iter ".st.":D('(fun x -> )')".et." ".st."stream".et.st.et
exec "Snippet Buffer Buffer.add_channel ".st."buf".et." ".st."ic".et." ".st."len".et.st.et
exec "Snippet Int32 Int32.abs ".st."i32".et.st.et
exec "Snippet List List.rev_map ".st.":D('(fun x -> )')".et." ".st."lst".et.st.et
exec "Snippet Scanf Scanf.bscaf ".st."sbuf".et." \"".st."format".et."\" ".st."f".et.st.et
exec "Snippet Int64 Int64.abs ".st."i64".et.st.et
exec "Snippet Map Map.Make ".st.":D('(Ord : OrderedType)')".et.st.et
exec "Snippet String String.iter ".st.":D('(fun c -> )')".et." ".st."str".et.st.et
exec "Snippet Genlex Genlex.make_lexer ".st."\"tok_lst\"".et." ".st."\"char_stream\"".et.st.et
exec "Snippet for for ".st."i}".et." = ".st.et." to ".st.et." do<CR>".st.et."<CR>done<CR>".st.et
exec "Snippet Stack Stack.iter ".st.":D('(fun x -> )')".et." ".st."stk".et.st.et
after/ftplugin/perl_snippets.vim
23
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet sub sub ".st."FunctionName".et." {<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet class package ".st."ClassName".et.";<CR><CR>".st.":D('use base qw(')".et.st."ParentClass".et.st.et.";<CR><CR>}sub new {<CR>my \$class = shift;<CR>\$class = ref \$class if ref \$class;<CR>my $self = bless {}, \$class;<CR>\$self;<CR>}<CR><CR>1;<CR>".st.et
exec "Snippet xfore ".st."expression".et." foreach @".st."array".et.";".st.et
exec "Snippet xwhile ".st."expression".et." while ".st."condition".et.";".st.et
exec "Snippet xunless ".st."expression".et." unless ".st."condition".et.";".st.et
exec "Snippet slurp my $".st."var".et.";<CR><CR>{ local $/ = undef; local *FILE; open FILE, \"<".st."file".et."\"; $".st."var".et." = <FILE>; close FILE }".st.et
exec "Snippet if if (".st.et.") {<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet unless unless (".st.et.") {<CR>".st."2".et."<CR>}<CR>".st.et
exec "Snippet ifee if (".st.et.") {<CR>".st."2".et."<CR>} elsif (".st.et.") {<CR>".st.et."<CR>} else {<CR>".st.et."}<CR>}<CR>".st.et
exec "Snippet ife if (".st.et.") {<CR>".st.et."<CR>} else {<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet for for (my \$".st."var".et." = 0; \$".st."var".et." < ".st."expression".et."; \$".st."var".et."++) {<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet fore foreach my \$".st."var".et." (@".st."array".et.") {<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet eval eval {<CR>".st.et."<CR>};<CR>if ($@) {<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet while while (".st.et.") {<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet xif ".st."expression".et." if ".st."condition".et.";".st.et
after/ftplugin/php_snippets.vim
30
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet elseif elseif ( ".st."condition".et." )<CR>{<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet do do<CR>{<CR>".st.et."<CR><CR>} while ( ".st.":D('$a <= 10')".et." );<CR>".st.et
exec "Snippet reql require_once( '".st."file".et."' );<CR>".st.et
exec "Snippet if? $".st."retVal".et." = ( ".st."condition".et." ) ? ".st."a".et." : ".st."b".et." ;<CR>".st.et
exec "Snippet php <?php<CR><CR>".st.et."<CR><CR>?>"
exec "Snippet switch switch ( ".st."variable".et." )<CR>{<CR>case '".st."value".et."':<CR>".st.et."<CR>break;<CR><CR>".st.et."<CR><CR>default:<CR>".st.et."<CR>break;<CR>}<CR>".st.et
exec "Snippet class #doc<CR>#classname:".st."ClassName".et."<CR>#scope:".st."PUBLIC".et."<CR>#<CR>#/doc<CR><CR>class ".st."ClassName".et." ".st."extendsAnotherClass".et."<CR>{<CR>#internal variables<CR><CR>#Constructor<CR>function __construct ( ".st."argument".et.")<CR>{<CR>".st.et."<CR>}<CR>###<CR><CR>}<CR>###".st.et
exec "Snippet incll include_once( '".st."file".et."' );".st.et
exec "Snippet incl include( '".st."file".et."' );".st.et
exec "Snippet foreach foreach( $".st."variable".et." as $".st."key".et." => $".st."value".et." )<CR>{<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet ifelse if ( ".st."condition".et." )<CR>{<CR>".st.et."<CR>}<CR>else<CR>{<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet $_ $_REQUEST['".st."variable".et."']<CR>".st.et
exec "Snippet case case '".st."variable".et."':<CR>".st.et."<CR>break;<CR>".st.et
exec "Snippet print print \"".st."string".et."\"".st.et.";".st.et."<CR>".st.et
exec "Snippet function ".st."public".et."function ".st."FunctionName".et." (".st.et.")<CR>{<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet if if ( ".st."condition".et." )<CR>{<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet else else<CR>{<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet array $".st."arrayName".et." = array( '".st.et."',".st.et." );".st.et
exec "Snippet -globals $GLOBALS['".st."variable".et."']".st.et.st."something".et.st.et.";<CR>".st.et
exec "Snippet req require( '".st."file".et."' );<CR>".st.et
exec "Snippet for for ( $".st."i".et."=".st.et."; $".st."i".et." < ".st.et."; $".st."i".et."++ )<CR>{ <CR>".st.et."<CR>}<CR>".st.et
exec "Snippet while while ( ".st.et." )<CR>{<CR>".st.et."<CR>}<CR>".st.et
after/ftplugin/phpdoc_snippets.vim
19
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet doc_d /**<CR>* ".st."undocumentedConstant".et."<CR>**/<CR>define(".st.et.", ".st.et.");".st.et."<CR>".st.et
exec "Snippet doc_vp /**<CR>* ".st."undocumentedClassVariable".et."<CR>*<CR>* @var ".st."string".et.st.et."<CR>**/".st.et."<CR>"
exec "Snippet doc_f /**<CR>* ".st."undocumentedFunction".et."<CR>*<CR>* @return ".st."void".et."<CR>* @author ".st.et."<CR>**/<CR>".st.et."function ".st.et."(".st.et.")<CR>{".st.et."<CR>}<CR>".st.et
exec "Snippet doc_s /**<CR>* ".st."undocumentedFunction".et."<CR>*<CR>* @return ".st."void".et."<CR>* @author ".st.et."<CR>**/<CR>".st.et."function ".st.et."(".st.et.");<CR>".st.et
exec "Snippet doc_h /**<CR>* ".st.et."<CR>*<CR>* @author ".st.et."<CR>* @version $Id$<CR>* @copyright ".st.et.", ".st.et."<CR>* @package ".st."default".et."<CR>**/<CR><CR>/**<CR>* Define DocBlock<CR>**/<CR><CR>".st.et
exec "Snippet doc_fp /**<CR>* ".st."undocumentedFunction".et."<CR>*<CR>* @return ".st."void".et."<CR>* @author ".st.et."<CR>**/".st.et."<CR>"
exec "Snippet doc_i /**<CR>* ".st."undocumentedClass".et."<CR>*<CR>* @package ".st."default".et."<CR>* @author ".st.et."<CR>**/<CR>interface ".st.et."<CR>{".st.et."<CR>} // END interface ".st.et."<CR>".st.et
exec "Snippet doc_fp /**<CR>* ".st."undocumentedConstant".et.st.et."<CR>**/".st.et."<CR>".st.et
exec "Snippet doc_v /**<CR>* ".st."undocumentedClassVariable".et."<CR>*<CR>* @var ".st."string".et."<CR>**/<CR><var> $".st.et.";".st.et."<CR>".st.et
exec "Snippet doc_cp /**<CR>* ".st."undocumentedClass".et."<CR>*<CR>* @package ".st."default".et."<CR>* @author ".st.et."<CR>**/".st.et
exec "Snippet doc_c /**<CR>* ".st."undocumentedClass".et."<CR>*<CR>* @package ".st."default".et."<CR>* @author ".st.et."<CR>**/<CR>".st."class".et."class ".st."a".et."<CR>{".st.et."<CR>} // END ".st."class".et."class ".st."a".et."<CR>".st.et
after/ftplugin/propel_snippets.vim
14
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet <i <index name=\"".st."key".et."_index\"><CR><index-column name=\"".st."key".et."\" /><CR></index><CR>".st.et
exec "Snippet <t <table name=\"".st."name".et."\" ".st.et."><CR>".st.et."<CR></table><CR>".st.et
exec "Snippet <u <unique name=\"unique_".st."key".et."\"><CR><unique-column name=\"".st."key".et."\" /><CR></unique><CR>".st.et
exec "Snippet <c <column name=\"".st."name".et."\" type=\"".st."type".et."\" ".st.et." /><CR>".st.et
exec "Snippet <p <column name=\"".st."id".et."\" type=\"".st."integer".et."\" required=\"true\" primaryKey=\"true\" autoincrement=\"true\" /><CR>".st.et
exec "Snippet <f <foreign-key foreignTable=\"".st."table".et."\"><CR><reference local=\"".st."table".et."_id\" foreign=\"<id>\"/><CR></foreign-key><CR>".st.et
after/ftplugin/python_snippets.vim
85
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

function! PyRemDefVal(text)
    return substitute(a:text,'=.*','','g')
endfunction

function! PyInitVars(text)
    if a:text != "args"
        let text = substitute(a:text,'=.\{-},','','g')
        let text = substitute(text,'=.\{-}$','','g')
        let text = substitute(text,',','','g')
        let ret = ''
        let tabs = indent(".")/&tabstop
        let tab_text = ''
        for i in range(tabs)
            let tab_text = tab_text.'\t'
        endfor
        for Arg in split(text, ' ')
            let ret = ret.'self.'.Arg.' = '.Arg.'\n'.tab_text
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
    let st = g:snip_start_tag
    let et = g:snip_end_tag

    if a:count == 0
        return "()"
    else
        return '('.repeat(st.et.', ', a:count).')'
    endif
endfunction

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

" Note to users: The following method of defininf snippets is to allow for
" changes to the default tags.
" Feel free to define your own as so:
"    Snippet mysnip This is the expansion text.<{}>
" There is no need to use exec if you are happy to hardcode your own start and
" end tags

exec "Snippet pf print \"".st."s".et."\" % ".st."s:PyArgList(Count(@z, '%[^%]'))".et."<CR>".st.et
exec "Snippet get def get".st."name".et."(self): return self._".st."name".et."<CR>".st.et
exec "Snippet classi class ".st."ClassName".et." (".st."object".et."):<CR><CR><Tab>def __init__(self".st."args:PyInit(@z)".et."):<CR>".st."args:PyInitVars(@z)".et."<CR><CR>".st.et
exec "Snippet set def set".st."name".et."(self, ".st."newValue".et."):<CR>self._".st."name".et." = ".st."newValue:PyRemDefVal(@z)".et."<CR>".st.et
exec "Snippet . self.".st.et
exec "Snippet def def ".st."fname".et."(".st."self".et."):<CR><Tab>".st."pass".et."<CR>".st.et
" Contributed by Panos
exec "Snippet ifn if __name__ == '__main__':<CR><Tab>".st.et
" Contributed by Kib2
exec "Snippet bc \"\"\"".st."description".et."\"\"\"<CR>".st.et
exec "Snippet lc # ".st."linecomment".et."<CR>".st.et
exec "Snippet sbl1 #!/usr/bin/env python<CR># -*- coding: Latin-1 -*-<CR>".st.et
exec "Snippet kfor for ".st."variable".et." in ".st."ensemble".et.":<CR><Tab>".st."pass".et."<CR>".st.et
exec "Snippet cm ".st."class".et." = classmethod(".st."class".et.")<CR>".st.et
after/ftplugin/rails_snippets.vim
54
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet mrnt rename_table \"".st."oldTableName".et."\", \"".st."newTableName".et."\"".st.et
exec "Snippet rfu render :file => \"".st."filepath".et."\", :use_full_path => ".st."false".et.st.et
exec "Snippet rns render :nothing => ".st."true".et.", :status => ".st.":D('401')".et.st.et
exec "Snippet ri render :inline => \"".st.":D(\"<%= 'hello' %".et."\")>\"".st.et
exec "Snippet rt render :text => \"".st.et."\"".st.et
exec "Snippet mcc t.column \"".st."title".et."\", :".st."string".et.st.et
exec "Snippet rpl render :partial => \"".st."item".et."\", :locals => { :".st."name".et." => \"".st."value".et."\"".st.et." }".st.et
exec "Snippet rea redirect_to :action => \"".st."index".et."\"".st.et
exec "Snippet rtlt render :text => \"".st.et."\", :layout => ".st."true".et.st.et
exec "Snippet ft <%= form_tag :action => \"".st."update".et."\" %>".st.et
exec "Snippet forin <% for ".st."item".et." in ".st.":D('@items')".et." %><CR><%= ".st."item".et.".".st."name".et." %><CR><% end %><CR>".st.et
exec "Snippet lia <%= link_to \"".st.et."\", :action => \"".st."index".et."\" %>".st.et
exec "Snippet rl render :layout => \"".st."layoutname".et."\"".st.et
exec "Snippet ra render :action => \"".st."action".et."\"".st.et
exec "Snippet mrnc rename_column \"".st."table".et."\", \"".st."oldColumnName".et."\", \"".st."newColumnName".et."\"".st.et
exec "Snippet mac add_column \"".st."table".et."\", \"".st."column".et."\", :".st."string".et.st.et
exec "Snippet rpc render :partial => \"".st."item".et."\", :collection => ".st."items".et.st.et
exec "Snippet rec redirect_to :controller => \"".st."items".et."\"".st.et
exec "Snippet rn render :nothing => ".st."true".et.st.et
exec "Snippet lic <%= link_to \"".st.et."\", :controller => \"".st.et."\" %>".st.et
exec "Snippet rpo render :partial => \"".st."item".et."\", :object => ".st."object".et.st.et
exec "Snippet rts render :text => \"".st.et."\", :status => ".st.":D('401')".et.st.et
exec "Snippet rcea render_component :action => \"".st."index".et."\"".st.et
exec "Snippet recai redirect_to :controller => \"".st."items".et."\", :action => \"".st."show".et."\", :id => ".st.":D('@item')".et.st.et
exec "Snippet mcdt create_table \"".st."table".et."\" do |t|<CR>".st.et."<CR>end<CR>".st.et."<CR>".st.et
exec "Snippet ral render :action => \"".st."action".et."\", :layout => \"".st."layoutname".et."\"".st.et
exec "Snippet rit render :inline => \"".st.et."\", :type => ".st.":D(':rxml')".et.st.et
exec "Snippet rceca render_component :controller => \"".st."items".et."\", :action => \"".st."index".et."\"".st.et
exec "Snippet licai <%= link_to \"".st.et."\", :controller => \"".st."items".et."\", :action => \"".st."edit".et."\", :id => ".st.":D('@item')".et." %>".st.et
exec "Snippet verify verify :only => [:".st.et."], :method => :post, :render => {:status => 500, :text => \"use HTTP-POST\"}".st.et
exec "Snippet mdt drop_table \"".st."table".et."\"".st.et
exec "Snippet rp render :partial => \"".st."item".et."\"".st.et
exec "Snippet rcec render_component :controller => \"".st."items".et."\"".st.et
exec "Snippet mrc remove_column \"".st."table".et."\", \"".st."column".et."\"".st.et
exec "Snippet mct create_table \"".st."table".et."\" do |t|<CR>".st.et."<CR>end<CR>".st.et
exec "Snippet flash flash[:".st."notice".et."] = \"".st.et."\"".st.et
exec "Snippet rf render :file => \"".st."filepath".et."\"".st.et
exec "Snippet lica <%= link_to \"".st.et."\", :controller => \"".st."items".et."\", :action => \"".st."index".et."\" %>".st.et
exec "Snippet liai <%= link_to \"".st.et."\", :action => \"".st."edit".et."\", :id => ".st.":D('@item')".et." %>".st.et
exec "Snippet reai redirect_to :action => \"".st."show".et."\", :id => ".st.":D('@item')".et.st.et
exec "Snippet logi logger.info \"".st.et."\"".st.et
exec "Snippet marc add_column \"".st."table".et."\", \"".st."column".et."\", :".st."string".et."<CR><CR>".st.et."<CR>".st.et
exec "Snippet rps render :partial => \"".st."item".et."\", :status => ".st.":D('500')".et.st.et
exec "Snippet ril render :inline => \"".st.et."\", :locals => { ".st.":D(':name')".et." => \"".st."value".et."\"".st.et." }".st.et
exec "Snippet rtl render :text => \"".st.et."\", :layout => \"".st.et."\"".st.et
exec "Snippet reca redirect_to :controller => \"".st."items".et."\", :action => \"".st."list".et."\"".st.et
after/ftplugin/ruby_snippets.vim
32
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet do do<CR>".st.et."<CR>end<CR>".st.et
exec "Snippet class class ".st."className".et."<CR>".st.et."<CR>end<CR>".st.et
exec "Snippet begin begin<CR>".st.et."<CR>rescue ".st."Exception".et." => ".st."e".et."<CR>".st.et."end<CR>".st.et
exec "Snippet each_with_index0 each_with_index do |".st."element".et.", ".st."index".et."|<CR>".st."element".et.".".st.et."<CR>end<CR>".st.et
exec "Snippet collect collect { |".st."element".et."| ".st."element".et.".".st.et." }<CR>".st.et
exec "Snippet forin for ".st."element".et." in ".st."collection".et."<CR>".st."element".et.".".st.et."<CR>end<CR>".st.et
exec "Snippet doo do |".st."object".et."|<CR>".st.et."<CR>end<CR>".st.et
exec "Snippet : :".st."key".et." => \"".st."value".et."\"".st.et."<CR>".st.et
exec "Snippet def def ".st."methodName".et."<CR>".st.et."<CR>end<CR>".st.et
exec "Snippet case case ".st."object".et."<CR>when ".st."condition".et."<CR>".st.et."<CR>end<CR>".st.et
exec "Snippet collecto collect do |".st."element".et."|<CR>".st."element".et.".".st.et."<CR>end<CR>".st.et
exec "Snippet each each { |".st."element".et."| ".st."element".et.".".st.et." }<CR>".st.et
exec "Snippet each_with_index each_with_index { |".st."element".et.", ".st."idx".et."| ".st."element".et.".".st.et." }<CR>".st.et
exec "Snippet if if ".st."condition".et."<CR>".st.et."<CR>end<CR>".st.et
exec "Snippet eacho each do |".st."element".et."|<CR>".st."element".et.".".st.et."<CR>end<CR>".st.et
exec "Snippet unless unless ".st."condition".et."<CR>".st.et."<CR>end<CR>".st.et
exec "Snippet ife if ".st."condition".et."<CR>".st.et."<CR>else<CR>".st.et."<CR>end<CR>".st.et
exec "Snippet when when ".st."condition".et."<CR>".st.et
exec "Snippet selecto select do |".st."element".et."|<CR>".st."element".et.".".st.et."<CR>end<CR>".st.et
exec "Snippet injecto inject(".st."object".et.") do |".st."injection".et.", ".st."element".et."| <CR>".st.et."<CR>end<CR>".st.et
exec "Snippet reject { |".st."element".et."| ".st."element".et.".".st.et." }<CR>".st.et
exec "Snippet rejecto reject do |".st."element".et."| <CR>".st."element".et.".".st.et."<CR>end<CR>".st.et
exec "Snippet inject inject(".st."object".et.") { |".st."injection".et.", ".st."element".et."| ".st.et." }<CR>".st.et
exec "Snippet select select { |".st."element".et."| ".st."element".et.".".st.et." }<CR>".st.et
after/ftplugin/sh_snippets.vim
12
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

"Snippet !env #!/usr/bin/env ${1:${TM_SCOPE/(?:source|.*)\\.(\\w+).*/$1/}}
exec "Snippet if if [[ ".st."condition".et." ]]; then<CR>".st.et."<CR>fi".st.et
exec "Snippet elif elif [[ ".st."condition".et." ]]; then<CR>".st.et
exec "Snippet for for (( ".st."i".et." = ".st.et."; ".st."i".et." ".st.et."; ".st."i".et.st.et." )); do<CR>".st.et."<CR>done".st.et
after/ftplugin/slate_snippets.vim
19
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet do do: [| :".st."each".et."| ".st.et."]<CR>".st.et
exec "Snippet proto define: #".st."NewName".et." &parents: {".st."parents".et."} &slots: {".st."slotSpecs".et."}.<CR>".st.et
exec "Snippet ifte ".st."condition".et." ifTrue: [".st.et.":then] ifFalse: [".st.et.":else]<CR>".st.et
exec "Snippet collect collect: [| :".st."each".et."| ".st.et."]<CR>".st.et
exec "Snippet if ".st."condition".et." ifTrue: [".st.et.":then]".st.et
exec "Snippet until [".st."condition".et."] whileFalse: [".st.et.":body]".st.et
exec "Snippet reject reject: [| :".st."each".et."| ".st.et."]<CR>".st.et
exec "Snippet dowith doWithIndex: [| :".st."each".et." :".st."index".et." | ".st.et."]<CR>".st.et
exec "Snippet select select: [| :".st."each".et."| ".st.et."]".st.et
exec "Snippet while [".st."condition".et."] whileTrue: [".st.et.":body]".st.et
exec "Snippet inject inject: ".st."object".et." [| :".st."injection".et.", :".st."each".et."| ".st.et."]".st.et
after/ftplugin/smarty_snippets.vim
35
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet {cycle {cycle values=\"#SELSTART#".st."foo".et.",".st."bar".et."#SELEND#\" name=\"default\" print=true advance=true delimiter=\",\" assign=varname }<CR>".st.et
exec "Snippet |regex_replace |regex_replace:\"".st."regex".et."\":\"".st.et."\"".st.et
exec "Snippet {counter {counter name=\"#INSERTION#\" start=1 skip=1 direction=\"up\" print=true<CR>assign=\"foo\" }<CR><CR>{counter}<CR>".st.et
exec "Snippet {eval {eval var=\"#SELSTART#{template_format}#SELEND#\" assign=varname} <CR>".st.et
"Snippet |date_format |date_format:"${1:strftime() formatting}" <CR><{}>
exec "Snippet |truncate |truncate:".st.":D('80')".et.":".st.et.":".st."false".et.""
exec "Snippet {if {if ".st."varname".et.st.et."<CR>\"".st."foo".et."\"}<CR><CR>{* $varname can also be a php call *}<CR><CR>".st.et."<CR><CR>{/if}<CR>".st.et
"Snippet |string_format |string_format:"${1:sprintf formatting}" <CR><{}>
exec "Snippet {assign {assign var=".st.et." value=\"".st.et."\"}".st.et
exec "Snippet {foreach {foreach from=".st."varname".et." item=i [key=k name=\"\"] }<CR><CR>".st.et."<CR><CR>{/foreach}<CR><CR>".st.et
exec "Snippet {capture {capture name=#INSERTION#}<CR><CR>#SELECT#<CR><CR>{/capture}<CR>".st.et
exec "Snippet |wordwrap |wordwrap:".st.":D('80')".et.":\"".st.et."\":".st.et
exec "Snippet |spacify |spacify:\"".st.et."\"".st.et." "
exec "Snippet |default |default:\"".st.et."\"".st.et
exec "Snippet {debug {debug output=\"#SELSTART#".st.et."#SELEND#\" }".st.et
exec "Snippet |replace |replace:\"".st."needle".et."\":\"".st.et."\"".st.et
exec "Snippet {include {include file=\"".st.et."\" [assign=varname foo=\"bar\"] }".st.et
exec "Snippet |escape |escape:\"".st.et."\"".st.et
exec "Snippet {strip {strip}<CR>".st.et."<CR>{/strip}".st.et
exec "Snippet {math {math equation=\"".st.et."\" assign=".st.et." ".st.et."}".st.et
exec "Snippet {config_load {config_load file=\"#INSERTION#\" [section=\"\" scope=\"local|parent|global\"] }".st.et
exec "Snippet |cat  |cat:\"".st.et."\"".st.et
exec "Snippet {insert {insert name=\"insert_".st.et."\" [assign=varname script=\"foo.php\" foo=\"bar\"] }".st.et
exec "Snippet {fetch {fetch file=\"#SELSTART#http:// or file#SELEND#\" assign=varname}".st.et
exec "Snippet {literal {literal}<CR><CR>".st.et."<CR><CR>{/literal}".st.et
exec "Snippet {include_php {include_php file=\"".st.et."\" [once=true]}".st.et
exec "Snippet |strip |strip:[\"".st.et."\"]".st.et
after/ftplugin/symfony_snippets.vim
21
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet image_tag image_tag('".st."imageName".et."'".st.et.")".st.et
exec "Snippet get public function get".st.et." ()<CR>{<CR>return $this->".st.et.";<CR>}<CR><CR>".st.et
exec "Snippet link_to link_to('".st."linkName".et."', '".st."moduleName".et."/".st."actionName".et.st.et."')".st.et
exec "Snippet sexecute public function execute<Action>()<CR>{<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet set public function set".st.et." ($".st.et.")<CR>{<CR>$this->".st.et." = ".st.et.";<CR>}<CR><CR>".st.et
exec "Snippet execute /**<CR>* ".st."className".et."<CR>*<CR>*/<CR>public function execute<Action>()<CR>{<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet tforeach <?php foreach ($".st."variable".et." as $".st."key".et.st.et."): ?><CR>".st.et."<CR><?php endforeach ?><CR>".st.et
exec "Snippet getparam $this->getRequestParameter('".st."id".et."')".st.et
exec "Snippet div <div".st.et."><CR>".st.et."<CR></div>".st.et
exec "Snippet tif <?php if (".st."condition".et."): ?><CR>".st.et."<CR><?php endif ?><CR>".st.et
exec "Snippet setget public function set".st."var".et." (".st."arg".et.")<CR>{<CR>$this->".st."arg".et." = ".st."arg".et.";<CR>}<CR><CR>public function get".st."var".et." ()<CR>{<CR>return $this->".st."var".et.";<CR>}<CR><CR>".st.et
exec "Snippet echo <?php echo ".st.et." ?>".st.et
exec "Snippet tfor <?php for($".st."i".et." = ".st.et."; $".st."i".et." <= ".st.et."; $".st."i".et."++): ?><CR>".st.et."<CR><?php endfor ?><CR>".st.et
after/ftplugin/tcl_snippets.vim
14
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet switch switch".st.et." -- $".st."var".et." {<CR>".st."match".et." {<CR>".st.et."<CR>}<CR>default<CR>{".st.et."}<CR>}<CR>".st.et
exec "Snippet foreach foreach ".st."var".et." $".st."list".et." {<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet proc proc ".st."name".et." {".st."args".et."} <CR>{<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet if if {".st."condition".et."} {<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet for for {".st."i".et." {".st.et."} {".st.et."} {<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet while while {".st."condition".et."} {<CR>".st.et."<CR>}<CR>".st.et
after/ftplugin/template_toolkit_snippets.vim
13
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet wrap [% WRAPPER ".st."template".et." %]<CR>".st.et."<CR>[% END %]<CR>".st.et
exec "Snippet if [% IF ".st."condition".et." %]<CR>".st.et."<CR>[% ELSE %]<CR>".st.et."<CR>[% END %]<CR>".st.et
exec "Snippet unl [% UNLESS ".st."condition".et." %]<CR>".st.et."<CR>[% END %]<CR>".st.et
exec "Snippet inc [% INCLUDE ".st."template".et." %]<CR>".st.et
exec "Snippet for  [% FOR ".st."var".et." IN ".st."set".et." %]<CR>".st.et."<CR>[% END %]".st.et
after/ftplugin/tex_snippets.vim
13
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet sub \\subsection{".st."name".et."}\\label{sub:".st."name:substitute(@z,'.','\\l&','g')".et."}<CR>".st.et
exec "Snippet $$ \\[<CR>".st.et."<CR>\\]<CR>".st.et
exec "Snippet ssub \\subsubsection{".st."name".et."}\\label{ssub:".st."name:substitute(@z,'.','\\l&','g')".et."}<CR>".st.et
exec "Snippet itd \\item[".st."desc".et."] ".st.et
exec "Snippet sec \\section{".st."name".et."}\\label{sec:".st."name:substitute(@z,'.','\\l&','g')".et."<CR>".st.et
after/ftplugin/xhtml_snippets.vim
48
if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet doctype <!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"<CR>\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\"><CR>".st.et
exec "Snippet aref <a href=\"".st.et."\" id=\"".st.et."\" title=\"".st.et."\">".st.et."</a>".st.et
exec "Snippet head  <head><CR>".st.et."<CR></head>".st.et
exec "Snippet script <script type=\"text/javascript\" language=\"<javascript>\" charset=\"".st.":D('utf-8')".et."\"><CR>// <![CDATA[<CR>".st.et."<CR>// ]]><CR></script>".st.et
exec "Snippet html <html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"".st."en".et."\"<CR>lang=\"".st."en".et."\"><CR>".st.et."<CR></html>"
exec "Snippet h3 <h3>".st.et."</h3>".st.et
exec "Snippet h4 <h4>".st.et."</h4>".st.et
exec "Snippet h5 <h5>".st.et."</h5>".st.et
exec "Snippet h6 <h6>".st.et."</h6>".st.et
exec "Snippet fieldset <fieldset><CR>".st.et."<CR></fieldset>".st.et
exec "Snippet noscript <noscript><CR>".st.et."<CR></noscript>".st.et
exec "Snippet ul <ul ".st.et."><CR>".st.et."<CR></ul>".st.et
exec "Snippet xml <?xml version=\"1.0\" encoding=\"iso-8859-1\"?><CR><CR>".st.et
exec "Snippet body <body id=\"".st.et."\" ".st.et."><CR>".st.et."<CR></body>".st.et
exec "Snippet legend <legend align=\"".st.et."\" accesskey=\"".st.et."\"><CR>".st.et."<CR></legend>".st.et
exec "Snippet title <title>".st."PageTitle".et."</title>".st.et
exec "Snippet scriptsrc <script src=\"".st.et."\" type=\"text/javascript\" language=\"<javascript>\" charset=\"".st.":D('utf-8')".et."\"></script>".st.et
exec "Snippet img <img src=\"".st.et."\" alt=\"".st.et."\" class=\"".st.et."\" />".st.et
exec "Snippet option <option label=\"".st."label".et."\" value=\"".st."value".et."\" ".st.et."></option> ".st.et
exec "Snippet optgroup <optgroup label=\"".st."Label".et."\"><CR>".st.et."<CR></optgroup>".st.et
exec "Snippet meta <meta name=\"".st."name".et."\" content=\"".st."content".et."\" />".st.et
exec "Snippet td <td".st.et.">".st.et."</td>".st.et
exec "Snippet dt <dt>".st.et."<CR></dt><CR><dd></dd>".st.et
exec "Snippet tfoot <tfoot><CR>".st.et."<CR></tfoot>".st.et
exec "Snippet div <!-- begin div.<id> --><CR><div id=\"".st."id".et."\"><CR>".st.et."<CR></div><CR><!-- end div.".st."id".et." --><CR>".st.et
exec "Snippet ol <ol ".st.et."><CR>".st.et."<CR></ol>".st.et
exec "Snippet txtarea <textarea id=\"".st."ID".et."\" name=\"".st."Name".et."\" rows=\"".st.et."\" cols=\"".st.et."\" tabindex=\"".st.et."\" ".st.et.">".st.et."</textarea>".st.et
exec "Snippet mailto <a href=\"mailto:".st.et."?subject=".st.et."\">".st.et."</a>".st.et
exec "Snippet table <table summary=\"".st."Summary".et."\" class=\"".st."className".et."\" width=\"".st.":D('100%')".et."\" cellspacing=\"".st.et."\" cellpadding=\"".st.et."\" border=\"".st.et."\"><CR>".st.et."<CR></table>".st.et
exec "Snippet hint <span class=\"hint\">".st.et."</span>".st.et
exec "Snippet link <link rel=\"".st."stylesheet".et."\" href=\"".st.et."\" type=\"text/css\" media=\"".st."screen".et."\" title=\"".st.et."\" charset=\"".st.":D('utf-8')".et."\" />".st.et
exec "Snippet form <form action=\"".st."urlToGoTo".et."\" method=\"".st."get".et."\" id=\"".st."formID".et."\" name=\"".st."formName".et."\"><CR>".st.et."<CR></form>".st.et
exec "Snippet tr <tr ".st.et."><CR>".st.et."<CR></tr>".st.et
exec "Snippet label <label for=\"".st."inputItem".et."\">".st.et."</label>".st.et
exec "Snippet image <img src=\"".st.et."\" alt=\"".st.et."\" width=\"".st.et."\" height=\"".st.et."\" ".st.et."/>"
exec "Snippet input <input name=\"".st.et."\" id=\"".st.et."\" type=\"radio\" value=\"".st."defaultValue".et."\" tabindex=\"".st.et."\" ".st.et." />".st.et
exec "Snippet select <select id=\"".st."ID".et."\" name=\"".st."Name".et."\" size=\"".st.et."\" tabindex=\"".st.et."\" ".st.et."><CR>".st.et."<CR></select><CR>".st.et
exec "Snippet style <style type=\"text/css\" media=\"".st."screen".et."\"><CR>/* <![CDATA[ */<CR>".st.et."<CR>/* ]]> */<CR></style><CR>".st.et
exec "Snippet div.header <!-- Begin HeaderDiv:: --><CR><div id=\"HeaderDiv\"><CR><!--logo in background --><CR><h1><CompanyName></h1><CR></div><CR><!-- End HeaderDiv:: --><CR>".st.et
exec "Snippet base <base href=\"".st.et."\" ".st.et."/>".st.et
plugin/snippetsEmu.vim
843
"        File: snippetsEmu.vim
"      Author: Felix Ingram
"              ( f.ingram.lists <AT> gmail.com )
" Description: An attempt to implement TextMate style Snippets. Features include
"              automatic cursor placement and command execution.
" $LastChangedDate: 2006-12-28 17:19:16 +0000 (Thu, 28 Dec 2006) $
" Version:     1.0
" $Revision: 97 $
"
" This file contains some simple functions that attempt to emulate some of the 
" behaviour of 'Snippets' from the OS X editor TextMate, in particular the
" variable bouncing and replacement behaviour.
"
" {{{ USAGE:
"
" Place the file in your plugin directory.
" Define snippets using the Snippet command.
" Snippets are best defined in the 'after' subdirectory of your Vim home
" directory ('~/.vim/after' on Unix). Filetype specific snippets can be defined
" in '~/.vim/after/ftplugin/<filetype>_snippets.vim. Using the <buffer> argument will
" By default snippets are buffer specific. To define general snippets available
" globally use the 'Iabbr' command.
"
" Example One:
" Snippet fori for �datum� in �data�:<CR>�datum�.��
"
" The above will expand to the following (indenting may differ):
" 
" for �datum� in �data�:
"   �datum�.��
" 
" The cursor will be placed after the first '�' in insert mode.
" Pressing <Tab> will 'tab' to the next place marker (�data�) in
" insert mode.  Adding text between � and � and then hitting �Tab� will
" remove the angle brackets and replace all markers with a similar identifier.
"
" Example Two:
" With the cursor at the pipe, hitting <Tab> will replace:
" for �MyVariableName|datum� in �data�:
"   �datum�.��
"
" with (the pipe shows the cursor placement):
"
" for MyVariableName in �data�:
"   MyVariableName.��
" 
" Enjoy.
"
" For more information please see the documentation accompanying this plugin.
"
" Additional Features:
"
" Commands in tags. Anything after a ':' in a tag will be run with Vim's
" 'execute' command. The value entered by the user (or the tag name if no change
" has been made) is passed in the @z register (the original contents of the
" register are restored once the command has been run).
"
" Named Tags. Naming a tag (the �datum� tag in the example above) and changing
" the value will cause all other tags with the same name to be changed to the
" same value (as illustrated in the above example). Not changing the value and
" hitting <Tab> will cause the tag's name to be used as the default value.
"
" Test tags for pattern matching:
" The following are examples of valid and invalid tags. Whitespace can only be
" used in a tag name if the name is enclosed in quotes.
"
" Valid tags
" ��
" �tagName�
" �tagName:command�
" �"Tag Name"�
" �"Tag Name":command�
"
" Invalid tags, random text
" �:�
" �:command�
" �Tag Name�
" �Tag Name:command�
" �"Tag Name":�
" �Tag �
" �OpenTag
"
" Here's our magic search term (assumes '�',':' and '�' as our tag delimiters:
" �\([^[:punct:] \t]\{-}\|".\{-}"\)\(:[^�]\{-1,}\)\?�
" }}}
" }}}

if v:version < 700
  echomsg "snippetsEmu plugin requires Vim version 7 or later"
  finish
endif

echom globpath(&rtp, 'snippetsEmu.vim')
if globpath(&rtp, 'plugin/snippetEmu.vim') != ""
  call confirm("It looks like you've got an old version of snippetsEmu installed. Please delete the file 'snippetEmu.vim'. Note lack of 's'")
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
    let g:snip_start_tag = "�"
endif

if !exists("g:snip_end_tag")
    let g:snip_end_tag = "�"
endif

if !exists("g:snip_elem_delim")
    let g:snip_elem_delim = ":"
endif

call <SID>Debug("Set variables")

let s:just_expanded = 0

" }}}
" {{{ Set up menu
for def_file in split(globpath(&rtp, "**/*_snippets.vim"), '\n')
  call <SID>Debug("Adding ".def_file." definitions to menu")
  let snip = substitute(def_file, '.*[\\/]\(.*\)_snippets.vim', '\1', '')
  exec "nmenu <silent> S&nippets.".snip." :source ".def_file."<CR>"
"  exec "amenu <silent> S&nippets.".snip." :set ft=".&ft.".".snip."<CR>"
endfor
" }}}
" {{{ Map Jumper to the default key if not set already
function! <SID>GetSuperTabSNR()
  let a_sav = @a
  redir @a
  exec "silent function"
  redir END
  let funclist = @a
  let @a = a_sav
  let func = split(split(matchstr(funclist,'.SNR.\{-}SuperTab(command)'),'\n')[-1])[1]
  return matchlist(func, '\(.*\)S')[1]
endfunction

if ( !hasmapto( '<Plug>Jumper', 'i' ) )
  if globpath(&rtp, 'plugin/supertab.vim') != ""
    call <SID>Debug("SuperTab installed")
    let s:supInstalled = 1
    let s:done_remap = 0
    imap <Tab> <Plug>Jumper
  else
    let s:supInstalled = 0
    imap <unique> <Tab> <Plug>Jumper
  endif
endif
if ( !hasmapto( 'i<BS><Tab>', 's' ) )
  smap <unique> <Tab> i<BS><Tab>
endif
imap <silent> <script> <Plug>Jumper <C-R>=<SID>Jumper()<CR>

call <SID>Debug("Mapped keys")

" }}}
" {{{ SetLocalTagVars()
function! <SID>SetLocalTagVars()
  if exists("b:snip_end_tag") && exists("b:snip_start_tag") && exists("b:snip_elem_delim")
    return [b:snip_start_tag, b:snip_elem_delim, b:snip_end_tag]
  else
    return [g:snip_start_tag, g:snip_elem_delim, g:snip_end_tag]
  endif
endfunction
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
  let [snip_start_tag, snip_elem_delim, snip_end_tag] = <SID>SetLocalTagVars()
  let b:search_str = snip_start_tag.'\([^'.
        \snip_start_tag.snip_end_tag.
        \'[:punct:] \t]\{-}\|".\{-}"\)\('.
        \snip_elem_delim.
        \'[^'.snip_end_tag.snip_start_tag.']\{-1,}\)\?'.snip_end_tag
  let b:search_commandVal = "[^".snip_elem_delim."]*"
  let b:search_endVal = "[^".snip_end_tag."]*"
endfunction
" }}}
" {{{ SetKeywords(text) - Add characters to the current buffer's iskeywords
" Refactored out here as it's used twice in SetCom
function! <SID>SetKeywords(text)
  if <SID>CheckForBufferTags()
    let snip_start_tag = b:snip_start_tag
  else
    let snip_start_tag = g:snip_start_tag
  endif
  let tokens = split(a:text, ' ')
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
    if stridx(snip_start_tag, char) != -1
      echom "One of the snippet definitions contains a character in your snip_start_tag; this could cause problems"
    endif
  endfor
  return [<SID>Hash(lhs), rhs]
endfunction
" }}}
" {{{ SetCom(text) - Set command function
function! <SID>SetCom(text)
  let text = substitute(a:text, '<CR>\|<Esc>\|<Tab>\|<BS>\|<Space>\|<C-r>\|<Pipe>\|\"\|\\','\\&',"g")

  if s:supInstalled == 1 && s:done_remap == 0
    let s:SupSNR = <SID>GetSuperTabSNR()
    imap <Tab> <Plug>Jumper
    let s:done_remap = 1
  endif

  let text = substitute(text, "$", "","")
  if match(text,"<buffer>") == 0
    let text = substitute(text, '\s*<buffer>\s*', "","")
    let [lhs, rhs] = <SID>SetKeywords(text)
    return "let b:snip_".lhs.' = "'.rhs.'"'
  else
    let text = substitute(text, '^\s*', "", "")
    let [lhs, rhs] = <SID>SetKeywords(text)
    return "let g:snip_".lhs.' = "'.rhs.'"'
  endif
endfunction
" }}}
" {{{ Check for end - Check whether the cursor is at the end of the current line
function! <SID>CheckForEnd()
  " Check to see whether we're at the end of a line so we can decide on
  " how to start inserting
  let [snip_start_tag, snip_elem_delim, snip_end_tag] = <SID>SetLocalTagVars()
  if col(".") == <SID>StrLen(getline("."))
    return 1
  elseif getline(".") =~ '^$'
    return 1
  elseif (getline(".")[col(".")] == snip_elem_delim) &&
      \(getline(".")[col(".") + 1] == snip_end_tag) &&
      \(col(".") + 2 == <SID>StrLen(getline(".")))
      return 1
  else
    return 0 
  endif
endfunction
" }}}
" {{{ DeleteEmptyTag 
function! <SID>DeleteEmptyTag()
  let [snip_start_tag, snip_elem_delim, snip_end_tag] = <SID>SetLocalTagVars()
  for i in range(<SID>StrLen(snip_start_tag) + <SID>StrLen(snip_end_tag))
    normal x
  endfor
endfunction
" }}}
" {{{ SetUpTags()
function! <SID>SetUpTags()
  call <SID>Debug("---- Start of SetUpTags() ----")
  let [snip_start_tag, snip_elem_delim, snip_end_tag] = <SID>SetLocalTagVars()
  if (strpart(getline("."), col(".")+strlen(snip_start_tag)-1, strlen(snip_end_tag)) == snip_end_tag)
    call <SID>Debug("Found an empty tag")
    let b:tag_name = ""
    if col(".") + <SID>StrLen(snip_start_tag.snip_end_tag) == <SID>StrLen(getline("."))
      call <SID>DeleteEmptyTag()
      if col(".") == <SID>StrLen(getline("."))
        return "\<Esc>a"
      else
        return ''
      endif
    else
      call <SID>DeleteEmptyTag()
      if col(".") == <SID>StrLen(getline("."))
        return "\<Esc>A"
      else
        return ''
      endif
    endif
  else
    " Not on an empty tag so it must be a normal tag
    let b:tag_name = <SID>ChopTags(matchstr(getline("."),b:search_str,col(".")-1))
    call <SID>Debug("On a tag called: ".b:tag_name)
    let start_skip = ""
    if <SID>DetectWinMode()
      let end_skip = ""
    else
      let end_skip = "h"
    endif
"    let start_skip = string(<SID>StrLen(snip_start_tag))."l"
    for i in range(<SID>StrLen(snip_start_tag)+1)
      let start_skip = start_skip."l"
    endfor
    call <SID>Debug("Start skip is: ".start_skip)
    call <SID>Debug("Col() is: ".col("."))
    if col(".") <= <SID>StrLen(snip_start_tag)
      call <SID>Debug("We're at the start of the line so don't need to skip the first char of start tag")
"      let start_skip = strpart(start_skip, <SID>StrLen(start_skip)-1)
      let start_skip = strpart(start_skip, 0, strlen(start_skip)-1)
      call <SID>Debug("Start skip is now: ".start_skip)
    endif
"    for i in range(strlen(snip_end_tag)-1)
"      let end_skip = end_skip."h"
"    endfor
    call <SID>Debug("---- End of SetUpTags() ----")
    return "\<Esc>".start_skip."v/".snip_end_tag."\<CR>".end_skip."\<C-g>"
  endif
endfunction
" }}}
" {{{ NextHop() - Jump to the next tag if one is available
function! <SID>NextHop()
  call <SID>Debug("---------------- Start of NextHop ----------------")
  let [snip_start_tag, snip_elem_delim, snip_end_tag] = <SID>SetLocalTagVars()
  if s:just_expanded == 1
    call cursor(s:curLine, 1)
    let s:just_expanded = 0
  else
    call cursor(s:curLine, s:curCurs-1)
  endif
  " Check to see whether we're sitting on a tag and if not then perform a
  " search
  call <SID>Debug("Col() is: ".col("."))
  call <SID>Debug("Position of next match = ".match(getline("."), b:search_str))
  " If the first match is after the current cursor position or not on this
  " line...
  if match(getline("."), b:search_str) >= col(".") || match(getline("."), b:search_str) == -1
    " Perform a search to jump to the next tag
    call <SID>Debug("Seaching for a tag")
    if search(b:search_str) != 0
      return <SID>SetUpTags()
    else
      " there are no more matches
      call <SID>Debug("No more tags in the buffer")
    endif
  else
    " The match on the current line is on or before the cursor, so we need to
    " move the cursor back
    call <SID>Debug("Moving the cursor back")
    call <SID>Debug("Col is: ".col("."))
    call <SID>Debug("Match is: ".match(getline("."), b:search_str))
    while col(".") > match(getline("."), b:search_str) + 1
      normal h
    endwhile
    call <SID>Debug("Col is: ".col("."))
    " Now we just set up the tag as usual
    return <SID>SetUpTags()
  endif
  return ''
endfunction
" }}}
" {{{ RunCommand() - Execute commands stored in tags
function! <SID>RunCommand(command, z)
  " Escape backslashes for the matching.  Not sure what other escaping is
  " needed here
  let [snip_start_tag, snip_elem_delim, snip_end_tag] = <SID>SetLocalTagVars()
  call <SID>Debug("RunCommand was passed this command: ".a:command." and this value: ".a:z)
  let command = a:command
  if command == ''
    return a:z
  endif
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
"  call setline(line("."),substitute(getline("."),snip_start_tag.s:replaceVal.s:matchVal.snip_elem_delim.s:snip_temp.snip_end_tag, @z, "g"))
endfunction
" }}}
" {{{ MakeChanges() - Search the document making all the changes required
" This function has been factored out to allow the addition of commands in tags
function! <SID>MakeChanges()
  " Make all the changes
  " Change all the tags with the same name and no commands defined
  call <SID>Debug("---------------- Start of MakeChanges() ----------------")
  let [snip_start_tag, snip_elem_delim, snip_end_tag] = <SID>SetLocalTagVars()

  if b:tag_name == ""
    call <SID>Debug("Nothing to do: tag_name is empty")
    call <SID>Debug("---------------- End of MakeChanges() ----------------")
    return
  endif
  let matchVal = snip_start_tag.b:tag_name.snip_end_tag
  let tagmatch = '\V'.matchVal
  call <SID>Debug("Matching on this value: ".matchVal)
  call <SID>Debug("Replacing with this value: ".s:replaceVal)
  try
    call <SID>Debug("Running these commands: ".join(b:command_dict[b:tag_name], "', '"))
  catch /E175/
    call <SID>Debug("Could not find this key in the dict: ".b:tag_name)
  endtry
  let ind = 0
  while search(matchVal,"w") > 0
    try
      let commandResult = <SID>RunCommand(b:command_dict[b:tag_name][0], s:replaceVal)
    catch /E175/
      call <SID>Debug("Could not find this key in the dict: ".b:tag_name)
    endtry
    call <SID>Debug("Got this result: ".commandResult)
    let lines = split(substitute(getline("."), tagmatch, commandResult, "g"),'\n')
    if len(lines) > 1
      call setline(".", lines[0])
      call append(".", lines[1:])
    else
      call setline(".", lines)
    endif
"    let ind = ind + 1
    try
      unlet b:command_dict[b:tag_name][0]
    catch /E175/
      call <SID>Debug("Could not find this key in the dict: ".b:tag_name)
    endtry
  endwhile
  call <SID>Debug("---------------- End of MakeChanges() ----------------")
endfunction

" }}}
" {{{ ChangeVals() - Set up values for MakeChanges()
function! <SID>ChangeVals(changed)
  call <SID>Debug("---------------- Start of ChangeVals() ----------------")
  if a:changed == 1
    let s:CHANGED_VAL = 1
  else
    let s:CHANGED_VAL = 0
  endif
  call <SID>Debug("CHANGED_VAL: ".s:CHANGED_VAL)
  let [snip_start_tag, snip_elem_delim, snip_end_tag] = <SID>SetLocalTagVars()
  call <SID>Debug("b:tag_name: ".b:tag_name)
  let elem_match = match(s:line, snip_elem_delim, s:curCurs)
  let tagstart = strridx(getline("."), snip_start_tag,s:curCurs)+strlen(snip_start_tag)
  call <SID>Debug("About to access b:command_dict")
  try
    let commandToRun = b:command_dict[b:tag_name][0]
    call <SID>Debug("Accessed command_dicT")
    call <SID>Debug("Running this command: ".commandToRun)
    unlet b:command_dict[b:tag_name][0]
    call <SID>Debug("Command list is now: ".join(b:command_dict[b:tag_name], "', '"))
  catch /E175/
    call <SID>Debug("Could not find this key in the dict: ".b:tag_name)
  endtry
  let commandMatch = substitute(commandToRun, '\', '\\\\', "g")
  if s:CHANGED_VAL
    " The value has changed so we need to grab our current position back
    " to the start of the tag
    let replaceVal = strpart(getline("."), tagstart,s:curCurs-tagstart)
    call <SID>Debug("User entered this value: ".replaceVal)
    let tagmatch = replaceVal
    call <SID>Debug("Col is: ".col("."))
    exec "normal ".<SID>StrLen(tagmatch)."h"
    call <SID>Debug("Col is: ".col("."))
  else
    " The value hasn't changed so it's just the tag name
    " without any quotes that are around it
    call <SID>Debug("Tag name is: ".b:tag_name)
    let replaceVal = substitute(b:tag_name, '^"\(.*\)"$', '\1', '')
"    let replaceVal = substitute(b:tag_name, '^\"\(.*\)\"$', "\1", "")
    call <SID>Debug("User did not enter a value. Replacing with this value: ".replaceVal)
    let tagmatch = ''
    call <SID>Debug("Col is: ".col("."))
  endif
  let tagmatch = snip_start_tag.tagmatch.snip_end_tag
  call <SID>Debug("Matching on this string: ".tagmatch)
  let tagsubstitution = <SID>RunCommand(commandToRun, replaceVal)
  let lines = split(substitute(getline("."), tagmatch, tagsubstitution, ""),'\n')
  if len(lines) > 1
    call setline(".", lines[0])
    call append(".", lines[1:])
  else
    call setline(".", lines)
  endif
  " We use replaceVal instead of tagsubsitution as otherwise the command
  " result will be passed to subsequent tags
  let s:replaceVal = replaceVal
  call <SID>MakeChanges()
  unlet s:CHANGED_VAL
  call <SID>Debug("---------------- End of ChangeVals() ----------------")
endfunction
" }}}
"{{{ SID() - Get the SID for the current script
function! s:SID()
  return matchstr(expand('<sfile>'), '<SNR>\zs\d\+\ze_SID$')
endfun
"}}}
"{{{ CheckForInTag() - Check whether we're in a tag
function! <SID>CheckForInTag()
  let [snip_start_tag, snip_elem_delim, snip_end_tag] = <SID>SetLocalTagVars()
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
" {{{ <SID>SubSpecialVars(text)
function! <SID>SubSpecialVars(text)
  let text = a:text
  let text = substitute(text, 'SNIP_FILE_NAME', expand('%'), 'g')
  let text = substitute(text, 'SNIP_ISO_DATE', strftime("%Y-%m-%d"), 'g')
  return text
endfunction
" }}}
" {{{ <SID>SubCommandOutput(text)
function! <SID>SubCommandOutput(text)
  call <SID>Debug("----- Start of SubCommandOutput -----")
  let search = '``.\{-}``'
  let text = a:text
  while match(text, search) != -1
    let command_match = matchstr(text, search)
    call <SID>Debug("Command found: ".command_match)
    let command = substitute(command_match, '^..\(.*\)..$', '\1', '')
    call <SID>Debug("Command being run: ".command)
    exec 'let output = '.command
    let output = escape(output, '\')
    let text = substitute(text, '\V'.escape(command_match, '\'), output, '')
  endwhile
  call <SID>Debug("----- End of SubCommandOutput -----")
  return text
endfunction
" }}}
" {{{ <SID>RemoveAndStoreCommands(text)
function! <SID>RemoveAndStoreCommands(text)
  call <SID>Debug("---------------- Start of RemoveAndStoreCommands ----------------")

  let [snip_start_tag, snip_elem_delim, snip_end_tag] = <SID>SetLocalTagVars()
  let text = a:text
  if !exists("b:command_dict")
    let b:command_dict = {}
  endif
  let tmp_command_dict = {}
"  if s:debug == 1
"    let b:command_dict = {}
"  endif
  let ind = 0
  let ind = match(text, b:search_str)
  while ind > -1
    call <SID>Debug("Text is: ".text)
    call <SID>Debug("index is: ".ind)
    let tag = matchstr(text, b:search_str, ind)
    call <SID>Debug("Tag is: ".tag)
    let commandToRun = matchstr(tag, snip_elem_delim.".*".snip_end_tag)

    if commandToRun != ''
      let tag_name = strpart(tag,strlen(snip_start_tag),match(tag,snip_elem_delim)-strlen(snip_start_tag))
      call <SID>Debug("Got this tag: ".tag_name)
      call <SID>Debug("Adding this command: ".commandToRun)
      if tag_name != ''
        if has_key(tmp_command_dict, tag_name)
          call add(tmp_command_dict[tag_name], strpart(commandToRun, 1, strlen(commandToRun)-strlen(snip_end_tag)-1))
        else
          let tmp_command_dict[tag_name] = [strpart(commandToRun, 1, strlen(commandToRun)-strlen(snip_end_tag)-1)]
        endif
      endif
      let text = substitute(text, '\V'.escape(commandToRun,'\'), snip_end_tag,'')
    else
      let tag_name = <SID>ChopTags(tag)
      if tag_name != ''
        if has_key(tmp_command_dict, tag_name)
          call add(tmp_command_dict[tag_name], '')
        else
          let tmp_command_dict[tag_name] = ['']
        endif
      endif
    endif
    call <SID>Debug(tag." found at ".ind)
    let ind = match(text, b:search_str, ind+strlen(snip_end_tag))
  endwhile
  for key in keys(tmp_command_dict)
    if has_key(b:command_dict, key)
      for item in reverse(tmp_command_dict[key])
        call insert(b:command_dict[key], item)
      endfor
    else
      let b:command_dict[key] = tmp_command_dict[key]
    endif
  endfor
  call <SID>Debug("---------------- End of RemoveAndStoreCommands ----------------")
  return text
endfunction
" }}}
" {{{ Jumper()
" We need to rewrite this function to reflect the new behaviour. Every jump
" will now delete the markers so we need to allow for the following conditions
" 1. Empty tags e.g. "��".  When we land inside then we delete the tags.
"  "�:�" is now an invalid tag (use "��" instead) so we don't need to check for
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
  call <SID>Debug("---------------- Start of Jumper ----------------")
  " Set up some useful variables
  let [snip_start_tag, snip_elem_delim, snip_end_tag] = <SID>SetLocalTagVars()

  if s:supInstalled == 1 && s:done_remap == 0
    let s:SupSNR = <SID>GetSuperTabSNR()
    imap <Tab> <Plug>Jumper
    let s:done_remap = 1
  endif

  if !exists('b:search_str')
    call <SID>Debug("---------------- End of Jumper ----------------")
    if s:supInstalled
      return "\<C-R>=".s:SupSNR."SuperTab('n')\<CR>"
    else
      return "\<Tab>"
    endif
  endif
  let s:curCurs = col(".") - 1
  let s:curLine = line(".")
  let s:line = getline(".")
  let s:replaceVal = ""
  " Save the value of hlsearch
  if &hls
    setlocal nohlsearch
    let b:hl_on = 1
  else
    let b:hl_on = 0
  endif
  " Check to see whether we're at the start of a tag.  
  " start then we should be assuming that we've got a 'default' value or a
  " command to run.  Otherwise the user will have pressed the jump key
  " without changing the value.
  " First we need to check that we're inside a tag i.e. the previous
  " jump didn't land us in a 1. style tag.
   
  " First we'll check that the user hasn't just typed a snippet to expand
  "
  let origword = matchstr(strpart(getline("."), 0, s:curCurs), '\k\{-}$')
  call <SID>Debug("Original word was: ".origword)
  let word = <SID>Hash(origword)
"    " The following code is lifted wholesale from the imaps.vim script - Many
"    " thanks for the inspiration to add the TextMate compatibility
"    " Unless we are at the very end of the word, we need to go back in order
"    " to find the last word typed.
  let rhs = ''
  " Check for buffer specific expansions
  if exists('b:snip_'.word)
    exe 'let rhs = b:snip_'.word
  elseif exists('g:snip_'.word)
  " also check for global definitions
    exe 'let rhs = g:snip_'.word
  endif

  if rhs != ''
    " if this is a mapping, then erase the previous part of the map
    " by also returning a number of backspaces.
    let bkspc = substitute(origword, '.', "\<bs>", "g")
    call <SID>Debug("Backspacing ".<SID>StrLen(origword)." characters")
    let delEndTag = ""
    if <SID>CheckForInTag()
      call <SID>Debug("We're doing a nested tag")
      call <SID>Debug("B:tag_name: ".b:tag_name)
      if b:tag_name != ''
        try
          call <SID>Debug("Commands for this tag are currently: ".join(b:command_dict[b:tag_name],"', '"))
          call <SID>Debug("Removing command for '".b:tag_name."'")
          unlet b:command_dict[b:tag_name][0]
          call <SID>Debug("Commands for this tag are now: ".join(b:command_dict[b:tag_name],"', '"))
        catch /E175/
          call <SID>Debug("Could not find this key in the dict: ".b:tag_name)
        endtry
      endif
      call <SID>Debug("Deleting start tag")
      let bkspc = bkspc.substitute(snip_start_tag, '.', "\<bs>", "g")
      call <SID>Debug("Deleting end tag")
      let delEndTag = substitute(snip_end_tag, '.', "\<Del>", "g")
      call <SID>Debug("Deleting ".<SID>StrLen(delEndTag)." characters")
    endif
    
    " We've found a mapping so we'll substitute special variables
    let rhs = <SID>SubSpecialVars(rhs)
    let rhs = <SID>SubCommandOutput(rhs)
    " Now we'll chop out the commands from tags
    let rhs = <SID>RemoveAndStoreCommands(rhs)

    " This movement method is from imaps again. It's kinda neat so we'll use it
    " here.
    let initial = "SnipStartSnipStart"

    call <SID>Debug("---------------- End of Jumper ----------------")
    return bkspc.delEndTag.initial.rhs."\<Esc>?".initial."\<CR>".strlen(initial)."xi\<C-r>=<SNR>".s:SID()."_NextHop()\<CR>"
  else
    " No definition so let's check to see whether we're in a tag
    if <SID>CheckForInTag()
      call <SID>Debug("No mapping and we're in a tag")
      " We're in a tag so we need to do processing
      if strpart(s:line, s:curCurs - strlen(snip_start_tag), strlen(snip_start_tag)) == snip_start_tag
        call <SID>Debug("Value not changed")
        call <SID>ChangeVals(0)
        call <SID>Debug("---------------- End of Jumper ----------------")
        return "\<C-r>=<SNR>".s:SID()."_NextHop()\<CR>"
      else
        call <SID>Debug("Value changed")
        call <SID>ChangeVals(1)
        call <SID>Debug("---------------- End of Jumper ----------------")
        return "\<C-r>=<SNR>".s:SID()."_NextHop()\<CR>"
      endif
    else
      " We're not in a tag so we'll see whether there are more tags
      if search(b:search_str, "n")
        " More tags so let's perform nexthop
        let s:replaceVal = ""
        call <SID>Debug("---------------- End of Jumper ----------------")
        return "\<C-r>=<SNR>".s:SID()."_NextHop()\<CR>"
      else
        " No more tags so let's return a Tab

        " Turn hlsearch back on if needed
        if b:hl_on == 1
          setlocal hlsearch
        endif
        if exists("b:command_dict")
          unlet b:command_dict
        endif
        if s:supInstalled
          call <SID>Debug('SuperTab installed. Returning <C-n> instead of <Tab>')
          call <SID>Debug("---------------- End of Jumper ----------------")
          return "\<C-R>=".s:SupSNR."SuperTab('n')\<CR>"
        else
          call <SID>Debug("---------------- End of Jumper ----------------")
          return "\<Tab>"
        endif
      endif
    endif
  endif
endfunction
" }}}
" {{{ Set up the 'Iabbr' and 'Snippet' commands
command! -nargs=+ Iabbr execute <SID>SetCom(<q-args>)
command! -nargs=+ Snippet execute <SID>SetCom("<buffer> ".<q-args>)
"}}}
" {{{ Utility functions

" This function will just return what's passed to it unless a change has been
" made
fun! D(text)
  if exists('s:CHANGED_VAL') && s:CHANGED_VAL == 1
    return @z
  else
    return a:text
  endif
endfun

" <SID>Hash allows the use of special characters in snippets
" This function is lifted straight from the imaps.vim plugin. Please let me know
" if this is against licensing.
function! <SID>Hash(text)
	return substitute(a:text, '\([^[:alnum:]]\)',
				\ '\="_".char2nr(submatch(1))."_"', 'g')
endfunction

" This function chops tags from any text passed to it
function! <SID>ChopTags(text)
  call <SID>Debug("---------------- Start of ChopTags ----------------")
  let text = a:text
  call <SID>Debug("ChopTags was passed this text: ".text)
  let [snip_start_tag, snip_elem_delim, snip_end_tag] = <SID>SetLocalTagVars()
  let text = strpart(text, strlen(snip_start_tag))
  let text = strpart(text, 0, strlen(text)-strlen(snip_end_tag))
  call <SID>Debug("ChopTags is returning this text: ".text)
  call <SID>Debug("---------------- End of ChopTags ----------------")
  return text
endfunction

" This function ensures we measure string lengths correctly
function! <SID>StrLen(str)
  call <SID>Debug("StrLen returned: ".strlen(substitute(a:str, '.', 'x', 'g'))." based on this text: ".a:str)
  return strlen(substitute(a:str, '.', 'x', 'g'))
endfunction

" This function checks some settings to see whether behave mswin has been set.
" This is then used to adjust the selection behaviour
function! <SID>DetectWinMode()
  if &selectmode == "mouse,key"  
        \ && &mousemodel == "popup" 
        \ && &keymodel == "startsel,stopsel"
        \ && &selection == "exclusive"
    return 1
  endif
  return 0
endfunction
" }}}
" vim: set tw=80 sw=2 sts=2 et foldmethod=marker :
doc/snippets_emu.txt
290
*snippets_emu.txt*  For Vim version 7.0. Last change: 2006 Dec 26


		  VIM REFERENCE MANUAL    by Panos Laganakos and Felix Ingram


1. SnippetsEmu Features                         |snippets_emu-features|
        Basic Snippets                              |basic-snippet|
        Named Tags                                  |named-tags|
        Tag Commands                                |snippet-commands|
        Buffer Specific Snippets                    |snip-buffer-specific|
        Filetype Specific Snippets                  |snip-ftplugin|
        Snippets menu                               |snip-ftplugin|
2. SnippetsEmu Options                          |snippets_emu-options|
        Start and End Tags                          |snip-start-end-tags|
        Element Delimiter                           |snip-start-end-tags|
3. Detailed Explanations                        |snip-detailed-explanations|
        Valid Tag Names                             |snip-tag-name-syntax|
        Advanced Tag Command Examples               |snip-advanced-tag-commands|
4. SnippetsEmu Contact Details                  |snip-contact-details|
5. SnippetsEmu Known Bugs                       |snippets_emu-bugs|
6. Troubleshooting                              |snippet_emu-troubleshooting|

{Vi does not have any of these features}

==============================================================================
SNIPPETSEMU FEATURES					*snippets_emu-features*

SnippetsEmu attempts to emulate several of the snippets features of the OS X
editor TextMate, in particular the variable bouncing and replacement behaviour.
Simple usage is built up around the following functionality:

	Basic Snippet					|basic-snippet|
	Named Tags					|named-tags|
	Executable Snippet				|snippet-commands|
        Buffer Specific Snippets                        |snip-buffer-specific|

                                                               *basic-snippet*
Basic Snippet ~

A basic snippet can save you a lot of typing. Define a word trigger and on
insertion it will be expanded to the full snippet. SnippetsEmu allows the
user to define markers within the larger piece of text which will be used
to place the cursor upon expansion.

The command used to define a snippet is 'Snippet'.

Basic Syntax: >

	Snippet trigger_name The cursor will be placed here: �� Trailing text

In insert mode typing 'trigger_name<tab>' will remove 'trigger_name' and
replace it with the text: 'The cursor will be placed here:  Trailing text'.
The cursor will be placed between the two spaces before the word 'Trailing'

NOTE: All text should be entered on the same command line. The formatting of
this document may mean that examples are wrapped but they should all be
entered on a single line.

                                                                  *named-tags*
Named tags ~

Instead of the simple '��' tags used for cursor placement a user can define
named tags. When the value of a named tag is changed then all other tags with
that name will be changed to the same value.

E.g. >

    Snippet trigger My name is �forename� �surname�. Call me �forename�.

In insert mode typing 'trigger<space>' will place the cursor inside the
'�forename�' tag. Whatever is entered inside the tag will replace the other
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

    Snippet trigger My name is �name�.  I SAID: MY NAME IS
    �name:substitute(@z,'.','\u&','g')�!

The value entered in the �name� tag will be passed to the command in the
second �name� tag in the @z register (any value already in @z will be
preserved and restored). The substitute command will change the entered value
to be in upper case. I.e. Entering 'trigger<space>' and typing 'Tycho<Tab>'
will result in the following text: >
    
    My name is Tycho. I SAID: MY NAME IS TYCHO!
~
                                                           *snip-special-vars*
There is a set of special variables which can be included in snippets. These
will be replaced before the snippet's text is inserted into the buffer. The
list of available variables is detailed below:

 * SNIP_FILE_NAME - The current file name (from 'expand("%")')
 * SNIP_ISO_DATE - The current date in YYYY-MM-DD format.

                                                       *snip-snippet-commands*
In addition to tag commands it is also possible to define commands which will
be executed before the snippet is inserted into the buffer. These are defined
within double backticks. 

E.g.
>
    Snippet date The current date is ``strftime("%c")``

Commands are standard Vim commands and will be 'exec'uted and the command
output substituted into the text.

                                                        *snip-buffer-specific*
Buffer Specific Snippets ~

The Snippet command defines buffer specific snippets. This is the recommended
option when using filetype specific snippets. It is possible to define
'global' snippets which will act across all buffers. These can be defined
using the legacy 'Iabbr' command (note the capital 'I'). 

E.g. >
    Iabbr for for �var� in �list�:<CR>��
~
                                                               *snip-ftplugin*
The preferred practice for defining filetype specific snippets is to include 
them in files named <filetype>_snippets.vim and for these files to be placed in the
~/.vim/after/ftplugin directory (or vimfiles\after\ftplugin under Windows).
When a file of a specific type is loaded so will all of the defined snippets.
The 'after' directory is used to ensure that the plugin has been loaded. It is
also recommended that the following is included at the top of the file: >
    
    if !exists('loaded_snippet') || &cp
        finish
    endif

This will stop errors being generated if the plugin has not loaded for any
reason.

                                                                   *snip-menu*
When loading the plugin will search for all files named '*_snippets.vim'.
These will be added to the 'Snippets' menu which is available in Normal mode.
Selecting options from the menu will source the file and hence load any
snippets defined within it.

===============================================================================
SNIPPETSEMU OPTIONS                                     *snippets_emu-options*
                                                         *snip-start-end-tags*
Start and End Tags ~

By default the start and end tags are set to be '�' and '�'. These can be
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
    �validName�
    �"valid name"�
    �tagName:command�
    �"Tag Name":command�

Invalid Examples: >
    �invalid name�
    �Tag Name:command�
    �:command�

~
                                                  *snip-advanced-tag-commands*
Advanced Tag Command Examples ~

Commands in tags can be as complex as desired. Readability is the main
limitation as the command will be placed in the document before execution.

The preferred method for defining complex commands is to hide the
functionality in a user function.

Example:
>
    
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
            return '('.repeat('��, ', a:count).')'
        endif
    endfunction

    Snippet pf print "�s�" % �s:PyArgList(Count(@z, '%[^%]'))�<CR>��

The above snippet will expand 'pf' to 'print "�s�" ...'. The user then
enters a format string. Once the string is entered the Count and PyArgList
functions are used to generate a number of empty tags.

                                                            *snip-limitations*
The above represents once of the limitations of the plugin. Due to the way
tags are identified it is not possible to include empty tags in another tag's
command. The only way to generate empty tags is to return them from a function
as in the above example. For other examples see the included bundles.

                                                                *snip-bundles*
The included bundles are not defined in the 'preferred style'. In order to
accommodate users who wish to redefine the default tags all snippet
definitions are 'executed' with the 'exec' command.

E.g.
>
    exec "Snippet test This isn't the right way to ".st.et." define snippets"

Executing the command allows 'st' and 'et' to be used in place of start and
end tags. 'st' and 'et' are defined elsewhere in the bundle file.

==============================================================================
SNIPPETSEMU CONTACT DETAILS                             *snip-contact-details*

To contact the author please email:

F <dot> Ingram <dot> lists <at> gmail <dot> com

The author welcomes corrections to this documentation, example snippets and
bug reports.

The plugin is also currently hosted at Google Code:
    http://code.google.com/p/snippetsemu

Bug reports can also be posted on the hosting site:
    http://code.google.com/p/snippetsemu/issues/list

==============================================================================
SNIPPETSEMU KNOWN BUGS                  		   *snippets_emu-bugs*

Bugs are currently tracked on Google Code. Please post any you find on the
issue tracker:
    http://code.google.com/p/snippetsemu/issues/list

==============================================================================
SNIPPETSEMU TROUBLESHOOTING                     *snippets_emu-troubleshooting*

Problem:    Bundles are not loading.
Answer:     Ensure that you have filetype plugins turned on. Include the
            following in your vimrc: >

                filetype plugin on


vim:tw=78:sw=4:ts=8:ft=help:norl:
