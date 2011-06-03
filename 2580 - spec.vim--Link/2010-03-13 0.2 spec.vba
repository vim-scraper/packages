" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
doc/spec.txt	[[[1
415
*spec.txt*          Behaviour-driven design (BDD) for vim scripts
                    Author: Tom Link, micathom at gmail com

This plugin provides a small set of commands to facilitate 
behaviour-driven design of vim plugins. It basically consists of two 
parts:

    spec   ... BDD-related commands
    should ... Convenience functions for testing

According to BDD theory, you are supposed to write a specification first 
and then make the code fulfill that specification. If you don't believe 
in BDD, you could also think of spec.vim as a more convenient approach 
to unit testing.

A example specification could look like this: >

    SpecBegin 'title': 'Foo', 'sfile': 'autoload/foo.vim'

    It should add numbers.
    Should be equal foo#Add(1, 2), 3
    
    It should not add values of other types.
    Should throw something 'foo#Add("1", 2)'

Spec knows how to rewrite certain rules in order to turn them into 
proper function call. Compare the following rules: >

    Should should#throw#Something('foo#Add([1], 2)')
    Should throw#Something('foo#Add([1], 2)')
    Should throw something('foo#Add({1:2}, 2)')
    Should throw something 'foo#Add({1:2}, 2)'

    Should !should#be#Equal(foo#Add(1, 2), 3)
    Should not be equal foo#Add(1, 2), 4

Specification scripts basically are normal vim scripts but must be run 
by the |:Spec| command. The |:SpecBegin| command also allows to define 
options sets against which the specification will be tested.

For an example specification see: ../spec/spec/spec.vim

CAVEAT: Please read at least the comments on |:Spec| and |:SpecBegin|.


-----------------------------------------------------------------------
Install~

Edit the vba file and type: >

    :so %

See :help vimball for details. If you have difficulties or use vim 7.0, 
please make sure, you have the current version of vimball
(vimscript #1502) installed or update your runtime.

Some "should"-functions require tlib (vimscript #1863) to be installed.


========================================================================
Contents~

        g:spec_cwindow ........................... |g:spec_cwindow|
        g:spec_option_sets ....................... |g:spec_option_sets|
        :Spec .................................... |:Spec|
        :SpecBegin ............................... |:SpecBegin|
        SpecInit ................................. |SpecInit()|
        :SpecInclude ............................. |:SpecInclude|
        :It ...................................... |:It|
        :Should .................................. |:Should|
        :Replay .................................. |:Replay|
        spec#RewriteRule ......................... |spec#RewriteRule()|
        spec#Include ............................. |spec#Include()|
        spec#Val ................................. |spec#Val()|
        spec#OpenScratch ......................... |spec#OpenScratch()|
        spec#CloseScratch ........................ |spec#CloseScratch()|
        spec#Feedkeys ............................ |spec#Feedkeys()|
        spec#Replay .............................. |spec#Replay()|
        spec#speckiller#Reset .................... |spec#speckiller#Reset()|
        spec#speckiller#OptionSets ............... |spec#speckiller#OptionSets()|
        should#be#Type ........................... |should#be#Type()|
        should#be#Number ......................... |should#be#Number()|
        should#be#String ......................... |should#be#String()|
        should#be#Funcref ........................ |should#be#Funcref()|
        should#be#List ........................... |should#be#List()|
        should#be#Dictionary ..................... |should#be#Dictionary()|
        should#be#Equal .......................... |should#be#Equal()|
        should#be#Unequal ........................ |should#be#Unequal()|
        should#be#Greater ........................ |should#be#Greater()|
        should#be#GreaterEqual ................... |should#be#GreaterEqual()|
        should#be#Less ........................... |should#be#Less()|
        should#be#LessEqual ...................... |should#be#LessEqual()|
        should#be#Empty .......................... |should#be#Empty()|
        should#be#NotEmpty ....................... |should#be#NotEmpty()|
        should#be#Match .......................... |should#be#Match()|
        should#be#NotMatch ....................... |should#be#NotMatch()|
        should#be#Existent ....................... |should#be#Existent()|
        should#be#Like ........................... |should#be#Like()|
        should#be#Unlike ......................... |should#be#Unlike()|
        should#finish#InSecs ..................... |should#finish#InSecs()|
        should#finish#InMicroSecs ................ |should#finish#InMicroSecs()|
        should#maintain#WindowLayout ............. |should#maintain#WindowLayout()|
        should#test#Init ......................... |should#test#Init()|
        should#throw#Something ................... |should#throw#Something()|
        should#throw#Exception ................... |should#throw#Exception()|
        should#yield#Buffer ...................... |should#yield#Buffer()|
        should#yield#SqueezedBuffer .............. |should#yield#SqueezedBuffer()|


========================================================================
plugin/spec.vim~

                                                    *g:spec_cwindow*
g:spec_cwindow                 (default: 'cwindow')
    The command that should be used for viewing the quickfix list.

                                                    *g:spec_option_sets*
g:spec_option_sets             (default: [])
    If |g:spec_killer| is non-null, test your specs against these 
    options -- a list of dictionaries.
    
    See also |:SpecBegin|.

                                                    *:Spec*
Spec[!] [PATH]
    Run one or more specification files.
    
    PATH can be either a file or a directory.
    
    If PATH is a directory, run all vim files (whose name doesn't begin 
    with an underscore "_") under PATH as specification scripts.
    
    If no PATH is given, run the current file only.
    
    With [!], also print a short list specifications by means of |:TLog|, 
    if available, or |:echom|. You might need to call |:messages| in order 
    to review this list.
    
    NOTES:
    Unit test scripts must not run other unit tests by using 
    |:source|. Use |:SpecInclude| if you have to include a vimscript file 
    that contains |:Should| commands.
    
    Even then it sometimes happens that spec cannot distinguish 
    between to identical tests in different contexts, which is why you 
    should only use one |:SpecBegin| command per file.

                                                    *:SpecBegin*
SpecBegin [ARGUMENTS AS INNER DICTIONNARY]
    Establish the environment for the current specification.
    This command also serves as a saveguard that should prevent users from 
    running specs with the |:source| command.
    
    Known keys for ARGUMENTS:
    
      title   ... The test's title.
      file    ... The script context.
      before  ... Code to be run before each test (only effective when run 
                  via |:SpecRun|.
      after   ... Code to be run after each test (only effective when run 
                  via |:SpecRun|.
      scratch ... Run spec in scratch buffer. If the value is "", use an 
                  empty buffer. If it is "%", read the spec file itself 
                  into the scratch buffer. Otherwise read the file of the 
                  given name.
      cleanup ... A list of function names that will be removed
      options ... Run the spec against these options (a list of 
                  dictionnaries or 'vim' for the default option set).
                  NOTE: If you test your specs against vim default 
                  settings, it's possible that you have to restart vim in 
                  order to get the usual environment.
    
    NOTES:
    Any global variables that were not defined at the time of the last 
    invocation of |:SpecBegin| are considered temporary variables and will 
    be removed.
    
    A specification file *should* ;-) include exactly one :SpecBegin 
    command.

                                                    *SpecInit()*
SpecInit()
    Include the line "exec SpecInit()" in your script in order to install 
    the function s:SpecVal(), which can be used to evaluate expressions in 
    script context. This initializations is necessary only if you call the 
    function |spec#Val()| in your tests.


========================================================================
macros/spec.vim~

                                                    *:SpecInclude*
SpecInclude _FILENAME
    Include a spec file. The filename of the included type should begin 
    with an underscore and it should not contain a |:SpecBegin| command.

                                                    *:It*
It[!] MESSAGE
    Insert a message.
    The message will be displayed when running the spec in verbose mode. 
    With [!], the message will be included in the quickfix list to mark a 
    pending specification.

                                                    *:Should*
Should {expr}
    Make sure that the value of an expression is not |empty()|. If used 
    after a |:SpecBegin| command, any occurrences of "<SID>" in the 
    expression is replaced with the current script's |<SNR>|.

                                                    *:Replay*
Replay[!] MACRO
    Replay a recorded key sequence.
    With [!], the argument is passed unprocessed on to |spec#Replay()|. 
    Otherwise, the macro is evaluated as in |expr-quote|.


========================================================================
autoload/spec.vim~

                                                    *spec#RewriteRule()*
spec#RewriteRule(rx, subst)
    Define your own rewrite rules.
    
    Care must be taken so that the rule expands to a single atomic 
    statement. The pattern should always match from the beginning of the 
    string.
    
    Example: The following rule is wrong: >
    
      \(\d\+\) be equal to \(\d\+\).* => \1 == \2
    
<   because it doesn't match from the beginning of the string and because 
    the substiution breaks other rules like "not => !". The following is 
    preferable: >
    
      ^!\?\zs\(\d\+\) be equal to \(\d\+\).* => (\1 == \2)
    
<   This pattern expands the pattern only when found in the right position 
    and the substiution can be prefixed with !.
    
    You could then write: >
    
      Should 1 be equal to 1
      Should not 1 be equal to 2
<

                                                    *spec#Include()*
spec#Include(filename, top_spec)

                                                    *spec#Val()*
spec#Val(expr)
    Evaluate an expression in the context of a script.
    Requires a call to |specInit()|.

                                                    *spec#OpenScratch()*
spec#ScratchBuffer(?filename="", ?filetype="") "{{{3
    Open the spec scratch buffer.

                                                    *spec#CloseScratch()*
spec#CloseScratch()
    Close the scratch buffer. (Requires the cursor to be located in the spec 
    scratch buffer.)

                                                    *spec#Feedkeys()*
spec#Feedkeys(sequence)

                                                    *spec#Replay()*
spec#Replay(macro)
    Replay a recorded macro.


========================================================================
autoload/spec/speckiller.vim~

                                                    *spec#speckiller#Reset()*
spec#speckiller#Reset()

                                                    *spec#speckiller#OptionSets()*
spec#speckiller#OptionSets(options, i)
    Return the i'th option set.


========================================================================
autoload/should/be.vim~


should#be#A(expr, type)
    Test if expr is of type, where type can be:
    
        - One of: 'number', 'string', 'funcref', 'list', 'dictionary'
        - A list of above type names (one of which must match)
        - A dictionary in which case the type is evaluated as object 
          template. Keys in the template that do not have a value of 0, 
          must exist in the object/expression.
    
    See also |type()|.

                                                    *should#be#Type()*
should#be#Type(expr, type)
    Faster checks than version above but without descriptive messages and 
    type must be a string.

                                                    *should#be#Number()*
should#be#Number(expr)

                                                    *should#be#String()*
should#be#String(expr)

                                                    *should#be#Funcref()*
should#be#Funcref(expr)

                                                    *should#be#List()*
should#be#List(expr)

                                                    *should#be#Dictionary()*
should#be#Dictionary(expr)

                                                    *should#be#Equal()*
should#be#Equal(expr, expected)

                                                    *should#be#Unequal()*
should#be#Unequal(expr, expected)

                                                    *should#be#Greater()*
should#be#Greater(a, b)

                                                    *should#be#GreaterEqual()*
should#be#GreaterEqual(a, b)

                                                    *should#be#Less()*
should#be#Less(a, b)

                                                    *should#be#LessEqual()*
should#be#LessEqual(a, b)

                                                    *should#be#Empty()*
should#be#Empty(expr)

                                                    *should#be#NotEmpty()*
should#be#NotEmpty(expr)

                                                    *should#be#Match()*
should#be#Match(expr, expected)

                                                    *should#be#NotMatch()*
should#be#NotMatch(expr, expected)

                                                    *should#be#Existent()*
should#be#Existent(expr)

                                                    *should#be#Like()*
should#be#Like(string, rx, ?case='')
    Case can be "#" or "?".

                                                    *should#be#Unlike()*
should#be#Unlike(string, rx, ?case='')
    Case can be "#" or "?".


========================================================================
autoload/should/finish.vim~

                                                    *should#finish#InSecs()*
should#finish#InSecs(expr, secs)

                                                    *should#finish#InMicroSecs()*
should#finish#InMicroSecs(expr, msecs)


========================================================================
autoload/should/maintain.vim~

                                                    *should#maintain#WindowLayout()*
should#maintain#WindowLayout(layout)
    Require tlib.


========================================================================
autoload/should/test.vim~

                                                    *should#test#Init()*
should#test#Init()


========================================================================
autoload/should/throw.vim~

                                                    *should#throw#Something()*
should#throw#Something(expr)
    Return the exception when evaluating expr or an empty string if 
    nothing was thrown.

                                                    *should#throw#Exception()*
should#throw#Exception(expr, expected)
    Check if the exception throws when evaluating expr matches the 
    expected |regexp|.


========================================================================
autoload/should/yield.vim~

                                                    *should#yield#Buffer()*
should#yield#Buffer(expr, filename)
    Compare the current buffer with the contents of filename after 
    |:exe|cuting expr.
    Useful for testing normal commands, mappings etc.

                                                    *should#yield#SqueezedBuffer()*
should#yield#SqueezedBuffer(expr, filename)
    Compare the current buffer with the contents of filename after 
    |:exe|cuting expr but ignore changes in whitespace.



vim:tw=78:fo=tcq2:isk=!-~,^*,^|,^":ts=8:ft=help:norl:
macros/spec.vim	[[[1
70
" spec.vim
" @Author:      Thomas Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2009-02-22.
" @Last Change: 2010-02-27.
" @Revision:    63

if &cp || exists("loaded_macros_spec")
    finish
endif
let loaded_macros_spec = 1

let s:save_cpo = &cpo
set cpo&vim


" :display: SpecInclude _FILENAME
" Include a spec file. The filename of the included type should begin 
" with an underscore and it should not contain a |:SpecBegin| command.
command! -nargs=1 SpecInclude call spec#Include(<args>, 0)


" :display: It[!] MESSAGE
" Insert a message.
" The message will be displayed when running the spec in verbose mode. 
" With [!], the message will be included in the quickfix list to mark a 
" pending specification.
command! -nargs=1 -bang It call spec#__Comment('It '. <q-args>, !empty('<bang>'))


" " :display: The MESSAGE
" " Insert a message.
" command! -nargs=1 The call spec#__Comment('The '. <q-args>)


" :display: Should {expr}
" Make sure that the value of an expression is not |empty()|. If used 
" after a |:SpecBegin| command, any occurrences of "<SID>" in the 
" expression is replaced with the current script's |<SNR>|.
command! -nargs=1 Should
            \ let s:spec_reason = '' |
            \ call spec#__Setup() |
            \ try |
            \   let s:spec_failed = empty(eval(spec#__Rewrite(<q-args>))) |
            \ catch |
            \   let s:spec_reason = v:exception |
            \   let s:spec_failed = 1 |
            \ endtry |
            \ call spec#__Teardown() |
            \ if s:spec_failed |
            \   call should#__InsertReason(<q-args>) |
            \   if !empty(s:spec_reason) | call should#__InsertReason(s:spec_reason) | endif |
            \   call spec#__AddQFL(<q-args>, should#__ClearReasons()) |
            \ endif


" :display: Replay[!] MACRO
" Replay a recorded key sequence.
" With [!], the argument is passed unprocessed on to |spec#Replay()|. 
" Otherwise, the macro is evaluated as in |expr-quote|.
command! -nargs=1 -bang Replay if empty("<bang>")
            \ | call spec#Replay(eval('"'. escape(<q-args>, '"') .'"'))
            \ | else
                \ | call spec#Replay(<q-args>)
                \ | endif


let &cpo = s:save_cpo
unlet s:save_cpo
plugin/spec.vim	[[[1
131
" spec.vim -- Behaviour-driven design for VIM script
" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2009-02-22.
" @Last Change: 2010-02-27.
" @Revision:    68
" GetLatestVimScripts: 2580 0 :AutoInstall: spec.vim

if &cp || exists("loaded_spec")
    finish
endif
let loaded_spec = 2

let s:save_cpo = &cpo
set cpo&vim


if !exists('g:spec_cwindow')
    " The command that should be used for viewing the quickfix list.
    let g:spec_cwindow = 'cwindow'   "{{{2
endif

if !exists('g:spec_option_sets')
    " If |g:spec_killer| is non-null, test your specs against these 
    " options -- a list of dictionaries.
    "
    " See also |:SpecBegin|.
    let g:spec_option_sets = []   "{{{2
endif

" if !exists('g:spec_vim')
"     " The (g)vim executable -- used for remote tests.
"     let g:spec_vim = 'gvim'  "{{{2
" endif


" :display: Spec[!] [PATH]
" Run one or more specification files.
"
" PATH can be either a file or a directory.
" 
" If PATH is a directory, run all vim files (whose name doesn't begin 
" with an underscore "_") under PATH as specification scripts.
"
" If no PATH is given, run the current file only.
" 
" With [!], also print a short list specifications by means of |:TLog|, 
" if available, or |:echom|. You might need to call |:messages| in order 
" to review this list.
"
" NOTES:
" Unit test scripts must not run other unit tests by using 
" |:source|. Use |:SpecInclude| if you have to include a vimscript file 
" that contains |:Should| commands.
"
" Even then it sometimes happens that spec cannot distinguish 
" between to identical tests in different contexts, which is why you 
" should only use one |:SpecBegin| command per file.
command! -nargs=? -complete=file -bang Spec
            \ | runtime macros/spec.vim
            \ | call spec#__Run(<q-args>, expand('%:p'), !empty("<bang>"))


" :display: SpecBegin [ARGUMENTS AS INNER DICTIONNARY]
" Establish the environment for the current specification.
" This command also serves as a saveguard that should prevent users from 
" running specs with the |:source| command.
"
" Known keys for ARGUMENTS:
"
"   title   ... The test's title.
"   file    ... The script context.
"   before  ... Code to be run before each test (only effective when run 
"               via |:SpecRun|.
"   after   ... Code to be run after each test (only effective when run 
"               via |:SpecRun|.
"   scratch ... Run spec in scratch buffer. If the value is "", use an 
"               empty buffer. If it is "%", read the spec file itself 
"               into the scratch buffer. Otherwise read the file of the 
"               given name.
"   cleanup ... A list of function names that will be removed
"   options ... Run the spec against these options (a list of 
"               dictionnaries or 'vim' for the default option set).
"               NOTE: If you test your specs against vim default 
"               settings, it's possible that you have to restart vim in 
"               order to get the usual environment.
" 
" NOTES:
" Any global variables that were not defined at the time of the last 
" invocation of |:SpecBegin| are considered temporary variables and will 
" be removed.
"
" A specification file *should* ;-) include exactly one :SpecBegin 
" command.
command! -nargs=* SpecBegin call spec#__Begin({<args>}, expand("<sfile>:p"))


" Include the line "exec SpecInit()" in your script in order to install 
" the function s:SpecVal(), which can be used to evaluate expressions in 
" script context. This initializations is necessary only if you call the 
" function |spec#Val()| in your tests.
fun! SpecInit()
    return "function! s:SpecVal(expr)\nreturn eval(a:expr)\nendf"
endf


let &cpo = s:save_cpo
unlet s:save_cpo
finish

TODO: SpecInclude spec.file
TODO: Delete spec commands when done.


POSSIBLE ENHANCEMENTS:
- log to file???
- Pass, Fail (current spec)
- remote testing (maybe we don't need this if the use of feedkeys() is 
sufficient for most interactive tests)


CHANGES:
0.1
- Initial release

0.2
- Display a message after having run all specs
- Raise an error when :SpecBegin is not called in a spec context (i.e. 
via the :Spec command)

autoload/should.vim	[[[1
69
" should.vim
" @Author:      Thomas Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2009-02-21.
" @Last Change: 2010-01-03.
" @Revision:    0.0.49

let s:save_cpo = &cpo
set cpo&vim
" call tlog#Log('Load: '. expand('<sfile>')) " vimtlib-sfile



" :nodoc:
fun! should#__Init() "{{{3
    " TLog 'should#__Init'
    let s:should_reason = []
endf


" :nodoc:
fun! should#__InsertReason(reason) "{{{3
    call insert(s:should_reason, a:reason)
    return a:reason
endf


" :nodoc:
fun! should#__Reasons() "{{{3
    return join(s:should_reason, ': ')
endf


" :nodoc:
fun! should#__ClearReasons() "{{{3
    let rv = should#__Reasons()
    let s:should_reason = []
    return rv
endf


" :nodoc:
fun! should#__Explain(reason)
    if exists('s:should_reason')
        call add(s:should_reason, a:reason)
    endif
endf


" :nodoc:
function! should#__Eval(expr) "{{{3
    if a:expr[0:0] == ':'
        exec a:expr
    else
        return eval(a:expr)
    endif
endf


function! should#__Require(what) "{{{3
    if !exists('g:loaded_'. a:what)
        throw 'should#maintain#WindowLayout requires '. a:what
    endif
endf


let &cpo = s:save_cpo
unlet s:save_cpo
autoload/should/be.vim	[[[1
239
" be.vim
" @Author:      Thomas Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2009-02-21.
" @Last Change: 2009-03-02.
" @Revision:    0.0.49


let s:save_cpo = &cpo
set cpo&vim


let s:types = ['number', 'string', 'funcref', 'list', 'dictionary']


" Test if expr is of type, where type can be:
"
"     - One of: 'number', 'string', 'funcref', 'list', 'dictionary'
"     - A list of above type names (one of which must match)
"     - A dictionary in which case the type is evaluated as object 
"       template. Keys in the template that do not have a value of 0, 
"       must exist in the object/expression.
"
" See also |type()|.
function! should#be#A(expr, type)
    return s:CheckType(a:expr, a:type, string(a:type))
endf


" Faster checks than version above but without descriptive messages and 
" type must be a string.
function! should#be#Type(expr, type)
    return type(a:expr) == index(s:types, a:type)
endf


function! should#be#Number(expr)
    return s:CheckType(a:expr, 0, 'number')
endf


function! should#be#String(expr)
    return s:CheckType(a:expr, 1, 'string')
endf


function! should#be#Funcref(expr)
    return s:CheckType(a:expr, 2, 'funcref')
endf


function! should#be#List(expr)
    return s:CheckType(a:expr, 3, 'list')
endf


function! should#be#Dictionary(expr)
    return s:CheckType(a:expr, 4, 'dictionary')
endf


function! should#be#Equal(expr, expected)
    let rv = type(a:expr) == type(a:expected) && a:expr == a:expected
    if !rv
        call should#__Explain('Expected '. string(a:expected) .' but got '. string(a:expr))
    endif
    return rv
endf


function! should#be#Unequal(expr, expected)
    let rv  = type(a:expr) != type(a:expected) || a:expr != a:expected
    if !rv
        call should#__Explain('Expected '. string(a:expected) .' is unequal to '. string(a:expr))
    endif
    return rv
endf


function! should#be#Greater(a, b) "{{{3
    return s:Compare(a:a, a:b, '>')
endf


function! should#be#GreaterEqual(a, b) "{{{3
    return s:Compare(a:a, a:b, '>=')
endf


function! should#be#Less(a, b) "{{{3
    return s:Compare(a:a, a:b, '<')
endf


function! should#be#LessEqual(a, b) "{{{3
    return s:Compare(a:a, a:b, '<=')
endf


function! s:Compare(a, b, comparator) "{{{3
    try
        exec 'let rv = a:a '. a:comparator .' a:b'
    catch
        let rv = 0
    endtry
    if !rv
        call should#__Explain('Expected '. string(a:a) .' '. a:comparator .' '. string(a:b))
    endif
    return rv
endf


function! should#be#Empty(expr)
    let rv = empty(a:expr)
    if !rv
        call should#__Explain(string(a:expr) .' isn''t empty')
    endif
    return rv
endf


function! should#be#NotEmpty(expr)
    let rv = !empty(a:expr)
    if !rv
        call should#__Explain(string(a:expr) .' is empty')
    endif
    return rv
endf


function! should#be#Match(expr, expected)
    let val = a:expr
    let rv  = val =~ a:expected
    if !rv
        call should#__Explain(string(val) .' doesn''t match '. string(a:expected))
    endif
    return rv
endf


function! should#be#NotMatch(expr, expected)
    let val = a:expr
    let rv  = val !~ a:expected
    if !rv
        call should#__Explain(add(s:assertReason, string(val) .' matches '. string(a:expected))
    endif
    return rv
endf


function! should#be#Existent(expr)
    let val = a:expr
    let rv = exists(val)
    if !rv
        call should#__Explain(add(s:assertReason, string(val) .' doesn''t exist')
    endif
    return rv
endf


" :display: should#be#Like(string, rx, ?case='')
" Case can be "#" or "?".
function! should#be#Like(string, rx, ...) "{{{3
    exec 'let rv = a:string =~'. (a:0 >= 1 ? a:1 : '') .'a:rx'
    if !rv
        call should#__Explain('Expected '. string(a:string) .' to match '. string(a:rx))
    endif
    return rv
endf


" :display: should#be#Unlike(string, rx, ?case='')
" Case can be "#" or "?".
function! should#be#Unlike(string, rx, ...) "{{{3
    exec 'let rv = a:string !~'. (a:0 >= 1 ? a:1 : '') .'a:rx'
    if !rv
        call should#__Explain('Expected '. string(a:string) .' not to match '. string(a:rx))
    endif
    return rv
endf


" :nodoc:
function! s:CheckType(expr, type, expected)
    " TLogVAR a:expr, a:type
    let type = type(a:expr)
    if type(a:type) == 3
        " type is a list of types
        for t in a:type
            let rv = s:CheckType(a:expr, t, a:expected)
            if rv
                return rv
            endif
        endfor
    elseif type(a:type) == 1
        " type is a type name
        let t = index(s:types, tolower(a:type))
        if t == -1
            throw 'Unknown type: '. string(a:type)
        else
            return s:CheckType(a:expr, t, a:expected)
        endif
    elseif type(a:type) == 4
        " type is a dictionary
        " let Val  = a:expr
        " let type = type(Val)
        if type == 4
            let rv = !len(filter(keys(a:type), '!s:CheckMethod(a:expr, a:type, v:val)'))
        else
            let rv = 0
        endif
    else
        " let type = type(Val)
        let rv = type == a:type
    endif
    if !rv
        call should#__Explain('Expected a '. a:expected .' but got a '. get(s:types, type, 'unknown') .': '. string(a:expr))
    endif
    return rv
endf


" :nodoc:
function! s:CheckMethod(dict, prototype, method)
    " if a:method == 'data'
    "     return 1
    " endif
    let m = a:prototype[a:method]
    if type(m) == 0 && !m
        return 1
    endif
    return has_key(a:dict, a:method)
endf



let &cpo = s:save_cpo
unlet s:save_cpo
autoload/should/finish.vim	[[[1
50
" finish.vim
" @Author:      Thomas Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2009-02-22.
" @Last Change: 2009-02-28.
" @Revision:    0.0.14

let s:save_cpo = &cpo
set cpo&vim


function! should#finish#InSecs(expr, secs) "{{{3
    let start = localtime()
    try
        call should#__Eval(a:expr)
    catch
    endtry
    let d  = localtime() - start
    let rv = d <= a:secs
    if !rv
        call should#__Explain('Expected '. a:expr .' to finish in less than '. a:secs .'s, but it took '. d .'s')
    endif
    return rv
endf


function! should#finish#InMicroSecs(expr, msecs) "{{{3
    if exists('g:loaded_tlib')
        let start = tlib#time#Now()
        try
            call should#__Eval(a:expr)
        catch
        endtry
        let d  = tlib#time#Diff(tlib#time#Now(), start)
        " TLogVAR d
        let rv = d <= a:msecs
        if !rv
            call should#__Explain('Expected '. a:expr .' to finish in less than '. a:msecs .'ms, but it took '. d .'ms')
        endif
        return rv
    else
        call should#__Explain('should#finish#InMicroSecs requires tlib')
        return 0
    endif
endf


let &cpo = s:save_cpo
unlet s:save_cpo
autoload/should/maintain.vim	[[[1
46
" maintain.vim
" @Author:      Thomas Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2009-02-26.
" @Last Change: 2009-03-07.
" @Revision:    0.0.8

let s:save_cpo = &cpo
set cpo&vim


" Require tlib.
function! should#maintain#WindowLayout(layout) "{{{3
    if !exists('loaded_tlib')
        throw 'should#maintain#WindowLayout requires tlib'
    endif
    let current_layout = tlib#win#GetLayout(1)
    let rv = 1
    let msg = []
    if a:layout.cmdheight != current_layout.cmdheight
        call add(msg, 'cmdheight')
    endif
    if a:layout.guioptions != current_layout.guioptions
        call add(msg, 'guioptions')
    endif
    if len(a:layout.views) != len(current_layout.views)
        call add(msg, 'number of windows')
    else
        for [n, view] in items(a:layout.views)
            if a:layout.views[n] != view
                call add(msg, 'window '. n)
            endif
        endfor
    endif
    if empty(msg)
        call should#__Explain(rv, 'Window layout has changed: '. join(msg, ', '))
        return 0
    else
        return 1
    endif
endf


let &cpo = s:save_cpo
unlet s:save_cpo
autoload/should/test.vim	[[[1
27
" test.vim
" @Author:      Thomas Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2009-02-21.
" @Last Change: 2009-02-21.
" @Revision:    0.0.5

let s:save_cpo = &cpo
set cpo&vim


let s:foo = 123

exec TAssertInit()

function! should#test#Init() "{{{3
    return "TAssert test"
endf

fun! s:TassertTest(a)
    return a:a + a:a
endf


let &cpo = s:save_cpo
unlet s:save_cpo
autoload/should/throw.vim	[[[1
55
" throw.vim
" @Author:      Thomas Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2009-02-21.
" @Last Change: 2010-02-27.
" @Revision:    0.0.20

let s:save_cpo = &cpo
set cpo&vim


" Return the exception when evaluating expr or an empty string if 
" nothing was thrown.
fun! should#throw#Something(expr)
    try
        call should#__Eval(a:expr)
        call should#__Explain('Expected exception but none was thrown')
        return 0
    catch
        " TLog v:exception
        return 1
    endtry
endf


" Check if the exception throws when evaluating expr matches the 
" expected |regexp|.
fun! should#throw#Exception(expr, expected)
    try
        call should#__Eval(a:expr)
        let msg = ''
    catch
        let msg = v:exception .': '. v:throwpoint
    endtry
    if empty(msg)
        call should#__Explain('Expected exception '. string(a:expected) .'but none was thrown')
        let rv = 0
    elseif msg =~ a:expected
        let rv = 1
    else
        call should#__Explain('Expected exception '. string(a:expected) .' but got '. string(msg))
        let rv = 0
    endif
    if rv == 0
        echohl Error
        echom msg
        echohl NONE
    endif
    return rv
endf


let &cpo = s:save_cpo
unlet s:save_cpo
autoload/should/yield.vim	[[[1
59
" yield.vim -- Interactive tests
" @Author:      Thomas Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2009-02-21.
" @Last Change: 2009-02-28.
" @Revision:    0.0.30

let s:save_cpo = &cpo
set cpo&vim


" Compare the current buffer with the contents of filename after 
" |:exe|cuting expr.
" Useful for testing normal commands, mappings etc.
function! should#yield#Buffer(expr, filename) "{{{3
    " TLogVAR a:expr, a:filename
    call should#__Eval(a:expr)
    let buf = getline(1, '$')
    let file = readfile(a:filename)
    return s:CompareLines(buf, file)
endf


" Compare the current buffer with the contents of filename after 
" |:exe|cuting expr but ignore changes in whitespace.
function! should#yield#SqueezedBuffer(expr, filename) "{{{3
    call should#__Eval(a:expr)
    let buf = getline(1, '$')
    call s:Squeeze(buf)
    let file = readfile(a:filename)
    call s:Squeeze(file)
    return s:CompareLines(buf, file)
endf


function! s:CompareLines(lines1, lines2) "{{{3
    " TLogVAR a:lines1, a:lines2
    for i in range(len(a:lines1))
        let line1 = a:lines1[i]
        let line2 = a:lines2[i]
        if line1 != line2
            call should#__Explain('In line '. (i + 1) .': Expected '. line2 .' but got '. line1)
            return 0
        endif
    endfor
    return 1
endf


function! s:Squeeze(lines) "{{{3
    call map(a:lines, 'substitute(v:val, ''^\s\+'',   "",  "")')
    call map(a:lines, 'substitute(v:val, ''\s\+$'',   "",  "")')
    call map(a:lines, 'substitute(v:val, ''\s\{2,}'', " ", "g")')
endf


let &cpo = s:save_cpo
unlet s:save_cpo
autoload/spec.vim	[[[1
488
" spec.vim
" @Author:      Thomas Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2009-02-22.
" @Last Change: 2010-02-27.
" @Revision:    0.0.379

let s:save_cpo = &cpo
set cpo&vim
" call tlog#Log('Load: '. expand('<sfile>')) " vimtlib-sfile


exec SpecInit()


let s:rewrite_should = '\(be\|throw\|yield\|finish\)'
let s:rewrite_table = [
            \ ['^\s*not\s\+', '!'],
            \ ['^!\?\(should#\)\?finish\s\+\zsin\s\+\(\d\+\)\s\+seconds\?\s\+\(.*\)$', 'InSecs(\3, \2)'],
            \ ['^!\?'. s:rewrite_should .'\zs\s\+\(.\)', '#\u\2'],
            \ ['^!\?\zs'. s:rewrite_should .'#', 'should#&'],
            \ ['^!\?\(\l\w\+#\)*\u\w*\zs\s\+\(.\{-}\)\s*$', '(\2)'],
            \ ]
            " \ '^!\?\(be\|throw\|yield\)\zs\s\+\(.\)': '#\u\1',


" :nodoc:
function! spec#__Rewrite(string) "{{{3
    let string = a:string
    for [rx, subst] in s:rewrite_table
        " TLogVAR rx, subst
        let string = substitute(string, rx, subst, 'g')
        " TLogVAR string
    endfor
    let string = s:ResolveSIDs(string)
    " if s:spec_verbose && &verbose >= 6
    "     call s:Log(3, a:string .' => '. string)
    " endif
    return string
endf


" Define your own rewrite rules.
"
" Care must be taken so that the rule expands to a single atomic 
" statement. The pattern should always match from the beginning of the 
" string.
"
" Example: The following rule is wrong: >
"
"   \(\d\+\) be equal to \(\d\+\).* => \1 == \2
"
" because it doesn't match from the beginning of the string and because 
" the substiution breaks other rules like "not => !". The following is 
" preferable: >
"
"   ^!\?\zs\(\d\+\) be equal to \(\d\+\).* => (\1 == \2)
"
" This pattern expands the pattern only when found in the right position 
" and the substiution can be prefixed with !.
"
" You could then write: >
"
"   Should 1 be equal to 1
"   Should not 1 be equal to 2
function! spec#RewriteRule(rx, subst) "{{{3
    let rule = [a:rx, a:subst]
    if index(s:rewrite_table, rule) == -1
        call add(s:rewrite_table, rule)
    endif
endf


" :nodoc:
fun! s:ResolveSIDs(string, ...)
    if stridx(a:string, '<SID>') != -1
        if a:0 >= 1
            let snr = a:1
        elseif s:spec_context != ''
            let snr = s:GetSNR(s:spec_context)
        else
            let snr = 0
        endif
        if !empty(snr)
            let string = substitute(a:string, '<SID>', '<SNR>'.snr.'_', 'g')
            " TLogDBG a:string .': '. snr
            return string
            " else
            "     TLog 'spec: Unknown script context: '. a:string .' '. snr
        endif
    endif
    return a:string
endf


" :nodoc:
fun! s:GetSNR(file, ...)
    let update = a:0 >= 1 ? a:1 : 0
    call spec#__InitSNR(update)
    " echom "DBG ". string(s:scripts)
    let file = substitute(a:file, '[/\\]', '[\\\\/]', 'g')
    for fn in s:scripts
        if fn[1] =~ file.'$'
            return fn[0]
        endif
    endfor
    if !update
        return s:GetSNR(a:file, 1)
    else
        " TLog 'spec: Unknown script file: '. a:file
        return 0
    endif
endf


" fun! s:GetScript(sid, ...)
"     let update = a:0 >= 1 ? a:1 : 0
"     call spec#__InitSNR(update)
"     for fn in s:scripts
"         if fn[0] == a:sid
"             return fn[1]
"         endif
"     endfor
"     if !update
"         return s:GetScript(a:sid, 1)
"     else
"         TLog 'spec: Unknown SID: '. a:sid
"         return 0
"     endif
" endf


" :nodoc:
fun! spec#__InitSNR(update)
    if a:update || !exists('s:scripts')
        redir => scriptnames
        silent! scriptnames
        redir END
        let s:scripts = split(scriptnames, "\n")
        call map(s:scripts, '[matchstr(v:val, ''^\s*\zs\d\+''), matchstr(v:val, ''^\s*\d\+: \zs.*$'')]')
    endif
endf


" :nodoc:
function! spec#__Begin(args, sfile) "{{{3
    if !exists('g:spec_run')
        throw 'Spec: Run the spec with the :Spec command'
    endif
    let s:spec_args = s:ParseArgs(a:args, a:sfile)
    let s:spec_vars = keys(g:)
    call spec#__Comment('')
endf


" :nodoc:
function! spec#__End(args) "{{{3
    for v in a:args
        if v =~ '()$'
            exec 'delfunction '. matchstr(v, '^[^(]\+')
        else
            exec 'unlet! '. v
        endif
    endfor

    if exists('s:spec_vars')
        let vars = keys(g:)
        call filter(vars, 'index(s:spec_vars, v:val) == -1')
        " TLogVAR vars
        call map(vars, '"g:". v:val')
        " TLogVAR vars
        if !empty(vars)
            exec 'unlet! '. join(vars, ' ')
        endif
    endif
endf


function! spec#__Setup() "{{{3
    " TLog 'spec#__Setup'
    call should#__Init()
    let s:should_counts[s:CurrentFile()] += 1
    let rv = s:MaybeOpenScratch()
    exec get(s:spec_args, 'before', '')
    return rv
endf


function! spec#__Teardown() "{{{3
    exec get(s:spec_args, 'after', '')
    " let s:spec_comment = ''
    call s:MaybeCloseScratch()
endf


function! s:MaybeOpenScratch() "{{{3
    let scratch = get(s:spec_args, 'scratch', '')
    if !empty(scratch)
        if type(scratch) == 1
            let scratch_args = [scratch]
        else
            let scratch_args = scratch
        endif
        if scratch_args[0] == '%'
            let scratch_args[0] = s:CurrentFile()
        endif
        " TAssert should#be#Type(scratch, 'list')
        call call('spec#OpenScratch', scratch_args)
        return 1
    else
        return 0
    endif
endf


function! s:MaybeCloseScratch() "{{{3
    let scratch = get(s:spec_args, 'scratch', '')
    if !empty(scratch)
        call spec#CloseScratch()
    endif
endf


function! spec#__AddQFL(expr, reason)
    " TLogVAR a:expr, a:reason
    let ncmd = 0
    let idx = 1
    let lnum = idx
    " call tlog#Debug(string(keys(s:spec_files)))
    for line in s:spec_files[s:CurrentFile()]
        if line =~# '^\s*\(Should\|Replay\)\s\+'
            " if exists('g:loaded_tlib') && line =~ '^\s*spec!\?\s\+'. tlib#rx#Escape(a:expr) .'\s*$'
            "     let lnum = idx
            "     break
            " endif
            let ncmd += 1
            if ncmd == s:should_counts[s:CurrentFile()]
                let lnum = idx
                break
            endif
        endif
        let idx += 1
    endfor
    let qfl = [{
                \ 'filename': s:CurrentFile(),
                \ 'lnum': lnum,
                \ 'text': a:reason,
                \ }]
    if !empty(s:spec_comment)
        call insert(qfl, {
                    \ 'filename': s:CurrentFile(),
                    \ 'lnum': lnum,
                    \ 'text': s:spec_comment,
                    \ })
    endif
    call setqflist(qfl, 'a')
endf


function! spec#__Comment(string, ...) "{{{3
    if a:0 >= 1 && a:1
        let s:spec_comment = ''
        call spec#__AddQFL('', a:string)
        call s:Log(1, toupper(a:string))
    else
        let s:spec_comment = a:string
        call s:Log(1, a:string)
    endif
endf


function! s:Log(level, string) "{{{3
    " TLogVAR a:level, a:string
    if s:spec_verbose && !empty(a:string)
        let string = repeat(' ', (&sw * a:level)) . a:string
        if exists(':TLog')
            :TLog string
        else
            echom string
        endif
    endif
endf


function! spec#__Run(path, file, bang) "{{{3
    " TLogVAR a:path, a:file, a:bang
    if empty(a:path)
        let files = [a:file]
    elseif filereadable(a:path)
        let files = [a:path]
    else
        let files = split(globpath(a:path, '**/*.vim'), '\n')
        call filter(files, 'fnamemodify(v:val, ":t") !~ "^_"')
    endif
    " TLogVAR files

    cexpr []
    let s:spec_verbose = a:bang
    let s:spec_files = {}
    let s:should_counts = {}
    let s:spec_file = []
    let s:spec_args = {}
    let g:spec_run = 1
    call spec#__Comment('')
    for file in files
        " TLogVAR file
        call spec#Include(file, 1)
        " TLogVAR len(getqflist())
    endfor

    echo " "
    redraw
    let nfiles = len(keys(s:should_counts))
    let nshoulds = 0
    for cnt in values(s:should_counts)
        let nshoulds += cnt
    endfor
    echom 'Spec: Checked '. nshoulds .' specs in '. nfiles .' file(s)'
    unlet! s:spec_verbose s:spec_files s:spec_file s:should_counts g:spec_run

    if len(getqflist()) > 0
        try
            exec g:spec_cwindow
        catch
            echohl Error
            echom v:exception
            echohl NONE
        endtry
    endif
endf


function! spec#Include(filename, top_spec) "{{{3
    " TLogVAR a:filename, a:top_spec
    let filename0 = s:CanonicalFilename(a:filename)
    call s:PushFile(filename0)
    let s:spec_files[filename0] = readfile(a:filename)
    let source = 'source '. fnameescape(a:filename)
    let s:spec_perm = -1
    let qfl_size = len(getqflist())
    let options = g:spec_option_sets
    " TLogVAR options
    while qfl_size == len(getqflist()) && (s:spec_perm < 0 || (a:top_spec && spec#speckiller#OptionSets(options, s:spec_perm)))
        call s:Log(0, 'Spec ['. (s:spec_perm  + 1) .']: '. a:filename)
        let s:should_counts[filename0] = 0
        try
            exec source
            let options1 = get(s:spec_args, 'options', [])
            " TLogVAR options1
            if !empty(options1) && s:spec_perm < 0
                let options = extend(deepcopy(options), options1)
                " TLogVAR options
            endif
        catch
            let msg = v:exception .': '. v:throwpoint
            call spec#__AddQFL(source, msg)
            echohl Error
            echom msg
            echohl NONE
            break
        finally
            if a:top_spec
                if s:spec_perm >= 0
                    call spec#speckiller#Reset()
                endif
                call spec#__End(get(s:spec_args, 'cleanup', []))
            endif
        endtry
        let s:spec_perm += 1
    endwh
    call s:PopFile()
    return qfl_size == len(getqflist())
endf


function! s:PushFile(filename) "{{{3
    call insert(s:spec_file, a:filename)
endf


function! s:PopFile() "{{{3
    call remove(s:spec_file, 0)
endf


function! s:CurrentFile() "{{{3
    return s:spec_file[0]
endf


function! s:CanonicalFilename(filename) "{{{3
    let filename = substitute(a:filename, '\\', '/', 'g')
    let filename = substitute(filename, '^.\ze:/', '\u&', '')
    return filename
endf


" :nodoc:
function! s:ParseArgs(args, sfile) "{{{3
    let s:spec_msg = get(a:args, 'title', '')
    let s:spec_context = get(a:args, 'sfile', a:sfile)
    if !has_key(a:args, 'sfile')
        let a:args['sfile'] = s:spec_context
    endif
    return a:args
endf


" Evaluate an expression in the context of a script.
" Requires a call to |specInit()|.
fun! spec#Val(expr)
    let fn = s:ResolveSIDs('<SID>SpecVal')
    if empty(fn)
        echoerr 'Spec: Uninitialized script: '. a:script
        return ''
    else
        return call(function(fn), [a:expr])
    endif
endf


" :display: spec#ScratchBuffer(?filename="", ?filetype="") "{{{3
" Open the spec scratch buffer.
function! spec#OpenScratch(...) "{{{3
    if bufname('%') != '__SPEC_SCRATCH_BUFFER__'
        silent split __SPEC_SCRATCH_BUFFER__
    endif
    setlocal buftype=nofile
    setlocal bufhidden=hide
    setlocal noswapfile
    setlocal nobuflisted
    setlocal modifiable
    setlocal foldmethod=manual
    setlocal foldcolumn=0
    if a:0 >= 1
        if !empty(a:1)
            silent 1,$delete
            exec 'silent 1read '. fnameescape(a:1)
            silent 1delete
        endif
        if a:0 >= 2
            if !empty(a:2)
                exec 'set ft='. a:a2
            endif
        endif
    endif
endf


" Close the scratch buffer. (Requires the cursor to be located in the spec 
" scratch buffer.)
function! spec#CloseScratch() "{{{3
    if bufname('%') == '__SPEC_SCRATCH_BUFFER__' && winnr('$') > 1
        wincmd c
    endif
endf


function! spec#Feedkeys(sequence) "{{{3
    " TLogVAR a:sequence
    " try
        call feedkeys(a:sequence)
    " catch
    " endtry
endf


" Replay a recorded macro.
function! spec#Replay(macro) "{{{3
    " TLogVAR a:macro
    if s:CanonicalFilename(expand('%:p')) != s:CurrentFile()
        if !spec#__Setup()
            throw 'Spec: Replay: spec file must be current buffer'
        endif
    endif
    let s = @s
    try
        let @s = a:macro
        norm! @s
    finally
        let @s = s
    endtry
endf


let &cpo = s:save_cpo
unlet s:save_cpo
autoload/spec/speckiller.vim	[[[1
111
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2009-03-01.
" @Last Change: 2009-03-14.
" @Revision:    124

let s:save_cpo = &cpo
set cpo&vim


function! s:Init(options) "{{{3
    " TLogVAR a:options
    let s:options_initial = {}
    for o in keys(a:options)
        exec 'let s:options_initial[o] = '. o
    endfor
endf


function! spec#speckiller#Reset() "{{{3
    " TLog "SpecKiller: Reset"
    if exists('s:options_initial')
        for o in keys(s:options_initial)
            exec 'let '. o .' = s:options_initial[o]'
        endfor
    endif
endf


" Return the i'th option set.
function! spec#speckiller#OptionSets(options, i) "{{{3
    " TLog "spec#speckiller#OptionSets"
    " TLogVAR a:options, a:i
    if a:i >= len(a:options)
        return 0
    endif
    let options = a:options[a:i]
    if type(options) == 1
        if options == 'vim'
            let options0 = s:OptionsDefault(options)
        elseif options == 'vi'
            let options0 = s:OptionsDefault(options)
        endif
        unlet options
        let options = options0
    endif
    call s:Init(options)
    for [name, value] in items(options)
        exec 'let '. name .' = value'
        " TLog name
    endfor
    return 1
endf


let &cpo = s:save_cpo
unlet s:save_cpo


let s:option_file = expand('<sfile>:p:h') .'/options_default_'. hostname() .'.dat'
let s:option_blacklist = [
            \ 'all',
            \ 'compatible',
            \ 'guifont',
            \ 'modified',
            \ 'termcap',
            \ 'term',
            \ 'ttytype',
            \ 'vim',
            \ ]


function! s:OptionsDefault(...) "{{{3
	if !exists('s:option_default')
		if filereadable(s:option_file)
			exec 'let s:option_default = '. join(readfile(s:option_file, 'b'), "\n")
		else
			let default = '&'. (a:0 >= 1 ? a:1 : 'vim')
			" From: Andy Wokula
			" Date: Mon, 09 Feb 2009 23:56:25 +0100
			" Subject: Re: A few questions(accessing the Vim code in VimL)
			" http://groups.google.com/group/vim_dev/msg/80d91c0a5e2ef4e4?hl=en
			exec "silent normal! :set \<C-A>'\<C-B>\<C-Right>\<C-U>\<Del>let str='\r"
			let optnames = split(str)
			exec "silent normal! :setlocal \<C-A>'\<C-B>\<C-Right>\<C-U>\<Del>let str='\r"
			let optnames_local = split(str)
			let s:option_default = {}
			" TLogVAR &cpo, &viminfo
			for opt in optnames
				if index(s:option_blacklist, opt) == -1
					if index(optnames_local, opt) == -1
						let prefix = '&g:'
						let set = 'set'
					else
						let prefix = '&l:'
						let set = 'setlocal'
					endif
					exec 'let val = '. prefix . opt
					" TLogVAR opt, val
					exec set .' '. opt . default
					exec 'let s:option_default[prefix . opt] = '. prefix . opt
					exec 'let '. prefix . opt .' = val'
				endif
			endfor
			call writefile([string(s:option_default)], s:option_file, 'b')
		endif
	endif
	return s:option_default
endfunc 

spec/spec/_dont_include.vim	[[[1
15
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2009-03-06.
" @Last Change: 2009-03-06.

let s:save_cpo = &cpo
set cpo&vim


throw 'It should not automatically include this file'


let &cpo = s:save_cpo
unlet s:save_cpo
spec/spec/_include.vim	[[[1
30
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2009-03-06.
" @Last Change: 2009-03-06.

let s:save_cpo = &cpo
set cpo&vim

if exists('g:spec_run')
    finish
endif


unlet! g:spec_foo
exec 'Spec '. expand('<sfile>:p:h') .'/included.vim'

" TLog exists('*SpecFoo')
if exists('*SpecFoo')
    throw "Should not exists('*SpecFoo')"
endif

" TLog exists('g:spec_foo')
if exists('g:spec_foo')
    throw "Should not exists('g:spec_foo')"
endif


let &cpo = s:save_cpo
unlet s:save_cpo
spec/spec/_included.vim	[[[1
29
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2009-03-06.
" @Last Change: 2009-03-06.

let s:save_cpo = &cpo
set cpo&vim



SpecBegin 'title': 'Included File',
            \ 'cleanup': ['SpecFoo()']
            " \ 'sfile': '<+SCRIPT CONTEXT+>',
            " \ 'scratch': '<+SCRATCH FILE+>',
            " \ 'before': '<+BEFORE EX COMMANDS+>',
            " \ 'after': '<+AFTER EX COMMANDS+>'

It should remove temporary global variables & functions when done.
let g:spec_foo = 1

function! SpecFoo(a) "{{{3
    return a:a * 2
endf



let &cpo = s:save_cpo
unlet s:save_cpo
spec/spec/should_be.vim	[[[1
89
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2009-03-06.
" @Last Change: 2009-03-06.

let s:save_cpo = &cpo
set cpo&vim


SpecBegin 'title': 'Should be', 'sfile': 'autoload/should/be.vim'

It should test if the argument is a number.
Should be#Number(1)
Should not be#Number("foo")

It should test if the argument is a string.
Should be#String("foo")
Should not be#String(1)

It should test if the argument is a funcref.
Should be#Funcref(function('should#be#Funcref'))
Should not be#Funcref(1)

It should test if the argument is a list.
Should be#List([1,2,3])
Should not be#List(1)

It should test if the argument is a dictionary.
Should be#Dictionary({1:2})
Should not be#Dictionary([1,2,3])

It should test if the argument is of a specified type.
Should be#A(1, 'number')
Should be a {"foo": "bar"}, 'dictionary'
Should not be a {"foo": "bar"}, 'list'

It should test for multiple types.
Should be a {"foo": "bar"}, ['list', 'dictionary']
Should not be a {"foo": "bar"}, ['number', 'string']

It should test for class equivalence.
Should be a {"foo": "bar"}, {"foo": 0}
Should be a {"foo": "bar"}, {"foo": 1}
Should be a {"foo": "bar"}, {"x": 0}
Should not be a {"foo": "bar"}, {"x": 1}

It should test for equality.
Should be#Equal(1, 1)
Should be#Equal({1:2}, {1:2})
Should not be#Equal(1, 2)
Should not be#Equal(1, "1")
Should not be#Equal({1:2}, {1:3})

It should test for inequality.
Should be#Unequal(1, 2)
Should be#Unequal(1, "2")
Should be#Unequal(1, "1")
Should not be#Unequal(1, 1)

It should test for <, <=, >, >=.
Should be#Greater(2, 1)
Should not be#Greater(2, 2)

Should be#GreaterEqual(1, 1)
Should be#GreaterEqual(2, 1)
Should not be#GreaterEqual(2, 3)

Should be#Less(1, 2)
Should not be#Less(3, 2)

Should be#LessEqual(1, 1)
Should be#LessEqual(1, 2)
Should not be#LessEqual(3, 2)

It should match strings.
Should be like "foo", '^f'
Should not be like "foo", '^x'
Should be unlike "foo", '^x'
Should be like "foo", '^f', '#'
Should be unlike "foo", '^F', '#'
Should be unlike "Foo", '^f', '#'
Should be like "Foo", '^F', '#'
Should be like "foo", '^F', '?'
Should be like "Foo", '^f', '?'


let &cpo = s:save_cpo
unlet s:save_cpo
spec/spec/should_finish.vim	[[[1
36
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2009-03-06.
" @Last Change: 2009-03-07.

let s:save_cpo = &cpo
set cpo&vim


SpecBegin 'title': 'Should finish',
            \ 'sfile': 'autoload/should/finish.vim',
            \ 'cleanup': ['TakeTime()']

function! TakeTime(n) "{{{3
    for i in range(a:n)
    endfor
endf

echo "Spec 'finish': The following test could take up to 5 seconds."
It should measure execution time in seconds.
Should finish#InSecs(':2sleep', 3)
Should not finish#InSecs(':2sleep', 1)


if exists('g:loaded_tlib')

    It should measure in microseconds but this depends on your OS so it probably doesn't.
    Should finish#InMicroSecs('TakeTime(10)', 40)
    Should not finish#InMicroSecs('TakeTime(100000)', 20)

endif


let &cpo = s:save_cpo
unlet s:save_cpo
spec/spec/should_throw.vim	[[[1
23
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2009-03-06.
" @Last Change: 2009-03-06.

let s:save_cpo = &cpo
set cpo&vim


SpecBegin 'title': 'Should throw', 'sfile': 'autoload/should/throw.vim'

It should test for exceptions.
Should throw#Something('1 + [2]')
Should not throw#Something('1 + 2')

It should test for specific exceptions.
Should throw#Exception('1 + [2]', ':E745:')
Should not throw#Exception('1 + [2]', ':E746:')


let &cpo = s:save_cpo
unlet s:save_cpo
spec/spec/should_yield.vim	[[[1
25
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2009-03-06.
" @Last Change: 2009-03-06.

let s:save_cpo = &cpo
set cpo&vim


let g:test_file = expand('<sfile>:p:h') .'/'
SpecBegin 'title': 'Should yield', 'sfile': 'autoload/should/yield.vim',
            \ 'scratch': [g:test_file . "test_yield.txt"]

It should test buffer content.
Should yield#Buffer(':silent 1,3delete', g:test_file.'test_yield1.txt')
Should not yield#Buffer(':silent 1,3delete', g:test_file.'should_yield.vim')

It should test squeezed buffer content.
Should yield#SqueezedBuffer(':silent 1,3delete', g:test_file.'test_yield2.txt')
Should not yield#SqueezedBuffer(':silent 1,3delete', g:test_file.'should_yield.vim')


let &cpo = s:save_cpo
unlet s:save_cpo
spec/spec/spec.vim	[[[1
91
" @Author:      Thomas Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2009-02-22.
" @Last Change: 2009-03-06.

let s:save_cpo = &cpo
set cpo&vim

let s:vars = len(g:)

let g:spec_bar = 1


SpecBegin 'title': 'Self-test',
            \ 'scratch': '%',
            \ 'sfile': 'autoload/spec.vim',
            \ 'before': 'let g:spec_bar = 2',
            \ 'after': 'let g:spec_bar = 1'


It should initialize the environment.
Should be#Equal(spec#Val('s:spec_vars'), keys(g:))
Should be#Equal(spec#Val('s:spec_msg'), 'Self-test')

Should be#Equal(spec#Val('s:should_counts['. string(<SID>CurrentFile()) .']'), 3)

Should be#Dictionary(spec#Val('s:spec_args'))
Should be#NotEmpty(spec#Val('s:spec_args'))

Should be#List(spec#Val('s:scripts'))
Should be#NotEmpty(spec#Val('s:scripts'))


It should execute before & after ex commands.
Should be#Equal(g:spec_bar, 2)
if g:spec_bar != 1
    throw 'Teardown failed'
endif


It should remember comments.
Should be#Equal(spec#Val('s:spec_comment'), 'It should remember comments.')
Should !be#Equal(spec#Val('s:spec_comment'), 'Foo')
Should be#NotEmpty(spec#Val('s:spec_comment'))


It should rewrite expressions.
Should be#Equal(spec#__Rewrite('not be#Equal'), '!should#be#Equal')
Should be#Equal(spec#__Rewrite('finish in 1 second "Fun2()"'), 'should#finish#InSecs("Fun2()", 1)')
Should be#Equal(spec#__Rewrite('not finish in 2 seconds "Fun2()"'), '!should#finish#InSecs("Fun2()", 2)')
Should be#Equal(spec#__Rewrite("throw something '1 + [1]'"), "should#throw#Something('1 + [1]')")
Should be#Equal(spec#__Rewrite("not exists('*SpecFoo')"), "!exists('*SpecFoo')")

" Should be#Equal(spec#__Rewrite('not be equal'), '!should#be#Equal')
Should not be#Equal(spec#__Rewrite('not be#Equal'), 'foo')
Should not be equal(spec#__Rewrite('not be#Equal'), 'foo')
Should not be Equal spec#__Rewrite('not be#Equal'), 'foo'
Should not be equal spec#__Rewrite('not be#Equal'), 'foo'


It should be able to access script-local functions.
Should be#Equal(<SID>CanonicalFilename('a:\foo/bar'), 'A:/foo/bar')


let g:spec_qfl_len = len(getqflist())
It should fail. Please ignore the entry below unless there is no descriptive explanation.
Should be#Equal("fail", "should")
It should integrate with the quickfix list.
Should be#Equal(len(getqflist()), g:spec_qfl_len + 2)


It! should always add this message to the quickfix list.
Should be#Equal(len(getqflist()), g:spec_qfl_len + 3)


It should replay key sequences.
Replay :let g:spec_char1 = getchar()\n\<f11>
Should be#Equal g:spec_char1, "€F1"


It should replay macros.
call spec#Replay(':let g:spec_char2 = getchar()€F1')
Should be#Equal g:spec_char2, "€F1"
unlet g:spec_char2
Replay! :let g:spec_char2 = getchar()€F1
Should be#Equal g:spec_char2, "€F1"


let &cpo = s:save_cpo
unlet s:save_cpo
spec/spec/speckiller.vim	[[[1
37
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2009-03-06.
" @Last Change: 2009-03-14.

let s:save_cpo = &cpo
set cpo&vim


if !exists('g:spec_foo')
    let g:spec_foo = "foo"
endif

SpecBegin 'title': 'Option sets',
            \ 'sfile': 'autoload/spec.vim',
            \ 'options': [
            \ {'&l:hidden': 1, '&acd': 0, 'g:spec_foo': 'bar'},
            \ {'&l:hidden': 0, '&acd': 1, 'g:spec_foo': 'bar'},
            \ ]

" echom "Round ". spec#Val('s:spec_perm')

if spec#Val('s:spec_perm') >= 0
    It should test the spec against option sets (:SpecBegin).
    Should &hidden || &acd
    Should not (&hidden && &acd)
    Should be equal &hidden + &acd, 1

    Should be equal g:spec_foo, 'bar'
else
    Should be equal g:spec_foo, 'foo'
endif


let &cpo = s:save_cpo
unlet s:save_cpo
spec/spec/test_yield.txt	[[[1
7
        1          1                1
            2          2
        3
            4         4
    5   5
    6 6
    77
spec/spec/test_yield1.txt	[[[1
4
            4         4
    5   5
    6 6
    77
spec/spec/test_yield2.txt	[[[1
4
4 4
5   5
6 6
77
