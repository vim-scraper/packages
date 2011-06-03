" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
doc/tcomment.txt	[[[1
342
*tcomment.txt*  An easily extensible & universal comment plugin
                Author: Thomas Link, micathom AT gmail com?subject=vim

tcomment provides easy to use, file-type sensible comments for Vim. It 
can handle embedded syntax.

TComment works like a toggle, i.e., it will comment out text that 
contains uncommented lines, and it will remove comment markup for 
already commented text (i.e. text that contains no uncommented lines).

If the file-type is properly defined, TComment will figure out which 
comment string to use. Otherwise you use |tcomment#DefineType()| to 
override the default choice.

TComment can properly handle an embedded syntax, e.g., ruby/python/perl 
regions in vim scripts, HTML or JavaScript in php code etc.

Demo:
http://vimsomnia.blogspot.com/2010/11/tcomment-vim-plugin.html


                                                    *tcomment-maps*
Key bindings~

Most of the time the default toggle keys will do what you want (or to be 
more precise: what I think you want it to do ;-).

                                                    *g:tcommentMapLeaderOp1*
                                                    *g:tcommentMapLeaderOp2*
As operator (the prefix can be customized via g:tcommentMapLeaderOp1 
and g:tcommentMapLeaderOp2):

    gc{motion}   :: Toggle comments (for small comments within one line 
                    the &filetype_inline style will be used, if 
                    defined)
    gcc          :: Toggle comment for the current line
    gC{motion}   :: Comment region
    gCc          :: Comment the current line

By default the cursor stays put. If you want the cursor to the end of 
the commented text, set |g:tcommentOpModeExtra| to '>' (but this may not 
work properly with exclusive motions).

Primary key maps:

    <c-_><c-_>   :: :TComment
    <c-_><space> :: :TComment <QUERY COMMENT-BEGIN ?COMMENT-END>
    <c-_>b       :: :TCommentBlock
    <c-_>a       :: :TCommentAs <QUERY COMMENT TYPE>
    <c-_>n       :: :TCommentAs &filetype <QUERY COUNT>
    <c-_>s       :: :TCommentAs &filetype_<QUERY COMMENT SUBTYPE>
    <c-_>i       :: :TCommentInline
    <c-_>r       :: :TCommentRight
    <c-_>p       :: Comment the current inner paragraph

A secondary set of key maps is defined for normal mode.

    <Leader>__       :: :TComment
    <Leader>_p       :: Comment the current inner paragraph
    <Leader>_<space> :: :TComment <QUERY COMMENT-BEGIN ?COMMENT-END>
    <Leader>_i       :: :TCommentInline
    <Leader>_r       :: :TCommentRight
    <Leader>_b       :: :TCommentBlock
    <Leader>_a       :: :TCommentAs <QUERY COMMENT TYPE>
    <Leader>_n       :: :TCommentAs &filetype <QUERY COUNT>
    <Leader>_s       :: :TCommentAs &filetype_<QUERY COMMENT SUBTYPE>


-----------------------------------------------------------------------
Install~

Edit the vba file and type: >

    :so %

See :help vimball for details. If you have difficulties or use vim 7.0, 
please make sure, you have the current version of vimball (vimscript 
#1502) installed or update your runtime.


========================================================================
Contents~

        :TComment ........................... |:TComment|
        :TCommentAs ......................... |:TCommentAs|
        :TCommentRight ...................... |:TCommentRight|
        :TCommentBlock ...................... |:TCommentBlock|
        :TCommentInline ..................... |:TCommentInline|
        :TCommentMaybeInline ................ |:TCommentMaybeInline|
        g:tcommentBlankLines ................ |g:tcommentBlankLines|
        g:tcommentOpModeExtra ............... |g:tcommentOpModeExtra|
        g:tcommentOptions ................... |g:tcommentOptions|
        g:tcommentGuessFileType ............. |g:tcommentGuessFileType|
        g:tcommentGuessFileType_dsl ......... |g:tcommentGuessFileType_dsl|
        g:tcommentGuessFileType_php ......... |g:tcommentGuessFileType_php|
        g:tcommentGuessFileType_html ........ |g:tcommentGuessFileType_html|
        g:tcommentGuessFileType_tskeleton ... |g:tcommentGuessFileType_tskeleton|
        g:tcommentGuessFileType_vim ......... |g:tcommentGuessFileType_vim|
        g:tcommentGuessFileType_django ...... |g:tcommentGuessFileType_django|
        g:tcommentIgnoreTypes_php ........... |g:tcommentIgnoreTypes_php|
        g:tcomment#syntax_substitute ........ |g:tcomment#syntax_substitute|
        g:tcommentSyntaxMap ................. |g:tcommentSyntaxMap|
        g:tcommentBlockC .................... |g:tcommentBlockC|
        g:tcommentBlockC2 ................... |g:tcommentBlockC2|
        g:tcommentInlineC ................... |g:tcommentInlineC|
        g:tcommentBlockXML .................. |g:tcommentBlockXML|
        g:tcommentInlineXML ................. |g:tcommentInlineXML|
        tcomment#DefineType ................. |tcomment#DefineType()|
        tcomment#Comment .................... |tcomment#Comment()|
        tcomment#Operator ................... |tcomment#Operator()|
        tcomment#OperatorLine ............... |tcomment#OperatorLine()|
        tcomment#OperatorAnyway ............. |tcomment#OperatorAnyway()|
        tcomment#OperatorLineAnyway ......... |tcomment#OperatorLineAnyway()|
        tcomment#CommentAs .................. |tcomment#CommentAs()|


========================================================================
plugin/tcomment.vim~

                                                    *:TComment*
:[range]TComment[!] ?ARGS...
    If there is a visual selection that begins and ends in the same line, 
    then |:TCommentInline| is used instead.
    The optional range defaults to the current line. With a bang '!', 
    always comment the line.
    
    ARGS... are either (see also |tcomment#Comment()|):
      1. a list of key=value pairs
      2. 1-2 values for: ?commentBegin, ?commentEnd

                                                    *:TCommentAs*
:[range]TCommentAs[!] commenttype ?ARGS...
    TCommentAs requires g:tcomment_{filetype} to be defined.
    With a bang '!', always comment the line.
    
    ARGS... are either (see also |tcomment#Comment()|):
      1. a list of key=value pairs
      2. 1-2 values for: ?commentBegin, ?commentEnd

                                                    *:TCommentRight*
:[range]TCommentRight[!] ?ARGS...
    Comment the text to the right of the cursor. If a visual selection was 
    made (be it block-wise or not), all lines are commented out at from 
    the current cursor position downwards.
    With a bang '!', always comment the line.
    
    ARGS... are either (see also |tcomment#Comment()|):
      1. a list of key=value pairs
      2. 1-2 values for: ?commentBegin, ?commentEnd

                                                    *:TCommentBlock*
:[range]TCommentBlock[!] ?ARGS...
    Comment as "block", e.g. use the {&ft}_block comment style. The 
    commented text isn't indented or reformated.
    With a bang '!', always comment the line.
    
    ARGS... are either (see also |tcomment#Comment()|):
      1. a list of key=value pairs
      2. 1-2 values for: ?commentBegin, ?commentEnd

                                                    *:TCommentInline*
:[range]TCommentInline[!] ?ARGS...
    Use the {&ft}_inline comment style.
    With a bang '!', always comment the line.
    
    ARGS... are either (see also |tcomment#Comment()|):
      1. a list of key=value pairs
      2. 1-2 values for: ?commentBegin, ?commentEnd

                                                    *:TCommentMaybeInline*
:[range]TCommentMaybeInline[!] ?ARGS...
    With a bang '!', always comment the line.
    
    ARGS... are either (see also |tcomment#Comment()|):
      1. a list of key=value pairs
      2. 1-2 values for: ?commentBegin, ?commentEnd


========================================================================
autoload/tcomment.vim~

                                                    *g:tcommentBlankLines*
g:tcommentBlankLines           (default: 1)
    If true, comment blank lines too

                                                    *g:tcommentOpModeExtra*
g:tcommentOpModeExtra          (default: '')
    Modifies how the operator works.
      > ... Move the cursor to the end of the comment

                                                    *g:tcommentOptions*
g:tcommentOptions              (default: {})
    Other key-value options used by |tcomment#Comment()|.
    
    Example: If you want to put the opening comment marker always in 
    the first column regardless of the block's indentation, put this 
    into your |vimrc| file: >
      let g:tcommentOptions = {'col': 1}
<

                                                    *g:tcommentGuessFileType*
g:tcommentGuessFileType        (default: 0)
    Guess the file type based on syntax names always or for some fileformat only
    If non-zero, try to guess filetypes.
    tcomment also checks g:tcommentGuessFileType_{&filetype} for 
    filetype specific values.
    
    Values:
      0        ... don't guess
      1        ... guess
      FILETYPE ... assume this filetype

                                                    *g:tcommentGuessFileType_dsl*
g:tcommentGuessFileType_dsl    (default: 'xml')
    For dsl documents, assumet filetype = xml.

                                                    *g:tcommentGuessFileType_php*
g:tcommentGuessFileType_php    (default: 'html')
    In php documents, the php part is usually marked as phpRegion. We 
    thus assume that the buffers default comment style isn't php but 
    html.

                                                    *g:tcommentGuessFileType_html*
g:tcommentGuessFileType_html   (default: 1)

                                                    *g:tcommentGuessFileType_tskeleton*
g:tcommentGuessFileType_tskeleton (default: 1)

                                                    *g:tcommentGuessFileType_vim*
g:tcommentGuessFileType_vim    (default: 1)

                                                    *g:tcommentGuessFileType_django*
g:tcommentGuessFileType_django (default: 1)

                                                    *g:tcommentIgnoreTypes_php*
g:tcommentIgnoreTypes_php      (default: 'sql')
    In php files, some syntax regions are wongly highlighted as sql 
    markup. We thus ignore sql syntax when guessing the filetype in 
    php files.

                                                    *g:tcomment#syntax_substitute*
g:tcomment#syntax_substitute   (default: {...})

                                                    *g:tcommentSyntaxMap*
g:tcommentSyntaxMap            (default: {...})
    tcomment guesses filetypes based on the name of the current syntax 
    region. This works well if the syntax names match 
    /filetypeSomeName/. Other syntax names have to be explicitly 
    mapped onto the corresponding filetype.

                                                    *g:tcommentBlockC*
g:tcommentBlockC               (default: {...})
    Generic c-like block comments.

                                                    *g:tcommentBlockC2*
g:tcommentBlockC2              (default: {...})
    Generic c-like block comments (alternative markup).

                                                    *g:tcommentInlineC*
g:tcommentInlineC              (default: "/* %s */")
    Generic c-like comments.

                                                    *g:tcommentBlockXML*
g:tcommentBlockXML             (default: "<!--%s-->\n  ")
    Generic xml-like block comments.

                                                    *g:tcommentInlineXML*
g:tcommentInlineXML            (default: "<!-- %s -->")
    Generic xml-like comments.

                                                    *tcomment#DefineType()*
tcomment#DefineType(name, commentdef)
    If you don't explicitly define a comment style, |:TComment| will use 
    'commentstring' instead. We override the default values here in order 
    to have a blank after the comment marker. Block comments work only if 
    we explicitly define the markup.
    
    The comment definition can be either a string or a dictionary.
    
    If it is a string:
    The format for block comments is similar to 'commentstrings' with the 
    exception that the format strings for blocks can contain a second line 
    that defines how "middle lines" (see :h format-comments) should be 
    displayed.
    
    If it is a dictionary:
    See the help on the args argument of |tcomment#Comment|.

                                                    *tcomment#Comment()*
tcomment#Comment(beg, end, ...)
    tcomment#Comment(line1, line2, ?commentMode, ?commentAnyway, ?args...)
    args... are either:
      1. a list of key=value pairs where known keys are (see also 
         |g:tcommentOptions|):
            as=STRING     ... Use a specific comment definition
            col=N         ... Start the comment at column N (in block mode; must 
                              be smaller than |indent()|)
            mode=STRING   ... See the notes below on the "commentMode" argument
            begin=STRING  ... Comment prefix
            end=STRING    ... Comment postfix
            middle=STRING ... Middle line comments in block mode
            rxbeg=N       ... Regexp to find the substring of "begin" that 
                              should be multipied by "count"
            rxend=N       ... The above for "end"
            rxmid=N       ... The above for "middle"
      2. 1-2 values for: ?commentPrefix, ?commentPostfix
      3. a dictionary (internal use only)
    
    commentMode:
      G ... guess the value of commentMode
      B ... block (use extra lines for the comment markers)
      i ... maybe inline, guess
      I ... inline
      R ... right (comment the line right of the cursor)
      v ... visual
      o ... operator
    By default, each line in range will be commented by adding the comment 
    prefix and postfix.

                                                    *tcomment#Operator()*
tcomment#Operator(type, ...)

                                                    *tcomment#OperatorLine()*
tcomment#OperatorLine(type)

                                                    *tcomment#OperatorAnyway()*
tcomment#OperatorAnyway(type)

                                                    *tcomment#OperatorLineAnyway()*
tcomment#OperatorLineAnyway(type)

                                                    *tcomment#CommentAs()*
tcomment#CommentAs(beg, end, commentAnyway, filetype, ?args...)
    Where args is either:
      1. A count NUMBER
      2. An args list (see the notes on the "args" argument of 
         |tcomment#Comment()|)
    comment text as if it were of a specific filetype



vim:tw=78:fo=tcq2:isk=!-~,^*,^|,^":ts=8:ft=help:norl:
plugin/tcomment.vim	[[[1
143
" tComment.vim -- An easily extensible & universal comment plugin 
" @Author:      Tom Link (micathom AT gmail com)
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     27-Dez-2004.
" @Last Change: 2010-12-07.
" @Revision:    716
" 
" GetLatestVimScripts: 1173 1 tcomment.vim

if &cp || exists('loaded_tcomment')
    finish
endif
let loaded_tcomment = 204

if !exists("g:tcommentMapLeader1")
    " g:tcommentMapLeader1 should be a shortcut that can be used with 
    " map, imap, vmap.
    let g:tcommentMapLeader1 = '<c-_>'
endif
if !exists("g:tcommentMapLeader2")
    " g:tcommentMapLeader2 should be a shortcut that can be used with 
    " map, xmap.
    let g:tcommentMapLeader2 = '<Leader>_'
endif
if !exists("g:tcommentMapLeaderOp1")
    let g:tcommentMapLeaderOp1 = 'gc'
endif
if !exists("g:tcommentMapLeaderOp2")
    let g:tcommentMapLeaderOp2 = 'gC'
endif


" :display: :[range]TComment[!] ?ARGS...
" If there is a visual selection that begins and ends in the same line, 
" then |:TCommentInline| is used instead.
" The optional range defaults to the current line. With a bang '!', 
" always comment the line.
"
" ARGS... are either (see also |tcomment#Comment()|):
"   1. a list of key=value pairs
"   2. 1-2 values for: ?commentBegin, ?commentEnd
command! -bang -range -nargs=* -complete=customlist,tcomment#CompleteArgs TComment
            \ keepjumps call tcomment#Comment(<line1>, <line2>, 'G', "<bang>", <f-args>)

" :display: :[range]TCommentAs[!] commenttype ?ARGS...
" TCommentAs requires g:tcomment_{filetype} to be defined.
" With a bang '!', always comment the line.
"
" ARGS... are either (see also |tcomment#Comment()|):
"   1. a list of key=value pairs
"   2. 1-2 values for: ?commentBegin, ?commentEnd
command! -bang -complete=customlist,tcomment#Complete -range -nargs=+ TCommentAs 
            \ call tcomment#CommentAs(<line1>, <line2>, "<bang>", <f-args>)

" :display: :[range]TCommentRight[!] ?ARGS...
" Comment the text to the right of the cursor. If a visual selection was 
" made (be it block-wise or not), all lines are commented out at from 
" the current cursor position downwards.
" With a bang '!', always comment the line.
"
" ARGS... are either (see also |tcomment#Comment()|):
"   1. a list of key=value pairs
"   2. 1-2 values for: ?commentBegin, ?commentEnd
command! -bang -range -nargs=* -complete=customlist,tcomment#CompleteArgs TCommentRight
            \ keepjumps call tcomment#Comment(<line1>, <line2>, 'R', "<bang>", <f-args>)

" :display: :[range]TCommentBlock[!] ?ARGS...
" Comment as "block", e.g. use the {&ft}_block comment style. The 
" commented text isn't indented or reformated.
" With a bang '!', always comment the line.
"
" ARGS... are either (see also |tcomment#Comment()|):
"   1. a list of key=value pairs
"   2. 1-2 values for: ?commentBegin, ?commentEnd
command! -bang -range -nargs=* -complete=customlist,tcomment#CompleteArgs TCommentBlock
            \ keepjumps call tcomment#Comment(<line1>, <line2>, 'B', "<bang>", <f-args>)

" :display: :[range]TCommentInline[!] ?ARGS...
" Use the {&ft}_inline comment style.
" With a bang '!', always comment the line.
"
" ARGS... are either (see also |tcomment#Comment()|):
"   1. a list of key=value pairs
"   2. 1-2 values for: ?commentBegin, ?commentEnd
command! -bang -range -nargs=* -complete=customlist,tcomment#CompleteArgs TCommentInline
            \ keepjumps call tcomment#Comment(<line1>, <line2>, 'I', "<bang>", <f-args>)

" :display: :[range]TCommentMaybeInline[!] ?ARGS...
" With a bang '!', always comment the line.
"
" ARGS... are either (see also |tcomment#Comment()|):
"   1. a list of key=value pairs
"   2. 1-2 values for: ?commentBegin, ?commentEnd
command! -bang -range -nargs=* -complete=customlist,tcomment#CompleteArgs TCommentMaybeInline
            \ keepjumps call tcomment#Comment(<line1>, <line2>, 'i', "<bang>", <f-args>)



if (g:tcommentMapLeader1 != '')
    exec 'noremap <silent> '. g:tcommentMapLeader1 . g:tcommentMapLeader1 .' :TComment<cr>'
    exec 'vnoremap <silent> '. g:tcommentMapLeader1 . g:tcommentMapLeader1 .' :TCommentMaybeInline<cr>'
    exec 'inoremap <silent> '. g:tcommentMapLeader1 . g:tcommentMapLeader1 .' <c-o>:TComment<cr>'
    exec 'noremap <silent> '. g:tcommentMapLeader1 .'p m`vip:TComment<cr>``'
    exec 'inoremap <silent> '. g:tcommentMapLeader1 .'p <c-o>:norm! m`vip<cr>:TComment<cr><c-o>``'
    exec 'noremap '. g:tcommentMapLeader1 .'<space> :TComment '
    exec 'inoremap '. g:tcommentMapLeader1 .'<space> <c-o>:TComment '
    exec 'inoremap <silent> '. g:tcommentMapLeader1 .'r <c-o>:TCommentRight<cr>'
    exec 'noremap <silent> '. g:tcommentMapLeader1 .'r :TCommentRight<cr>'
    exec 'vnoremap <silent> '. g:tcommentMapLeader1 .'i :TCommentInline<cr>'
    exec 'vnoremap <silent> '. g:tcommentMapLeader1 .'r :TCommentRight<cr>'
    exec 'noremap '. g:tcommentMapLeader1 .'b :TCommentBlock<cr>'
    exec 'inoremap '. g:tcommentMapLeader1 .'b <c-o>:TCommentBlock<cr>'
    exec 'noremap '. g:tcommentMapLeader1 .'a :TCommentAs '
    exec 'inoremap '. g:tcommentMapLeader1 .'a <c-o>:TCommentAs '
    exec 'noremap '. g:tcommentMapLeader1 .'n :TCommentAs <c-r>=&ft<cr> '
    exec 'inoremap '. g:tcommentMapLeader1 .'n <c-o>:TCommentAs <c-r>=&ft<cr> '
    exec 'noremap '. g:tcommentMapLeader1 .'s :TCommentAs <c-r>=&ft<cr>_'
    exec 'inoremap '. g:tcommentMapLeader1 .'s <c-o>:TCommentAs <c-r>=&ft<cr>_'
endif
if (g:tcommentMapLeader2 != '')
    exec 'noremap <silent> '. g:tcommentMapLeader2 .'_ :TComment<cr>'
    exec 'xnoremap <silent> '. g:tcommentMapLeader2 .'_ :TCommentMaybeInline<cr>'
    exec 'noremap <silent> '. g:tcommentMapLeader2 .'p vip:TComment<cr>'
    exec 'noremap '. g:tcommentMapLeader2 .'<space> :TComment '
    exec 'xnoremap <silent> '. g:tcommentMapLeader2 .'i :TCommentInline<cr>'
    exec 'noremap <silent> '. g:tcommentMapLeader2 .'r :TCommentRight<cr>'
    exec 'xnoremap <silent> '. g:tcommentMapLeader2 .'r :TCommentRight<cr>'
    exec 'noremap '. g:tcommentMapLeader2 .'b :TCommentBlock<cr>'
    exec 'noremap '. g:tcommentMapLeader2 .'a :TCommentAs '
    exec 'noremap '. g:tcommentMapLeader2 .'n :TCommentAs <c-r>=&ft<cr> '
    exec 'noremap '. g:tcommentMapLeader2 .'s :TCommentAs <c-r>=&ft<cr>_'
endif
if (g:tcommentMapLeaderOp1 != '')
    exec 'nnoremap <silent> '. g:tcommentMapLeaderOp1 .' :let w:tcommentPos = getpos(".") \| set opfunc=tcomment#Operator<cr>g@'
    exec 'nnoremap <silent> '. g:tcommentMapLeaderOp1 .'c :let w:tcommentPos = getpos(".") \| set opfunc=tcomment#OperatorLine<cr>g@$'
    exec 'xnoremap <silent> '. g:tcommentMapLeaderOp1 .' :TCommentMaybeInline<cr>'
endif 
if (g:tcommentMapLeaderOp2 != '')
    exec 'nnoremap <silent> '. g:tcommentMapLeaderOp2 .' :let w:tcommentPos = getpos(".") \| set opfunc=tcomment#OperatorAnyway<cr>g@'
    exec 'nnoremap <silent> '. g:tcommentMapLeaderOp2 .'c :let w:tcommentPos = getpos(".") \| set opfunc=tcomment#OperatorLineAnyway<cr>g@$'
    exec 'xnoremap <silent> '. g:tcommentMapLeaderOp2 .' :TCommentMaybeInline<cr>'
endif 

autoload/tcomment.vim	[[[1
981
" tcomment.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-09-17.
" @Last Change: 2011-01-20.
" @Revision:    0.0.315

" call tlog#Log('Load: '. expand('<sfile>')) " vimtlib-sfile

if !exists("g:tcommentBlankLines")
    " If true, comment blank lines too
    let g:tcommentBlankLines = 1    "{{{2
endif

if !exists("g:tcommentOpModeExtra")
    " Modifies how the operator works.
    "   > ... Move the cursor to the end of the comment
    let g:tcommentOpModeExtra = ''   "{{{2
endif

if !exists('g:tcommentOptions')
    " Other key-value options used by |tcomment#Comment()|.
    "
    " Example: If you want to put the opening comment marker always in 
    " the first column regardless of the block's indentation, put this 
    " into your |vimrc| file: >
    "   let g:tcommentOptions = {'col': 1}
    let g:tcommentOptions = {}   "{{{2
endif

if !exists("g:tcommentGuessFileType")
    " Guess the file type based on syntax names always or for some fileformat only
    " If non-zero, try to guess filetypes.
    " tcomment also checks g:tcommentGuessFileType_{&filetype} for 
    " filetype specific values.
    "
    " Values:
    "   0        ... don't guess
    "   1        ... guess
    "   FILETYPE ... assume this filetype
    let g:tcommentGuessFileType = 0   "{{{2
endif
if !exists("g:tcommentGuessFileType_dsl")
    " For dsl documents, assumet filetype = xml.
    let g:tcommentGuessFileType_dsl = 'xml'   "{{{2
endif
if !exists("g:tcommentGuessFileType_php")
    " In php documents, the php part is usually marked as phpRegion. We 
    " thus assume that the buffers default comment style isn't php but 
    " html.
    let g:tcommentGuessFileType_php = 'html'   "{{{2
endif
if !exists("g:tcommentGuessFileType_html")
    let g:tcommentGuessFileType_html = 1   "{{{2
endif
if !exists("g:tcommentGuessFileType_tskeleton")
    let g:tcommentGuessFileType_tskeleton = 1   "{{{2
endif
if !exists("g:tcommentGuessFileType_vim")
    let g:tcommentGuessFileType_vim = 1   "{{{2
endif
if !exists("g:tcommentGuessFileType_django")
    let g:tcommentGuessFileType_django = 1   "{{{2
endif

if !exists("g:tcommentIgnoreTypes_php")
    " In php files, some syntax regions are wongly highlighted as sql 
    " markup. We thus ignore sql syntax when guessing the filetype in 
    " php files.
    let g:tcommentIgnoreTypes_php = 'sql'   "{{{2
endif

if !exists('g:tcomment#syntax_substitute')
    " :read: let g:tcomment#syntax_substitute = {...}   "{{{2
    " Perform replacements on the syntax name.
    let g:tcomment#syntax_substitute = {
                \ '\C^javaScript': {'sub': 'javascript'}
                \ }
endif

if !exists('g:tcommentSyntaxMap')
    " tcomment guesses filetypes based on the name of the current syntax 
    " region. This works well if the syntax names match 
    " /filetypeSomeName/. Other syntax names have to be explicitly 
    " mapped onto the corresponding filetype.
    " :read: let g:tcommentSyntaxMap = {...}   "{{{2
    let g:tcommentSyntaxMap = {
            \ 'vimMzSchemeRegion': 'scheme',
            \ 'vimPerlRegion':     'perl',
            \ 'vimPythonRegion':   'python',
            \ 'vimRubyRegion':     'ruby',
            \ 'vimTclRegion':      'tcl',
            \ }
endif

if !exists("g:tcommentBlockC")
    " Generic c-like block comments.
    " :read: let g:tcommentBlockC = {...}   "{{{2
    let g:tcommentBlockC = {
                \ 'commentstring': '/*%s */',
                \ 'middle': ' * ',
                \ 'rxbeg': '\*\+',
                \ 'rxend': '\*\+',
                \ 'rxmid': '\*\+',
                \ }
endif
if !exists("g:tcommentBlockC2")
    " Generic c-like block comments (alternative markup).
    " :read: let g:tcommentBlockC2 = {...}   "{{{2
    let g:tcommentBlockC2 = {
                \ 'commentstring': '/**%s */',
                \ 'middle': ' * ',
                \ 'rxbeg': '\*\+',
                \ 'rxend': '\*\+',
                \ 'rxmid': '\*\+',
                \ }
endif
if !exists("g:tcommentInlineC")
    " Generic c-like comments.
    let g:tcommentInlineC = "/* %s */"   "{{{2
endif

if !exists("g:tcommentBlockXML")
    " Generic xml-like block comments.
    let g:tcommentBlockXML = "<!--%s-->\n  "   "{{{2
endif
if !exists("g:tcommentInlineXML")
    " Generic xml-like comments.
    let g:tcommentInlineXML = "<!-- %s -->"   "{{{2
endif

let s:typesDirty = 1

let s:definitions = {}

" If you don't explicitly define a comment style, |:TComment| will use 
" 'commentstring' instead. We override the default values here in order 
" to have a blank after the comment marker. Block comments work only if 
" we explicitly define the markup.
"
" The comment definition can be either a string or a dictionary.
"
" If it is a string:
" The format for block comments is similar to 'commentstrings' with the 
" exception that the format strings for blocks can contain a second line 
" that defines how "middle lines" (see :h format-comments) should be 
" displayed.
" 
" If it is a dictionary:
" See the help on the args argument of |tcomment#Comment|.
function! tcomment#DefineType(name, commentdef)
    if !has_key(s:definitions, a:name)
        if type(a:commentdef) == 4
            let cdef = copy(a:commentdef)
        else
            let cdef = a:0 >= 1 ? a:1 : {}
            let cdef.commentstring = a:commentdef
        endif
        let s:definitions[a:name] = cdef
    endif
    let s:typesDirty = 1
endf

" :nodoc:
" Return 1 if a comment type is defined.
function! tcomment#TypeExists(name)
    return has_key(s:definitions, a:name)
endf

call tcomment#DefineType('aap',              '# %s'             )
call tcomment#DefineType('ada',              '-- %s'            )
call tcomment#DefineType('apache',           '# %s'             )
call tcomment#DefineType('autoit',           '; %s'             )
call tcomment#DefineType('asm',              '; %s'             )
call tcomment#DefineType('awk',              '# %s'             )
call tcomment#DefineType('catalog',          '-- %s --'         )
call tcomment#DefineType('catalog_block',    "--%s--\n  "       )
call tcomment#DefineType('cpp',              '// %s'            )
call tcomment#DefineType('cpp_inline',       g:tcommentInlineC  )
call tcomment#DefineType('cpp_block',        g:tcommentBlockC   )
call tcomment#DefineType('css',              '/* %s */'         )
call tcomment#DefineType('css_inline',       g:tcommentInlineC  )
call tcomment#DefineType('css_block',        g:tcommentBlockC   )
call tcomment#DefineType('c',                '/* %s */'         )
call tcomment#DefineType('c_inline',         g:tcommentInlineC  )
call tcomment#DefineType('c_block',          g:tcommentBlockC   )
call tcomment#DefineType('cfg',              '# %s'             )
call tcomment#DefineType('conf',             '# %s'             )
call tcomment#DefineType('crontab',          '# %s'             )
call tcomment#DefineType('cs',               '// %s'            )
call tcomment#DefineType('cs_inline',        g:tcommentInlineC  )
call tcomment#DefineType('cs_block',         g:tcommentBlockC   )
call tcomment#DefineType('desktop',          '# %s'             )
call tcomment#DefineType('django',           '{# %s #}'         )
call tcomment#DefineType('django_block',     "{%% comment %%}%s{%% endcomment %%}\n ")
call tcomment#DefineType('docbk',            '<!-- %s -->'      )
call tcomment#DefineType('docbk_inline',     g:tcommentInlineXML)
call tcomment#DefineType('docbk_block',      g:tcommentBlockXML )
call tcomment#DefineType('dosbatch',         'rem %s'           )
call tcomment#DefineType('dosini',           '; %s'             )
call tcomment#DefineType('dsl',              '; %s'             )
call tcomment#DefineType('dylan',            '// %s'            )
call tcomment#DefineType('eiffel',           '-- %s'            )
call tcomment#DefineType('erlang',           '%%%% %s'          )
call tcomment#DefineType('eruby',            '<%%# %s'          )
call tcomment#DefineType('fstab',            '# %s'             )
call tcomment#DefineType('gitcommit',        '# %s'             )
call tcomment#DefineType('gtkrc',            '# %s'             )
call tcomment#DefineType('go',               '// %s'            )
call tcomment#DefineType('go_inline',        g:tcommentInlineC  )
call tcomment#DefineType('go_block',         g:tcommentBlockC   )
call tcomment#DefineType('groovy',           '// %s'            )
call tcomment#DefineType('groovy_inline',    g:tcommentInlineC  )
call tcomment#DefineType('groovy_block',     g:tcommentBlockC   )
call tcomment#DefineType('groovy_doc_block', g:tcommentBlockC2  )
call tcomment#DefineType('haskell',          '-- %s'            )
call tcomment#DefineType('haskell_block',    "{-%s-}\n   "      )
call tcomment#DefineType('haskell_inline',   '{- %s -}'         )
call tcomment#DefineType('html',             '<!-- %s -->'      )
call tcomment#DefineType('html_inline',      g:tcommentInlineXML)
call tcomment#DefineType('html_block',       g:tcommentBlockXML )
call tcomment#DefineType('io',               '// %s'            )
call tcomment#DefineType('javaScript',       '// %s'            )
call tcomment#DefineType('javaScript_inline', g:tcommentInlineC )
call tcomment#DefineType('javaScript_block', g:tcommentBlockC   )
call tcomment#DefineType('javascript',       '// %s'            )
call tcomment#DefineType('javascript_inline', g:tcommentInlineC )
call tcomment#DefineType('javascript_block', g:tcommentBlockC   )
call tcomment#DefineType('java',             '/* %s */'         )
call tcomment#DefineType('java_inline',      g:tcommentInlineC  )
call tcomment#DefineType('java_block',       g:tcommentBlockC   )
call tcomment#DefineType('java_doc_block',   g:tcommentBlockC2  )
call tcomment#DefineType('jproperties',      '# %s'             )
call tcomment#DefineType('lisp',             '; %s'             )
call tcomment#DefineType('lynx',             '# %s'             )
call tcomment#DefineType('matlab',           '%% %s'            )
call tcomment#DefineType('m4',               'dnl %s'           )
call tcomment#DefineType('mail',             '> %s'             )
call tcomment#DefineType('msidl',            '// %s'            )
call tcomment#DefineType('msidl_block',      g:tcommentBlockC   )
call tcomment#DefineType('nroff',            '.\\" %s'          )
call tcomment#DefineType('nsis',             '# %s'             )
call tcomment#DefineType('objc',             '/* %s */'         )
call tcomment#DefineType('objc_inline',      g:tcommentInlineC  )
call tcomment#DefineType('objc_block',       g:tcommentBlockC   )
call tcomment#DefineType('ocaml',            '(* %s *)'         )
call tcomment#DefineType('ocaml_inline',     '(* %s *)'         )
call tcomment#DefineType('ocaml_block',      "(*%s*)\n   "      )
call tcomment#DefineType('pascal',           '(* %s *)'         )
call tcomment#DefineType('pascal_inline',    '(* %s *)'         )
call tcomment#DefineType('pascal_block',     "(*%s*)\n   "      )
call tcomment#DefineType('perl',             '# %s'             )
call tcomment#DefineType('perl_block',       "=cut%s=cut"       )
call tcomment#DefineType('php',              '// %s'            )
call tcomment#DefineType('php_inline',       g:tcommentInlineC  )
call tcomment#DefineType('php_block',        g:tcommentBlockC   )
call tcomment#DefineType('php_2_block',      g:tcommentBlockC2  )
call tcomment#DefineType('po',               '# %s'             )
call tcomment#DefineType('prolog',           '%% %s'            )
call tcomment#DefineType('rc',               '// %s'            )
call tcomment#DefineType('readline',         '# %s'             )
call tcomment#DefineType('ruby',             '# %s'             )
call tcomment#DefineType('ruby_3',           '### %s'           )
call tcomment#DefineType('ruby_block',       "=begin rdoc%s=end")
call tcomment#DefineType('ruby_nodoc_block', "=begin%s=end"     )
call tcomment#DefineType('r',                '# %s'             )
call tcomment#DefineType('sbs',              "' %s"             )
call tcomment#DefineType('scheme',           '; %s'             )
call tcomment#DefineType('sed',              '# %s'             )
call tcomment#DefineType('sgml',             '<!-- %s -->'      )
call tcomment#DefineType('sgml_inline',      g:tcommentInlineXML)
call tcomment#DefineType('sgml_block',       g:tcommentBlockXML )
call tcomment#DefineType('sh',               '# %s'             )
call tcomment#DefineType('sql',              '-- %s'            )
call tcomment#DefineType('spec',             '# %s'             )
call tcomment#DefineType('sps',              '* %s.'            )
call tcomment#DefineType('sps_block',        "* %s."            )
call tcomment#DefineType('spss',             '* %s.'            )
call tcomment#DefineType('spss_block',       "* %s."            )
call tcomment#DefineType('tcl',              '# %s'             )
call tcomment#DefineType('tex',              '%% %s'            )
call tcomment#DefineType('tpl',              '<!-- %s -->'      )
call tcomment#DefineType('viki',             '%% %s'            )
call tcomment#DefineType('viki_3',           '%%%%%% %s'        )
call tcomment#DefineType('viki_inline',      '{cmt: %s}'        )
call tcomment#DefineType('vim',              '" %s'             )
call tcomment#DefineType('vim_3',            '""" %s'           )
call tcomment#DefineType('websec',           '# %s'             )
call tcomment#DefineType('x86conf',          '# %s'             )
call tcomment#DefineType('xml',              '<!-- %s -->'      )
call tcomment#DefineType('xml_inline',       g:tcommentInlineXML)
call tcomment#DefineType('xml_block',        g:tcommentBlockXML )
call tcomment#DefineType('xs',               '// %s'            )
call tcomment#DefineType('xs_block',         g:tcommentBlockC   )
call tcomment#DefineType('xslt',             '<!-- %s -->'      )
call tcomment#DefineType('xslt_inline',      g:tcommentInlineXML)
call tcomment#DefineType('xslt_block',       g:tcommentBlockXML )
call tcomment#DefineType('yaml',             '# %s'             )


function! s:DefaultValue(option)
    exec 'let '. a:option .' = &'. a:option
    exec 'set '. a:option .'&'
    exec 'let default = &'. a:option
    exec 'let &'. a:option .' = '. a:option
    return default
endf

let s:defaultComments      = s:DefaultValue('comments')
let s:defaultCommentString = s:DefaultValue('commentstring')
let s:nullCommentString    = '%s'

" tcomment#Comment(line1, line2, ?commentMode, ?commentAnyway, ?args...)
" args... are either:
"   1. a list of key=value pairs where known keys are (see also 
"      |g:tcommentOptions|):
"         as=STRING     ... Use a specific comment definition
"         col=N         ... Start the comment at column N (in block mode; must 
"                           be smaller than |indent()|)
"         mode=STRING   ... See the notes below on the "commentMode" argument
"         begin=STRING  ... Comment prefix
"         end=STRING    ... Comment postfix
"         middle=STRING ... Middle line comments in block mode
"         rxbeg=N       ... Regexp to find the substring of "begin" that 
"                           should be multipied by "count"
"         rxend=N       ... The above for "end"
"         rxmid=N       ... The above for "middle"
"   2. 1-2 values for: ?commentPrefix, ?commentPostfix
"   3. a dictionary (internal use only)
"
" commentMode:
"   G ... guess the value of commentMode
"   B ... block (use extra lines for the comment markers)
"   i ... maybe inline, guess
"   I ... inline
"   R ... right (comment the line right of the cursor)
"   v ... visual
"   o ... operator
" By default, each line in range will be commented by adding the comment 
" prefix and postfix.
function! tcomment#Comment(beg, end, ...)
    let commentMode   = a:0 >= 1 ? a:1 : 'G'
    let commentAnyway = a:0 >= 2 ? (a:2 == '!') : 0
    " TLogVAR a:beg, a:end, a:1, commentMode, commentAnyway
    " save the cursor position
    let pos = getpos('.')
    let s:pos_end = getpos("'>")
    if commentMode =~# 'i'
        let commentMode = substitute(commentMode, '\Ci', line("'<") == line("'>") ? 'I' : 'G', 'g')
    endif
    let [cstart, cend] = s:GetStartEnd(commentMode)
    " TLogVAR commentMode, cstart, cend
    " get the correct commentstring
    let cdef = copy(g:tcommentOptions)
    if a:0 >= 3 && type(a:3) == 4
        call extend(cdef, a:3)
    else
        call extend(cdef, s:GetCommentDefinition(a:beg, a:end, commentMode))
        let ax = 3
        if a:0 >= 3 && a:3 != '' && stridx(a:3, '=') == -1
            let ax = 4
            let cdef.begin = a:3
            if a:0 >= 4 && a:4 != '' && stridx(a:4, '=') == -1
                let ax = 5
                let cdef.end = a:4
            endif
        endif
        if a:0 >= ax
            call extend(cdef, s:ParseArgs(a:beg, a:end, commentMode, a:000[ax - 1 : -1]))
        endif
        if !empty(get(cdef, 'begin', '')) || !empty(get(cdef, 'end', ''))
            let cdef.commentstring = s:EncodeCommentPart(get(cdef, 'begin', ''))
                        \ . '%s'
                        \ . s:EncodeCommentPart(get(cdef, 'end', ''))
        endif
        let commentMode = cdef.mode
    endif
    if !empty(filter(['count', 'cbeg', 'cend', 'cmid'], 'has_key(cdef, v:val)'))
        call s:RepeatCommentstring(cdef)
    endif
    " echom "DBG" string(cdef) string(a:000)
    let cms0 = s:BlockGetCommentString(cdef)
    let cms0 = escape(cms0, '\')
    " make whitespace optional; this conflicts with comments that require some 
    " whitespace
    let cmtCheck = substitute(cms0, '\([	 ]\)', '\1\\?', 'g')
    " turn commentstring into a search pattern
    let cmtCheck = printf(cmtCheck, '\(\_.\{-}\)')
    " set commentMode and indentStr
    let [indentStr, uncomment] = s:CommentDef(a:beg, a:end, cmtCheck, commentMode, cstart, cend)
    " TLogVAR indentStr, uncomment
    let col = get(cdef, 'col', -1)
    if col >= 0
        let col -= 1
        let indent = len(indentStr)
        if col > indent
            let cms0 = repeat(' ', col - indent) . cms0
        else
            let indentStr = repeat(' ', col)
        endif
    endif
    if commentAnyway
        let uncomment = 0
    endif
    " go
    if commentMode =~# 'B'
        " We want a comment block
        call s:CommentBlock(a:beg, a:end, uncomment, cmtCheck, cdef, indentStr)
    else
        " call s:CommentLines(a:beg, a:end, cstart, cend, uncomment, cmtCheck, cms0, indentStr)
        " We want commented lines
        " final search pattern for uncommenting
        let cmtCheck   = escape('\V\^\(\s\{-}\)'. cmtCheck .'\$', '"/\')
        " final pattern for commenting
        let cmtReplace = escape(cms0, '"/')
        silent exec a:beg .','. a:end .'s/\V'. 
                    \ s:StartRx(cstart) . indentStr .'\zs\(\.\{-}\)'. s:EndRx(cend) .'/'.
                    \ '\=s:ProcessedLine('. uncomment .', submatch(0), "'. cmtCheck .'", "'. cmtReplace .'")/ge'
    endif
    " reposition cursor
    " TLogVAR commentMode
    if commentMode =~ '>'
        call setpos('.', s:pos_end)
    else
        " TLogVAR pos
        call setpos('.', pos)
    endif
endf


function! s:GetStartEnd(commentMode) "{{{3
    let commentMode = a:commentMode
    if commentMode =~# 'R' || commentMode =~# 'I'
        let cstart = col("'<")
        if cstart == 0
            let cstart = col('.')
        endif
        if commentMode =~# 'R'
            let commentMode = substitute(commentMode, '\CR', 'G', 'g')
            let cend = 0
        else
            let cend = col("'>")
            if commentMode =~# 'o'
                let cend += 1
            endif
        endif
    else
        let cstart = 0
        let cend   = 0
    endif
    return [cstart, cend]
endf


function! s:RepeatCommentstring(cdef) "{{{3
    " TLogVAR a:cdef
    let cms = s:BlockGetCommentString(a:cdef)
    let mid = s:BlockGetMiddleString(a:cdef)
    let cms_fbeg = match(cms, '\s*%\@<!%s')
    let cms_fend = matchend(cms, '%\@<!%s\s*')
    let rxbeg = get(a:cdef, 'rxbeg', '^.*$')
    let rxend = get(a:cdef, 'rxend', '^.*$')
    let rpbeg = repeat('&', get(a:cdef, 'cbeg', get(a:cdef, 'count', 1)))
    let rpend = repeat('&', get(a:cdef, 'cend', get(a:cdef, 'count', 1)))
    let a:cdef.commentstring = substitute(cms[0 : cms_fbeg - 1], rxbeg, rpbeg, '')
                \. cms[cms_fbeg : cms_fend - 1]
                \. substitute(cms[cms_fend : -1], rxend, rpend, '')
    " TLogVAR cms, a:cdef.commentstring
    if !empty(mid)
        let rxmid = get(a:cdef, 'rxmid', '^.*$')
        let rpmid = repeat('&', get(a:cdef, 'cmid', get(a:cdef, 'count', 1)))
        let a:cdef.middle = substitute(mid, rxmid, rpmid, '')
        " TLogVAR mid, a:cdef.middle
    endif
    return a:cdef
endf


function! s:ParseArgs(beg, end, commentMode, arglist) "{{{3
    let args = {}
    for arg in a:arglist
        let key = matchstr(arg, '^[^=]\+')
        let value = matchstr(arg, '=\zs.*$')
        if key == 'as'
            call extend(args, s:GetCommentDefinitionForType(a:beg, a:end, a:commentMode, value))
        else
            let args[key] = value
        endif
    endfor
    return args
endf


function! tcomment#Operator(type, ...) "{{{3
    let commentMode = a:0 >= 1 ? a:1 : ''
    let bang = a:0 >= 2 ? a:2 : ''
    if !exists('w:tcommentPos')
        let w:tcommentPos = getpos(".")
    endif
    let sel_save = &selection
    let &selection = "inclusive"
    let reg_save = @@
    " let pos = getpos('.')
    " TLogVAR a:type
    try
        if a:type == 'line'
            silent exe "normal! '[V']"
            let commentMode1 = 'G'
        elseif a:type == 'block'
            silent exe "normal! `[\<C-V>`]"
            let commentMode1 = 'I'
        else
            silent exe "normal! `[v`]"
            let commentMode1 = 'i'
        endif
        if empty(commentMode)
            let commentMode = commentMode1
        endif
        let beg = line("'[")
        let end = line("']")
        norm! 
        let commentMode .= g:tcommentOpModeExtra
        call tcomment#Comment(beg, end, commentMode.'o', bang)
    finally
        let &selection = sel_save
        let @@ = reg_save
        if g:tcommentOpModeExtra !~ '>'
            " TLogVAR pos
            " call setpos('.', pos)
            if exists('w:tcommentPos')
                call setpos('.', w:tcommentPos)
                unlet! w:tcommentPos
            else
                echohl WarningMsg
                echom "TComment: w:tcommentPos wasn't set. Please report this to the plugin author"
                echohl NONE
            endif
        endif
    endtry
endf


function! tcomment#OperatorLine(type) "{{{3
    call tcomment#Operator(a:type, 'G')
endf


function! tcomment#OperatorAnyway(type) "{{{3
    call tcomment#Operator(a:type, '', '!')
endf


function! tcomment#OperatorLineAnyway(type) "{{{3
    call tcomment#Operator(a:type, 'G', '!')
endf


" :display: tcomment#CommentAs(beg, end, commentAnyway, filetype, ?args...)
" Where args is either:
"   1. A count NUMBER
"   2. An args list (see the notes on the "args" argument of 
"      |tcomment#Comment()|)
" comment text as if it were of a specific filetype
function! tcomment#CommentAs(beg, end, commentAnyway, filetype, ...)
    if a:filetype =~ '_block$'
        let commentMode = 'B'
        let ft = substitute(a:filetype, '_block$', '', '')
    elseif a:filetype =~ '_inline$'
        let commentMode = 'I'
        let ft = substitute(a:filetype, '_inline$', '', '')
    else 
        let commentMode = 'G'
        let ft = a:filetype
    endif
    if a:0 >= 1
        if type(a:1) == 0
            let cdef = {'count': a:0 >= 1 ? a:1 : 1}
        else
            let cdef = s:ParseArgs(a:beg, a:end, commentMode, a:000)
        endif
    else
        let cdef = {}
    endif
    " echom "DBG" string(cdef)
    call extend(cdef, s:GetCommentDefinitionForType(a:beg, a:end, commentMode, ft))
    keepjumps call tcomment#Comment(a:beg, a:end, commentMode, a:commentAnyway, cdef)
endf


" collect all known comment types
" :nodoc:
function! tcomment#CollectFileTypes()
    if s:typesDirty
        let s:types = keys(s:definitions)
        let s:typesRx = '\V\^\('. join(s:types, '\|') .'\)\(\u\.\*\)\?\$'
        let s:typesDirty = 0
    endif
endf

call tcomment#CollectFileTypes()


" return a list of filetypes for which a tcomment_{&ft} is defined
" :nodoc:
function! tcomment#Complete(ArgLead, CmdLine, CursorPos) "{{{3
    call tcomment#CollectFileTypes()
    let completions = copy(s:types)
    let filetype = s:Filetype()
    if index(completions, filetype) != -1
        " TLogVAR filetype
        call insert(completions, filetype)
    endif
    if !empty(a:ArgLead)
        call filter(completions, 'v:val =~ ''\V\^''.a:ArgLead')
    endif
    let completions += tcomment#CompleteArgs(a:ArgLead, a:CmdLine, a:CursorPos)
    return completions
endf


" :nodoc:
function! tcomment#CompleteArgs(ArgLead, CmdLine, CursorPos) "{{{3
    let completions = ['as=', 'col=', 'count=', 'mode=', 'begin=', 'end=']
    if !empty(a:ArgLead)
        if a:ArgLead =~ '^as='
            call tcomment#CollectFileTypes()
            let completions += map(copy(s:types), '"as=". v:val')
        endif
        call filter(completions, 'v:val =~ ''\V\^''.a:ArgLead')
    endif
    return completions
endf


function! s:EncodeCommentPart(string)
    return substitute(a:string, '%', '%%', 'g')
endf


function! s:GetCommentDefinitionForType(beg, end, commentMode, filetype) "{{{3
    let cdef = s:GetCommentDefinition(a:beg, a:end, a:commentMode, a:filetype)
    let cms  = cdef.commentstring
    let commentMode = cdef.mode
    let pre  = substitute(cms, '%s.*$', '', '')
    let pre  = substitute(pre, '%%', '%', 'g')
    let post = substitute(cms, '^.\{-}%s', '', '')
    let post = substitute(post, '%%', '%', 'g')
    let cdef.begin = pre
    let cdef.end   = post
    return cdef
endf


" s:GetCommentDefinition(beg, end, commentMode, ?filetype="")
function! s:GetCommentDefinition(beg, end, commentMode, ...)
    let ft = a:0 >= 1 ? a:1 : ''
    if ft != ''
        let cdef = s:GetCustomCommentString(ft, a:commentMode)
    else
        let cdef = {'mode': a:commentMode}
    endif
    let cms = get(cdef, 'commentstring', '')
    if empty(cms)
        let filetype = s:Filetype()
        if exists('b:commentstring')
            let cms = b:commentstring
            return s:GetCustomCommentString(filetype, a:commentMode, cms)
        elseif exists('b:commentStart') && b:commentStart != ''
            let cms = s:EncodeCommentPart(b:commentStart) .' %s'
            if exists('b:commentEnd') && b:commentEnd != ''
                let cms = cms .' '. s:EncodeCommentPart(b:commentEnd)
            endif
            return s:GetCustomCommentString(filetype, a:commentMode, cms)
        elseif g:tcommentGuessFileType || (exists('g:tcommentGuessFileType_'. filetype) 
                    \ && g:tcommentGuessFileType_{filetype} =~ '[^0]')
            if g:tcommentGuessFileType_{filetype} == 1
                let altFiletype = ''
            else
                let altFiletype = g:tcommentGuessFileType_{filetype}
            endif
            return s:GuessFileType(a:beg, a:end, a:commentMode, filetype, altFiletype)
        else
            return s:GetCustomCommentString(filetype, a:commentMode, s:GuessCurrentCommentString(a:commentMode))
        endif
        let cdef.commentstring = cms
    endif
    return cdef
endf

function! s:StartRx(pos)
    if a:pos == 0
        return '\^'
    else
        return '\%'. a:pos .'c'
    endif
endf

function! s:EndRx(pos)
    if a:pos == 0
        return '\$'
    else
        return '\%'. a:pos .'c'
    endif
endf

function! s:GetIndentString(line, start)
    let start = a:start > 0 ? a:start - 1 : 0
    return substitute(strpart(getline(a:line), start), '\V\^\s\*\zs\.\*\$', '', '')
endf

function! s:CommentDef(beg, end, checkRx, commentMode, cstart, cend)
    let mdrx = '\V'. s:StartRx(a:cstart) .'\s\*'. a:checkRx .'\s\*'. s:EndRx(0)
    let line = getline(a:beg)
    if a:cstart != 0 && a:cend != 0
        let line = strpart(line, 0, a:cend - 1)
    endif
    let uncomment = (line =~ mdrx)
    let indentStr = s:GetIndentString(a:beg, a:cstart)
    let il = indent(a:beg)
    let n  = a:beg + 1
    while n <= a:end
        if getline(n) =~ '\S'
            let jl = indent(n)
            if jl < il
                let indentStr = s:GetIndentString(n, a:cstart)
                let il = jl
            endif
            if a:commentMode =~# 'G'
                if !(getline(n) =~ mdrx)
                    let uncomment = 0
                endif
            endif
        endif
        let n = n + 1
    endwh
    if a:commentMode =~# 'B'
        let t = @t
        try
            silent exec 'norm! '. a:beg.'G1|v'.a:end.'G$"ty'
            let uncomment = (@t =~ mdrx)
        finally
            let @t = t
        endtry
    endif
    return [indentStr, uncomment]
endf

function! s:ProcessedLine(uncomment, match, checkRx, replace)
    " TLogVAR a:uncomment, a:match, a:checkRx, a:replace
    if !(a:match =~ '\S' || g:tcommentBlankLines)
        return a:match
    endif
    let ml = len(a:match)
    if a:uncomment
        let rv = substitute(a:match, a:checkRx, '\1\2', '')
    else
        let rv = printf(a:replace, a:match)
    endif
    " TLogVAR rv
    " let md = len(rv) - ml
    let s:pos_end = getpos('.')
    let s:pos_end[2] += len(rv)
    " TLogVAR pe, md, a:match
    if v:version > 702 || (v:version == 702 && has('patch407'))
        let rv = escape(rv, '')
    else
        let rv = escape(rv, '\')
    endif
    let rv = substitute(rv, '\n', '\\\n', 'g')
    return rv
endf

function! s:CommentLines(beg, end, cstart, cend, uncomment, cmtCheck, cms0, indentStr) "{{{3
    " We want commented lines
    " final search pattern for uncommenting
    let cmtCheck   = escape('\V\^\(\s\{-}\)'. a:cmtCheck .'\$', '"/\')
    " final pattern for commenting
    let cmtReplace = escape(a:cms0, '"/')
    silent exec a:beg .','. a:end .'s/\V'. 
                \ s:StartRx(a:cstart) . a:indentStr .'\zs\(\.\{-}\)'. s:EndRx(a:cend) .'/'.
                \ '\=s:ProcessedLine('. a:uncomment .', submatch(0), "'. a:cmtCheck .'", "'. cmtReplace .'")/ge'
endf

function! s:CommentBlock(beg, end, uncomment, checkRx, cdef, indentStr)
    let t = @t
    try
        silent exec 'norm! '. a:beg.'G1|v'.a:end.'G$"td'
        let ms = s:BlockGetMiddleString(a:cdef)
        let mx = escape(ms, '\')
        if a:uncomment
            let @t = substitute(@t, '\V\^\s\*'. a:checkRx .'\$', '\1', '')
            if ms != ''
                let @t = substitute(@t, '\V\n'. a:indentStr . mx, '\n'. a:indentStr, 'g')
            endif
            let @t = substitute(@t, '^\n', '', '')
            let @t = substitute(@t, '\n\s*$', '', '')
        else
            let cs = s:BlockGetCommentString(a:cdef)
            let cs = a:indentStr . substitute(cs, '%s', '%s'. a:indentStr, '')
            if ms != ''
                let ms = a:indentStr . ms
                let mx = a:indentStr . mx
                let @t = substitute(@t, '^'. a:indentStr, '', 'g')
                let @t = ms . substitute(@t, '\n'. a:indentStr, '\n'. mx, 'g')
            endif
            let @t = printf(cs, "\n". @t ."\n")
        endif
        silent norm! "tP
    finally
        let @t = t
    endtry
endf


function! s:Filetype(...) "{{{3
    let ft = a:0 >= 1 ? a:1 : &filetype
    let ft = substitute(ft, '\..*$', '', '')
    return ft
endf


" inspired by Meikel Brandmeyer's EnhancedCommentify.vim
" this requires that a syntax names are prefixed by the filetype name 
" s:GuessFileType(beg, end, commentMode, filetype, ?fallbackFiletype)
function! s:GuessFileType(beg, end, commentMode, filetype, ...)
    if a:0 >= 1 && a:1 != ''
        let cdef = s:GetCustomCommentString(a:1, a:commentMode)
        if empty(get(cdef, 'commentstring', ''))
            let cdef.commentstring = s:GuessCurrentCommentString(a:commentMode)
        endif
    else
        let cdef = {'commentstring': s:GuessCurrentCommentString(0), 'mode': s:CommentMode(a:commentMode, 'G')}
    endif
    let n  = a:beg
    " TLogVAR n, a:beg, a:end
    while n <= a:end
        let m  = indent(n) + 1
        let le = len(getline(n))
        " TLogVAR m, le
        while m < le
            let syntaxName = s:GetSyntaxName(n, m)
            " TLogVAR syntaxName, n, m
            let ftypeMap   = get(g:tcommentSyntaxMap, syntaxName)
            if !empty(ftypeMap)
                " TLogVAR ftypeMap
                return s:GetCustomCommentString(ftypeMap, a:commentMode, cdef.commentstring)
            elseif syntaxName =~ s:typesRx
                let ft = substitute(syntaxName, s:typesRx, '\1', '')
                " TLogVAR ft
                if exists('g:tcommentIgnoreTypes_'. a:filetype) && g:tcommentIgnoreTypes_{a:filetype} =~ '\<'.ft.'\>'
                    let m += 1
                else
                    return s:GetCustomCommentString(ft, a:commentMode, cdef.commentstring)
                endif
            elseif syntaxName == '' || syntaxName == 'None' || syntaxName =~ '^\u\+$' || syntaxName =~ '^\u\U*$'
                let m += 1
            else
                break
            endif
        endwh
        let n += 1
    endwh
    return cdef
endf


function! s:GetSyntaxName(lnum, col) "{{{3
    let syntaxName = synIDattr(synID(a:lnum, a:col, 1), 'name')
    if !empty(g:tcomment#syntax_substitute)
        for [rx, subdef] in items(g:tcomment#syntax_substitute)
            if !has_key(subdef, 'if') || eval(subdef.if)
                let syntaxName = substitute(syntaxName, rx, subdef.sub, 'g')
            endif
        endfor
    endif
    " TLogVAR syntaxName
    return syntaxName
endf


function! s:CommentMode(commentMode, newmode) "{{{3
    return substitute(a:commentMode, '\w\+', a:newmode, 'g')
endf

function! s:GuessCurrentCommentString(commentMode)
    let valid_cms = (stridx(&commentstring, '%s') != -1)
    if &commentstring != s:defaultCommentString && valid_cms
        " The &commentstring appears to have been set and to be valid
        return &commentstring
    endif
    if &comments != s:defaultComments
        " the commentstring is the default one, so we assume that it wasn't 
        " explicitly set; we then try to reconstruct &cms from &comments
        let cms = s:ConstructFromComments(a:commentMode)
        if cms != s:nullCommentString
            return cms
        endif
    endif
    if valid_cms
        " Before &commentstring appeared not to be set. As we don't know 
        " better we return it anyway if it is valid
        return &commentstring
    else
        " &commentstring is invalid. So we return the identity string.
        return s:nullCommentString
    endif
endf

function! s:ConstructFromComments(commentMode)
    exec s:ExtractCommentsPart('')
    if a:commentMode =~# 'G' && line != ''
        return line .' %s'
    endif
    exec s:ExtractCommentsPart('s')
    if s != ''
        exec s:ExtractCommentsPart('e')
        return s.' %s '.e
    endif
    if line != ''
        return line .' %s'
    else
        return s:nullCommentString
    endif
endf

function! s:ExtractCommentsPart(key)
    " let key   = a:key != "" ? a:key .'[^:]*' : ""
    let key = a:key . '[bnflrxO0-9-]*'
    let val = substitute(&comments, '^\(.\{-},\)\{-}'. key .':\([^,]\+\).*$', '\2', '')
    if val == &comments
        let val = ''
    else
        let val = substitute(val, '%', '%%', 'g')
    endif
    let var = a:key == '' ? 'line' : a:key
    return 'let '. var .'="'. escape(val, '"') .'"'
endf

" s:GetCustomCommentString(ft, commentMode, ?default="")
function! s:GetCustomCommentString(ft, commentMode, ...)
    let commentMode   = a:commentMode
    let customComment = tcomment#TypeExists(a:ft)
    if commentMode =~# 'B' && tcomment#TypeExists(a:ft .'_block')
        let def = s:definitions[a:ft .'_block']
    elseif commentMode =~? 'I' && tcomment#TypeExists(a:ft .'_inline')
        let def = s:definitions[a:ft .'_inline']
    elseif customComment
        let def = s:definitions[a:ft]
        let commentMode = s:CommentMode(commentMode, 'G')
    elseif a:0 >= 1
        let def = {'commentstring': a:1}
        let commentMode = s:CommentMode(commentMode, 'G')
    else
        let def = {}
        let commentMode = s:CommentMode(commentMode, 'G')
    endif
    let cdef = copy(def)
    let cdef.mode = commentMode
    return cdef
endf

function! s:BlockGetCommentString(cdef)
    if has_key(a:cdef, 'middle')
        return a:cdef.commentstring
    else
        return matchstr(a:cdef.commentstring, '^.\{-}\ze\(\n\|$\)')
    endif
endf

function! s:BlockGetMiddleString(cdef)
    if has_key(a:cdef, 'middle')
        return a:cdef.middle
    else
        return matchstr(a:cdef.commentstring, '\n\zs.*')
    endif
endf


redraw

