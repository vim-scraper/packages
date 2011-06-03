" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
doc/tskeleton.txt	[[[1
604
*tskeleton.txt*              tskeleton -- File Templates and Code Skeletons

Author: Thomas Link, micathom AT gmail com?subject=vim

tskeleton provides file templates and code skeletons (snippets). These 
templates may contain special tags that are replaced with some computed 
value (e.g., variables, user input ...), vimscript code, or place 
holders/jump positions (see |tskeleton-jump-positions|).


-----------------------------------------------------------------------
                                                    *tskeleton-install*
Install~

Edit the vba file and type:

    :so %

See :help vimball for details. If you use vim 7.0, you probably need to 
update vimball first.

If you don't already have some skeletons, you may want to download 
tskeleton-Samples.zip from:

    http://www.vim.org/scripts/script.php?script_id=1160

Extract zip (or copy the contained files) to your local vimfiles 
directory (see also |add-global-plugin|). The directory structure should 
look like this:

    ~/.vim/skeletons/
        FILE TEMPLATES ...
        map/
            MAP FILES FOR CONDITIONAL EXPANSION
        bits/
            &filetype.txt (single line templates)
            general/
                GENERAL CODE SKELETONS ...
            &filetype/
                FILETYPE SPECIFIC CODE SKELETONS: ONE SKELETON PER FILE ...

                                                    *g:tskelDir*
If you re-use skeletons from version 1.0 with a later version, you have 
to update the |tskeleton-place-holder| markup. Make sure the variable 
g:tskelDir points to the right directory.

You might want to use imaps.vim's (vimscript #244 or vimscript #475) place 
holders in conjunction with template bits.

If you don't use imaps.vim, you can use |TSkeletonMapGoToNextTag()|.


-----------------------------------------------------------------------
                                                    *tskeleton-usage*
File templates~

The file skeletons are stored in the skeletons subdirectory. Which 
template is used for which new file is controlled by |:autocmd|s. This 
provides greater flexibility than a &filetype based approach as you can 
select templates on the basis of a filename pattern or a specific 
directory.

Currently, the following file types are supported by default:

   - batch.bat
   - deplate.txt
   - latex.tex
   - php.inc.php
   - php.php
   - plugin.vim
   - ruby.rb
   - shell.sh
   - text.txt

In order to add support for a new filetype, save a skeleton file to 
~/.vim/skeletons/file.suffix and add something like this to your .vimrc 
file: >

    autocmd BufNewFile *.suffix       TSkeletonSetup template.suffix
    autocmd BufNewFile /here/*.suffix TSkeletonSetup othertemplate.suffix


Alternatively, you can store templates as: >

    ~/vimfiles/skeletons/templates/GROUP/FILETYPE PATTERN

where GROUP is an arbitrary name for a collection of auto templates, 
FILETYPE is a know vim filetype, and the pattern is an encoded pattern (# == 
*, %XX=Char XXh) for auto-generated autocmds.

Example: Templates file names for some vim-related files: >

    ~/vimfiles/skeletons/templates/vim/help #%2fvimfiles%2fdoc%2f#.txt
    ~/vimfiles/skeletons/templates/vim/vim #%2fvimfiles#%2fftplugin%2f#.vim
    ~/vimfiles/skeletons/templates/vim/vim #%2fvimfiles#%2fsyntax%2f#.vim

< 
                                                    *tskeleton-place-holder*

Tags (which look like <+...+>) serve two purposes:
    - marker for jump positions
    - dynamic content expansion

You can use tags to define cursor jump positions. tskeleton also 
supports some special tags that are expanded when inserting the 
skeleton.

A list of special tags:
                                                    *tSkeletion-<+FILE NAME ROOT+>*
    <+FILE NAME ROOT+>
        The file name root
                                                    *tSkeletion-<+FILE NAME+>*
    <+FILE NAME+>
        The file name
                                                    *tSkeletion-<+FILE SUFFIX+>*
    <+FILE SUFFIX+>
        The file suffix
                                                    *tSkeletion-<+FILE DIRNAME+>*
    <+FILE DIRNAME+>
        The file's directory
                                                    *tSkeletion-<+NOTE+>*
    <+NOTE+>
        A note
                                                    *tSkeletion-<+DATE+>*
    <+DATE+>
        The current date (the format is controlled via 
        g:tskelDateFormat)
                                                    *tSkeletion-<+AUTHOR+>*
    <+AUTHOR+>
        The author's name (g:tskelUserName)
                                                    *tSkeletion-<+EMAIL+>*
    <+EMAIL+>
        The author's e-mail (g:tskelUserEmail)
                                                    *tSkeletion-<+WEBSITE+>*
    <+WEBSITE+>
        The author's homepage (g:tskelUserWWW)
                                                    *tSkeletion-<+LICENSE+>*
    <+LICENSE+>
        The name of the license this file is released under 
        (g:tskelLicense)

In order to define your own tag, you have to define a function called 
TSkeleton_TAGNAME() that returns the text to be filled in.

                                                    *tskeleton-tags*
tskeleton also supports the following pseudo-tags:

    <+CURSOR+>                                      *tSkeletion-<+CURSOR+>*
        Where to place the cursor after insertion

    <+&NAME+>                                       *tSkeletion-<+&+>*
        A vim option

    <+g:NAME+>                                      *tSkeletion-<+g:+>*
        A global variable

    <+b:NAME+>                                      *tSkeletion-<+b:+>*
        A buffer local variable
 
    <+?QUERY?+>                                     *tSkeletion-<+?+>*
        Query[1] the user

    <+?VAR|QUERY?+>
        Query[1] the user and propose some choices from the variable VAR

                                                    *tSkeletion-<+bit+>*
    <+bit:BIT+>, <+bit:BIT|"DEFAULT"+>, <+bit:BIT|COMMANDS+>
        Insert a bit; if the bit isn't defined for the current filetype, 
        use DEFAULT; if DEFAULT matches ".*" insert it as a string; 
        otherwise interpret it as a command sequence to be fed to normal

    <+tskel:TSKELETON+>                             *tSkeletion-<+tskel+>*
        Same as the above

    <+call:FUNCTION(ARGS)+>                         *tSkeletion-<+call+>*
        Insert the result value of some function

    <+include(TSKELETON)+>                           *tSkeletion-<+include+>*
        Another synonym for the above.

    <+execute(EX COMMAND)+>                         *tSkeletion-<+execute+>*
        Run a vim command.

[1] If the query ends with a colon, the second question mark will be 
removed. Up to verson 2.4 VAR is a string, separating the items by an 
"\n". From 3.0 on, VAR is a list.

NOTE: Flow control and loop tags are experimental and it's more than 
just likely that the output doesn't meet your expectations.

Flow control:                                       *tSkeletion-<+if+>*
    <+if(CONDITION)+>
    <+elseif(CONDITION)+>
    <+else+>
    <+endif+>

Loops:                                              *tSkeletion-<+for+>*
    <+for(VAR in LIST)+>
    <+endfor+>, <+endfor(VAR)+>
        If you nest for loops, you have to add the variable name to the 
        endfor tag. And no, this isn't the most clever way to do this.

Variables:                                          *tSkeletion-<+let+>*
    <+let(VAR=VALUE)+>
        The variable will be unset or restored to its original value 
        after processing the current template.
        VAR may have some modifiers attached:
            VAR? ... Set only if undefined
            VAR& ... Don't restore original variable

Interaction:                                        *tSkeletion-<+input+>*
    <+input(VAR, QUERY, [DEFAULT], [COMPLETION])+>
        This will insert the user input and set the variable VAR (unless 
        empty), which can be used throughout the template. The variable 
        will be removed after finishing the current template. If the 
        variable was previously defined, the original value will be 
        restored.
        If VAR ends with '!', no text will be inserted in the buffer. In 
        this case any whitespace (including one newline) after the tag 
        will be removed. See also the notes on |tSkeletion-<+input+>|.

                                                    *tSkeletion-<+select+>*
    <+select(VAR, LIST, [TYPE='s'], [SEPARATOR=', '])+>
        Let the user select an item from a list.
        VAR is a string.
        TYPE is either 's' (single selection) or 'm' (multiple selection) -- 
        see also |tlib#input#List()|.
        If TYPE is 'm', the results are joined with SEPARATOR.
        Example: >
            before(<+select("s:type", [":all", ":each"])+>)
            before(<+select("s:type", ["A", "B", "C", "D"], "m", "-")+>)
< 

Other:                                              *tSkeletion-<+nl+>*
    <+nl+>
        Insert a newline
                
    <+joinline+>                                    *tSkeletion-<+joinline+>*
        Join with next line. Delete any whitespace.
            
    <+nop+>                                         *tSkeletion-<+nop+>*
        Insert nothing (could be necessary in conjunction with the "for" 
        tag)

A wiki like table could then be constructed using a skeleton like this: >

    <tskel:before>
    let s:my_rows = input('Rows: ')
    let s:my_cols = input('Columns: ')
    </tskel:before>
    <tskel:after>
    unlet! s:my_rows s:my_cols
    </tskel:after>
    <+CURSOR+><+for(i in range(s:my_rows))+>
    |<+for(j in range(s:my_cols))+> <+CELL+> |<+endfor(j)+><+nop+>
    <+endfor(i)+>

or: >

    <+input('s:my_rows?!', 'Rows: ')+>
    <+input('s:my_cols?!', 'Cols: ')+>
    <+for(i in range(s:my_rows))+>
    |<+for(j in range(s:my_cols))+> <+CURSOR+> |<+endfor(j)+><+nop+>
    <+endfor(i)+>

NOTE: The <+nop+> is necessary in order to prevent the <+endfor+> tag to 
"eat" the newline. If we include this bit from another bit that already 
sets s:my_rows and/or s:my_cols, the user won't be queried again (because of 
the "?" in the input statement). E.g. >

    <+let(s:my_rows = 3)+>
    <+let(s:my_cols = 3)+>
    <+include(table)+>

                                                    *tSkeletion-backslash*
Any special tag can be preceded with a backslash in order to prevent 
expansion. Examples: >

    <+\if(foo=1)+>
    
surprisingly expands to >

    <+if(foo=1)+>

It's best to use global variables with "if" and "for" tags as the body 
is most likely evaluated in a different buffer in a hypothetically 
unknown context.


                                                    *tskeleton-jump-positions*
Unknown tags are kept in the expanded skeleton. These tags can be used 
as cursor jump marks. This syntax was originally used by imap (vimscript 
#244 or vimscript #475). If you don't want to install imap, you can also  
use |TSkeletonMapGoToNextTag()|.

Nameless tags (<++>) will disappear when they are selected.

Example:>

    case <+CURSOR+> in
    <+PATTERN+>)
        <+BODY+>
        ;;
    *)
        <+DEFAULT+>
        ;;
    esac
    <++>

When you insert this skeleton, the cursor will be placed at 
"<+CURSOR+>".  If you press <c-j>, the cursor will jump to "<+PATTERN+>" 
(the tag will remain selected). If you press <c-j> three times, the 
cursor will jump to "<++>" (the tag will be deleted).

Check out the "test_tskeleton" skeleton for examples.


                                                    *tskeleton-modifiers*
Tags can be modified using modifiers, like in: >

    <+TAG NAME:MODIFIER+>

Known modifiers:

    l          :: lower case
    u          :: upper case
    c          :: capitalize
    C          :: transform to CamelCase
    s/FROM/TO/ :: replace text (actually a s//g); this has to be the 
                  last modifier; the pattern separator can be selected 
                  arbitrarily

Example for a ruby class template: >

    class <+FILE NAME ROOT:cs*\W*_*+>
        <+CURSOR+>
    end
    <++>

-----------------------------------------------------------------------
                                                    *tskeleton-code-skeletons*
                                                    *tskeleton-bits*
Bits/Code Skeletons~

Smaller skeleton bits are stored in SKELETONS/bits/FILETYPE/ or 
SKELETONS/bits/general/. I.e., code skeletons can be filetype specific 
or generally available.

The filenames of the bits may be grouped in submenus as in:

    ../tex/&Define.%5Cnew&command
    ../tex/&Define.%5Cnew&environment

This will create skeletons for \newcommand and \newenvironment but will 
group the skeletons under the TSkel.Define. menu with the respective 
accelerators.

                                                    *tskeleton-Types* *g:tskelTypes*
                                                    *tskeleton#Initialize()*
tskeleton supports several types of code templates:

    - skeleton (standard tskeleton functionality)
    - abbreviations (VIM abbreviations)
    - functions (VIM script functions extracted from :function)
    - mini ("mini" bits, one-liners etc.)
    - tags (tags-based code templates, requires ctags, I presume)

Not all types are enabled by default. User have to select, which types 
they want to use, by setting the g:tskelTypes at start-up. When changing 
the variable, users might have to call tskeleton#Initialize().

                                                    *tskeleton-Skeleton*
Smaller skeleton bits are stored in SKELETONS/bits/FILETYPE/ or 
SKELETONS/bits/general/. I.e., code skeletons can be filetype specific 
or generally available.

Skeleton bits can be filled in by typing: >

    :TSkeletonBit NAME

For this command, command line completion is implemented. Calling this 
command will insert the contents of the respective file below the 
current line.

NOTE: Bit names should not contain ampersand (as these are interpreted 
as menu accelerators) and periods (which are used to construct 
submenus). Other special characters can be included by encoding them in 
hex form as %XX as it is done in URLs.  Example: "%5Csection" becomes 
"\section".

                                                    *tskeleton-key-bindings*
The default key bindings for inserting code skeletons are:

    <Leader>## ... Expand name under cursor
    <Leader>#t ... Insert code skeleton via command line
    <c-\><c-\> ... In insert mode, expand the bit before the cursor (on 
                   a German keyboard this happens to be <c-#><c-#>)

                                                    *g:tskelKeyword_{&filetype}*
A bit name usually is the |word| under the cursor. If this doesn't fit 
your needs, you can define g:tskelKeyword_{&filetype} to define what 
makes up a skeleton name. Example: >

    let g:tskelKeyword_viki = '\(#\|{\)\?[^#{[:blank:]]\{-}'


                                                    *tskeleton-embedded-code*
Code skeletons may contain vim code that is evaluated before or after 
expanding the tags. The before/after blocks are fed to |:exec| and must 
not contain function definitions.

NOTE: The "parser" is quite primitive. These tags have to start as 
single statement in a line at column 1, and they have to appear in the 
following order:

    <tskel:msg>                                     *<tskel:msg>*
        Display an explanatory message after template expansion

    <tskel:before>                                  *<tskel:before>*
        Execute code before template expansion in the target buffer

    <tskel:after>                                   *<tskel:after>*
        Execute code after template expansion in the target buffer

    <tskel:here_before>                             *<tskel:here_before>*
        Execute code before template expansion in the template buffer

    <tskel:here_after>                              *<tskel:here_after>*
        Execute code after template expansion in the template buffer

    <tskel:abbrev>                                  *<tskel:abbrev>*
        Make the bit available via |:abbreviate| under the enclosed name.

    <tskel:menu>                                    *<tskel:menu>*
        Use this menu name instead of the default one.

    <tskel:condition>                               *<tskel:condition>*
        An expression that checks whether a bit is eligible in the 
        current context.

BibTeX example: >

    <tskel:msg>
        Insert a collection entry
    </tskel:msg>
    <tskel:before>
        let b:tskelArticleID = input("ID of bibentry: ")
        if b:tskelArticleID == "" | let b:tskelArticleID = "<+CURSOR+>" | endif
    </tskel:before>
    <tskel:after>
        unlet b:tskelArticleID
    </tskel:after>
    @INCOLLECTION{<+b:tskelArticleID+>,
        author   = {<+CURSOR+>},
        title    = {<+ARTICLE TITLE+>},
        crossref = {<+CROSSREF+>},
        pages    = {<+PAGES+>},
        abstract = {[[~/Docs/Abstracts/<+b:tskelArticleID+>.txt]]},
    }
    <++>

In the above example, we query the user for an ID and insert this ID as 
entry key and as an abstract's file name.

The before/after blocks are evaluated in the destination buffer. The 
variants here_before/here_after are evaluated in the scratch buffer for 
the current code skeleton.

                                                    *tskeleton-groups*
                                                    *g:tskelBitGroup_{&filetype}*
Groups~

Some filetype's bits might be of use for other filetypes too. You can 
make them accessible by defining a g:tskelBitGroup_{&filetype} variable. 
E.g., in php mode all html bits are made accessible by setting this 
variable (the default): >

    let g:tskelBitGroup_php = ['php', 'html']

Bits of type "general" are always available.

                                                    *tskeleton-context*
                                                    *tskeleton-map*
Maps -- Context-sensitive expansion~

To some extent, tskeleton is capable of offering the user only a small 
selection of eligible bits for a specific context if a map file 
($VIMFILES/skeletons/map/{&filetype}) is provided. Such a map file is made up 
of regular expressions matching a specific context (before the cursor 
only) and a blank-separated list of eligible bits. The regexp and the 
list are separated by whitespace: >

    REGEXP  BIT1 BIT2 ... BITn

Example: >

    <form\\([^>]\\|\\n\\)*	name= action= method=

If an eligible bit is undefined, the name is inserted as is. I.e. you 
don't have to define skeletons for all these options and argument names.


                                                    *tskeleton-minibits*
Minibits~

Mini bits are kept in the files:

    - $CWD/.tskelmini
    - $VIMFILES/skeletons/bits/{&filetype}.txt

These files contain whitespace-separated pairs of bit names and their 
expansions. These files are meant to keep expansions of accronyms and 
abbreviations and the like. Example: >

    IMHO    In my humble opinion
    AFAIK   As far as I know

                                                    *tskeleton-menu*
                                                    *g:tskelMenuPrefix*
Menu~

If g:tskelMenuPrefix is non-empty, tskeleton will display a menu 
containing all eligible bits for a certain filetype.

The menu can be hierarchical and certain entries may have shortcuts by 
properly naming the bits. Example: >

    &Environment.&Quote
    &Environment.Q&uotation

This will create the submenu "Environment" that can be selected by 
typing "e" (on Windows) and two entries, the first of which can be 
selected by typing "q" and the second by typing "u".

Be aware that the actual bit names are Quote and Quotation (i.e. the 
submenu and the ampersand are stripped off).


                                                    *g:tskelMapComplete*
Insert-mode completion~

If g:tskelMapComplete is true, 'completefunc' is set to 
TSkeleton_complete so that the user will be presented with a selection 
of elegible skeletons when pressing |i_CTRL-X_CTRL-U|.


-----------------------------------------------------------------------
                                                    *tskeleton-commands*
Commands~
                                                    *:TSkeletonNewFile*
:TSkeletonNewFile ?template, ?destDir, ?destFileName

                                                    *:TSkeletonEdit*
:TSkeletonEdit ?skelDir

                                                    *:TSkeletonBit*
:TSkeletonBit NAME


-----------------------------------------------------------------------
                                                    *tskeleton-utilities*
Utilities~

                                                    *tskeleton#IncreaseRevisionNumber()*
The function tskeleton#IncreaseRevisionNumber() provides a way to 
automatically update a revision number in the form >

    @Revision: 1.0.314

In order to use this function, add something like this to your |vimrc| 
file: >

    autocmd BufWritePre * call tskeleton#IncreaseRevisionNumber()

                                                    *:TSkeletonCleanUpBibEntry*
The TSkeletonCleanUpBibEntry command can be used to purge the current bibtex 
entry from expendable fields (i.e., lines matching <+.\{-}+>).

For bibtex files, this command is bound to: <Leader>tc

                                                    *TSkeletonMapGoToNextTag()*
                                                    *tskeleton#GoToNextTag()*
If you don't want to install imaps.vim, this function will map <c-j> to 
tskeleton#GoToNextTag() in order to easily jump between tags.

You can insert >

    call TSkeletonMapGoToNextTag()

in after/plugin/tskeleton.vim to map tskeleton#GoToNextTag() to <c-j> 
similar to imap.vim.

This will map <c-j> to tskeleton#GoToNextTag() that makes the cursor jump 
to the next tag.

This function provides one or two extras over the version of imaps.vim. 
An explanation:

    - ###, +++, ???, !!! are used as markers too.
    - If a marker is empty (e.g. <++>), the marker will be removed (as 
      imaps.vim does).
    - If a marker matches <+NAME/DEFAULT+>, the marker will be replaced 
      with DEFAULT.

autoload/tskeleton/abbreviations.vim	[[[1
71
" abbreviations.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-09-15.
" @Last Change: 2009-08-11.
" @Revision:    0.0.62

if &cp || exists("loaded_tskeleton_abbreviations_autoload")
    finish
endif
let loaded_tskeleton_abbreviations_autoload = 1


function! tskeleton#abbreviations#Reset() "{{{3
    let s:abbrevs = tlib#cmd#OutputAsList('abbrev')
endf


function! tskeleton#abbreviations#Initialize() "{{{3
    call tskeleton#abbreviations#Reset()
endf


function! tskeleton#abbreviations#GetAbbreviations() "{{{3
    if !exists('s:abbrev')
        call tskeleton#abbreviations#Reset()
    endif
    return s:abbrev
endf


function! tskeleton#abbreviations#BufferBits(dict, filetype) "{{{3
    call filter(s:abbrevs, 'v:val =~ ''^[i!]''')
    let rx = '^\(.\)\s\+\(\S\+\)\s\+\(.\+\)$'
    for abbr in sort(s:abbrevs)
        let matches = matchlist(abbr, rx)
        " TLogVAR abbr, rx, matches
        let name = matches[2]
        " TLogVAR name
        " TLogDBG has_key(a:dict, name.g:tskelAbbrevPostfix)
        let text = matches[3]
        if text !~ printf(tlib#rx#Escape(tskeleton#ExpandedAbbreviationTemplate()), '.\{-}')
            let a:dict[name] = {
                        \ 'text': text,
                        \ 'abbrev_type': matches[1],
                        \ 'abbrev': '',
                        \ 'menu': 'Abbreviation.'. escape(name, '.\'),
                        \ 'type': 'abbreviations'
                        \ }
        endif
    endfor
endf


function! tskeleton#abbreviations#Retrieve(bit, indent, ft) "{{{3
    let def = tskeleton#BitDef(a:bit)
    let text = def.text
    " let text .= tskeleton#CursorMarker()
    " TLogVAR a:bit, a:def
    " if text[0] == '@'
    "     exec 'norm! i'. text[1:-1]
    " else
    "     exec 'norm! i'. text
    " endif
    exec 'norm i'. a:bit
    call tlib#buffer#InsertText0(tskeleton#CursorMarker())
    return line('.')
endf


autoload/tskeleton/functions.vim	[[[1
33
" functions.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-09-15.
" @Last Change: 2009-02-15.
" @Revision:    0.0.5

if &cp || exists("loaded_tskeleton_functions_autoload")
    finish
endif
let loaded_tskeleton_functions_autoload = 1


function! tskeleton#functions#Initialize() "{{{3
endf


function! tskeleton#functions#FiletypeBits_vim(dict, filetype) "{{{3
    " TAssert IsDictionary(a:dict)
    " TAssert IsString(a:filetype)
    let fnl = tlib#cmd#OutputAsList('fun')
    call map(fnl, 'matchstr(v:val, ''^\S\+\s\+\zs.\+$'')')
    call filter(fnl, 'v:val[0:4] != ''<SNR>''')
    for f in sort(fnl)
        let fn = matchstr(f, '^.\{-}\ze(')
        let fr = substitute(f, '(\(.\{-}\))$', '\=tskeleton#ReplacePrototypeArgs(submatch(1), ''\V...'')', "g")
        " TLogDBG fn ." -> ". fr
        let a:dict[fn] = {'text': fr, 'menu': 'Function.'. fn, 'type': 'tskeleton'}
    endfor
endf


autoload/tskeleton/mini.vim	[[[1
30
" mini.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-09-15.
" @Last Change: 2010-09-04.
" @Revision:    0.0.17

if &cp || exists("loaded_tskeleton_mini_autoload") "{{{2
    finish
endif
let loaded_tskeleton_mini_autoload = 1


function! tskeleton#mini#Initialize() "{{{3
endf


function! tskeleton#mini#FiletypeBits(dict, type) "{{{3
    " TLogVAR a:dict, a:type
    " call tskeleton#FetchMiniBits(a:dict, expand('%:p:h') .'/.tskelmini', 1)
    let files = findfile('.tskelmini', expand('%:p:h') .';', -1)
    " TLogVAR files
    for file in reverse(files)
        " TLogVAR file
        call tskeleton#FetchMiniBits(a:dict, file, 1)
    endfor
endf


autoload/tskeleton/skeleton.vim	[[[1
45
" skeleton.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-09-15.
" @Last Change: 2010-05-13.
" @Revision:    0.0.25

if &cp || exists("loaded_tskeleton_skeleton_autoload")
    finish
endif
let loaded_tskeleton_skeleton_autoload = 1


function! tskeleton#skeleton#Initialize() "{{{3
endf

function! tskeleton#skeleton#FiletypeBits(dict, type) "{{{3
    " TAssert IsDictionary(a:dict)
    " TAssert IsString(a:type)
    call tskeleton#FetchMiniBits(a:dict, g:tskelBitsDir . a:type .'.txt', 0)
    let bf = tskeleton#GlobBits(g:tskelBitsDir . a:type .'/', 2)
    " let cx = tskeleton#CursorMarker('rx')
    " let cm = tskeleton#CursorMarker()
    for f in bf
        " TLogVAR f
        if !isdirectory(f) && filereadable(f)
            let fname = fnamemodify(f, ":t")
            let [cname, mname] = tskeleton#PurifyBit(fname)
            " TLogVAR cname
            let body = join(readfile(f), "\n")
            let [body, meta] = tskeleton#ExtractMeta(body)
            " if body !~ cx
            "     let body .= cm
            " endif
            if has_key(meta, 'menu') && !empty(meta.menu)
                let mname = meta.menu
            endif
            let a:dict[cname] = {'text': body, 'menu': mname, 'meta': meta, 'bitfile': f, 'type': 'tskeleton'}
            " TLogVAR a:dict[cname]
        endif
    endfor
endf


autoload/tskeleton/tags.vim	[[[1
95
" tags.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-09-15.
" @Last Change: 2009-02-15.
" @Revision:    0.0.16

if &cp || exists("loaded_tskeleton_tags_autoload")
    finish
endif
let loaded_tskeleton_tags_autoload = 1


let s:tag_defs = {}


function! tskeleton#tags#Initialize() "{{{3
endf


function! s:SortBySource(a, b)
    let ta = s:sort_tag_defs[a:a]
    let tb = s:sort_tag_defs[a:b]
    let fa = ta.source
    let fb = tb.source
    if fa == fb
        return ta.menu == tb.menu ? 0 : ta.menu > tb.menu ? 1 : -1
    else
        return fa > fb ? 1 : -1
    endif
endf


function! s:SortByFilename(tag1, tag2)
    let f1 = a:tag1['filename']
    let f2 = a:tag2['filename']
    return f1 == f2 ? 0 : f1 > f2 ? 1 : -1
endf


function! tskeleton#tags#BufferBits(dict, filetype) "{{{3
    " TAssert IsDictionary(a:dict)
    " TAssert IsString(a:filetype)
    " TLogVAR a:filetype
    if exists('*tskeleton#tags#Process_'. a:filetype)
        let td_id = join(map(tagfiles(), 'fnamemodify(v:val, ":p")'), '\n')
        " TLogVAR td_id
        if !empty(td_id)
            let tag_defs = get(s:tag_defs, td_id, {})
            " TLogDBG len(tag_defs)
            if empty(tag_defs)
                echom 'tSkeleton: Building tags menu for '. expand('%')
                let tags = taglist('.')
                call sort(tags, 's:SortByFilename')
                call filter(tags, 'tskeleton#tags#Process_{a:filetype}(tag_defs, v:val)')
                let s:tag_defs[td_id] = tag_defs
                echo
                redraw
            endif
            call extend(a:dict, tag_defs, 'keep')
            let menu_prefix = tlib#var#Get('tskelMenuPrefix_tags', 'bg')
            if !empty(menu_prefix)
                let s:sort_tag_defs = tag_defs
                let tagnames = sort(keys(tag_defs), 's:SortBySource')
                " TLogVAR tagnames
                " call filter(tagnames, 'tskeleton#NewBufferMenuItem(b:tskelBufferMenu, v:val)')
                call filter(tagnames, 'tskeleton#NewBufferMenuItem(b:tskelBufferMenu, v:val)')
            endif
        endif
    endif
endf


function! tskeleton#tags#Process_vim(dict, tag)
    return tskeleton#ProcessTag_functions_with_parentheses('f', a:dict, a:tag, '\V...')
endf


function! tskeleton#tags#Process_ruby(dict, tag)
    return tskeleton#ProcessTag_functions_with_parentheses('f', a:dict, a:tag, '\*\a\+\s*$')
endf


function! tskeleton#tags#Process_c(dict, tag)
    return tskeleton#ProcessTag_functions_with_parentheses('f', a:dict, a:tag, '')
endf


function! tskeleton#tags#Process_java(dict, tag)
    return tskeleton#ProcessTag_functions_with_parentheses('f', a:dict, a:tag, '\V...')
endf



autoload/tskeleton/completefunc.vim	[[[1
26
" completefunc.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2009-08-23.
" @Last Change: 2010-02-26.
" @Revision:    0.0.12

let s:save_cpo = &cpo
set cpo&vim


function! tskeleton#completefunc#Initialize() "{{{3
endf


function! tskeleton#completefunc#FiletypeBits(dict, type) "{{{3
    " TAssert IsDictionary(a:dict)
    " TAssert IsString(a:type)
    call tskeleton#Complete_use_completefunc('', a:dict)
endf



let &cpo = s:save_cpo
unlet s:save_cpo
autoload/tskeleton/omnicomplete.vim	[[[1
26
" omnicomplete.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2009-08-23.
" @Last Change: 2010-02-26.
" @Revision:    0.0.15

let s:save_cpo = &cpo
set cpo&vim


function! tskeleton#omnicomplete#Initialize() "{{{3
endf


function! tskeleton#omnicomplete#FiletypeBits(dict, type) "{{{3
    " TAssert IsDictionary(a:dict)
    " TAssert IsString(a:type)
    call tskeleton#Complete_use_omnifunc('', a:dict)
endf



let &cpo = s:save_cpo
unlet s:save_cpo
autoload/tskeleton.vim	[[[1
3183
" tskeleton.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-09-03.
" @Last Change: 2010-09-04.
" @Revision:    0.0.1784


" call tlog#Log('Load: '. expand('<sfile>')) " vimtlib-sfile


if !exists('g:tskelMapsDir') "{{{2
    let g:tskelMapsDir = g:tskelDir .'map/'
endif
let g:tskelMapsDir = tlib#dir#CanonicName(g:tskelDir)

if !exists('g:tskelBitsIgnore')
    let g:tskelBitsIgnore = tlib#rx#Suffixes()   "{{{2
endif

if !exists("g:tskelTypes") "{{{2
    " 'skeleton' (standard tSkeleton functionality)
    " 'abbreviations' (VIM abbreviations)
    " 'functions' (VIM script functions extracted from :function)
    " 'mini' ("mini" bits, one-liners etc.)
    " 'tags' (tags-based code templates, requires ctags, I presume)
    let g:tskelTypes = ['skeleton', 'mini']
endif

if !exists('g:tskelLicense') "{{{2
    let g:tskelLicense = 'GPL (see http://www.gnu.org/licenses/gpl.txt)'
endif

if !exists("g:tskelMarkerHiGroup") | let g:tskelMarkerHiGroup = 'Special'     | endif "{{{2
if !exists("g:tskelMarkerLeft")    | let g:tskelMarkerLeft    = "<+"           | endif "{{{2
if !exists("g:tskelMarkerRight")   | let g:tskelMarkerRight   = "+>"           | endif "{{{2
if !exists('g:tskelMarkerExtra')   | let g:tskelMarkerExtra   = '???\|+++\|!!!\|###' | endif
if !exists("g:tskelMarkerCursor_mark") | let g:tskelMarkerCursor_mark = "CURSOR"           | endif "{{{2
if !exists("g:tskelMarkerCursor_volatile") | let g:tskelMarkerCursor_volatile = "/CURSOR"           | endif "{{{2
if !exists("g:tskelMarkerCursor_rx")   | let g:tskelMarkerCursor_rx = 'CURSOR\(/\(.\{-}\)\)\?' | endif "{{{2
if !exists("g:tskelDateFormat")    | let g:tskelDateFormat    = '%Y-%m-%d'    | endif "{{{2
if !exists("g:tskelUserName")      | let g:tskelUserName      = g:tskelMarkerLeft."NAME".g:tskelMarkerRight    | endif "{{{2
if !exists("g:tskelUserAddr")      | let g:tskelUserAddr      = g:tskelMarkerLeft."ADDRESS".g:tskelMarkerRight | endif "{{{2
if !exists("g:tskelUserEmail")     | let g:tskelUserEmail     = g:tskelMarkerLeft."EMAIL".g:tskelMarkerRight   | endif "{{{2
if !exists("g:tskelUserWWW")       | let g:tskelUserWWW       = g:tskelMarkerLeft."WWW".g:tskelMarkerRight     | endif "{{{2

if !exists("g:tskelRevisionMarkerRx") | let g:tskelRevisionMarkerRx = '@Revision:\s\+' | endif "{{{2
if !exists("g:tskelRevisionVerRx")    | let g:tskelRevisionVerRx = '\(RC\d*\|pre\d*\|p\d\+\|-\?\d\+\)\.' | endif "{{{2
if !exists("g:tskelRevisionGrpIdx")   | let g:tskelRevisionGrpIdx = 3 | endif "{{{2

if !exists("g:tskelMaxRecDepth") | let g:tskelMaxRecDepth = 10 | endif "{{{2
if !exists("g:tskelChangeDir")   | let g:tskelChangeDir   = 1  | endif "{{{2
if !exists("g:tskelMapComplete") | let g:tskelMapComplete = 1  | endif "{{{2
" if g:tskelMapComplete
"     set completefunc=tskeleton#Complete
" endif

if !exists("g:tskelMenuPriority")   | let g:tskelMenuPriority = 90         | endif "{{{2
if !exists("g:tskelMenuMiniPrefix") | let g:tskelMenuMiniPrefix = 'etc.'   | endif "{{{2
if !exists("g:tskelAutoAbbrevs")    | let g:tskelAutoAbbrevs = 0           | endif "{{{2
if !exists("g:tskelAbbrevPostfix")  | let g:tskelAbbrevPostfix = '#'       | endif "{{{2

" By default bit names are case sensitive.
"  1 ... case sensitive
" -1 ... default (see 'smartcase')
"  0 ... case insensitive
if !exists("g:tskelCaseSensitive")        | let g:tskelCaseSensitive = 1        | endif "{{{2
if !exists("g:tskelCaseSensitive_html")   | let g:tskelCaseSensitive_html = 0   | endif "{{{2
if !exists("g:tskelCaseSensitive_bbcode") | let g:tskelCaseSensitive_bbcode = 0 | endif "{{{2

if !exists("g:tskelUseBufferCache") | let g:tskelUseBufferCache = 0             | endif "{{{2
if !exists("g:tskelBufferCacheDir") | let g:tskelBufferCacheDir = '.tskeleton'  | endif "{{{2

if !exists("g:tskelMenuPrefix_tags") | let g:tskelMenuPrefix_tags = 'Tags.' | endif "{{{2

if !exists("g:tskelQueryType") "{{{2
    " if has('gui_win32') || has('gui_win32s') || has('gui_gtk')
    "     let g:tskelQueryType = 'popup'
    " else
        let g:tskelQueryType = 'query'
    " end
endif

if !exists("g:tskelPopupNumbered") | let g:tskelPopupNumbered = 1 | endif "{{{2

" set this to v for using visual mode when calling TSkeletonGoToNextTag()
if !exists("g:tskelSelectTagMode") | let g:tskelSelectTagMode = 's' | endif "{{{2

" \Q will be replaced with a quantifier
if !exists("g:tskelKeyword_bbcode") | let g:tskelKeyword_bbcode = '\(\[\*\|[\[\\][*[:alnum:]]\Q\)' | endif "{{{2
if !exists("g:tskelKeyword_bib")  | let g:tskelKeyword_bib  = '[@[:alnum:]]\Q'       | endif "{{{2
if !exists("g:tskelKeyword_java") | let g:tskelKeyword_java = '[[:alnum:]_@<&]\Q'    | endif "{{{2
if !exists("g:tskelKeyword_php")  | let g:tskelKeyword_java = '[[:alnum:]_@<&$]\Q'   | endif "{{{2
if !exists("g:tskelKeyword_html") | let g:tskelKeyword_html = '<\?[^>[:blank:]]\Q'   | endif "{{{2
if !exists("g:tskelKeyword_sh")   | let g:tskelKeyword_sh   = '[\[@${([:alpha:]]\Q'  | endif "{{{2
if !exists("g:tskelKeyword_tex")  | let g:tskelKeyword_tex  = '\\\?\k\Q'             | endif "{{{2
if !exists("g:tskelKeyword_viki") | let g:tskelKeyword_viki = '\(#\|{\|\\\)\?[^#{[:blank:][:punct:]-]\Q' | endif "{{{2
" if !exists("g:tskelKeyword_viki") | let g:tskelKeyword_viki = '\(#\|{\)\?[^#{[:blank:]]\{-}' | endif "{{{2

if !exists("g:tskelBitGroup_html") "{{{2
    let g:tskelBitGroup_html = ['html', 'html_common']
endif
if !exists("g:tskelBitGroup_bbcode") "{{{2
    let g:tskelBitGroup_bbcode = ['bbcode', 'tex']
endif
if !exists("g:tskelBitGroup_php") "{{{2
    let g:tskelBitGroup_php  = ['php', 'html', 'html_common']
endif
if !exists("g:tskelBitGroup_java") "{{{2
    let g:tskelBitGroup_java = ['java', 'html_common']
endif
if !exists("g:tskelBitGroup_viki") "{{{2
    let g:tskelBitGroup_viki = ['tex', 'viki']
endif
if !exists("g:tskelBitGroup_xslt") "{{{2
    let g:tskelBitGroup_xslt = ['xslt', 'xml']
endif


if !exists("g:tskel_completions") "{{{2
    let g:tskel_completions = {
                \ 'use_omnifunc': 'tskeleton#Complete_use_omnifunc',
                \ 'use_completefunc': 'tskeleton#Complete_use_completefunc',
                \ 'scan_words': 'tskeleton#Complete_scan_words',
                \ 'scan_tags': 'tskeleton#Complete_scan_tags',
                \ }
endif




if !exists('*TSkeleton_FILE_DIRNAME') "{{{2
    function! TSkeleton_FILE_DIRNAME() "{{{3
        return tskeleton#EvalInDestBuffer('expand("%:p:h")')
    endf
endif


if !exists('*TSkeleton_FILE_SUFFIX') "{{{2
    function! TSkeleton_FILE_SUFFIX() "{{{3
        return tskeleton#EvalInDestBuffer('expand("%:e")')
    endf
endif


if !exists('*TSkeleton_FILE_NAME_ROOT') "{{{2
    function! TSkeleton_FILE_NAME_ROOT() "{{{3
        return tskeleton#EvalInDestBuffer('expand("%:t:r")')
    endf
endif


if !exists('*TSkeleton_FILE_NAME') "{{{2
    function! TSkeleton_FILE_NAME() "{{{3
        return tskeleton#EvalInDestBuffer('expand("%:t")')
    endf
endif


if !exists('*TSkeleton_NOTE') "{{{2
    function! TSkeleton_NOTE() "{{{3
        let title = tskeleton#GetVar("tskelTitle", 'input("Please describe the project: ")', '')
        let note  = title != "" ? " -- ".title : ""
        return note
    endf
endif


if !exists('*TSkeleton_DATE') "{{{2
    function! TSkeleton_DATE() "{{{3
        return strftime(tskeleton#GetVar('tskelDateFormat'))
    endf
endif


if !exists('*TSkeleton_TIME') "{{{2
    function! TSkeleton_TIME() "{{{3
        return strftime('%X')
    endf
endif


if !exists('*TSkeleton_AUTHOR') "{{{2
    function! TSkeleton_AUTHOR() "{{{3
        return tskeleton#GetVar('tskelUserName')
    endf
endif


if !exists('*TSkeleton_EMAIL') "{{{2
    function! TSkeleton_EMAIL() "{{{3
        let email = tskeleton#GetVar('tskelUserEmail')
        " return substitute(email, "@"," AT ", "g")
        return email
    endf
endif


if !exists('*TSkeleton_WEBSITE') "{{{2
    function! TSkeleton_WEBSITE() "{{{3
        return tskeleton#GetVar('tskelUserWWW')
    endf
endif


if !exists('*TSkeleton_LICENSE') "{{{2
    function! TSkeleton_LICENSE() "{{{3
        return tskeleton#GetVar('tskelLicense')
    endf
endif


function! TSkeletonCB_FILENAME() "{{{3
    return input('File name: ', '', 'file')
endf


function! TSkeletonCB_DIRNAME() "{{{3
    return input('Directory name: ', '', 'dir')
endf


function! TSkelNewScratchHook_viki()
    let b:vikiMarkInexistent = 0
endf






" let s:tskelScratchVars = ['tskelMarkerCursor', 'tskelMarkerLeft', 'tskelMarkerRight']
let s:tskelScratchVars = []
let s:tskelScratchIdx  = 0
let s:tskelScratchMax  = 0
let s:tskelDestBufNr   = -1
let s:tskelBuiltMenu   = 0
let s:tskelLine        = 0
let s:tskelCol         = 0
let s:tskelProcessing  = 0
let s:tskelConditions  = []
let s:tskelLoops       = []
let s:initialized      = []


function! tskeleton#Initialize(...) "{{{3
    TVarArg ['types', g:tskelTypes]
    " TLogVAR types
    for type in types
        if index(s:initialized, type) == -1
            " TLogVAR type
            call {'tskeleton#'. type .'#Initialize'}()
            call add(s:initialized, type)
        endif
    endfor
endf


function! tskeleton#WrapMarker(text, ...) "{{{3
    TVarArg 'type'
    let left  = tlib#var#Get('tskelMarkerLeft', 'bg')
    let right = tlib#var#Get('tskelMarkerRight', 'bg')
    if type == 'rx'
        return tlib#rx#Escape(left) . a:text . tlib#rx#Escape(right)
    else
        return left . a:text . right
    endif
endf


function! tskeleton#CursorMarker(...) "{{{3
    TVarArg ['type', 'mark']
    let cursor = tlib#var#Get('tskelMarkerCursor_'.type, 'bg')
    return tskeleton#WrapMarker(cursor, type)
endf


function! tskeleton#TagRx() "{{{3
    " let quoted = '"\(\\"\|[^"]\)*"\|''\(''''\|[^'']\)*'''
    " return tskeleton#WrapMarker("\\("
    "         \ ."[\\&].\\{-}\\|[gbws]:.\\{-}\\|\\(bit\\|tskel\\):.\\{-}"
    "         \ ."\\|call:\\(". quoted ."\\|[bgs]:\\|.\\)\\{-1,}"
    "         \ ."\\|[a-zA-Z_ -]*\\(/.\\{-}\\)\\?"
    "         \ ."\\|\\(if\\|elseif\\|for\\|input\\|let\\|\\include\\|execute\\)(\\([^'\"]*\\|". quoted ."\\)\\{-})"
    "         \ ."\\|?.\\{-}?"
    "         \ ."\\)\\(: *.\\{-} *\\)\\?"
    "         \ , 'rx')
    return tskeleton#WrapMarker("\\("
            \ ."[\\&].\\{-}\\|[gbws]:.\\{-}\\|\\(bit\\|tskel\\):.\\{-}"
            \ ."\\|call:\\('[^']*'\\|\"\\(\\\\\"\\|[^\"]\\)*\"\\|[bgs]:\\|.\\)\\{-1,}"
            \ ."\\|[a-zA-Z_ -]*\\(/.\\{-}\\)\\?"
            \ ."\\|\\(if\\|elseif\\|for\\|input\\|select\\|let\\|\\include\\|execute\\)(.\\{-})"
            \ ."\\|?.\\{-}?"
            \ ."\\)\\(: *.\\{-} *\\)\\?"
            \ , 'rx')
endf

function! tskeleton#ExpandedAbbreviationTemplate() "{{{3
    return '<c-\><c-o>:call tskeleton#ExpandBitUnderCursor("n", %s)<cr>'
endf


" :def: function! tskeleton#FillIn(bit, ?filetype='', ?meta={})
" Expand special tags.
function! tskeleton#FillIn(bit, ...) "{{{3
    " try
        " TLogVAR a:bit
        let filetype = a:0 >= 1 && a:1 != '' ? a:1 : s:Filetype()
        " TLogVAR filetype
        call tskeleton#PrepareBits(filetype)
        let b:tskelTemporaryVariables = []
        if a:0 >= 2
            let meta = a:2
        else
            let bitdef = tskeleton#BitDef(a:bit)
            " TLogVAR bitdef
            let meta = get(bitdef, 'meta', {})
        endif
        " TLogVAR meta
        if !empty(meta)
            let msg = get(meta, 'msg', '')
            if !empty(msg)
                echom msg
            endif
            call s:EvalBitProcess(get(meta, 'before'), 1)
            call s:EvalBitProcess(get(meta, 'here_before'), 0)
        endif
        " TLogDBG string(getline(1, '$'))
        " silent norm! G$
        silent norm! gg0
        " call tlog#Debug(tskeleton#TagRx())
        " call tlog#Debug(s:tskelScratchIdx)
        let s:tskelLine_{s:tskelScratchIdx} = search(tskeleton#TagRx(), 'cW')
        while s:tskelLine_{s:tskelScratchIdx} > 0
            " TLogDBG string(getpos('.'))
            " TLogDBG string(getline(1, '$'))
            let s:tskelPos0 = getpos('.')
            " call tlog#Debug(s:tskelLine_{s:tskelScratchIdx})
            " let col  = virtcol(".")
            let col  = col('.')
            " TLogVAR col
            let line = strpart(getline('.'), col - 1)
            let mlst = matchlist(line, tskeleton#TagRx())
            " TLogVAR mlst
            " TLogVAR getline('.'), col, line
            let text0 = matchstr(line, tskeleton#TagRx())
            " TLogVAR text0
            " let text  = substitute(line, tskeleton#TagRx() .'.*$', '\1', '')
            let text  = mlst[1]
            " TLogVAR text
            let s:tskelPostExpand = ''
            let [postprocess, repl] = s:HandleTag(text, b:tskelFiletype)
            " TLogVAR postprocess, repl
            if postprocess == -1
                call s:ReplaceLine(col, repl)
                exec 'norm! '. len(repl) .'l'
            elseif postprocess == 1
                if repl != '' && line =~ '\V\^'. escape(repl, '\')
                    norm! l
                else
                    let mod  = get(mlst, 7)
                    " TLogVAR mod
                    let repl = s:Modify(repl, mod)
                    " TLogVAR repl
                    call s:ReplaceLine(col, repl)
                endif
            endif
            if s:tskelPostExpand != ''
                " call tlog#Debug(s:tskelPostExpand)
                exec s:tskelPostExpand
                let s:tskelPostExpand = ''
            end
            if s:tskelLine_{s:tskelScratchIdx} > 0
                " call tlog#Debug('search(tskeleton#TagRx(), "W")')
                let s:tskelLine_{s:tskelScratchIdx} = search(tskeleton#TagRx(), 'cW')
            endif
		endwh
        " TLogDBG "endwhile"
        if !empty(meta)
            call s:EvalBitProcess(get(meta, 'here_after'), 0)
            call s:EvalBitProcess(get(meta, 'after'), 1)
        endif
        " TLogVAR b:tskelTemporaryVariables
        for def in b:tskelTemporaryVariables
            let var = get(def, 0)
            " TLogVAR def, var
            if len(def) == 1
                exec 'unlet! '. var
            else
                call s:SetVar(var, get(def, 1))
            endif
        endfor
        if empty(a:bit)
            " TLogDBG "tskeleton#SetCursor"
            call tskeleton#SetCursor('%', '')
        endif
        " TLogDBG "done"
    " catch
    "     echom "An error occurred in tskeleton#FillIn() ... ignored"
    " endtry
endf


function! s:ReplaceLine(col, repl) "{{{3
    let tagrx = escape(tskeleton#TagRx(), '/')
    exec 'silent! s/\%'. a:col .'c'. tagrx .'//'
    call tlib#buffer#InsertText0(a:repl, {
                \ 'pos': 's',
                \ 'col': a:col,
                \ 'indent': 1,
                \ })
    " call tlib#buffer#InsertText(a:repl, {
    "             \ 'pos': 's',
    "             \ 'shift': -1,
    "             \ 'col': a:col,
    "             \ 'indent': 1,
    "             \ })
endf


function! tskeleton#ExtractMeta(text)
    " TLogVAR a:text
    let meta = {'type': 'tskeleton'}
    let [text, meta.msg]         = s:GetBitProcess(a:text, 'msg', 2)
    let [text, meta.before]      = s:GetBitProcess(text, 'before', 1)
    " TLogVAR meta.before
    let [text, meta.after]       = s:GetBitProcess(text, 'after', 1)
    " TLogVAR meta.after
    let [text, meta.here_before] = s:GetBitProcess(text, 'here_before', 0)
    " TLogVAR meta.here_before
    let [text, meta.here_after]  = s:GetBitProcess(text, 'here_after', 0)
    " TLogVAR meta.here_after
    let [text, meta.abbrev]      = s:GetBitProcess(text, 'abbrev', 0)
    " TLogVAR meta.abbrev
    let [text, meta.menu]        = s:GetBitProcess(text, 'menu', 0)
    let [text, meta.condition]   = s:GetBitProcess(text, 'condition', 0)
    if empty(meta.condition)
        let meta.condition = 1
    endif
    " TLogVAR meta.condition
    " TLogVAR text
    return [text, meta]
endf


function! s:HandleTag(match, filetype) "{{{3
    " TLogVAR a:match
    let s:tskel_highlight = 1
    " TLogDBG a:match =~# '^[bgsw]:'
    if a:match =~# '^[bgsw]:'
        return [1, s:Var(a:match)]
    elseif a:match =~# '^nl$'
        return [1, "\n"]
    elseif a:match =~# '^nop$'
        return [1, ""]
    elseif a:match =~# '^joinline$'
        call s:JoinLine()
        return [1, ""]
    elseif a:match =~# '^input('
        return s:Input(strpart(a:match, 5))
    elseif a:match =~# '^select('
        return s:Select(strpart(a:match, 6))
    elseif a:match =~# '^if('
        return [0, s:SwitchIf(strpart(a:match, 2))]
    elseif a:match =~# '^elseif('
        return [0, s:SwitchElseif(strpart(a:match, 6))]
    elseif a:match =~# '^else$'
        return [0, s:SwitchElse()]
    elseif a:match =~# '^endif$'
        return [0, s:SwitchEndif()]
    elseif a:match =~# '^for('
        return [0, s:LoopFor(strpart(a:match, 3))]
    elseif a:match =~# '^let('
        return [0, s:LetVar(strpart(a:match, 3))]
    elseif a:match =~# '^include('
        return [1, s:Expand(matchstr(a:match, '(\zs.\{-}\ze)$'), a:filetype)]
    elseif a:match =~# '^execute('
        return [0, s:Execute(matchstr(a:match, '(\zs.\{-}\ze)$'))]
    elseif a:match =~# '^\([A-Z ]\+\)'
        return [1, s:Dispatch(a:match)]
    elseif a:match[0] == '&'
        return [1, s:Exec(a:match)]
    elseif a:match[0] == '\'
        return [-1, tskeleton#WrapMarker(strpart(a:match, 1))]
    elseif a:match[0] == '?'
        return [1, s:Query(strpart(a:match, 1, strlen(a:match) - 2))]
    elseif strpart(a:match, 0, 4) =~# '\(bit\|tskel\):'
        " return [1, s:Expand(strpart(a:match, 4), a:filetype)]
        return [1, s:Expand(matchstr(a:match, ':\zs.*'), a:filetype)]
    " elseif strpart(a:match, 0, 6) =~# 'tskel:'
    "     return [1, s:Expand(strpart(a:match, 6), a:filetype)]
    " elseif strpart(a:match, 0, 6) =~# 'include:'
    "     return [1, s:Expand(strpart(a:match, 8), a:filetype)]
    elseif strpart(a:match, 0, 5) =~# 'call:'
        return [1, s:Call(strpart(a:match, 5))]
    else
        return [1, tskeleton#WrapMarker(a:match)]
    end
endf


" tskeleton#SetCursor(from, to, ?mode='n', ?findOnly)
function! tskeleton#SetCursor(from, to, ...) "{{{3
    " TLogVAR a:from, a:to
    let mode     = a:0 >= 1 ? a:1 : 'n'
    let findOnly = a:0 >= 2 ? a:2 : (s:tskelScratchIdx > 1)
    " TLogVAR mode, findOnly
    let c = col('.')
    " if s:IsEOL(mode) && s:IsInsertMode(mode)
    "     let c += 1
    " end
    let l = line('.')
    if a:to == ''
        if a:from == '%'
            silent norm! gg0
        else
            exec a:from
        endif
    else
        exec a:to
        norm! 0
    end
    let cursor_rx = tskeleton#CursorMarker('rx')
    " if line('.') == 1
    "     norm! G$
        let l = search(cursor_rx, 'Wc')
    " else
    "     norm! k$
    "     let l = search(cursor_rx, 'W')
    " end
    " TLogVAR l, c
    if l == 0
        " if findOnly
        "     let l = line('$')
        "     call cursor(l, len(getline('$')))
        "     return l
        " else
            call cursor(l, c)
            return 0
        " endif
    elseif !findOnly
        let c = col('.')
        " TLogDBG getline('.')
        let smarttaglen = len(get(matchlist(getline('.')[c - 1 :], cursor_rx), 2, ''))
        silent exec 's/'. escape(cursor_rx, '/') .'/\2/e'
        call cursor(0, c)
        if smarttaglen > 0
            exec 'norm! v'. smarttaglen .'l'
        endif
        " TLogDBG getline('.')
    endif
    " TLogVAR l
    return l
endf


function! s:JoinLine()
    let s:tskelPostExpand = 'silent norm! d/\S'
endf


function! s:Input(text) "{{{3
    " TLogVAR a:text
    let args = eval('['. matchstr(a:text, '^(\zs.*\ze)$') .']')
    " TLogVAR args
    let var  = get(args, 0, '')
    let vdef = s:VarName(var)
    if var[-1:-1] == '!'
        call s:DelTo('input(.\{-}', 1)
        let s:tskelPostExpand = "call setpos('.', ". string(s:tskelPos0) .")"
        " TLogDBG string(getline(1, '$'))
        let reval = 0
        let var   = var[0:-2]
    else
        let reval = 1
    endif
    if s:SkipVar(vdef)
        let val = s:GetVar(vdef.name)
    else
        let val = call('input', args[1:-1])
        call s:TemporaryLet(var, val)
    endif
    return [reval, val]
endf


function! s:Select(text) "{{{3
    let args = eval('['. matchstr(a:text, '^(\zs.*\ze)$') .']')
    " TLogVAR args
    let var  = get(args, 0, '')
    let type = get(args, 2, 's')
    let join = get(args, 3, ', ')
    let vdef = s:VarName(var)
    if var[-1:-1] == '!'
        call s:DelTo('select(.\{-}', 1)
        let s:tskelPostExpand = "call setpos('.', ". string(s:tskelPos0) .")"
        " TLogDBG string(getline(1, '$'))
        let reval = 0
        let var   = var[0:-2]
    else
        let reval = 1
    endif
    if s:SkipVar(vdef)
        let val = s:GetVar(vdef.name)
    else
        let val0 = tlib#input#List(type, 'Select item:', get(args, 1, ['Malformed arguments']))
        if type =~# 'm'
            let val = join(val0, join)
        else
            let val = val0
        endif
        call s:TemporaryLet(var, val)
    endif
    return [reval, val]
endf


function! s:TemporaryLet(var, val) "{{{3
    if !empty(a:var)
        " TLogVAR a:var
        let var = s:VarName(a:var)
        if s:SkipVar(var)
            return
        endif
        if var.mod !~ '&'
            if exists(var.name)
                call insert(b:tskelTemporaryVariables, [var.name, s:GetVar(var.name)])
            else
                call insert(b:tskelTemporaryVariables, [var.name])
            endif
        endif
        call s:SetVar(var.name, a:val)
    endif
endf


function! s:VarName(var) "{{{3
     let [match, var, mod; rest] = matchlist(a:var, '^\(.\{-}\)\([!?&]*\)$')
     return {'match': match, 'name': var, 'mod': mod}
endf


function! s:SkipVar(var) "{{{3
    return a:var.mod =~ '?' && exists(a:var.name)
endf

function! s:GetVar(var) "{{{3
    " if a:var[0] == '@'
    "     return getreg(a:var[1:])
    " elseif a:var[0] == '&'
        return eval(a:var)
    " else
    "     return {a:var}
    " endif
endf

function! s:SetVar(var, val) "{{{3
    exec 'let '. a:var .'= a:val'
    return
    " if a:var[0] == '@'
    "     call setreg(a:var[1:], a:val)
    " elseif a:var[0] == '&'
    "     " exec 'let &l:'. a:var[1:] .'= a:val'
    "     exec 'let '. a:var .'= a:val'
    " else
    "     let {a:var} = {a:val}
    " endif
endf


function! s:SwitchIf(text)
    " TLogDBG 'if'
    " TLogVAR a:text
    if empty(s:tskelConditions) || s:LastCondition()
        if eval(a:text)
            " TLogDBG 'true'
            call add(s:tskelConditions, 1)
            call s:DelTo('if(.\{-}', 1)
        else
            " TLogDBG 'false'
            call add(s:tskelConditions, 0)
            call s:DelTo('\(if(.\{-}\|elseif(.\{-}\|else\|endif\)', 0)
        endif
    else
        call add(s:tskelConditions, 0)
        call s:DelTo('\(if(.\{-}\|endif\)', 0)
    endif
    return ''
endf


function! s:SwitchElseif(text)
    " TLogDBG 'elseif'
    " TLogVAR a:text
    " TLogDBG string(getline(1, '$'))
    if s:LastCondition()
        " TLogDBG 'elseif obsolete'
        call s:DelTo('endif', 1)
    elseif eval(a:text)
        " TLogDBG 'elseif true'
        call add(s:tskelConditions, 1)
        call s:DelTo('elseif(.\{-}', 1)
    else
        " TLogDBG 'elseif false'
        call s:DelTo('\(elseif(.\{-}\|else\|endif\)', 0)
    endif
    return ''
endf


function! s:SwitchElse()
    " TLogDBG 'else'
    if s:LastCondition()
        " TLogDBG 'else false'
        call s:DelTo('endif', 1)
    else
        " TLogDBG 'else true'
        let s:tskelConditions[-1] = 1
        call s:DelTo('else', 1)
    endif
    return ''
endf


function! s:SwitchEndif()
    " TLogDBG 'endif'
    if empty(s:tskelConditions)
        echoerr 'tSkeleton: "endif" without "if"'
    endif
    call remove(s:tskelConditions, -1)
    call s:DelTo('endif', 1)
    return ''
endf


function! s:LastCondition() "{{{3
    if !empty(s:tskelConditions)
        return s:tskelConditions[-1]
    endif
    echoerr 'tSkeleton: "else/elseif/endif" without "if"'
    return 0
endf


function! s:LetVar(text) "{{{3
    let var = matchstr(a:text, '^(\s*\zs[^=]\{-}\ze\s*=')
    let val = matchstr(a:text, '=\s*\zs.\{-}\ze)$')
    " TLogVAR var, val
    call s:TemporaryLet(var, eval(val))
    call s:DelTo('let.\{-}', 1)
    return ''
endf


function! s:LoopFor(text) "{{{3
    " TLogDBG 'for'
    " TLogVAR a:text
    let var   = matchstr(a:text, '^(\zs\S\+')
    let endrx = 'endfor\(('.var.')\)\?'
    " TLogVAR var, endrx
    let lists = matchstr(a:text, '\sin\s\+\zs.\{-}\ze)$')
    " TLogVAR var, lists
    let t = @t
    try
        call s:DoVisual('for(.\{-}', 1, '"ty')
        let head = @t
        " TLogVAR head
        call s:DelTo('for(.\{-}', 1)
        call s:DoVisual(endrx, 0, '"ty')
        let body = @t
        " TLogVAR body
        call s:DelTo(endrx, 1)
    finally
        let @t = t
    endtry
    let acc = []
    for e in eval(lists)
        call add(acc, tskeleton#WrapMarker(printf('let(%s=%s)', var, string(e))))
        call add(acc, body)
    endfor
    " TLogVAR acc
    " call s:InsertBitText('i', join(acc, ''))
    call s:InsertBitText('gil', join(acc, ''))
    let s:tskelPostExpand = "call setpos('.', ". string(s:tskelPos0) .")"
    return ''
endf


function! s:DoVisual(pattern, inclusive, cmd) "{{{3
    let rx = tskeleton#WrapMarker(a:pattern, 'rx')
    " TLogVAR rx
    exec 'silent! norm! v/'. escape(rx, '/') .(a:inclusive ? '/e1' : '') ."\<cr>". a:cmd
endf


function! s:DelTo(pattern, inclusive)
    call s:DoVisual(a:pattern, a:inclusive, 'd')
endf



function! s:Var(arg) "{{{3
    " TLogVAR a:arg
    if exists(a:arg)
        " exec 'return '.a:arg
        " TLogVAR {a:arg}
        return {a:arg}
    else
        return tskeleton#EvalInDestBuffer(printf('exists("%s") ? %s : "%s"', a:arg, a:arg, a:arg))
    endif
endf

function! s:Execute(text) "{{{3
    " TLogDBG string(getline(1, '$'))
    call s:DelTo('execute(.\{-}', 1)
    exec a:text
    " TLogDBG string(getline(1, '$'))
    return ''
endf

function! s:Exec(arg) "{{{3
    return tskeleton#EvalInDestBuffer(a:arg)
endf


" function! TSkelIncreaseIndex(var) "{{{3
"     exec 'let '. a:var .'='. a:var .'+1'
"     return a:var
" endf


function! s:Query(arg) "{{{3
    let sepx = stridx(a:arg, '|')
    let var  = strpart(a:arg, 0, sepx)
    " let text = substitute(strpart(a:arg, sepx + 1), ':?$', ':', '')
    let text = strpart(a:arg, sepx + 1)
    let tsep = stridx(text, '|')
    if tsep == -1
        let repl = ''
    else
        let repl = strpart(text, tsep + 1)
        let text = strpart(text, 0, tsep)
    endif
    if var != ''
        if !tskeleton#EvalInDestBuffer('exists('. string(var) .')')
            echom 'Unknown choice variable: '. var
        else
            let val0 = tskeleton#EvalInDestBuffer(var)
            if type(val0) == 3
                let val = val0
            else
                let val = split(val0, '\n')
            endif
            " TAssert IsList(val)
            let val = sort(copy(val))
            " TLogVAR val
            let rv = tlib#input#List('s', 'Choices:', val)
            " TLogVAR rv
            if repl != '' && rv != ''
                let rv = s:sprintf1(repl, rv)
            endif
            " TLogVAR rv
            return rv
        endif
    endif
    let rv = input(text. ' ', '')
    if rv != '' && repl != ''
        let rv = s:sprintf1(repl, rv)
    endif
    return rv
endf


function! s:GetVarName(name, global) "{{{3
    if a:global == 2
        return 's:tskelBitProcess_'. a:name
    elseif a:global == 1
        return 's:tskelBitProcess_'. s:tskelScratchIdx .'_'. a:name
    else
        return 'b:tskelBitProcess_'. a:name
    endif
endf


function! s:SaveBitProcess(name, match, global) "{{{3
    let s:tskelGetBit = a:match
    return ''
endf


function! s:GetBitProcess(text, name, global) "{{{3
    let s:tskelGetBit = ''
    let text = substitute(a:text, '^\s*<tskel:'. a:name .'>\s*\n\(\(.\{-}\n\)\{-}\)\s*<\/tskel:'. a:name .'>\s*\n', '\=s:SaveBitProcess("'. a:name .'", submatch(1), '. a:global .')', '')
    " call tlog#Debug(s:tskelGetBit)
    return [text, tlib#string#Chomp(s:tskelGetBit)]
endf


function! s:EvalBitProcess(eval, global) "{{{3
    " TLogVAR a:eval
    " TLogVAR a:global
    if !empty(a:eval)
        if a:global
            call tskeleton#ExecInDestBuffer(a:eval)
        else
            exec a:eval
        endif
    endif
    " TLogVAR 'done'
endf


function! s:Modify(text, modifier) "{{{3
    " let rv = escape(a:text, '\&~')
    let rv = a:text
    let premod = '^[:ulcs]\{-}'
    if a:modifier =~# premod.'u'
        let rv = toupper(rv)
    endif
    if a:modifier =~# premod.'l'
        let rv = tolower(rv)
    endif
    if a:modifier =~# premod.'c'
        let rv = toupper(rv[0]) . tolower(strpart(rv, 1))
    endif
    if a:modifier =~# premod.'C'
        let rv = substitute(rv, '\(^\|[^a-zA-Z0-9]\)\(.\)', '\u\2', 'g')
    endif
    if a:modifier =~# premod.'s'
        " let mod  = matchstr(a:modifier, '^[^s]*s\zs.*\ze$')
        let mod  = matchstr(a:modifier, 's\zs.*\ze$')
        " let rxm  = '\V'
        let rxm  = ''
        let sep  = mod[0]
        let esep = escape(sep, '\')
        let pat  = '\(\[^'. sep .']\*\)'
        let rx   = '\V\^'. esep . pat . esep . pat . esep .'\$'
        let from = matchlist(mod, rx)[1]
        let to   = matchlist(mod, rx)[2]
        let rv   = substitute(rv, rxm . from, to, 'g')
    endif
    return rv
endf


function! s:Dispatch(name) "{{{3
    let name = matchstr(a:name, '^ *\zs.\{-}\ze *$')
    let name = substitute(name, ' ', '_', 'g')
    " TLogVAR name
    if exists('*TSkeleton_'. name)
        let rv = TSkeleton_{name}()
        " TLogVAR rv
    else
        let rv = tskeleton#WrapMarker(a:name)
    endif
    return rv
endf


function! s:Call(fn) "{{{3
    return tskeleton#EvalInDestBuffer(a:fn)
endf


function! s:Expand(bit, ...) "{{{3
    " TLogVAR a:bit
    let filetype = a:0 >= 1 && a:0 != '' ? a:1 : &filetype
    " TLogVAR filetype
    " TLogVAR b:tskelFiletype
    call tskeleton#PrepareBits(filetype)
    let sepx = match(a:bit, '|')
    if sepx == -1
        let name    = a:bit
        let default = ''
    else
        let name    = strpart(a:bit, 0, sepx)
        let default = strpart(a:bit, sepx + 1)
    endif
    let bittext = ''
    " TLogVAR name, default
    " TLogDBG string(keys(b:tskelBitDefs))
    let indent = s:GetIndent(getline('.'))
    if s:IsDefined(name)
        let [setCursor, bittext] = s:RetrieveBit('text', name, indent, filetype)
        " TLogVAR setCursor, bittext
    endif
    " TLogVAR bittext
    if empty(bittext)
        if default =~ '".*"'
            let bittext = matchstr(default, '^"\ze.*\ze"$')
        elseif default != ''
            let s:tskelPostExpand = s:tskelPostExpand .'|norm '. default
        else
            let bittext = tskeleton#WrapMarker('bit:'.a:bit)
        endif
    endif
    " TLogVAR bittext
    return bittext
endf


" :def: function! tskeleton#GetVar(name, ?default=g:name)
" Get the value of variable name from the destination buffer.
function! tskeleton#GetVar(name, ...) "{{{3
    if tskeleton#EvalInDestBuffer('exists("b:'. a:name .'")')
        return tskeleton#EvalInDestBuffer('b:'. a:name)
    elseif a:0 >= 1
        exec 'return '. a:1
    else
        exec 'return g:'. a:name
    endif
endf


" Evaluate code in the destination buffer.
function! tskeleton#EvalInDestBuffer(code) "{{{3
    return tskeleton#ExecInDestBuffer('return '. a:code)
endf


" Execute code in the destination buffer.
function! tskeleton#ExecInDestBuffer(code) "{{{3
    let cb = bufnr('%')
    let wb = bufwinnr('%')
    " TLogVAR cb
    let sb = s:tskelDestBufNr >= 0 && s:tskelDestBufNr != cb
    let lazyredraw = &lazyredraw
    set lazyredraw
    if sb
        let ws = bufwinnr(s:tskelDestBufNr)
        if ws != -1
            try
                exec ws.'wincmd w'
                let code = substitute("\n". a:code ."\n", '\n\s*".\{-}\ze\n', "", "g")
                " TLogVAR a:code
                exec a:code
            finally
                exec wb.'wincmd w'
            endtry
        else
            try
                silent exec 'sbuffer! '. s:tskelDestBufNr
                exec a:code
            finally
                wincmd c
            endtry
        endif
    else
        exec a:code
    endif
    let &lazyredraw = lazyredraw
    " TLogDBG 'done'
endf


function! tskeleton#FindTemplate(template) "{{{3
    "<+BODY+>
endf


function! tskeleton#CollectTemplates() "{{{3
    "<+BODY+>
endf


" :def: function! tskeleton#Setup(template, ?unconditionally=0)
" Fill in a file template.
function! tskeleton#Setup(template, ...) "{{{3
    " TLogVAR a:template
    let anyway = a:0 >= 1 ? a:1 : 0
    " TLogVAR anyway
    if anyway || !exists('b:tskelDidFillIn') || !b:tskelDidFillIn
        if filereadable(g:tskelDir . a:template)
            let tf = g:tskelDir . a:template
        elseif filereadable(a:template)
            let tf = a:template
        else
            echoerr 'Unknown skeleton: '. a:template
            return
        endif
        let unset_ft = !exists('g:tskelFiletype')
        if unset_ft
            let g:tskelFiletype = &filetype
        endif
        try
            let meta = s:ReadSkeleton(tf)
            let s:tskel_highlight = 0
            call tskeleton#FillIn('', &filetype, meta)
            if s:tskel_highlight && !exists("b:tskelHighlight")
                call tskeleton#Highlight()
            endif
            if g:tskelChangeDir
                let cd = substitute(expand('%:p:h'), '\', '/', 'g')
                let cd = substitute(cd, '^\@<!//\+', '/', 'g')
                exec 'cd '. tlib#arg#Ex(cd)
            endif
            let b:tskelDidFillIn = 1
        finally
            if unset_ft
                unlet g:tskelFiletype
            endif
        endtry
    endif
endf


function! s:GetTemplates(aslist) "{{{3
    let files = split(glob(g:tskelDir. '*'), '\n') + split(glob(g:tskelDir. 'templates/**'), '\n')
    call filter(files, '!isdirectory(v:val)')
    " call map(files, 'fnamemodify(v:val, ":t")')
    let fns = len(g:tskelDir)
    call map(files, 'v:val[fns : -1]')
    if a:aslist
        return files
    else
        return join(files, "\n")
    endif
endf


" Command line completion.
function! tskeleton#SelectTemplate(ArgLead, CmdLine, CursorPos) "{{{3
    if a:CmdLine =~ '^.\{-}\s\+.\{-}\s'
        return ''
    else
        return s:GetTemplates(0)
    endif
endf


" Command line completion.
function! tskeleton#EditBitCompletion(ArgLead, CmdLine, CursorPos) "{{{3
    if a:CmdLine =~ '^.\{-}\s\+.\{-}\s'
        return []
    endif
    let bitdefs = s:GetBitDefs()
    if !empty(bitdefs)
        let bits  = filter(copy(bitdefs), 'has_key(v:val, "bitfile")')
        let bits  = map(bits, 'tlib#file#Relative(v:val["bitfile"], g:tskelBitsDir)')
        let bvals = values(bits)
        if !empty(a:ArgLead)
            call filter(bvals, 's:MatchBit(v:val, ''\V\^''. escape(a:ArgLead, ''\''))')
        endif
        return bvals
    endif
    return []
endf


function! s:Browse(save, title, initdir, default) "{{{3
    let tpl = tlib#input#List('s', 'Select template', s:GetTemplates(1), [
                \ {'display_format': 'filename'},
                \ ])
    return tpl
endf


" :def: function! tskeleton#Edit(?dir)
" Edit a file template.
function! tskeleton#Edit(...) "{{{3
    let tpl = a:0 >= 1 && !empty(a:1) ? a:1 : s:Browse(0, "Template", g:tskelDir, "")
    if !empty(tpl)
        let tf = tlib#arg#Ex(g:tskelDir . tpl)
        " TLogVAR tf
        exe 'edit '. tf
    end
endf


" Edit a skeleton bit.
function! tskeleton#EditBit(bit) "{{{3
    if !empty(a:bit)
        let tf = tlib#arg#Ex(g:tskelBitsDir. a:bit)
        " TLogVAR tf
        exe 'edit '. tf
    end
endf


" :def: function! tskeleton#NewFile(?template, ?dir, ?fileName)
" Create a new file template.
function! tskeleton#NewFile(...) "{{{3
    if a:0 >= 1 && a:1 != ""
        let tpl = g:tskelDir. a:1
    else
        let tpl = s:Browse(0, "Template", g:tskelDir, "")
        if tpl == ""
            return
        else
            let tpl = fnamemodify(tpl, ":p")
        endif
    endif
    if a:0 >= 2 && a:2 != ""
        let dir = a:2
    else
        let dir = getcwd()
    endif
    if a:0 >= 3
        let fn = a:3
    else
        let fn = s:Browse(1, "New File", dir, "new.".fnamemodify(tpl, ":e"))
        if fn == ""
            return
        else
            let fn = fnamemodify(fn, ":p")
        endif
    endif
    if fn != "" && tpl != ""
        " TLogVAR tpl
        exe 'edit '. tpl
        exe 'saveas '. fn
        let s:tskel_highlight = 0
        call tskeleton#FillIn('', &filetype)
        if s:tskel_highlight && !exists("b:tskelHighlight")
            call tskeleton#Highlight()
        endif
        exe "bdelete ". tpl
    endif
endf


" tskeleton#GlobBits(path, ?mode=1)
function! tskeleton#GlobBits(path, ...) "{{{3
    let mode = a:0 >= 1 ? a:1 : 1
    let pt   = "*"
    let rvs  = globpath(a:path, pt)
    let rvs  = substitute(rvs, '\\', '/', 'g')
    let rv   = sort(split(rvs, "\n"))
    let stop_rx = tlib#var#Get('tskelBitsIgnore', 'bg')
    if !empty(stop_rx)
        call filter(rv, 'v:val !~ stop_rx')
    endif
    if mode == 0
        call map(rv, 'fnamemodify(v:val, ":t")')
    elseif mode == 1
        call map(rv, 'tskeleton#PurifyBit(fnamemodify(v:val, ":t"))[0]')
    elseif mode == 2
    else
        echoerr 'tSkeleton: Unknown mode: '. mode
    endif
    " TAssert IsList(rv)
    return rv
endf


function! s:PrepareMiniBit(dict, def, buildmenu) "{{{3
    " TAssert IsDictionary(a:dict)
    " TAssert IsString(a:def)
    if !empty(a:def)
        " TLogVAR a:def
        if a:def =~ '\S\+\s'
            let bit = matchstr(a:def, '^\S\+\ze\s')
            let exp = matchstr(a:def, '\s\zs.\+$')
        else
            let bit = a:def
            let exp = a:def
        endif
        " TAssert IsString(exp)
        let a:dict[bit] = {'text': exp, 'menu': g:tskelMenuMiniPrefix . bit, 'type': 'tskeleton'}
        if a:buildmenu
            call tskeleton#NewBufferMenuItem(b:tskelBufferMenu, bit)
        endif
        " TAssert IsNotEmpty(a:dict[bit])
    endif
endf


function! tskeleton#NewBufferMenuItem(menu, bit, ...)
    TVarArg ['subpriority', 10]
    " TLogVAR a:menu
    " TLogVAR a:bit
    " TLogVAR subpriority
    let min = s:PrepareMenuEntry(a:bit, subpriority, "n")
    " TLogVAR min
    call add(a:menu, min)
    let mii = s:PrepareMenuEntry(a:bit, subpriority, "i")
    " TLogVAR mii
    call add(a:menu, mii)
    if tlib#var#Get('tskelAutoAbbrevs', 'bg')
        let mia = s:PrepareAbbreviation(a:bit)
        " TLogVAR mia
        if !empty(mia)
            call add(a:menu, mia)
        endif
    endif
    " TLogDBG 'tskeleton#NewBufferMenuItem end'
endf


function! tskeleton#FetchMiniBits(dict, filename, buildmenu) "{{{3
    " TAssert IsDictionary(a:dict)
    " TLogVAR a:filename
    let c = s:ReadFile(a:filename)
    if c =~ '\S'
        for line in sort(split(c, "\n"))
            call s:PrepareMiniBit(a:dict, line, a:buildmenu)
        endfor
    endif
    return a:dict
endf


function! tskeleton#ProcessTag_functions_with_parentheses(kinds, dict, tag, restargs)
    if empty(a:kinds) || stridx(a:kinds, a:tag['kind']) != -1
        let source0 = fnamemodify(a:tag['filename'], ':p')
        let source  = source0
        let xname   = a:tag['name']
        if has_key(a:tag, 'signature')
            let args0 = a:tag.signature
        else
            let args0 = matchstr(a:tag['cmd'], '(.\{-})')
        endif
        let args    = matchstr(args0, '(\zs.\{-}\ze)')
        let bname0  = xname . args0 .'@'
        let bname   = bname0 . fnamemodify(source, ':t')
        if has_key(a:dict, bname)
            if fnamemodify(get(a:dict[bname], 'source', ''), ':p') == source0
                return ''
            else
                let bname = bname0 . source
            endif
        endif
        let xname .= tskeleton#ReplacePrototypeArgs(args, a:restargs)
        let a:dict[bname] = {'text': xname, 'source': source, 'type': 'tskeleton'}
        let menu_prefix = tlib#var#Get('tskelMenuPrefix_tags', 'bg')
        if !empty(menu_prefix)
            " let smenu  = join(map(split(source, '[\/]'), 'escape(v:val, ". ")'), '.')
            " let mname  = 'Tag.'. smenu .'.'. escape(bname, '. ')
            let smenu  = join(map(split(source, '[\/]'), 'escape(v:val, ".")'), '.')
            let mname  = menu_prefix . smenu .'.'. escape(bname, '.')
            " TLogDBG xname .' -- '. xname
            let a:dict[bname]['menu'] = mname
        endif
        return bname
    endif
    return ''
endf


function! tskeleton#ReplacePrototypeArgs(text, rest)
    let args = split(a:text, ',\s\+')
    if empty(args)
        return '()' . tskeleton#CursorMarker()
    else
        let max = len(args) - 1
        " let rv  = map(range(0, max), '!empty(a:rest) && args[v:val] =~# a:rest ? tskeleton#WrapMarker("") : (v:val == 0 ? "" : ", ") . printf(tskeleton#WrapMarker("%s"), toupper(args[v:val]))')
        let rv  = map(range(0, max), '!empty(a:rest) && args[v:val] =~# a:rest ? tskeleton#WrapMarker("") : (v:val == 0 ? "" : ", ") . printf(tskeleton#WrapMarker("%s"), s:CleanArgument(args[v:val]))')
        return printf('(%s%s)%s', tskeleton#CursorMarker(), join(rv, ''), tskeleton#WrapMarker(''))
    endif
endf


function! s:CleanArgument(arg) "{{{3
    return substitute(a:arg, '[?&:|()<>]', ' ', 'g')
endf


function! s:ExpandMiniBit(bit) "{{{3
    let rv = ''
    if s:IsDefined(a:bit)
        let rv = tskeleton#BitDef(a:bit, 'text')
    endif
    " TAssert IsString(rv)
    return rv
endf


function! s:sprintf1(string, arg) "{{{3
    let rv = substitute(a:string, '\C\(^\|%%\|[^%]\)\zs%s', escape(a:arg, '"\'), 'g')
    let rv = substitute(rv, '%%', '%', 'g')
    return rv
    " return printf(a:string, a:arg)
endf


function! s:GetBitGroup(filetype, ...) "{{{3
    let general_first = a:0 >= 1 ? a:1 : 0
    let filetype = substitute(a:filetype, '\W', '_', 'g')
    let bg = tlib#var#Get('tskelBitGroup_'. filetype, 'bg')
    if !empty(bg)
        if type(bg) == 1
            echom 'tSkeleton: [bg]:tskelBitGroup_'. filetype .' should be a list'
            let rv = split(bg, "\n")
        else
            let rv = copy(bg)
        endif
    else
        let rv = [filetype]
    endif
    " TAssert IsList(rv)
    if filetype != 'general'
        if general_first
            call insert(rv, 'general')
        else
            call add(rv, 'general')
        endif
    endif
    return rv
endf


function! tskeleton#PurifyBit(bit) "{{{3
    " let rv = a:bit
    " let rv = substitute(rv, '^[^[:cntrl:]]\{-}[/.]\([^/.[:cntrl:]]\{-}\)$', '\1', 'g')
    " let rv = substitute(rv, '^[^[:cntrl:]]\{-}[/.]\([^/.[:cntrl:]]\{-}\)$', '\1', 'g')
    " let rv = fnamemodify(rv, ':t')
    let mname = tlib#url#Decode(a:bit)
    let cname = a:bit
    let cname = substitute(cname, '^.\{-}\.\ze[^.]\+$', '', '')
    let cname = tlib#url#Decode(substitute(cname, '&', '', 'g'))
    return [cname, mname]
endf


function! s:DidSetup(filetype) "{{{3
    return exists('g:tskelBits_'. a:filetype)
endf


function! s:ToBeInitialized(list, filetype) "{{{3
    return index(a:list, a:filetype) != -1
endf


function! s:FiletypesToBeInitialized(ftgroup, explicit_reset) "{{{3
    if a:explicit_reset
        return a:ftgroup
    endif
    return filter(copy(a:ftgroup), 's:FiletypeToBeInitialized(v:val)')
endf


function! s:FiletypeToBeInitialized(filetype) "{{{3
    if !s:DidSetup(a:filetype)
        return 1
    else
        let ftm = s:GetMenuCacheFilename(a:filetype)
        if empty(ftm)
            return 0
        else
            return !filereadable(ftm)
        endif
    endif
endf


" s:PrepareMenu(type, ?menuprefix='')
function! s:PrepareMenu(filetype, ...) "{{{3
    if g:tskelMenuCache == '' || g:tskelMenuPrefix == ''
        return
    endif
    " TLogVAR a:filetype
    let menu_file = s:GetMenuCacheFilename(a:filetype)
    " TLogVAR menu_file
    if menu_file != ''
        let sub = a:0 >= 1 ? a:1 : ''
        let tskelMenuPrefix = g:tskelMenuPrefix
        let verbose    = &verbose
        let lazyredraw = &lazyredraw
        let backup     = &backup
        let patchmode  = &patchmode
        let g:tskeleton_SetFiletype = 0
        set lazyredraw
        set nobackup
        set patchmode=
        set verbose&
        try
            let menu = s:MakeMenuEntry(keys(g:tskelBits_{a:filetype}), sub)
            if !empty(menu)
                exec 'redir! > '. menu_file
                if exists('*TSkelMenuCacheEditHook')
                    silent! call TSkelMenuCacheEditHook()
                endif
                silent! echo join(menu, "\n")
                if exists('*TSkelMenuCachePostWriteHook')
                    silent! call TSkelMenuCachePostWriteHook()
                endif
                redir END
            endif
        catch
            echohl Error
            echom v:errmsg
            echohl NONE
        finally
            let &verbose    = verbose
            let &lazyredraw = lazyredraw
            let &backup     = backup
            let &patchmode  = patchmode
            let g:tskeleton_SetFiletype = 1
            let g:tskelMenuPrefix = tskelMenuPrefix
        endtry
    endif
endf


function! s:MakeMenuEntry(items, ...)
    let sub = a:0 >= 1 ? a:1 : ''
    " TLogVAR a:items
    " TAssert IsList(a:items)
    if sub != ''
        let g:tskelMenuPrefix = g:tskelMenuPrefix .'.'. sub
        let subpriority = 10
    else
        let subpriority = 20
    endif
    let menu = []
    call filter(sort(copy(a:items)), 'tskeleton#NewBufferMenuItem(menu, v:val, subpriority)')
    " TLogVAR menu
    return menu
endf


function! s:GetCacheFilename(filetype, what) "{{{3
    " TLogVAR a:filetype
    if a:filetype == ''
        return ''
    endif
    " let filetype = 'tskel_'. a:filetype
    let cfn  = tlib#cache#Filename(a:what, a:filetype, 1)
    " TLogVAR cfn
    return cfn
endf


function! s:GetMenuCacheFilename(filetype) "{{{3
    return s:GetCacheFilename(a:filetype, 'tskel_menu')
endf


function! s:GetFiletypeBitsCacheFilename(filetype) "{{{3
    return s:GetCacheFilename(a:filetype, 'tskel_bits')
endf


function! s:ResetBufferCacheForFiletype(filetype) "{{{3
    let dir = s:GetCacheFilename(a:filetype, 'tskel_bbits')
    if !empty(dir)
        let files = split(globpath(dir, '**'), '\n')
        for fname in files
            if !isdirectory(fname)
                " TLogVAR fname
                call delete(fname)
            endif
        endfor
    endif
endf


function! s:GetBufferCacheFilename(filetype, ...) "{{{3
    if g:tskelUseBufferCache
        let fname = expand('%:t')
        " TLogVAR fname
        if !empty(fname)
            let create_dir = a:0 >= 1 ? a:1 : 0
            let dir = s:GetCacheFilename(a:filetype, 'tskel_bbits')
            if !empty(dir)
                let dir = tlib#file#Join([
                            \ dir,
                            \ substitute(expand('%:p:h'), '[:&<>]\|//\+\|\\\\\+', '_', 'g')
                            \ ])
                " TLogVAR dir
                if create_dir && !isdirectory(dir)
                    call tlib#dir#Ensure(dir)
                endif
                " let fname = expand('%:t') .'.'. a:filetype
                let cname = tlib#file#Join([dir, fname])
                " TLogVAR cname
                return cname
            endif
        endif
    endif
    return ''
endf


function! tskeleton#BitDef(name, ...) "{{{3
    TVarArg 'field', 'default'
    " TLogVAR a:name, field, default
    let def = get(s:GetBitDefs(), a:name, {})
    " TLogVAR def
    if empty(field)
        return def
    else
        return get(def, field, default)
    endif
endf


function! s:PrepareAbbreviation(name) "{{{3
    if a:name =~ '\S'
        " TLogVAR a:name
        let bit = tskeleton#BitDef(a:name)
        if !(has_key(bit, 'abbrev') && empty(bit.abbrev))
            " let abb = empty(g:tskelAbbrevPostfix) ? '' : (a:name.g:tskelAbbrevPostfix)
            let abb = a:name. tlib#var#Get('tskelAbbrevPostfix', 'bg')
            if abb =~ '\W\W$'
                echom 'tSkeleton: Invalid name for an abbreviation: '. abb
            else
                if !empty(bit)
                    let meta = get(bit, 'meta', {})
                    let abbr = get(meta, 'abbrev', get(bit, 'abbrev', ''))
                    let abbr = substitute(abbr, '\s', '', 'g')
                    " let abbr = tlib#string#Chomp(abbr)
                    if !empty(abbr)
                        let abb = abbr
                    endif
                endif
                " TLogVAR abb
                if !empty(abb)
                    return 'iabbrev <buffer> '. abb .' '. printf(tskeleton#ExpandedAbbreviationTemplate(), string(a:name))
                endif
            endif
        endif
    endif
    return ''
endf


function! s:PrepareMenuEntry(name, subpriority, mode) "{{{3
    " TLogVAR a:name
    if a:name =~ '\S'
        " TLogVAR a:mode
        let bit   = tskeleton#BitDef(a:name)
        " TLogVAR bit
        let mname = a:name
        " let mname = escape(a:name, ' 	')
        " let mname = escape(a:name, ' 	\')
        " TLogVAR mname
        if !empty(bit)
            let mname = get(bit, 'menu', mname)
        endif
        let mname = escape(mname, ' 	\')
        " TLogVAR mname
        let acc = []
        for menu_name in split(mname, "\n")
            " TLogVAR menu_name
            let spri  = stridx(menu_name, '.') >= 0 ? a:subpriority - 1 : a:subpriority
            " TLogVAR spri
            let pri   = g:tskelMenuPriority .'.'. spri
            " TLogVAR pri
            " if a:mode == 'i'
            if s:IsInsertMode(a:mode)
                call add(acc, 'imenu '. pri .' '. g:tskelMenuPrefix .'.'. menu_name .
                            \ ' <c-\><c-o>:call tskeleton#ExpandBitUnderCursor("i", '. string(a:name) .')<cr>')
            else
                call add(acc, 'menu '. pri .' '. g:tskelMenuPrefix .'.'. menu_name .
                            \ ' :call tskeleton#ExpandBitUnderCursor("n", '. string(a:name) .')<cr>')
            endif
        endfor
        return join(acc, "\n")
    else
        return ''
    endif
endf


function! s:InitBufferMenu()
    if !exists('b:tskelBufferMenu')
        let b:tskelBufferMenu = []
    endif
endf


function! tskeleton#BuildBufferMenu(prepareBits) "{{{3
    " TLogVAR a:prepareBits
    if !s:tskelProcessing && &filetype != '' && g:tskelMenuCache != '' && g:tskelMenuPrefix != ''
        if a:prepareBits
            call tskeleton#PrepareBits()
        endif
        if s:tskelBuiltMenu == 1
            try
                silent exec 'aunmenu '. g:tskelMenuPrefix
            finally
            endtry
        endif
        let pri = g:tskelMenuPriority .'.'. 5
        exec 'amenu '. pri .' '. g:tskelMenuPrefix .'.Reset :TSkeletonBitReset<cr>'
        exec 'amenu '. pri .' '. g:tskelMenuPrefix .'.-tskel1- :'
        let bg = s:GetBitGroup(&filetype, 1)
        " TLogVAR bg
        call map(bg, 's:GetMenuCache(v:val)')
        if exists('b:tskelBufferMenu')
            " TLogVAR b:tskelBufferMenu
            for m in b:tskelBufferMenu
                exec m
            endfor
        endif
        let s:tskelBuiltMenu = 1
    endif
endf


function! s:GetMenuCache(filetype) "{{{3
    let pg = s:GetMenuCacheFilename(a:filetype)
    " TLogVAR pg
    if filereadable(pg)
        exec 'source '. pg
    endif
endf


" :def: function! tskeleton#PrepareBits(?filetype=&filetype, ?reset=0)
" Prepare the buffer for use with tskeleton.
function! tskeleton#PrepareBits(...) "{{{3
    if a:0 >= 1 && !empty(a:1)
        let filetype = a:1
        " TLogVAR 'a:1', filetype
    else
        let filetype = s:Filetype()
    endif
    " TLogVAR a:0, filetype, &filetype
    call tskeleton#Initialize()
    " if filetype == ''
    "     let b:tskelFiletype = ''
    "     return
    " endif
    let explicit_reset = a:0 >= 2 ? a:2 : 0
    if explicit_reset
        for idx in range(1, s:tskelScratchMax)
            let ibn = bufnr(s:tskelScratchNr{idx})
            if bufloaded(ibn)
                exec 'bdelete! '. ibn
            endif
        endfor
        let s:tskelScratchMax = 0
        if g:tskelUseBufferCache
            call s:ResetBufferCacheForFiletype(filetype)
        endif
    endif
    " TLogVAR explicit_reset
    let init_buffer    = !exists('b:tskelFiletype') || b:tskelFiletype != filetype
    " TLogVAR init_buffer
    if !explicit_reset && !init_buffer
        return
    endif
    " TLogVAR filetype
    let ft_group = s:GetBitGroup(filetype)
    " TAssert IsList(ft_group)
    " TLogVAR ft_group
    let to_be_initialized = s:FiletypesToBeInitialized(ft_group, explicit_reset)
    " TAssert IsList(to_be_initialized)
    " TLogVAR to_be_initialized
    if init_buffer || !empty(to_be_initialized)
        if !explicit_reset && g:tskelUseBufferCache && s:HasCachedBufferBits(filetype)
            " TLogDBG 'PrepareBufferFromCache'
            call s:PrepareBufferFromCache(filetype)
        else
            call s:InitBufferMenu()
            let b:tskelBitDefs  = {}
            for gfiletype in ft_group
                " TLogVAR gfiletype
                let reset = s:ToBeInitialized(to_be_initialized, gfiletype)
                let resetcache = explicit_reset || !s:FiletypeInCache(gfiletype)
                " TLogVAR reset, resetcache
                if reset
                    if resetcache
                        call s:PrepareFiletype(gfiletype, reset)
                    else
                        call s:PrepareFiletypeFromCache(gfiletype)
                    endif
                endif
                " TLogDBG 'ExtendBitDefs'
                call s:ExtendBitDefs(b:tskelBitDefs, gfiletype)
                " TLogDBG 'PrepareFiletypeMap'
                call s:PrepareFiletypeMap(gfiletype, reset)
                if reset
                    if resetcache
                        " TLogDBG 'CacheFiletypeBits'
                        call s:CacheFiletypeBits(gfiletype)
                    endif
                    if !tskeleton#IsScratchBuffer()
                        " TLogDBG 'PrepareFiletypeMenu'
                        call s:PrepareFiletypeMenu(gfiletype)
                    endif
                endif
            endfor
            " if s:PrepareBuffer(gfiletype) && empty(&buftype)
            if s:PrepareBuffer(gfiletype) && g:tskelUseBufferCache
                " TLogDBG 'CacheBufferBits'
                call s:CacheBufferBits(gfiletype)
            endif
        endif
        " TAssert IsDictionary(b:tskelBitDefs)
        " let b:tskelBitNames = keys(b:tskelBitDefs)
        " let b:tskelBitNames = tlib#list#Compact(tlib#list#Flatten(b:tskelBitNames))
        " if g:tskelPopupNumbered
        "     call map(b:tskelBitNames, "substitute(v:val, '&', '', 'g')")
        " endif
        " TLogDBG 'tskeleton#BuildBufferMenu'
        call tskeleton#BuildBufferMenu(0)
        let b:tskelFiletype = filetype
    endif
endf


function! s:HasCachedBufferBits(filetype) "{{{3
    let cname = s:GetBufferCacheFilename(a:filetype)
    return filereadable(cname)
endf


function! s:CacheBufferBits(filetype) "{{{3
    let cname = s:GetBufferCacheFilename(a:filetype, 1)
    if !empty(cname)
        " TLogVAR cname
        call writefile([string(s:GetBitDefs())], cname, 'b')
    endif
endf


function! s:PrepareBufferFromCache(filetype) "{{{3
    let cname = s:GetBufferCacheFilename(a:filetype)
    let b:tskelBitDefs = eval(join(readfile(cname, 'b'), "\n"))
endf


function! s:FiletypeInCache(filetype) "{{{3
    let cache = s:GetFiletypeBitsCacheFilename(a:filetype)
    return filereadable(cache)
endf


function! s:PrepareFiletypeFromCache(filetype) "{{{3
    let cache = s:GetFiletypeBitsCacheFilename(a:filetype)
    if !empty(cache)
        let g:tskelBits_{a:filetype} = eval(join(readfile(cache, 'b'), "\n"))
    endif
endf


function! s:CacheFiletypeBits(filetype) "{{{3
    let cache = s:GetFiletypeBitsCacheFilename(a:filetype)
    if !empty(cache)
        call writefile([string(g:tskelBits_{a:filetype})], cache, 'b')
    endif
endf


function! s:PrepareFiletype(filetype, reset)
    " TLogVAR a:filetype
    " TLogVAR a:reset
    let g:tskelBits_{a:filetype} = {}
    let fns = s:CollectFunctions('tskeleton#%s#FiletypeBits')
                \ + s:CollectFunctions('tskeleton#%s#FiletypeBits_'. a:filetype)
    " TLogVAR fns
    for fn in fns
        " TLogDBG 'PrepareFiletype '.fn
        call {fn}(g:tskelBits_{a:filetype}, a:filetype)
    endfor
    " TLogDBG 'bits for '. a:filetype .'='. string(keys(g:tskelBits_{a:filetype}))
endf


function! s:PrepareBuffer(filetype)
    " TLogDBG bufname('%')
    let fns = s:CollectFunctions('tskeleton#%s#BufferBits')
                \ + s:CollectFunctions('tskeleton#%s#BufferBits_'. a:filetype)
    " TLogVAR fns
    for fn in fns
        " TLogDBG 'PrepareBuffer '.fn
        call {fn}(s:GetBitDefs(), a:filetype)
    endfor
    " TLogDBG string(keys(b:tskelBitDefs))
    return !empty(fns)
endf


function! s:CollectFunctions(pattern)
    let rv = []
    let ts = tlib#var#Get('tskelTypes', 'bg')
    call tskeleton#Initialize(ts)
    for t in ts
        let f = printf(a:pattern, t)
        " TLogVAR f
        " TLogDBG exists('*'. f)
        if exists('*'. f)
            call add(rv, f)
        endif
    endfor
    " let types   = '\('. join(tlib#var#Get('tskelTypes', 'bg'), '\|') .'\)'
    " let pattern = printf(a:pattern, types)
    " let fns = tlib#cmd#OutputAsList('function /'. pattern)
    " let rv  = map(fns, 'matchstr(v:val, ''^\S\+\s\+\zs.\{-}\ze('')')
    " call filter(rv, '!empty(v:val)')
    " TLogVAR a:pattern, ts, rv
    return rv
endf


" function! s:PrepareConditionEntry(pattern, eligible) "{{{3
"     let pattern  = escape(substitute(a:pattern, '%', '%%', 'g'), '"')
"     let eligible = escape(a:eligible, '"')
"     return 'if search("'. pattern .'%s", "W") | return "'. eligible .'" | endif | '
" endf


function! s:ReadFile(filename) "{{{3
    " TAssert IsString(a:filename)
    if filereadable(a:filename)
        return join(readfile(a:filename), "\n")
    endif
    return ''
endf


function! s:ReadSkeleton(filename) "{{{3
    let lines = readfile(a:filename)
    let [text, meta] = tskeleton#ExtractMeta(join(lines, "\n"))
    " TLogVAR text
    " TLogDBG string(getlines(1, line('$')))
    " let lines = split(text, '\n', 1)
    " TLogVAR lines
    " call append(0, lines)
    call tlib#buffer#InsertText(text)
    " norm! Gdd
    " TLogDBG string(getlines(1, line('$')))
    return meta
endf


function! s:PrepareFiletypeMap(filetype, anyway) "{{{3
    if !exists('g:tskelBitMap_'. a:filetype) || a:anyway
        let fn = g:tskelMapsDir . a:filetype
        let c  = s:ReadFile(fn)
        if c =~ '\S'
            let g:tskelBitMap_{a:filetype} = {}
            for line in split(c, "\n")
                let pattern = matchstr(line, '^.\{-}\ze\t')
                if !empty(pattern)
                    let bits    = matchstr(line, '\t\zs.*$')
                    let g:tskelBitMap_{a:filetype}[pattern] = split(bits, '\s\+')
                endif
            endfor
        endif
    endif
endf


function! s:PrepareFiletypeMenu(filetype) "{{{3
    " TLogVAR a:filetype
    if a:filetype == 'general'
        call s:PrepareMenu('general', 'General')
    else
        call s:PrepareMenu(a:filetype)
    endif
endf


function! s:ExtendBitDefs(dict, filetype) "{{{3
    " TAssert IsDictionary(a:dict)
    if s:DidSetup(a:filetype)
        let bm = g:tskelBits_{a:filetype}
        " TAssert IsDictionary(bm)
        if !empty(bm)
            call extend(a:dict, bm)
        endif
    endif
endf


function! tskeleton#ResetBits(filetype) "{{{3
    let filetype =  empty(a:filetype) ? &filetype : a:filetype
    for bn in range(1, bufnr('$'))
        if bufloaded(bn) && (filetype == 'general' || getbufvar(bn, 'tskelFiletype') == filetype)
            call setbufvar(bn, 'tskelFiletype', '')
        endif
    endfor
    call tskeleton#PrepareBits(filetype, 1)
endf


function! tskeleton#SelectBit(ArgLead, CmdLine, CursorPos) "{{{3
    call tskeleton#PrepareBits()
    return join(s:EligibleBits(&filetype), "\n")
endf


function! s:SetLine(mode) "{{{3
    let s:tskelLine = line('.')
    let s:tskelCol  = col('.')
endf


function! s:UnsetLine() "{{{3
    let s:tskelLine = 0
    let s:tskelCol  = 0
endf


function! s:GetEligibleBits(filetype) "{{{3
    let pos = '\\%'. s:tskelLine .'l\\%'. s:tskelCol .'c'
    for pattern in keys(g:tskelBitMap_{a:filetype})
        if search(pattern.pos, 'W')
            return g:tskelBitMap_{a:filetype}[pattern]
        endif
    endfor
    return []
endf


function! s:EligibleBits(filetype) "{{{3
    if s:tskelLine && exists('g:tskelBitMap_'. a:filetype)
        norm! {
        let eligible = s:GetEligibleBits(a:filetype)
        " TAssert IsList(eligible)
        call cursor(s:tskelLine, s:tskelCol)
        if !empty(eligible)
            " TLogDBG a:filetype.': '. string(eligible)
            return eligible
        endif
    endif
    let ok = filter(items(s:GetBitDefs()), 'eval(get(get(get(v:val, 1, {}), "meta", {}), "condition", 1))')
    return map(ok, 'v:val[0]')
    " if exists('b:tskelBitNames')
    "     " TAssert IsList(b:tskelBitNames)
    "     " TLogVAR b:tskelBitNames
    "     return b:tskelBitNames
    " else
    "     return []
    " endif
endf


function! s:EditScratchBuffer(filetype, ...) "{{{3
    let idx = a:0 >= 1 ? a:1 : s:tskelScratchIdx
    if exists("s:tskelScratchNr". idx) && s:tskelScratchNr{idx} >= 0
        let tsbnr = bufnr(s:tskelScratchNr{idx})
    else
        let tsbnr = -1
    endif
    let vars = map(copy(s:tskelScratchVars), 'exists("b:".v:val) ? ["b:".v:val, b:{v:val}] : ["", ""]')
    let tskel_bitDefs = b:tskelBitDefs
    let tskelFiletype = b:tskelFiletype
    if tsbnr >= 0
        " TLogVAR tsbnr
        silent exec "sbuffer ". tsbnr
    else
        silent split
        " TLogVAR idx
        " TLogDBG 'edit __TSkeletonScratch_'. idx .'__'
        silent exec 'edit __TSkeletonScratch_'. idx .'__'
        " TLogDBG 1
        let s:tskelScratchNr{idx} = bufnr("%")
        " let b:tskelScratchBuffer = 1
    endif
    for [var, val] in vars
        if !empty(var) && !exists(var)
            let {var} = val
        endif
    endfor
    " TLogDBG 2
    setlocal buftype=nofile
    setlocal bufhidden=hide
    setlocal noswapfile
    setlocal nobuflisted
    setlocal foldlevel=99
    setlocal foldmethod=manual
    silent norm! ggdG
    " TLogVAR a:filetype, tskelFiletype
    " TLogDBG 3
    " Let's assume the bits get inherited from the parent buffer
    let b:tskelFiletype = tskelFiletype
    let b:tskelBitDefs = copy(tskel_bitDefs)
    " TLogDBG 4
    if exists('*TSkelNewScratchHook_'. a:filetype)
        call TSkelNewScratchHook_{a:filetype}()
    endif
    " TLogDBG 'exit'
endf


function! tskeleton#IsScratchBuffer()
    " return exists('b:tskelScratchBuffer') || bufname('%') =~ '\V__TSkeletonScratch_\d\+__'
    return bufname('%') =~ '\V__TSkeletonScratch_\d\+__'
endf


function! tskeleton#Retrieve(name) "{{{3
    let [setCursor, bittext] = s:RetrieveBit('text', a:name)
    return bittext
endf


function! s:Filetype() "{{{3
    if exists('b:tskelFiletype')
        let filetype = b:tskelFiletype
    elseif exists('g:tskelFiletype')
        let filetype = g:tskelFiletype
    else
        let filetype = &filetype
    endif
    " TLogVAR filetype
    return filetype
endf


" s:RetrieveBit(agent, bit, ?indent, ?filetype) => setCursor?; @t=expanded template bit
function! s:RetrieveBit(agent, bit, ...) "{{{3
    if s:tskelScratchIdx >= g:tskelMaxRecDepth
        return 0
    endif
    " TLogVAR a:agent, a:bit
    let indent = a:0 >= 1 ? a:1 : ''
    let filetype = a:0 >= 2 ? a:2 : s:Filetype()
    " TLogVAR filetype
    let rv     = ''
    if s:tskelScratchIdx == 0
        let s:tskelDestBufNr = bufnr("%")
    endif
    let s:tskelScratchIdx = s:tskelScratchIdx + 1
    if s:tskelScratchIdx > s:tskelScratchMax
        let s:tskelScratchMax = s:tskelScratchIdx
        let s:tskelScratchNr{s:tskelScratchIdx} = -1
    endif
    let setCursor  = 0
    let pos        = getpos('.')
    let processing = s:SetProcessing()
    let t          = @t
    try
        call s:EditScratchBuffer(filetype)
        if filetype != ""
            call tskeleton#PrepareBits(filetype)
        endif
        let type = tskeleton#BitDef(a:bit, 'type', 'tskeleton')
        " TLogVAR type
        " TLogExec sleep 3 | redraw
        if type != 'tskeleton'
            let retriever = printf('tskeleton#%s#Retrieve', type)
            " TLogVAR retriever
            let setCursor = {retriever}(a:bit, indent, filetype)
        else
            if s:RetrieveAgent_{a:agent}(a:bit)
                " TLogDBG string(getline(1, '$'))
                " call s:IndentLines(2, line("$"), indent)
                " TLogDBG string(getline(1, '$'))
                silent norm! gg
                call tskeleton#FillIn(a:bit, filetype)
                " TLogDBG string(getline(1, '$'))
                let setCursor = tskeleton#SetCursor('%', '', '', 1)
                " TLogDBG string(getline(1, '$'))
            else
                echoerr "Internal error"
            endif
        endif
        " TLogVAR setCursor
        " TLogExec sleep 3 | redraw
        let bot = line('$')
        let rv = join(getline(1, bot), "\n")
        " TLogVAR rv
        let rv = tlib#string#Chomp(rv)
        " TLogVAR rv
    finally
        let @t = t
        call s:SetProcessing(processing)
        wincmd c
        let s:tskelScratchIdx = s:tskelScratchIdx - 1
        if s:tskelScratchIdx == 0
            silent exec 'buffer '. s:tskelDestBufNr
            let s:tskelDestBufNr = -1
        else
            silent exec 'buffer '. s:tskelScratchNr{s:tskelScratchIdx}
        endif
        call setpos('.', pos)
    endtry
    " TLogVAR rv
    return [setCursor, rv]
endf


function! s:SetProcessing(...) "{{{3
    if a:0 >= 1
        let s:tskelProcessing = a:1
        return a:1
    else
        let rv = s:tskelProcessing
        let s:tskelProcessing = 1
        return rv
    endif
endf


" function! s:RetrieveAgent_read(bit) "{{{3
"     let cpo = &cpo
"     try
"       set cpoptions-=a
"       silent exec "0read ". escape(a:bit, '\#%')
"       norm! Gdd
"     finally
"       let &cpo = cpo
"     endtry
" endf


function! s:RetrieveAgent_text(bit) "{{{3
    " TLogVAR a:bit
    if s:IsDefined(a:bit)
        let text  = tskeleton#BitDef(a:bit, 'text')
        " TLogVAR text
        let lines = split(text, '\n', 1)
        " if lines[-1] != ''
        "     call add(lines, '')
        " endif
        " TLogVAR lines
        call append(0, lines)
        " TLogVAR line('$'), len(lines)
        if line('$') > len(lines)
        " if getline('$') == ""
            norm! Gdd
        endif
        return 1
    else
        " TLogDBG "Undefined bit: ". a:bit
        echoerr "Undefined bit: ". a:bit
        return 0
    endif
    " norm! Gdd
    " TLogDBG string(getline(1, '$'))
endf


function! s:InsertBit(agent, bit, mode) "{{{3
    " TLogVAR a:agent, a:bit, a:mode
    let c  = col(".")
    let e  = col("$")
    let l  = line(".")
    let li = getline(l)
    let sli = s:Subline(li, c, a:mode)
    let by = line2byte(l) + c
    " Adjust for vim idiosyncrasy
    if c == e - 1 && li[c - 1] == " "
        let e = e - 1
    endif
    " TLogVAR li, sli, c, e, l, by
    let i = s:GetIndent(sli)
    let [setCursor, bittext] = s:RetrieveBit(a:agent, a:bit, i)
    " TLogVAR setCursor, bittext
    " TLogVAR getpos('.')
    call cursor(0, c)
    " TLogVAR getpos('.')
    let shift = s:InsertBitText(a:mode, bittext)
    " TLogVAR shift, getpos('.')
    if setCursor
        let ll = l + setCursor - 1
        " TLogVAR ll
        call tskeleton#SetCursor(l, ll, a:mode)
    else
        let byte = by + len(bittext) - 1 + shift
        " TLogVAR byte, by, len(bittext)
        if s:IsInsertMode(a:mode) && s:IsEOL(a:mode) && len(li) > 0
            let byte += 1
        end
        exec 'go '. byte
        " TLogVAR getpos('.')
    endif
endf


function! s:InsertBitText(mode, bittext) "{{{3
    " TLogVAR a:mode, a:bittext
    " TLogDBG "Pre: ". getline('.')
    " call tlib#buffer#InsertText0(a:bittext, {
    "             \ 'pos': 's',
    "             \ 'indent': a:mode =~# 'l' ? 0 : 1,
    "             \ 'mode': a:mode,
    "             \ })
    " TLogDBG string(getline(1, '$'))
    " TLogExec sleep 3 | redraw
    let rv = tlib#buffer#InsertText(a:bittext, {
                \ 'shift': s:IsEOL(a:mode) ? 0 : -1,
                \ 'pos': 's',
                \ 'indent': a:mode =~# 'l' ? 0 : 1,
                \ })
    if !s:IsInsertMode(a:mode) && !s:IsEOL(a:mode)
        let rv -= 1
    endif
    " TLogExec sleep 3 | redraw
    " TLogDBG string(getline(1, '$'))
    " TLogDBG "Post: ". getline('.')
    return rv
endf


function! s:GetIndent(line) "{{{3
    return matchstr(a:line, '^\(\s*\)')
endf


function! s:IndentLines(from, to, indent) "{{{3
    " silent exec a:from.",".a:to.'s/\(^\|\n\)/\1'. escape(a:indent, '/\') .'/g'
    " TLogVAR a:indent
    silent exec a:from.",".a:to.'s/^/'. escape(a:indent, '/\') .'/g'
endf


function! s:CharRx(char) "{{{3
    let rv = '&\?'
    if a:char == '\\'
        return rv .'\('. tlib#url#EncodeChar('\') .'\|\\\)'
    elseif a:char =~ '[/*#<>|:"?{}~]'
        return rv .'\('. tlib#url#EncodeChar(a:char) .'\|'. a:char .'\)'
    else
        return rv . a:char
    endif
endf


function! s:BitRx(bit, escapebs) "{{{3
    let rv = substitute(escape(a:bit, '\'), '\(\\\\\|.\)', '\=s:CharRx(submatch(1))', 'g')
    return rv
endf


function! s:FindValue(list, function, ...)
    " TLogDBG "function=". a:function
    " TLogDBG "list=". string(a:list)
    for elt in a:list
        try
            let fn  = printf(a:function, escape(string(elt), '\'))
            " TLogDBG "fn=". fn
            let val = eval(fn)
            " TLogDBG "val=". val
            if !empty(val)
                " TLogDBG "rv=". val
                return val
            endif
        catch
            echohl Error
            echom v:errmsg
            echohl NONE
        endtry
        unlet elt
    endfor
    return a:0 >= 1 ? a:1 : 0
endf


function! s:IsDefined(bit) "{{{3
    " TLogVAR bufname('%'), !empty(a:bit), exists('b:tskelBitDefs')
    " call tlog#Log("has_key(b:tskelBitDefs, a:bit) = ". has_key(b:tskelBitDefs, a:bit))
    return !empty(a:bit) && exists('b:tskelBitDefs') && has_key(b:tskelBitDefs, a:bit)
endf


function! s:GetBitDefs() "{{{3
    if !exists('b:tskelBitDefs')
        call tskeleton#PrepareBits()
    endif
    return exists('b:tskelBitDefs') ? b:tskelBitDefs : {}
endf


function! s:SelectAndInsert(bit, mode) "{{{3
    " TLogVAR a:bit
    if s:IsDefined(a:bit)
        call s:InsertBit('text', a:bit, a:mode)
        return 1
    endif
    return 0
endf


" tskeleton#Bit(bit, ?mode='n')
function! tskeleton#Bit(bit, ...) "{{{3
    " TLogVAR a:bit
    " TAssert IsNotEmpty(a:bit)
    call tskeleton#PrepareBits()
    let mode = a:0 >= 1 ? a:1 : 'n'
    let processing = s:SetProcessing()
    " let wlayout    = tlib#win#GetLayout(1)
    let wlayout    = tlib#win#GetLayout()
    let s:tskel_highlight = 0
    try
        if empty(a:bit)
            call s:BitMenu(a:bit, mode, s:Filetype())
        elseif s:SelectAndInsert(a:bit, mode)
            " call tlog#Debug('s:SelectAndInsert ok')
            return 1
        else
            " call tlog#Debug("TSkeletonBit: Unknown bit '". a:bit ."'")
            if s:IsPopup(mode)
                call s:InsertBitText(mode, a:bit)
                return 1
            endif
            return 0
        endif
        " catch
        "     echom "An error occurred in TSkeletonBit() ... ignored"
    finally
        if s:tskel_highlight && !exists("b:tskelHighlight")
            call tskeleton#Highlight()
        endif
        call tlib#win#SetLayout(wlayout)
        call s:SetProcessing(processing)
    endtry
endf


function! s:IsInsertMode(mode) "{{{3
    return a:mode =~? 'i'
endf


function! s:IsEOL(mode) "{{{3
    if a:mode =~ 'g'
        let mode = a:mode . s:Eol(a:mode, col('.'))
    else
        let mode = a:mode
    endif
    return mode =~? '1'
endf


function! s:IsPopup(mode) "{{{3
    return a:mode =~? 'p'
endf


function! s:Subline(line, col, mode) "{{{3
    " TLogVAR a:line, a:col, a:mode
    let e = a:col - 1
    " if s:IsInsertMode(a:mode) || s:IsEOL(a:mode)
    " if a:col == 1 && s:IsInsertMode(a:mode)
    "     let e -= 1
    " endif
    if s:IsEOL(a:mode)
        let e += 1
    endif
    if e < 0
        let e = 0
    endif
    let rv = strpart(a:line, 0, e)
    " TLogVAR rv
    return rv
endf


function! s:BitMenu(bit, mode, filetype) "{{{3
    " TLogVAR a:bit
    if has("menu") && g:tskelQueryType == 'popup'
        return s:BitMenu_menu(a:bit, a:mode, a:filetype)
    else
        return s:BitMenu_query(a:bit, a:mode, a:filetype)
    endif
endf


function! s:BitMenuEligible(agent, bit, mode, filetype) "{{{3
    " TLogVAR a:agent, a:bit, a:mode, a:filetype
    call s:SetLine(a:mode)
    let t = copy(s:EligibleBits(a:filetype))
    " TLogVAR len(t)
    " TAssert IsList(t)
    let s:tskelMenuEligibleIdx = 0
    let s:tskelMenuEligibleRx  = '^'. s:BitRx(a:bit, 0)
    call filter(t, 'v:val =~ ''\S'' && s:MatchBit(v:val, s:tskelMenuEligibleRx)')
    " TLogVAR len(t)
    if g:tskelPopupNumbered
        call sort(t)
    endif
    let e = map(t, 's:BitMenuEligible_'. a:agent .'_cb(v:val, '. string(a:mode) .')')
    " TAssert IsList(e)
    " TLogDBG 'e='. string(e)
    return tlib#list#Compact(e)
endf


function! s:MatchBit(value, rx) "{{{3
    let case_sensitive = tlib#var#Get('tskelCaseSensitive_'. s:Filetype(), 'bg', tlib#var#Get('tskelCaseSensitive', 'bg'))
    if case_sensitive == 1
        return a:value =~# a:rx
    elseif case_sensitive == -1
        return a:value =~ a:rx
    elseif case_sensitive == 0
        return a:value =~? a:rx
    else
        echoerr 'tskelCaseSensitive: Must be one of: 1, -1, 0'
    endif
endf


function! s:BitMenuEligible_complete_cb(bit, mode) "{{{3
   return s:BitMenuEligible_query_cb(a:bit, a:mode)
endf


function! s:BitMenu_query(bit, mode, filetype) "{{{3
    let s:tskelQueryAcc = s:BitMenuEligible('query', a:bit, a:mode, a:filetype)
    if len(s:tskelQueryAcc) <= 1
        let rv = get(s:tskelQueryAcc, 0, a:bit)
        let rv = get(s:tskelQueryAcc, 0, '')
    else
        let qu = "s:tskelQueryAcc|Select bit:"
        let rv = s:Query(qu)
    endif
    " TLogVAR rv
    if rv != ''
        call tskeleton#Bit(rv, a:mode .'p')
        return 1
    endif
    return 0
endf


function! s:BitMenuEligible_query_cb(bit, mode) "{{{3
    return tlib#url#Decode(a:bit)
endf


function! s:BitMenu_menu(bit, mode, filetype) "{{{3
    try
        silent! aunmenu ]TSkeleton
    catch
        " echohl Error
        " echom v:errmsg
        " echohl NONE
    endtry
    " TLogDBG 'bit='. a:bit
    let rv = s:BitMenuEligible('menu', a:bit, a:mode, a:filetype)
    " TAssert IsList(rv)
    let j = len(rv)
    if j == 1
        exec s:tskelMenuEligibleEntry
        return 1
    elseif j > 0
        popup ]TSkeleton
        return 1
    endif
    return 0
endf


function! s:BitMenuEligible_menu_cb(bit, mode) "{{{3
    " TAssert IsString(a:bit)
    " TLogDBG 'bit='. a:bit
    " call tlog#Debug('tskelMenuEligibleRx=~'. a:bit =~ s:tskelMenuEligibleRx)
    let s:tskelMenuEligibleIdx = s:tskelMenuEligibleIdx + 1
    if g:tskelPopupNumbered
        if stridx(a:bit, '&') == -1
            let x = substitute(s:tskelMenuEligibleIdx, '\(.\)$', '\&\1', '')
        else
            let x = s:tskelMenuEligibleIdx
        end
        let x .= '\ '
        let m = a:bit
    else
        let x = ''
        let m = escape(tskeleton#BitDef(a:bit, 'menu'), '"\ 	')
    endif
    let s:tskelMenuEligibleEntry = 'call tskeleton#Bit('. string(a:bit) .', "'. a:mode .'p")'
    " call tlog#Debug(s:tskelMenuEligibleEntry)
    exec 'amenu ]TSkeleton.'. x . m .' :'. s:tskelMenuEligibleEntry .'<cr>'
    return 1
endf


function! s:Eol(mode, col) "{{{3
    " TLogVAR a:mode, a:col, col('$'), a:col >= col('$')
    " TLogDBG col('.') .'-'. col('$')
    " echom "DBG Eol ". a:mode .' '. s:IsInsertMode(a:mode)
    if s:IsInsertMode(a:mode)
        " return a:col + 1 >= col('$')
        return a:col >= col('$') - 1
    else
        return a:col >= col('$') && &virtualedit =~ '^\(block\|onemore\)\?$'
    endif
endf


" tskeleton#ExpandBitUnderCursor(mode, ?bit="", ?default={})
" See also |g:tskelHyperComplete|.
function! tskeleton#ExpandBitUnderCursor(mode, ...) "{{{3
    let bit     = a:0 >= 1 && a:1 != '' ? a:1 : ''
    let default = a:0 >= 2 && !empty(a:2) ? a:2 : {}
    let line  = line('.')
    let l     = getline(line)
    let col0  = col('.')
    " TLogDBG 'ExpandBit saveview '. line('w0')
    let view  = winsaveview()
    " TLogVAR line, col0, l
    " TLogVAR bit
    call tskeleton#PrepareBits()
    let t = @t
    let lazyredraw = &lazyredraw
    set lazyredraw
    let unset_ft = !exists('g:tskelFiletype')
    if unset_ft
        let g:tskelFiletype = &filetype
    endif
    " TLogVAR g:tskelFiletype
    try
        let @t    = ''
        let filetype = s:Filetype()
        let imode = s:IsInsertMode(a:mode)
        " TLogVAR col0
        let col   = col0
        if imode
            if s:Eol(a:mode, col)
                let eol_adjustment = 1
            else
                let col -= 1
                let eol_adjustment = 0
            endif
        else
            let eol_adjustment = (col + 1 >= col('$'))
        endif
        " TLogVAR imode, eol_adjustment
        let mode = a:mode . eol_adjustment
        " TLogVAR mode
        if bit != ''
            let @t = bit
        else
            let c = l[col - 1]
            let pos = '\%#'
            if c =~ '\s'
                let @t = ''
                " TLogDBG " 0 @t=". @t
                if !imode && !eol_adjustment
                    norm! l
                endif
            elseif s:SearchKeyword(filetype, pos)
                if imode && eol_adjustment
                    let d = col - col('.')
                else
                    let d = col - col('.') + 1
                endif
                exec 'silent norm! "td'. d .'l'
                " TLogDBG " 1 @t='". @t ."'"
            elseif imode && !eol_adjustment
                silent norm! h"tdiw
                " TLogDBG " 2 @t='". @t ."'"
            else
                silent norm! "tdiw
                " TLogDBG " 3 @t='". @t ."'"
            endif
        endif
        " TLogDBG "#1". getline(line)
        let bit = @t
        if bit =~ '^\s\+$'
            let bit = ''
        endif
        " TLogDBG " 4 bit='". bit ."'"
        if bit != '' && tskeleton#Bit(bit, mode) == 1
            " call tlog#Debug("TSkeletonBit succeeded!")
            return 1
        elseif (bit	!= '' || empty(default)) && s:BitMenu(bit, mode, filetype)
            " call tlog#Debug("s:BitMenu succeeded!")
            " TLogVAR mode, bit, default
            " return s:InsertDefault(mode, bit, default)
            return 0
        endif
        " TLogDBG "#2". getline(line)
        " TLogVAR bit
        " TLogVAR default
        let s:got_default_string = 0
        " TLogDBG 'ExpandBit w0='. line('w0')
        if s:InsertDefault(mode, bit, default)
            " TLogDBG 's:InsertDefault succeeded! '. line('w0')
            return 1
        else
            " TLogDBG "#3". getline(line)
            " TLogDBG 'Last ressort'
            if s:got_default_string
                call s:InsertBitText(mode, bit)
                let success = 0
            else
                let [success, default_string] = s:GetDefaultString(bit, default)
                " TLogVAR success, default_string
                call s:InsertBitText(mode, default_string)
            endif
            if !success
                echom "TSkeletonBit: Unknown bit '". bit ."'"
            endif
            " TLogVAR line, col0
            " call cursor(line, col0, imode)
            " call cursor(line, col0)
            " TLogDBG col('.')
            call winrestview(view)
            " TLogDBG 'ExpandBit restview '. line('w0')
            return success
        endif
    finally
        let @t = t
        call s:UnsetLine()
        let lazyredraw  = &lazyredraw
        if unset_ft
            unlet g:tskelFiletype
        endif
    endtry
endf


function! s:KeywordRx(...) "{{{3
    TVarArg ['quantifier', '\{-}'], ['filetype', '']
    " TLogVAR filetype
    if empty(filetype)
        let filetype = s:Filetype()
    endif
    let rx = tlib#var#Get('tskelKeyword_'. filetype, 'bg', '\k\Q')
    " TLogVAR filetype, rx
    let rx = substitute(rx, '\(\\\)\@<!\\Q', tlib#rx#EscapeReplace(quantifier), 'g')
    " TLogVAR rx
    return rx
endf


function! s:SearchKeyword(filetype, pos) "{{{3
    let kw = s:KeywordRx()
    " TLogVAR kw
    return !empty(kw) && search(kw . a:pos) != -1
endf


function! tskeleton#WithSelection(pre) "{{{3
    let text = @"
    let bit = input('Skeleton: ', '', 'custom,tskeleton#SelectBit')
    " TLogVAR bit
    if !empty(bit)
        call tskeleton#Bit(bit)
        call tlib#buffer#InsertText(a:pre . tlib#string#Chomp(text), {
                    \ 'shift': -1,
                    \ 'pos': 's',
                    \ 'indent': 0,
                    \ })
    else
        norm! u
    endif
endf


function! s:InsertDefault(mode, bit, default) "{{{3
    " TLogVAR a:mode, a:bit, a:default
    if !empty(a:default)
        let view = winsaveview()
        let [success, default_string] = s:GetDefaultString(a:bit, a:default)
        " TLogVAR success, default_string
        if success
            call winrestview(view)
            let pos = getpos('.')
            call s:InsertBitText(a:mode, default_string)
            " TLogDBG col('.')
            if default_string =~ '<+CURSOR+>'
                call setpos('.', pos)
                if search('<+CURSOR+>', 'W')
                    norm! cf>
                endif
            else
                let left = len(default_string)
                if col('.') == 1
                    let left -= 1
                endif
                exec 'norm! '. left .'l'
            endif
            if s:IsInsertMode(a:mode)
                call cursor(line('.'), col('.') + 1)
            endif
        endif
        return success
    else
        return 0
    endif
endf


function! s:AddDefaultCompletions(completions, bit, default) "{{{3
    " TLogVAR a:default
    for [name, fn] in items(tlib#var#Get('tskel_completions', 'bg', {}))
        if has_key(a:default, name)
            call call(fn, [a:bit, a:completions])
        endif
    endfor
    return a:completions
endf


function! s:GetDefaultString(bit, default) "{{{3
    " TLogVAR a:bit, a:default
    " Don't call this function twice in one go -- see tskeleton#ExpandBitUnderCursor().
    let s:got_default_string = 1
    let completions = {}
    call s:AddDefaultCompletions(completions, a:bit, a:default)

    if !empty(completions)
        " TLogDBG 'Process completions'
        let s:tskelQueryAcc = keys(completions)
        " call sort(s:tskelQueryAcc, 1)
        " call tlog#Debug(string(s:tskelQueryAcc))
        let rv = s:Query("s:tskelQueryAcc|Select bit:")
        " TLogVAR rv
        return [!empty(rv), rv]
    endif

    if has_key(a:default, 'string')
        " TLogDBG 'Process string'
        " TLogVAR a:default.string
        return [1, a:bit . a:default.string]
    endif

    " TLogDBG "Fall back"
    " TLogVAR a:bit
    return [0, a:bit]
endf


function! tskeleton#Complete_use_omnifunc(bit, completions) "{{{3
    " TLogDBG 'use_omnifunc'
    if !empty(&omnifunc)
        " TLogDBG 'use_omnicomplete'
        for w in tskeleton#GetCompletions(&omnifunc, a:bit)
            let [cname, mname] = tskeleton#PurifyBit(w)
            let a:completions[w] = {'text': w, 'menu': 'OmniComplete.'. mname, 'type': 'tskeleton'}
        endfor
    endif
endf


function! tskeleton#Complete_use_completefunc(bit, completions) "{{{3
    if !empty(&completefunc)
        " TLogDBG 'use_completefunc'
        for w in tskeleton#GetCompletions(&completefunc, a:bit)
            let [cname, mname] = tskeleton#PurifyBit(w)
            let a:completions[w] = {'text': w, 'menu': 'CompleteFunc.'. mname, 'type': 'tskeleton'}
        endfor
    endif
endfun


function! tskeleton#Complete_scan_words(bit, completions) "{{{3
    " TLogDBG 'scan_tags saveview '. line('w0')
    let view = winsaveview()
    if empty(a:bit)
        let rx = s:KeywordRx()
        " TLogVAR rx
    else
        let kw = '\k\+'
        let rx = tlib#rx#Escape(a:bit) . kw
        " TLogVAR kw, rx
    endif
    norm! G$
    let [lnum, col] = searchpos(rx, 'w')
    " TLogVAR lnum, col
    while lnum > 0
        let subline = getline(lnum)[col - 1 : -1]
        let match = matchstr(subline, '^'. rx)
        if !empty(match)
            let a:completions[match] = {'text': match, 'type': 'tskeleton'}
        endif
        norm! w
        " TLogVAR lnum, col, match, subline
        let [lnum, col] = searchpos(rx, 'W')
    endwh
    call winrestview(view)
    " TLogDBG 'scan_tags restview '. line('w0')
endf


function! tskeleton#Complete_scan_tags(bit, completions) "{{{3
    " TLogDBG 'scan_tags'
    let tags = taglist('^'. tlib#rx#Escape(a:bit))
    if !empty(tags)
        for tag in tags
            let a:completions[tag.name] = {'text': tag.name, 'type': 'tskeleton'}
        endfor
    endif
endf


function! tskeleton#GetCompletions(func, bit) "{{{3
    let b:compl_context = getline('.')[0 : col('.')] . a:bit
    let completions = call(a:func, [0, a:bit])
    " TLogVAR a:func, completions
    call map(completions, 'substitute(type(v:val) == 4 ? v:val.word : v:val, "($", "", "")')
    call filter(completions, 'v:val != a:bit')
    return completions
endf


" :display: tskeleton#Complete(findstart, base, ?default={})
" A function suitable for use as 'completefunc' or 'omnifunc'.
" You have to accept a match with <c-y> in order to trigger skeleton 
" expansion. See |popupmenu-keys|.
function! tskeleton#Complete(findstart, base, ...)
    " TLogVAR a:findstart, a:base
    if a:findstart
        let pattern = s:KeywordRx()
        let line    = strpart(getline('.'), 0, col('.') - 1)
        let start   = match(line, pattern.'$')
        " TLogVAR pattern, line, start
        return start == -1 ? col('.') - 1 : start
    else
        let default = a:0 >= 1 ? a:1 : {}
        " TAssertType default, 'dictionary'
        let filetype = s:Filetype()
        let t = s:BitMenuEligible('complete', a:base, 'i', filetype)
        let setup_done = s:DidSetup(filetype)
        " TLogVAR len(t), setup_done
        if setup_done
            let completions = copy(g:tskelBits_{filetype})
            " TLogVAR len(completions)
            call filter(completions, 'strpart(v:val.text, 0, len(a:base)) ==# a:base')
            " TLogVAR len(completions)
            if !empty(default)
                call s:AddDefaultCompletions(completions, a:base, default)
            endif
            " TLogVAR len(completions), completions
            for [bit, def] in items(completions)
                " TLogVAR bit, def
                " TAssert IsDictionary(def)
                call add(t, {'word': def['text'], 'abbr': bit})
            endfor
        endif
        if len(t) > 0
            let s:pum_complete_args = {
                        \ 'pos': getpos('.'),
                        \ 'eol': col('.') >= col('$'),
                        \ 'view': winsaveview(),
                        \ 'eligible': t,
                        \ }
            " call tlog#Debug("s:pum_complete_args: ". string(s:pum_complete_args))
            autocmd! tSkeleton CursorMoved,CursorMovedI,CursorHold,CursorHoldI <buffer>
            autocmd tSkeleton CursorMoved,CursorMovedI,CursorHold,CursorHoldI <buffer> call s:ExpandCompletion(s:pum_complete_args)
        endif
        " TAssert IsList(t)
        return t
    endif
endf


function! s:ExpandCompletion(args) "{{{3
    " TLogVAR pumvisible()
    if !pumvisible()
        autocmd! tSkeleton CursorMoved,CursorMovedI,CursorHold,CursorHoldI <buffer>
        " TLogVAR a:args.pos, bufnr('%'), col('.')
        if a:args.pos[0] == 0 || a:args.pos[0] == bufnr('%')
            let line = getline('.')
            let beg  = a:args.pos[2] - 1
            let end  = col('.') - 1
            let bit  = strpart(line, beg, end)
            " TLogVAR line, beg, end, bit
            if index(a:args.eligible, bit) != -1
                " exec 'norm! a'. repeat("\<BS>", len(bit))
                " call tskeleton#ExpandBitUnderCursor("n", bit)
                " norm! a<++><Left><Left><Left><Left><Left>
                call tskeleton#ExpandBitUnderCursor("i")
                redraw
            endif
        endif
    endif
endf


function! s:TagSelect(chars, mode) "{{{3
    " TLogDBG 'chars='. a:chars .' mode='. a:mode
    let chars = &selection == 'exclusive' ? a:chars : a:chars - 1
    " TLogVAR chars
    if a:mode == 'd'
        let cp = col('.') + chars
        " TLogDBG 'col.='. col('.') .' colp='. cp .' col$='. col('$')
        if cp >= col('$')
            if &ve =~ 'all'
                let correction = '$l'
            else
                let correction = '$'
            endif
        else
            let correction = ''
        endif
        " TLogVAR correction
        exec 'norm! d'. chars .'l'.correction
    else
        " TLogDBG 'norm! v'. chars .'l'
        exec 'norm! v'. chars .'l'
        if g:tskelSelectTagMode[0] == 's'
            exec "norm! \<c-g>"
        endif
    endif
endf


function! tskeleton#GoToNextTag() "{{{3
    let rx = '\('. g:tskelMarkerExtra .'\|'. tskeleton#WrapMarker('') .'\|'. tskeleton#TagRx() .'\)'
    let x  = search(rx, 'c')
    if x > 0
        let lc = exists('b:tskelLastCol')  ? b:tskelLastCol : col('.')
        " TLogVAR lc
        let l  = strpart(getline(x), lc - 1)
        " TLogVAR l
        let ms = matchstr(l, rx)
        " TLogVAR ms
        let ml = len(ms)
        " TLogVAR ml
        if ms =~# g:tskelMarkerExtra
            call s:TagSelect(ml, 'v')
        else
            if ml == 4
                call s:TagSelect(ml, 'd')
            else
                let defrx = tskeleton#WrapMarker('.\{-}/\zs.\{-}\ze', 'rx')
                if ms =~ defrx
                    let default = matchstr(ms, defrx)
                    " TLogVAR getline('.')
                    call s:TagSelect(ml, 'd')
                    " TLogVAR getline('.')
                    let shift = s:Eol('i', col('.')) ? 0 : -1
                    " TLogVAR shift
                    call tlib#buffer#InsertText(default, {'pos': 's', 'shift': shift})
                    " TLogVAR getline('.')
                    " TLogVAR col('.'), col('$')
                    if col('.') > 1
                        norm! l
                    endif
                    exec 'norm! v'. len(default) .'l'
                    if !empty(&selectmode)
                        call feedkeys("\<c-g>")
                    endif
                else
                    call s:TagSelect(ml, 'v')
                endif
            endif
        endif
    endif
endf


function! tskeleton#Highlight()
    if !empty(g:tskelMarkerHiGroup)
        " exec 'syntax match TSkelPlaceHolder /'. escape(tskeleton#WrapMarker('\w*', 'rx'), '/') .'/'
        exec 'syntax match TSkelPlaceHolder /'. escape(tskeleton#WrapMarker('.\{-}', 'rx'), '/') .'/'
        exec 'hi def link TSkelPlaceHolder '. g:tskelMarkerHiGroup
    endif
    let b:tskelHighlight = 1
endf


function! tskeleton#LateExpand() "{{{3
    let l  = getline('.')
    let lc = col('.') - 1
    let left  = tlib#var#Get('tskelMarkerLeft', 'bg')
    let right = tlib#var#Get('tskelMarkerRight', 'bg')
    while strpart(l, lc, len(left)) != left
        " TLogVAR lc
        " TLogDBG strpart(l, lc, 2)
        let lc -= 1
        if lc <= -1 || strpart(l, lc, len(right)) == right
            throw "TSkeleton: No tag under cursor"
        endif
    endwh
    let l  = strpart(l, lc)
    let me = matchend(l, tskeleton#WrapMarker('.\{-}', 'rx'))
    if me < 0
        throw "TSkeleton: No tag under cursor"
    else
        let lp = substitute(strpart(l, 2, me - 4), '\W', '_', 'g')
        let v  = ''
        if exists('*TSkeletonCB_'. lp)
            let v = TSkeletonCB_{lp}()
        elseif exists('*TSkeleton_'. lp)
            let v = TSkeleton_{lp}()
        else
            throw 'TSkeleton: No callback defined for '. lp .' (TSkeletonCB_'. lp .')'
        endif
        if v != ''
            " exec 'norm! '. (lc + 1) .'|d'. me .'li'. v
            call cursor(0, lc + 1)
            exec 'norm! d'. me .'li'. v
            return
        endif
    endif
endf


" misc utilities {{{1
function! tskeleton#IncreaseRevisionNumber() "{{{3
    let rev = exists("b:revisionRx") ? b:revisionRx : g:tskelRevisionMarkerRx
    let ver = exists("b:versionRx")  ? b:versionRx  : g:tskelRevisionVerRx
    let pos = getpos('.')
    let rs  = @/
    " exec 'keepjumps %s/'.rev.'\('.ver.'\)*\zs\(-\?\d\+\)/\=(submatch(g:tskelRevisionGrpIdx) + 1)/e'
    exec '%s/'.rev.'\('.ver.'\)*\zs\(-\?\d\+\)/\=(submatch(g:tskelRevisionGrpIdx) + 1)/e'
    let @/  = rs
    call setpos('.', pos)
endf


function! tskeleton#CleanUpBibEntry() "{{{3
    " '{,'}s/^.*<+.\{-}+>.*\n//e
    exec printf('''{,''}s/^.*%s.*\n//e', tskeleton#WrapMarker('.\{-}', 'rx'))
    if exists('*TSkeletonCleanUpBibEntry_User')
        call TSkeletonCleanUpBibEntry_User()
    endif
endf


" tskeleton#Repeat(n, string, ?sep="\n")
function! tskeleton#Repeat(n, string, ...) "{{{3
    let sep = a:0 >= 1 ? a:1 : "\n"
    let rv  = a:string
    let n   = a:n - 1
    while n > 0
        let rv = rv . sep . a:string
        let n  = n - 1
    endwh
    return rv
endf


function! tskeleton#InsertTable(rows, cols, rowbeg, rowend, celljoin) "{{{3
    let y = a:rows
    let r = ''
    while y > 0
        let x = a:cols
        let r = r . a:rowbeg
        while x > 0
            if x == a:cols
                let r = r .tskeleton#WrapMarker('CELL')
            else
                let r = r . a:celljoin .tskeleton#WrapMarker('CELL')
            end
            let x = x - 1
        endwh
        let r = r. a:rowend
        if y > 1
            let r = r ."\n"
        endif
        let y = y - 1
    endwh
    return r
endf


function! tskeleton#HyperComplete_query(mode, default) "{{{3
    let col = col('.') - 1
    if s:IsInsertMode(a:mode) && col > 0
        let col -= 1
    endif
    let pre = getline('.')[0 : col]
    " TLogVAR col, pre
    if empty(pre) || pre =~ '\s$'
        " TLogVAR a:default
        let view = winsaveview()
        exec 'norm! '. a:default
        call winrestview(view)
    else
        " TLogVAR a:mode
        let w:tskeleton_hypercomplete = 1
        try
            let filetype = s:Filetype()
            call tskeleton#ExpandBitUnderCursor(a:mode, '', tlib#var#Get('tskelHyperComplete_'. filetype, 'bg', tlib#var#Get('tskelHyperComplete', 'bg', {})))
        finally
            unlet! w:tskeleton_hypercomplete
        endtry
    endif
endf


function! tskeleton#HyperComplete_pum(mode, default) "{{{3
    let start = tskeleton#Complete(1, "")
    let pos   = getpos('.')
    let line  = pos[1]
    let col   = pos[2]
    let text0 = getline(line)
    let base0 = strpart(text0, start)
    let kwrx  = s:KeywordRx('\+')
    let kwrx  = substitute(kwrx, '\(\\\)\@<!\\{-}', '\\+', 'g')
    " TLogVAR line, col, start, kwrx, text0, base0
    let base  = matchstr(base0, '^'. kwrx)
    let text  = strpart(text0, 0, start) . strpart(text0, start + len(base))
    " TLogVAR text, base
    let pos[2] = start + 1
    exec line .'delete'
    " TLogVAR text
    call append(line - 1, text)
    call setpos('.', pos)
    let filetype = s:Filetype()
    let hyper = tlib#var#Get('tskelHyperComplete_'. filetype, 'bg', tlib#var#Get('tskelHyperComplete', 'bg', {}))
    let completions = tskeleton#Complete(0, base, hyper)
    " TLogVAR len(completions)
    call sort(completions)
    call complete(col, completions)
    return ''
endf


plugin/tskeleton.vim	[[[1
545
" tSkeleton.vim
" @Author:      Tom Link (micathom AT gmail com?subject=vim)
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     21-Sep-2004.
" @Last Change: 2010-02-26.
" @Revision:    3897
"
" GetLatestVimScripts: 1160 1 tSkeleton.vim
" http://www.vim.org/scripts/script.php?script_id=1160
"
" TODO: When bits change, opened hidden buffer don't get updated it 
" seems.
" TODO: Enable multiple skeleton directories (and maybe other sources 
" like DBs).
" TODO: Sorted menus.
" TODO: ADD: More html bits
" TODO: ADD: <tskel:post> embedded tag (evaluate some vim code on the 
" visual region covering the final expansion)

if &cp || exists("loaded_tskeleton") "{{{2
    finish
endif
if !exists('loaded_tlib') || loaded_tlib < 29
    runtime plugin/02tlib.vim
    if !exists('loaded_tlib') || loaded_tlib < 29
        echoerr "tSkeleton requires tlib >= 0.29"
        finish
    endif
endif
let loaded_tskeleton = 409


if !exists("g:tskelDir") "{{{2
    let g:tskelDir = get(split(globpath(&rtp, 'skeletons/'), '\n'), 0, '')
endif
if !isdirectory(g:tskelDir) "{{{2
    echoerr 'tSkeleton: g:tskelDir ('. g:tskelDir .') isn''t readable. See :help tSkeleton-install for details!'
    finish
endif
let g:tskelDir = tlib#dir#CanonicName(g:tskelDir)

if !exists('g:tskelBitsDir') "{{{2
    let g:tskelBitsDir = g:tskelDir .'bits/'
    " call tlib#dir#Ensure(g:tskelBitsDir)
endif

let g:tskeleton_SetFiletype = 1

if !exists("g:tskelMapLeader")     | let g:tskelMapLeader     = "<Leader>#"   | endif "{{{2
if !exists("g:tskelMapInsert")     | let g:tskelMapInsert     = '<c-\><c-\>'  | endif "{{{2
if !exists("g:tskelAddMapInsert")  | let g:tskelAddMapInsert  = 0             | endif "{{{2
if !exists("g:tskelMenuCache")      | let g:tskelMenuCache = '.tskelmenu'  | endif "{{{2
if !exists("g:tskelMenuPrefix")     | let g:tskelMenuPrefix  = 'TSke&l'    | endif "{{{2

if !exists("g:tskelMapHyperComplete") "{{{2
    if empty(maparg('<c-space>') . maparg('<c-space>', 'i'))
        let g:tskelMapHyperComplete = '<c-space>'
    else
        let g:tskelMapHyperComplete = ''
    endif
endif

if !exists("g:tskelHyperComplete") "{{{2
    " let g:tskelHyperComplete = {'use_completefunc': 1, 'use_omnifunc': 1, 'scan_words': 1, 'scan_tags': 1}
    let g:tskelHyperComplete = {'use_completefunc': 1, 'scan_words': 1, 'scan_tags': 1}
endif

if !exists('g:tskelHyperType')
    " Either query or pum.
    " If you set the variable to "pum", you have to accept completions 
    " with <c-y>.
    " This variable must be set in your |vimrc| file before loading the 
    " tskeleton plugin.
    let g:tskelHyperType = 'query'   "{{{2
    " let g:tskelHyperType = 'pum'   "{{{2
endif


function! TSkeletonMapGoToNextTag() "{{{3
    nnoremap <silent> <c-j> :call tskeleton#GoToNextTag()<cr>
    vnoremap <silent> <c-j> <c-\><c-n>:call tskeleton#GoToNextTag()<cr>
    inoremap <silent> <c-j> <c-\><c-o>:call tskeleton#GoToNextTag()<cr>
endf


" In the current buffer, map a:key so that
"   - If the cursor is located at the beginning of the line or if the 
"     the cursor is over a whitespace character, indent the current
"     line
"   - otherwise expand the bit under the cursor or (if not suitable bit
"     was found) use &omnifunc, &completefunc, tags, and (as fallback 
"     strategy) the words in the buffer as possible completions.
function! TSkeletonMapHyperComplete(key, ...) "{{{3
    let default = a:0 >= 1 ? a:1 : '=='
    if g:tskelHyperType == 'pum'
        exec 'inoremap '. a:key .' <C-R>=tskeleton#HyperComplete_'. g:tskelHyperType .'("i", '. string(default) .')<cr>'
    elseif g:tskelHyperType == 'query'
        exec 'inoremap '. a:key .' <c-\><c-o>:call tskeleton#HyperComplete_'. g:tskelHyperType .'("i", '. string(default) .')<cr>'
    else
        echoerr "tSkeleton: Unknown type for g:tskelHyperType: "+ g:tskelHyperType
    endif
    exec 'noremap '. a:key .' :call tskeleton#HyperComplete_query("n", '. string(default) .')<cr>'
endf
if !empty(g:tskelMapHyperComplete)
    call TSkeletonMapHyperComplete(g:tskelMapHyperComplete)
endif


command! -nargs=* -complete=custom,tskeleton#SelectTemplate TSkeletonSetup 
            \ call tskeleton#Setup(<f-args>)


command! -nargs=? -complete=custom,tskeleton#SelectTemplate TSkeletonEdit 
            \ call tskeleton#Edit(<q-args>)


command! -nargs=? -complete=customlist,tskeleton#EditBitCompletion TSkeletonEditBit 
            \ call tskeleton#EditBit(<q-args>)


command! -nargs=* -complete=custom,tskeleton#SelectTemplate TSkeletonNewFile 
            \ call tskeleton#NewFile(<f-args>)


command! -bar -nargs=? TSkeletonBitReset call tskeleton#ResetBits(<q-args>)


command! -nargs=? -complete=custom,tskeleton#SelectBit TSkeletonBit
            \ call tskeleton#Bit(<q-args>)


command! TSkeletonCleanUpBibEntry call tskeleton#CleanUpBibEntry()

if !empty(g:tskelMapLeader)
    " noremap <unique> <Leader>tt ""diw:TSkeletonBit <c-r>"
    exec "noremap <unique> ". g:tskelMapLeader ."t :TSkeletonBit "
    
    exec "nnoremap <unique> ". g:tskelMapLeader ."# :call tskeleton#ExpandBitUnderCursor('n')<cr>"
    if g:tskelAddMapInsert
        exec "inoremap <unique> ". g:tskelMapInsert ." <c-\\><c-o>:call tskeleton#ExpandBitUnderCursor('i','', {'string':". string(g:tskelMapInsert) ."})<cr>"
    else
        exec "inoremap <unique> ". g:tskelMapInsert ." <c-\\><c-o>:call tskeleton#ExpandBitUnderCursor('i')<cr>"
    endif
    
    exec "vnoremap <unique> ". g:tskelMapLeader ."# d:call tskeleton#WithSelection('')<cr>"
    exec "vnoremap <unique> ". g:tskelMapLeader ."<space> d:call tskeleton#WithSelection(' ')<cr>"
    
    exec "nnoremap <unique> ". g:tskelMapLeader ."x :call tskeleton#LateExpand()<cr>"
    exec "vnoremap <unique> ". g:tskelMapLeader ."x <esc>`<:call tskeleton#LateExpand()<cr>"
endif


augroup tSkeleton
    autocmd!
    if !exists("g:tskelDontSetup") "{{{2
        function! s:DefineAutoCmd(template) "{{{3
            " TLogVAR a:template
            " let sfx = fnamemodify(a:template, ':e')
            let tpl = fnamemodify(a:template, ':t')
            " TLogVAR tpl
            let filetype = tlib#url#Decode(matchstr(tpl, '^\S\+'))
            let pattern  = matchstr(tpl, '^\S\+ \+\zs.*$')
            if !empty(filetype) && !empty(pattern)
                " TLogVAR pattern
                let pattern  = substitute(pattern, '#', '*', 'g')
                " TLogVAR pattern
                let pattern  = tlib#url#Decode(pattern)
                " TLogVAR pattern
                " TLogDBG 'autocmd BufNewFile '. escape(pattern, ' ') .' set ft='. escape(filetype, ' ') .' | TSkeletonSetup '. escape(a:template, ' ')
                exec 'autocmd BufNewFile '. escape(pattern, ' ') .' set ft='. escape(filetype, ' ') .' | TSkeletonSetup '. escape(a:template, ' ')
            endif
        endf

        call map(split(glob(tlib#file#Join([g:tskelDir, 'templates', '**'], 1)), '\n'), 'isdirectory(v:val) || s:DefineAutoCmd(v:val)')
        delfunction s:DefineAutoCmd

        autocmd BufNewFile *.bat       TSkeletonSetup batch.bat
        autocmd BufNewFile *.tex       TSkeletonSetup latex.tex
        autocmd BufNewFile tc-*.rb     TSkeletonSetup tc-ruby.rb
        autocmd BufNewFile *.rb        TSkeletonSetup ruby.rb
        autocmd BufNewFile *.rbx       TSkeletonSetup ruby.rb
        autocmd BufNewFile *.sh        TSkeletonSetup shell.sh
        autocmd BufNewFile *.txt       TSkeletonSetup text.txt
        autocmd BufNewFile *.vim       TSkeletonSetup plugin.vim
        autocmd BufNewFile *.inc.php   TSkeletonSetup php.inc.php
        autocmd BufNewFile *.class.php TSkeletonSetup php.class.php
        autocmd BufNewFile *.php       TSkeletonSetup php.php
        autocmd BufNewFile *.tpl       TSkeletonSetup smarty.tpl
        autocmd BufNewFile *.html      TSkeletonSetup html.html

    endif

    exec 'autocmd BufNewFile,BufRead '. escape(g:tskelDir, ' ') .'* if g:tskeleton_SetFiletype | set ft=tskeleton | endif'
    exec 'autocmd BufWritePost '. escape(g:tskelBitsDir, ' ') .'* exec "TSkeletonBitReset ".expand("<afile>:p:h:t")'
    autocmd SessionLoadPost,BufEnter * if (g:tskelMenuPrefix != '' && g:tskelMenuCache != '' && !tskeleton#IsScratchBuffer()) | call tskeleton#BuildBufferMenu(1) | endif
    
    autocmd FileType bib if !hasmapto(":TSkeletonCleanUpBibEntry") | exec "noremap <buffer> ". g:tskelMapLeader ."c :TSkeletonCleanUpBibEntry<cr>" | endif
augroup END


" call tskeleton#PrepareBits('general')


finish

-------------------------------------------------------------------
CHANGES:

1.0
- Initial release

1.1
- User-defined tags
- Modifiers <+NAME:MODIFIERS+> (c=capitalize, u=toupper, l=tolower, 
  s//=substitute)
- Skeleton bits
- the default markup for tags has changed to <+TAG+> (for 
  "compatibility" with imaps.vim), the cursor position is marked as 
  <+CURSOR+> (but this can be changed by setting g:tskelMarkerLeft, 
  g:tskelMarkerRight, and g:tskelMarkerCursor)
- in the not so simple mode, skeleton bits can contain vim code that 
  is evaluated after expanding the template tags (see 
  .../skeletons/bits/vim/if for an example)
- function TSkeletonExpandBitUnderCursor(), which is mapped to 
  <Leader>#
- utility function: TSkeletonIncreaseRevisionNumber()

1.2
- new pseudo tags: bit (recursive code skeletons), call (insert 
  function result)
- before & after sections in bit definitions may contain function 
  definitions
- fixed: no bit name given in s:SelectBit()
- don't use ={motion} to indent text, but simply shift it

1.3
- TSkeletonCleanUpBibEntry (mapped to <Leader>tc for bib files)
- complete set of bibtex entries
- fixed problem with [&bg]: tags
- fixed typo that caused some slowdown
- other bug fixes
- a query must be enclosed in question marks as in <+?Which ID?+>
- the "test_tSkeleton" skeleton can be used to test if tSkeleton is 
  working
- and: after/before blocks must not contain function definitions

1.4
- Popup menu with possible completions if 
  TSkeletonExpandBitUnderCursor() is called for an unknown code 
  skeleton (if there is only one possible completion, this one is 
  automatically selected)
- Make sure not to change the alternate file and not to distort the 
  window layout
- require genutils
- Syntax highlighting for code skeletons
- Skeleton bits can now be expanded anywhere in the line. This makes 
  it possible to sensibly use small bits like date or time.
- Minor adjustments
- g:tskelMapLeader for easy customization of key mapping (changed the 
  map leader to "<Leader>#" in order to avoid a conflict with Align; 
  set g:tskelMapLeader to "<Leader>t" to get the old mappings)
- Utility function: TSkeletonGoToNextTag(); imaps.vim like key 
  bindings via TSkeletonMapGoToNextTag()

1.5
- Menu of small skeleton "bits"
- TSkeletonLateExpand() (mapped to <Leader>#x)
- Disabled <Leader># mapping (use it as a prefix only)
- Fixed copy & paste error (loaded_genutils)
- g:tskelDir defaults to $HOME ."/vimfiles/skeletons/" on Win32
- Some speed-up

2.0
- You can define "groups of bits" (e.g. in php mode, all html bits are 
  available too)
- context sensitive expansions (only very few examples yet); this 
  causes some slowdown; if it is too slow, delete the files in 
  .vim/skeletons/map/
- one-line "mini bits" defined in either 
  ./vim/skeletons/bits/{&filetype}.txt or in $PWD/.tskelmini
- Added a few LaTeX, HTML and many Viki skeleton bits
- Added EncodeURL.vim
- Hierarchical bits menu by calling a bit "SUBMENU.BITNAME" (the 
  "namespace" is flat though; the prefix has no effect on the bit 
  name; see the "bib" directory for an example)
- the bit file may have an ampersand (&) in their names to define the 
  keyboard shortcut
- Some special characters in bit names may be encoded as hex (%XX as 
  in URLs)
- Insert mode: map g:tskelMapInsert ('<c-\><c-\>', which happens to be 
  the <c-#> key on a German qwertz keyboard) to 
  TSkeletonExpandBitUnderCursor()
- New <tskel:msg> tag in skeleton bits
- g:tskelKeyword_{&filetype} variable to define keywords by regexp 
  (when 'iskeyword' isn't flexible enough)
- removed the g:tskelSimpleBits option
- Fixed some problems with the menu
- Less use of globpath()

2.1
- Don't accidentally remove torn off menus; rebuild the menu less 
  often
- Maintain insert mode (don't switch back to normal mode) in 
  <c-\><c-\> imap
- If no menu support is available, use the s:Query function to let 
  the user select among eligible bits (see also g:tskelQueryType)
- Create a normal and an insert mode menu
- Fixed selection of eligible bits
- Ensure that g:tskelDir ends with a (back)slash
- Search for 'skeletons/' in &runtimepath & set g:tskelDir accordingly
- If a template is named "#.suffix", an autocmd is created  
  automatically.
- Set g:tskelQueryType to 'popup' only if gui is win32 or gtk.
- Minor tweak for vim 7.0 compatibility

2.2
- Don't display query menu, when there is only one eligible bit
- EncodeURL.vim now correctly en/decoded urls
- UTF8 compatibility -- use col() instead of virtcol() (thanks to Elliot 
  Shank)

2.3
- Support for current versions of genutils (> 2.0)

2.4
- Changed the default value for g:tskelDateFormat from "%d-%b-%Y" to 
'%Y-%m-%d'
- 2 changes to TSkeletonGoToNextTag(): use select mode (as does 
imaps.vim, set g:tskelSelectTagMode to 'v' to get the old behaviour), 
move the cursor one char to the left before searching for the next tag 
(thanks to M Stubenschrott)
- added a few AutoIt3 skeletons
- FIX: handle tabs properly
- FIX: problem with filetypes containing non-word characters
- FIX: check the value of &selection
- Enable normal tags for late expansion

3.0
- Partial rewrite for vim7 (drop vim6 support)
- Now depends on tlib (vimscript #1863)
- "query" now uses a more sophisticated version from autoload/tlib.vim
- The default value for g:tskelQueryType is "query".
- Experimental (proof of concept) code completion for vim script 
(already sourced user-defined functions only). Use :delf 
TSkelFiletypeBits_functions_vim to disable this as it can take some 
time on initialization.
- Experimental (proof of concept) tags-based code completion for ruby.  
Use :delf TSkelProcessTag_ruby to disable this. It's only partially 
useful as it simply works on method names and knows nothing about 
classes, modules etc. But it gives you an argument list to fill in. It 
shouldn't be too difficult to adapt this for other filetypes for which 
such an approach could be more useful.
- The code makes it now possible to somehow plug in custom bit types by 
defining TSkelFiletypeBits_{NAME}(dict, filetype), or 
TSkelFiletypeBits_{NAME}_{FILETYPE}(dict, filetype), 
TSkelBufferBits_{NAME}(dict, filetype), 
TSkelBufferBits_{NAME}_{FILETYPE}(dict, filetype).
- FIX s:RetrieveAgent_read(): Delete last line, which should fix the 
problem with extraneous return characters in recursively included 
skeleton bits.
- FIX: bits containing backslashes
- FIX TSkeletonGoToNextTag(): Moving cursor when no tag was found.
- FIX: Minibits are now properly displayed in the menu.

3.1
- Tag-based code completion for vim
- Made the supported skeleton types configurable via g:tskelTypes
- FIX: Tag-based skeletons the name of which contain blanks
- FIX: Undid shortcut that prevented the <+bit:+> tag from working
- Preliminary support for using keys like <space> for insert mode 
expansion.

3.2
- "tags" & "functions" types are disabled by default due to a noticeable 
delay on initialization; add 'tags' and 'functions' to g:tskelTypes to 
re-enable them (with the new caching strategy, it's usable, but can 
produce much noise; but this depends of course on the way you handle 
tags)
- Improved caching strategy: cache filetype bits in 
skeletons/cache_bits; cache buffer-specific bits in 
skeletons/cache_bbits/&filetype/path (set g:tskelUseBufferCache to 0 to 
turn this off; this speeds up things quite a lot but creates many files 
on the long run, so you might want to purge the cache from time to time)
- embedded <tskel:> tags are now extracted on initialization and not 
when the skeleton is expanded (I'm not sure yet if it is better this 
way)
- CHANGE: dropped support for the ~/.vim/skeletons/prefab subdirectory; 
you'll have to move the templates, if any, to ~/.vim/skeletons
- FIX: :TSkeletonEdit, :TSkeletonSetup command-line completion
- FIX: Problem with fold markers in bits when &fdm was marker
- FIX: Problems with PrepareBits()
- FIX: Problems when the skeletons/menu/ subdirectory didn't exist
- TSkeletonExecInDestBuffer(code): speed-up
- Moved functions from EncodeURL.vim to tlib.vim
- Updated the manual
- Renamed the skeletons/menu subdirectory to skeletons/cache_menu

3.3
- New :TSkeletonEditBit command
- FIX: Embedded <tskel> tags in file templates didn't work

3.4
- Automatically reset bits information after editing a bit.
- Automatically define autocommands for templates with the form "NAME 
PATTERN" (where "#" in the pattern is replaced with "*"), i.e. the 
template file "text #%2ffoo%2f#.txt" will define a template for all new 
files matching "*/foo/*.txt"; the filetype will be set to "text"
- These "auto templates" must be located in 
~/.vim/skeletons/templates/GROUP/
- TSkeletonCB_FILENAME(), TSkeletonCB_DIRNAME()
- FIX: TSkeletonGoToNextTag() didn't work properly with ### type of 
markers.
- FIX: TSkeletonLateExpand(): tag at first column
- FIX: In templates, empty lines sometimes were not inserted in the 
document
- FIX: Build menu on SessionLoadPost event.
- FIX: Protect against autocommands that move the cursor on a BufEnter 
event
- FIX: Some special characters in the skeleton bit expansion were escaped 
twice with backslashes.
- Require tlib 0.9
- Make sure &foldmethod=manual in the scratch buffer

3.5
- FIX: Minor problem with auto-templates

4.0
- Renamed g:tskelPattern* variables to g:tskelMarker*
- If g:tskelMarkerHiGroup is non-empty, place holders will be 
highlighted in this group.
- Re-enable 'mini' in g:tskelTypes.
- Calling TSkeletonBit with no argument, brings up the menu.
- Require tlib 0.12
- CHANGE: The cache is now stored in ~/vimfiles/cache/ (use 
tlib#cache#Filename)
- INCOMPATIBLE CHANGE: Use autoload/tskeleton.vim
- FIX: Problem with cache name
- FIX: Problem in s:IsDefined()
- FIX: TSkeletonEditBit completion didn't work before expanding a bit.
- FIX: Command-line completion when tSkeleton wasn't invoked yet (and 
menu wasn't built).

4.1
- Automatically define iabbreviations by adding [bg]:tskelAbbrevPostfix 
(default: '#') to the bit name (i.e., a bit with the file "foo.bar" will 
by default create the menu entry "TSkel.foo.bar" for the bit "bar" and 
the abbreviation "bar#"). If this causes problems, set 
g:tskelAutoAbbrevs to 0.
- Bits can have a <tskel:abbrev> section that defines the abbreviation.
- New type 'abbreviations': This will make your abbreviations accessible 
as a template (in case you can't remember their names)
- New experimental <tskel:condition> section (a vim expression) that 
checks if a bit is eligible in the current context.
- New <+input()+> tag.
- New <+execute()+> tag.
- New <+let(VAR=VALUE)+> tag.
- <+include(NAME)+> as synonym for <+bit:NAME+>.
- Experimental <+if()+> ... <+elseif()+> ... <+else+> ... <+endif+>, 
<+for(var in list)+> ... <+endfor+> tags.
- Special tags <+nop+>, <+joinline+>, <+nl+> to prevent certain 
problems.
- These special tags have to be lower case.
- Made tskeleton#GoToNextTag() smarter in recognizing something like: 
<+/DEFAULT+>.
- Defined <Leader>## and <Leader>#<space> (see g:tskelMapLeader) as 
visual command (the user will be queried for the name of a skeleton)
- Some functions have moved and changed names. It should now be possible 
to plug-in custom template expanders (or re-use others).
- Use append() via tlib#buffer#InsertText() to insert bits. This could 
cause old problems to reappear although it seems to work fine.
- The markup should now be properly configurable (per buffer; you can 
set template-specific markers in the tskel:here_before section).
- Require tlib 0.14
- The default value for g:tskelUseBufferCache is 0 as many people might 
find the accumulation of cached information somewhat surprising. Unless 
you use tag/functions type of skeleton bit, it's unnecessary anyway.
- Removed the dependency on genutils.
- The g:tskelMarkerCursor variable was removed and replaced with 
g:tskelMarkerCursor_mark and g:tskelMarkerCursor_rx.

4.2
- Enable <+CURSOR/foo+>. After expansion "foo" will be selected.
- New (old) default values: removed 'abbreviations' from g:tskelTypes 
and set g:tskelAutoAbbrevs to 0 in order to minimize surprises.
- Enabled tex-Skeletons for the viki filetype
- FIX: Place the cursor at the end of an inserted bit that contains no 
cursor marker (which was the original behaviour).
- Split html bits into html and html_common; the java group includes 
html_common.
- CHANGE: Made bit names case-sensitive
- NEW: select() tag (similar to the query tag)

4.3
- bbcode group
- tskelKeyword_{&ft} and tskelGroup_{&ft} variables can be buffer-local
- Case-sensitivity can be configured via [bg]:tskelCaseSensitive and 
[bg]:tskelCaseSensitive_{&filetype}
- Make sure tlib is loaded even if it is installed in a different 
rtp-directory

4.4
- Make sure tlib is loaded even if it is installed in a different 
rtp-directory

4.5
- Call s:InitBufferMenu() earlier.
- C modifier: Consider _ whitespace
- g:tskelMarkerExtra (extra markers for tskeleton#GoToNextTag)

4.6
- Minibits: Allow single words as bit definition: "word" expands to 
"word<+CURSOR+>"
- Require tlib 0.29

4.7
- TSkeletonSetup: allow full filenames as argument
- Auto templates: don't cd into the templates directory
- tskeleton#ExpandBitUnderCursor(): Third argument is a dictionary.
- TSkeletonMapHyperComplete() (default: <c-space>): Map a magic key that 
expands skeletons or, if no matching templates were found, completions, 
tags, words etc.
- FIX: Problem with <+name/expandsion+> kind of tags when located at the 
beginning or end of a line
- s:GetBitDefs()
- Improved tskeleton#Complete() (for use as completefunc or omnifunc)
- FIX: Cursor positioning after expanding templates without a <+CURSOR+> 
tag
- Don't build the menu for tSkeleton scratch buffers

4.8
- Moved the definition of some variables from plugin/tSkeleton.vim to 
autoload/tskeleton.vim
- If g:tskelMapLeader is empty, don't define maps.
- Don't build a menu if g:tskelMenuPrefix == ''.
- If g:tskelDontSetup is defined and g:tskelMenuPrefix == '', 
autoload/tskeleton.vim won't be loaded on startup.
- Don't create g:tskelBitsDir if it doesn't exist

4.9
- "Mini bits": Load all .tskelmini files from the current file's 
directory upwards
- s:InsertDefault handles <+CURSOR+> tags
- tskeleton#HyperComplete_query(): Set w:tskeleton_hypercomplete
- FIX: g:tskelHyperType = "pum" didn't work properly.

syntax/tskeleton.vim	[[[1
50
" tskeleton.vim
" @Author:      Tom Link (micathom AT gmail com?subject=vim)
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     30-Dez-2003.
" @Last Change: 2009-02-15.
" @Revision: 0.491

if version < 600
    syntax clear
elseif exists("b:current_syntax")
    finish
endif

let s:tskel_filetype=expand("%:p:h:t")
if s:tskel_filetype != 'tskeleton'
    try
        exec "runtime! syntax/". s:tskel_filetype .".vim"
        unlet b:current_syntax
    catch
    endtry
endif

if s:tskel_filetype == 'vim'
    syn region tskeletonProcess matchgroup=tskeletonProcessMarker
                \ start='^\s*<\z(tskel:[a-z_:]\+\)>\s*$' end='^\s*</\z1>\s*$'
else
    syntax include @Vim syntax/vim.vim
    syn region tskeletonProcess matchgroup=tskeletonProcessMarker
                \ start='^\s*<\z(tskel:[a-z_:]\+\)>\s*$' end='^\s*</\z1>\s*$'
                \ contains=@Vim
endif

unlet s:tskel_filetype

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_tskeleton_syntax_inits")
    if version < 508
        let did_tskeleton_syntax_inits = 1
        command! -nargs=+ HiLink hi link <args>
    else
        command! -nargs=+ HiLink hi def link <args>
    endif
    
    HiLink tskeletonProcessMarker Statement
    
    delcommand HiLink
endif

