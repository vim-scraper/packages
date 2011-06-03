" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
autoload/Preamble.vim	[[[1
190
if &cp || exists("s:loaded_preamble")
    finish
endif
let s:loaded_preamble = 1


" ########################################################
" Public Methods 
" ########################################################


" ======================================================== 
" Enable: enables automatic preamble for the specified  
"   filetypes
"
" Parameters --
"   filetypes - -- string of comma delimited filetypes,
"               -- an empty string disables the plugin 
"               -- "*" enables plugin for all filetyps
" Example --  
"   In your .vimrc file, add a line like this: 
"       call Preamble#Enable("c,cpp,java")
" ======================================================== 

fun! Preamble#Enable(filetypes)
    silent! au! AugroupPreamble
    if a:filetypes == "" | return | endif 
  
    augroup AugroupPreamble
        exe "au BufWinEnter " . a:filetypes . " call Preamble#Fold()"
    augroup END
endfunction



" ======================================================== 
" Fold: folds the preamble according to the options that
"   that are set
" ======================================================== 

fun! Preamble#Fold()
    if exists('b:preamble_disable_thisbuffer') && b:preamble_disable_thisbuffer != 0 
        return 
    endif 

    call s:LoadOptions()
    
    let pl = s:Length()
    if pl < s:preamble_min_lines | return | endif 
    if pl > s:preamble_max_lines && !s:preamble_fold_partial | return | endif

    " if a fold aleady exists in line 1, just close it 
    if foldlevel(1) && foldclosed(1) == -1 
        exe ":1,".pl."foldclose" 
        return 
    endif 

    if &foldmethod=="syntax" | return | endif 
        
    exe "set foldmethod=manual | :1,".pl."fold | :1,".pl."foldclose"
endfunction



" ########################################################
" Private Methods 
" ########################################################

" ======================================================== 
" LoadOptions: Initializes user options
"
"   If defined, the buffer specific option is used. 
"   Else, if defined, the global option is used. 
"   Else, the default is used. 
" ======================================================== 

fun! s:LoadOptions()

    " ------------------------------------------------------------------ 
    " Option: preamble_min_lines
    "
    " Minimum lines that preamble should have before it can be folded
    " If the preamble is less than this number, no fold is created
    "
    " Default: 25
    " ------------------------------------------------------------------ 

    if !exists('g:preamble_min_lines')
        let s:preamble_min_lines=25
    else
        let s:preamble_min_lines=g:preamble_min_lines
    endif
    if exists('b:preamble_min_lines')
        let s:preamble_min_lines=b:preamble_min_lines
    endif

    " ------------------------------------------------------------------ 
    " Option: preamble_max_lines
    "
    " Maximum lines to include in the preamble fold 
    " If the preamble is greater than this number, behavior is 
    " determined by the 'preamble_fold_partial' option
    "
    " Default: 150
    " ------------------------------------------------------------------ 

    if !exists('g:preamble_max_lines')
        let s:preamble_max_lines=150
    else
        let s:preamble_max_lines=g:preamble_max_lines
    endif
    if exists('b:preamble_max_lines')
        let s:preamble_max_lines=b:preamble_max_lines
    endif 
    if line('$') < s:preamble_max_lines
        let s:preamble_max_lines = line('$')
    endif
    
    " ------------------------------------------------------------------ 
    " Option: preamble_fold_partial
    "
    " If the preamble is greater the 'max_lines'
    " fold or do not fold up to 'max_lines'
    "
    " Values:   0 = do not fold partial preambles  (default)
    "           1 = do fold max lines of the preamble
    " ------------------------------------------------------------------ 

    if !exists('g:preamble_fold_partial')
        let s:preamble_fold_partial=0
    else
        let s:preamble_fold_partial=g:preamble_fold_partial
    endif 
    if exists('b:preamble_fold_partial')
        let s:preamble_fold_partial=b:preamble_fold_partial
    endif
     
    " ------------------------------------------------------------------ 
    " Option: b:preamble_disable_thisbuffer
    "
    " Disable folding for buffer
    " This option is only for buffer level folding 
    "
    " Values:  exists and not 0 = do not create fold
    " ------------------------------------------------------------------ 
    " b:preamble_disable_thisbuffer
    
    
endfunction
 

" ======================================================== 
" Length: Calculates the preamble's length
"
"   The preamble 
"       -- starts on line 1 of the file
"       -- includes initial blank lines
"       -- includes lines having a character in column one
"          with a 'Comment' syntax 
"       -- stops when first non-Comment line is reached
"
"   Blank lines before the first comment are included.
"   Blank lines after the first comment line are not.
" ======================================================== 

fun! s:Length()

    let line_pos = 1
    let is_blankline = 1

    while(line_pos <= s:preamble_max_lines)

        " skip blank lines at top of file
        if is_blankline && getline(line_pos) =~ "\S"
            let is_blankline = 0
        endif 
        
        " assume each line of a preamble has a character in col one
        if !is_blankline
            let synId = synID(line_pos, 1, 1)
            let realSynId = synIDtrans(synId)
            if (synIDattr( realSynId, 'name' ) != "Comment") | break | endif
        endif

        let line_pos = line_pos + 1
    endwhile 
   
    return line_pos-1
 endfunction 

doc/Preamble.txt	[[[1
152
*preamble.txt*	Folding preamble comments		               *preamble* *Preamble*           

Author:         Mel Davis <ZZmeldavisZZ@ZZsignals.selfip.orgZZ> (remove 'Z's)
Last Change:    Jan 4, 2010    

Plugin Version: 0.1

Requires:       
    Vim:        7.2 and above (not tested on earlier versions)
    Plugins:    none

==============================================================================
CONTENTS~

    1. Purpose                  
    2. What it does
    3. Interface
    4. Options
    5. Installation
    6. Copyright
    7. ChangeLog

==============================================================================
PURPOSE~

This plugin will automatically fold long preambles, such as GPL copyright
notices, located at the head of source files.  You may find it helpful if,
like me, you use syntax folding infrequently. 

Very often, I read third-party library sources and examples, such as Nokia's
Qt library, various GNU utilities, and many others.  I want to see the 
functions and read the comments but not the 50 lines of licensing and 
copyright notices that head every file. Because these are third-party 
sources, I do not want to modify the files to include Vim fold markers.

==============================================================================
WHAT IT DOES~

This plugin does not use fold markers and does not modify files.
It adds the fold by changing the foldmethod to "manual" and inserting a 
manual fold. 

If the foldmethod is 'syntax', the plugin will do nothing except try to 
close the fold on line 1, if one exists.  No fold is created and the
foldmethod is not changed.

The preamble has to start at the top of the file, but may be preceded by 
empty lines.  It will consist of leading blank lines followed by lines 
with comments starting in column one. The preamble will stop at the first line
not having a comment in the first column.  

Note: If the preamble uses 'c' style comments without a border of characters
in column 1 the preamble will not be folded. 


==============================================================================
INTERFACE~

Preamble#Enable(filetypes)                          *Preamble#Enable*

    * Enables automatic preamble folding for specified file types.
    * If filetypes = "*", the plugin is enabled for all file types.
    * An empty string will disable the plugin.

Preamble#Fold()                                     *Preamble#Fold*

    * If a fold at line 1 already exists, it will close it and return.
    * When foldmethod is not 'syntax', it will change the foldmethod
      to 'manual' and create a manual fold.


==============================================================================
OPTIONS~

All options have default values, which the user can change both globally and
on a per buffer basis.  The buffer options may be helpful when used along    
with some other plugins such as "project" by Aric Blumer. 

Prefix the options below with either 'g:' or 'b:'. Buffer options take
precedence over global.

------------------------------------------------------------------------------
                                                 *preamble_min_lines*

preamble_min_lines:  Sets the minimum size of a preamble.

    Type:       Integer
    Default:    50

    If the preamble size is less, no fold will be created.

------------------------------------------------------------------------------
                                                 *preamble_max_lines*

preamble_max_lines:  Sets the maximum size of a preamble. 

    Type:       Integer
    Default:    150

    The plugin will not scan beyond this row in the source file.
    If the preamble is longer than max_lines, the behavior is 
    determined by the |preamble_fold_partial| option. 

------------------------------------------------------------------------------
                                                 *preamble_fold_partial*

preamble_fold_partial:  Permits the folding of partial preambles
                        when their length exeeds max lines.

    Type:       Integer
    Default:    0

    If this option is non-zero, the first 'max-lines' of a
    too-long preamble will be folded. Otherwise, no fold will be 
    created.
------------------------------------------------------------------------------
                                                 *preamble_disable_thisbuffer*

b:preamble_disable_thisbuffer:  Disables preamble folding 
                                for the buffer.

    Type:       Integer

   This option is only available for buffers.  Set to a 
   non-zero value to disable preamble folding for this buffer.
   To disable plugin globally, call |Preamble#Enable| with an 
   empty string.  This option may be helpful when used with 
   other plugins such as "project" by Aric Blumer.


==============================================================================
INSTALLATION~

Files installed:
    autoload/Preamble.vim
    doc/Preamble.txt

Examples of .vimrc additions: >
    call Preamble#Enable('cpp,java')    # to enable 
    let g:preamble_min_lines=35         # if you want other than default
<

==============================================================================
COPYRIGHT~

Public domain.   

==============================================================================
CHANGELOG~

01/04/2010  Initial release

