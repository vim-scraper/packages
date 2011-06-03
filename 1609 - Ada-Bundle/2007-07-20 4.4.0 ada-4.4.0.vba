" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
ada_options.vim	[[[1
105
"------------------------------------------------------------------------------
"  Description: Options setable by the Ada plugin
"	   $Id: ada_options.vim 745 2007-07-08 15:57:40Z krischik $
"    Copyright: Copyright (C) 2006 Martin Krischik
"   Maintainer:	Martin Krischik
"      $Author: krischik $
"	 $Date: 2007-07-08 17:57:40 +0200 (So, 08 Jul 2007) $
"      Version: 4.4
"    $Revision: 745 $
"     $HeadURL: http://gnuada.svn.sourceforge.net/svnroot/gnuada/trunk/tools/vim/ada_options.vim $
"      History:	24.05.2006 MK Unified Headers
"		16.07.2006 MK Ada-Mode as vim-ball
"		02.10.2006 MK Folding for "gnat pretty"
"		15.10.2006 MK Bram's suggestion for runtime integration
"		05.11.2006 MK Bram suggested to save on spaces
"	 Usage: copy content into your .vimrc and change options to your
"		likeing.
"    Help Page: ft-ada-options
"------------------------------------------------------------------------------

echoerr 'It is suggested to copy the content of ada_options into .vimrc!'
finish " 1}}}

" Section: Ada options {{{1

   let   g:ada_abbrev		       = 1
   let   g:ada_default_compiler	       = 'gnat'
   let   g:ada_extended_tagging	       = 'list'
   let   g:ada_folding		       = 'i'
"  let   g:ada_folding		       = 'gibxp'
"  let   g:ada_folding		       = 'spftc'
   let   g:ada_rainbow_color	       = 1
   let   g:ada_space_errors	       = 1
   let   g:ada_standard_types	       = 1
   let   g:ada_with_gnat_project_files = 1
"  let	 g:ada_extended_completion     = 1
"  let   g:ada_line_errors	       = 1
"  let   g:ada_omni_with_keywords      = 1

   let   g:Tlist_Exit_OnlyWindow       = 1
   let   g:Tlist_File_Fold_Auto_Close  = 1
   let   g:Tlist_Sort_Type	       = "name"

   let   g:NERD_use_ada_with_spaces    = 1

   let   g:backup_directory	       = '.backups'
   let   g:backup_purge		       = 10

   let   g:mapleader		       = "<F12>"

   if &diff
       let   g:Tlist_Auto_Open	       = 0
   else
       let   g:Tlist_Auto_Open	       = 1
   endif

   filetype plugin indent on
   syntax enable

" }}}1

" Section: Vimball options {{{1
:set expandtab fileformat=unix encoding=utf-8
:.+2,.+13 MkVimball ada-4.4.0

ada_options.vim
autoload/ada.vim
autoload/adacomplete.vim
autoload/decada.vim
autoload/gnat.vim
compiler/decada.vim
compiler/gnat.vim
doc/ft_ada.txt
ftdetect/ada.vim
ftplugin/ada.vim
indent/ada.vim
syntax/ada.vim

" }}}1

" Section: Tar options {{{1

tar --create --bzip2	      \
   --file="ada-4.3.0.tar.bz2" \
   autoload/ada.vim	      \
   autoload/adacomplete.vim   \
   autoload/decada.vim	      \
   autoload/gnat.vim	      \
   compiler/decada.vim	      \
   compiler/gnat.vim	      \
   doc/ft_ada.txt	      \
   ftdetect/ada.vim	      \
   ftplugin/ada.vim	      \
   indent/ada.vim	      \
   syntax/ada.vim	      ;

" }}}1

"------------------------------------------------------------------------------
"   Copyright (C) 2006	Martin Krischik
"
"   Vim is Charityware - see ":help license" or uganda.txt for licence details.
"------------------------------------------------------------------------------
" vim: nowrap tabstop=8 shiftwidth=3 softtabstop=3 noexpandtab
" vim: textwidth=0 foldmethod=marker foldmethod=marker
autoload/ada.vim	[[[1
622
"------------------------------------------------------------------------------
"  Description: Perform Ada specific completion & tagging.
"     Language: Ada (2005)
"	   $Id: ada.vim 748 2007-07-15 18:11:29Z krischik $
"   Maintainer: Martin Krischik <krischik@users.sourceforge.net>
"		Taylor Venable <taylor@metasyntax.net>
"		Neil Bird <neil@fnxweb.com>
"      $Author: krischik $
"	 $Date: 2007-07-15 20:11:29 +0200 (So, 15 Jul 2007) $
"      Version: 4.4
"    $Revision: 748 $
"     $HeadURL: http://gnuada.svn.sourceforge.net/svnroot/gnuada/trunk/tools/vim/autoload/ada.vim $
"      History: 24.05.2006 MK Unified Headers
"		26.05.2006 MK ' should not be in iskeyword.
"		16.07.2006 MK Ada-Mode as vim-ball
"		02.10.2006 MK Better folding.
"		15.10.2006 MK Bram's suggestion for runtime integration
"		05.11.2006 MK Bram suggested not to use include protection for
"			      autoload
"		05.11.2006 MK Bram suggested to save on spaces
"		08.07.2007 TV fix mapleader problems.
"	        09.05.2007 MK Session just won't work no matter how much
"			      tweaking is done
"    Help Page: ft-ada-functions
"------------------------------------------------------------------------------

if version < 700
   finish
endif 

" Section: Constants {{{1
"
let g:ada#DotWordRegex	   = '\a\w*\(\_s*\.\_s*\a\w*\)*'
let g:ada#WordRegex	   = '\a\w*'
let g:ada#Comment	   = "\\v^(\"[^\"]*\"|'.'|[^\"']){-}\\zs\\s*--.*"
let g:ada#Keywords	   = []

" Section: g:ada#Keywords {{{1
"
" Section: add Ada keywords {{{2
"
for Item in ['abort', 'else', 'new', 'return', 'abs', 'elsif', 'not', 'reverse', 'abstract', 'end', 'null', 'accept', 'entry', 'select', 'access', 'exception', 'of', 'separate', 'aliased', 'exit', 'or', 'subtype', 'all', 'others', 'synchronized', 'and', 'for', 'out', 'array', 'function', 'overriding', 'tagged', 'at', 'task', 'generic', 'package', 'terminate', 'begin', 'goto', 'pragma', 'then', 'body', 'private', 'type', 'if', 'procedure', 'case', 'in', 'protected', 'until', 'constant', 'interface', 'use', 'is', 'raise', 'declare', 'range', 'when', 'delay', 'limited', 'record', 'while', 'delta', 'loop', 'rem', 'with', 'digits', 'renames', 'do', 'mod', 'requeue', 'xor']
    let g:ada#Keywords += [{
	    \ 'word':  Item,
	    \ 'menu':  'keyword',
	    \ 'info':  'Ada keyword.',
	    \ 'kind':  'k',
	    \ 'icase': 1}]
endfor

" Section: GNAT Project Files {{{3
"
if exists ('g:ada_with_gnat_project_files')
    for Item in ['project']
       let g:ada#Keywords += [{
	       \ 'word':  Item,
	       \ 'menu':  'keyword',
	       \ 'info':  'GNAT projectfile keyword.',
	       \ 'kind':  'k',
	       \ 'icase': 1}]
    endfor
endif

" Section: add	standart exception {{{2
"
for Item in ['Constraint_Error', 'Program_Error', 'Storage_Error', 'Tasking_Error', 'Status_Error', 'Mode_Error', 'Name_Error', 'Use_Error', 'Device_Error', 'End_Error', 'Data_Error', 'Layout_Error', 'Length_Error', 'Pattern_Error', 'Index_Error', 'Translation_Error', 'Time_Error', 'Argument_Error', 'Tag_Error', 'Picture_Error', 'Terminator_Error', 'Conversion_Error', 'Pointer_Error', 'Dereference_Error', 'Update_Error']
    let g:ada#Keywords += [{
	    \ 'word':  Item,
	    \ 'menu':  'exception',
	    \ 'info':  'Ada standart exception.',
	    \ 'kind':  'x',
	    \ 'icase': 1}]
endfor

" Section: add	GNAT exception {{{3
"
if exists ('g:ada_gnat_extensions')
    for Item in ['Assert_Failure']
	let g:ada#Keywords += [{
		\ 'word':  Item,
		\ 'menu':  'exception',
		\ 'info':  'GNAT exception.',
		\ 'kind':  'x',
		\ 'icase': 1}]
    endfor
endif

" Section: add Ada buildin types {{{2
"
for Item in ['Boolean', 'Integer', 'Natural', 'Positive', 'Float', 'Character', 'Wide_Character', 'Wide_Wide_Character', 'String', 'Wide_String', 'Wide_Wide_String', 'Duration']
    let g:ada#Keywords += [{
	    \ 'word':  Item,
	    \ 'menu':  'type',
	    \ 'info':  'Ada buildin type.',
	    \ 'kind':  't',
	    \ 'icase': 1}]
endfor

" Section: add GNAT buildin types {{{3
"
if exists ('g:ada_gnat_extensions')
    for Item in ['Short_Integer', 'Short_Short_Integer', 'Long_Integer', 'Long_Long_Integer', 'Short_Float', 'Short_Short_Float', 'Long_Float', 'Long_Long_Float']
	let g:ada#Keywords += [{
		\ 'word':  Item,
		\ 'menu':  'type',
		\ 'info':  'GNAT buildin type.',
		\ 'kind':  't',
		\ 'icase': 1}]
    endfor
endif

" Section: add Ada Attributes {{{2
"
for Item in ['''Access', '''Address', '''Adjacent', '''Aft', '''Alignment', '''Base', '''Bit_Order', '''Body_Version', '''Callable', '''Caller', '''Ceiling', '''Class', '''Component_Size', '''Compose', '''Constrained', '''Copy_Sign', '''Count', '''Definite', '''Delta', '''Denorm', '''Digits', '''Emax', '''Exponent', '''External_Tag', '''Epsilon', '''First', '''First_Bit', '''Floor', '''Fore', '''Fraction', '''Identity', '''Image', '''Input', '''Large', '''Last', '''Last_Bit', '''Leading_Part', '''Length', '''Machine', '''Machine_Emax', '''Machine_Emin', '''Machine_Mantissa', '''Machine_Overflows', '''Machine_Radix', '''Machine_Rounding', '''Machine_Rounds', '''Mantissa', '''Max', '''Max_Size_In_Storage_Elements', '''Min', '''Mod', '''Model', '''Model_Emin', '''Model_Epsilon', '''Model_Mantissa', '''Model_Small', '''Modulus', '''Output', '''Partition_ID', '''Pos', '''Position', '''Pred', '''Priority', '''Range', '''Read', '''Remainder', '''Round', '''Rounding', '''Safe_Emax', '''Safe_First', '''Safe_Large', '''Safe_Last', '''Safe_Small', '''Scale', '''Scaling', '''Signed_Zeros', '''Size', '''Small', '''Storage_Pool', '''Storage_Size', '''Stream_Size', '''Succ', '''Tag', '''Terminated', '''Truncation', '''Unbiased_Rounding', '''Unchecked_Access', '''Val', '''Valid', '''Value', '''Version', '''Wide_Image', '''Wide_Value', '''Wide_Wide_Image', '''Wide_Wide_Value', '''Wide_Wide_Width', '''Wide_Width', '''Width', '''Write']
    let g:ada#Keywords += [{
	    \ 'word':  Item,
	    \ 'menu':  'attribute',
	    \ 'info':  'Ada attribute.',
	    \ 'kind':  'a',
	    \ 'icase': 1}]
endfor

" Section: add GNAT Attributes {{{3
"
if exists ('g:ada_gnat_extensions')
    for Item in ['''Abort_Signal', '''Address_Size', '''Asm_Input', '''Asm_Output', '''AST_Entry', '''Bit', '''Bit_Position', '''Code_Address', '''Default_Bit_Order', '''Elaborated', '''Elab_Body', '''Elab_Spec', '''Emax', '''Enum_Rep', '''Epsilon', '''Fixed_Value', '''Has_Access_Values', '''Has_Discriminants', '''Img', '''Integer_Value', '''Machine_Size', '''Max_Interrupt_Priority', '''Max_Priority', '''Maximum_Alignment', '''Mechanism_Code', '''Null_Parameter', '''Object_Size', '''Passed_By_Reference', '''Range_Length', '''Storage_Unit', '''Target_Name', '''Tick', '''To_Address', '''Type_Class', '''UET_Address', '''Unconstrained_Array', '''Universal_Literal_String', '''Unrestricted_Access', '''VADS_Size', '''Value_Size', '''Wchar_T_Size', '''Word_Size']
    let g:ada#Keywords += [{
	    \ 'word':  Item,
	    \ 'menu':  'attribute',
	    \ 'info':  'GNAT attribute.',
	    \ 'kind':  'a',
	    \ 'icase': 1}]
    endfor
endif

" Section: add Ada Pragmas {{{2
"
for Item in ['All_Calls_Remote', 'Assert', 'Assertion_Policy', 'Asynchronous', 'Atomic', 'Atomic_Components', 'Attach_Handler', 'Controlled', 'Convention', 'Detect_Blocking', 'Discard_Names', 'Elaborate', 'Elaborate_All', 'Elaborate_Body', 'Export', 'Import', 'Inline', 'Inspection_Point', 'Interface (Obsolescent)', 'Interrupt_Handler', 'Interrupt_Priority', 'Linker_Options', 'List', 'Locking_Policy', 'Memory_Size (Obsolescent)', 'No_Return', 'Normalize_Scalars', 'Optimize', 'Pack', 'Page', 'Partition_Elaboration_Policy', 'Preelaborable_Initialization', 'Preelaborate', 'Priority', 'Priority_Specific_Dispatching', 'Profile', 'Pure', 'Queueing_Policy', 'Relative_Deadline', 'Remote_Call_Interface', 'Remote_Types', 'Restrictions', 'Reviewable', 'Shared (Obsolescent)', 'Shared_Passive', 'Storage_Size', 'Storage_Unit (Obsolescent)', 'Suppress', 'System_Name (Obsolescent)', 'Task_Dispatching_Policy', 'Unchecked_Union', 'Unsuppress', 'Volatile', 'Volatile_Components']
    let g:ada#Keywords += [{
	    \ 'word':  Item,
	    \ 'menu':  'pragma',
	    \ 'info':  'Ada pragma.',
	    \ 'kind':  'p',
	    \ 'icase': 1}]
endfor

" Section: add GNAT Pragmas {{{3
"
if exists ('g:ada_gnat_extensions')
    for Item in ['Abort_Defer', 'Ada_83', 'Ada_95', 'Ada_05', 'Annotate', 'Ast_Entry', 'C_Pass_By_Copy', 'Comment', 'Common_Object', 'Compile_Time_Warning', 'Complex_Representation', 'Component_Alignment', 'Convention_Identifier', 'CPP_Class', 'CPP_Constructor', 'CPP_Virtual', 'CPP_Vtable', 'Debug', 'Elaboration_Checks', 'Eliminate', 'Export_Exception', 'Export_Function', 'Export_Object', 'Export_Procedure', 'Export_Value', 'Export_Valued_Procedure', 'Extend_System', 'External', 'External_Name_Casing', 'Finalize_Storage_Only', 'Float_Representation', 'Ident', 'Import_Exception', 'Import_Function', 'Import_Object', 'Import_Procedure', 'Import_Valued_Procedure', 'Initialize_Scalars', 'Inline_Always', 'Inline_Generic', 'Interface_Name', 'Interrupt_State', 'Keep_Names', 'License', 'Link_With', 'Linker_Alias', 'Linker_Section', 'Long_Float', 'Machine_Attribute', 'Main_Storage', 'Obsolescent', 'Passive', 'Polling', 'Profile_Warnings', 'Propagate_Exceptions', 'Psect_Object', 'Pure_Function', 'Restriction_Warnings', 'Source_File_Name', 'Source_File_Name_Project', 'Source_Reference', 'Stream_Convert', 'Style_Checks', 'Subtitle', 'Suppress_All', 'Suppress_Exception_Locations', 'Suppress_Initialization', 'Task_Info', 'Task_Name', 'Task_Storage', 'Thread_Body', 'Time_Slice', 'Title', 'Unimplemented_Unit', 'Universal_Data', 'Unreferenced', 'Unreserve_All_Interrupts', 'Use_VADS_Size', 'Validity_Checks', 'Warnings', 'Weak_External']
	let g:ada#Keywords += [{
		\ 'word':  Item,
		\ 'menu':  'pragma',
		\ 'info':  'GNAT pragma.',
		\ 'kind':  'p',
		\ 'icase': 1}]
    endfor
endif
" 1}}}

" Section: g:ada#Ctags_Kinds {{{1
"
let g:ada#Ctags_Kinds = {
   \ 'P': ["packspec",	  "package specifications"],
   \ 'p': ["package",	  "packages"],
   \ 'T': ["typespec",	  "type specifications"],
   \ 't': ["type",	  "types"],
   \ 'U': ["subspec",	  "subtype specifications"],
   \ 'u': ["subtype",	  "subtypes"],
   \ 'c': ["component",   "record type components"],
   \ 'l': ["literal",	  "enum type literals"],
   \ 'V': ["varspec",	  "variable specifications"],
   \ 'v': ["variable",	  "variables"],
   \ 'f': ["formal",	  "generic formal parameters"],
   \ 'n': ["constant",	  "constants"],
   \ 'x': ["exception",   "user defined exceptions"],
   \ 'R': ["subprogspec", "subprogram specifications"],
   \ 'r': ["subprogram",  "subprograms"],
   \ 'K': ["taskspec",	  "task specifications"],
   \ 'k': ["task",	  "tasks"],
   \ 'O': ["protectspec", "protected data specifications"],
   \ 'o': ["protected",   "protected data"],
   \ 'E': ["entryspec",   "task/protected data entry specifications"],
   \ 'e': ["entry",	  "task/protected data entries"],
   \ 'b': ["label",	  "labels"],
   \ 'i': ["identifier",  "loop/declare identifiers"],
   \ 'a': ["autovar",	  "automatic variables"],
   \ 'y': ["annon",	  "loops and blocks with no identifier"]}

" Section: ada#Word (...) {{{1
"
" Extract current Ada word across multiple lines
" AdaWord ([line, column])\
"
function ada#Word (...)
   if a:0 > 1
      let l:Line_Nr    = a:1
      let l:Column_Nr  = a:2 - 1
   else
      let l:Line_Nr    = line('.')
      let l:Column_Nr  = col('.') - 1
   endif

   let l:Line = substitute (getline (l:Line_Nr), g:ada#Comment, '', '' )

   " Cope with tag searching for items in comments; if we are, don't loop
   " backards looking for previous lines
   if l:Column_Nr > strlen(l:Line)
      " We were in a comment
      let l:Line = getline(l:Line_Nr)
      let l:Search_Prev_Lines = 0
   else
      let l:Search_Prev_Lines = 1
   endif

   " Go backwards until we find a match (Ada ID) that *doesn't* include our
   " location - i.e., the previous ID. This is because the current 'correct'
   " match will toggle matching/not matching as we traverse characters
   " backwards. Thus, we have to find the previous unrelated match, exclude
   " it, then use the next full match (ours).
   " Remember to convert vim column 'l:Column_Nr' [1..n] to string offset [0..(n-1)]
   " ... but start, here, one after the required char.
   let l:New_Column = l:Column_Nr + 1
   while 1
      let l:New_Column = l:New_Column - 1
      if l:New_Column < 0
	 " Have to include previous l:Line from file
	 let l:Line_Nr = l:Line_Nr - 1
	 if l:Line_Nr < 1  ||  !l:Search_Prev_Lines
	    " Start of file or matching in a comment
	    let l:Line_Nr     = 1
	    let l:New_Column  = 0
	    let l:Our_Match   = match (l:Line, g:ada#WordRegex )
	    break
	 endif
	 " Get previous l:Line, and prepend it to our search string
	 let l:New_Line    = substitute (getline (l:Line_Nr), g:ada#Comment, '', '' )
	 let l:New_Column  = strlen (l:New_Line) - 1
	 let l:Column_Nr   = l:Column_Nr + l:New_Column
	 let l:Line	   = l:New_Line . l:Line
      endif
      " Check to see if this is a match excluding 'us'
      let l:Match_End = l:New_Column +
		      \ matchend (strpart (l:Line,l:New_Column), g:ada#WordRegex ) - 1
      if l:Match_End >= l:New_Column  &&
       \ l:Match_End < l:Column_Nr
	 " Yes
	 let l:Our_Match = l:Match_End+1 +
			 \ match (strpart (l:Line,l:Match_End+1), g:ada#WordRegex )
	 break
      endif
   endwhile

   " Got anything?
   if l:Our_Match < 0
      return ''
   else
      let l:Line = strpart (l:Line, l:Our_Match)
   endif

   " Now simply add further lines until the match gets no bigger
   let l:Match_String = matchstr (l:Line, g:ada#WordRegex)
   let l:Last_Line    = line ('$')
   let l:Line_Nr      = line ('.') + 1
   while l:Line_Nr <= l:Last_Line
      let l:Last_Match = l:Match_String
      let l:Line = l:Line .
	 \ substitute (getline (l:Line_Nr), g:ada#Comment, '', '')
      let l:Match_String = matchstr (l:Line, g:ada#WordRegex)
      if l:Match_String == l:Last_Match
	 break
      endif
   endwhile

   " Strip whitespace & return
   return substitute (l:Match_String, '\s\+', '', 'g')
endfunction ada#Word

" Section: ada#List_Tag (...) {{{1
"
"  List tags in quickfix window
"
function ada#List_Tag (...)
   if a:0 > 1
      let l:Tag_Word = ada#Word (a:1, a:2)
   elseif a:0 > 0
      let l:Tag_Word = a:1
   else
      let l:Tag_Word = ada#Word ()
   endif

   echo "Searching for" l:Tag_Word

   let l:Pattern = '^' . l:Tag_Word . '$'
   let l:Tag_List = taglist (l:Pattern)
   let l:Error_List = []
   "
   " add symbols
   "
   for Tag_Item in l:Tag_List
      if l:Tag_Item['kind'] == ''
	 let l:Tag_Item['kind'] = 's'
      endif

      let l:Error_List += [
	 \ l:Tag_Item['filename'] . '|' .
	 \ l:Tag_Item['cmd']	  . '|' .
	 \ l:Tag_Item['kind']	  . "\t" .
	 \ l:Tag_Item['name'] ]
   endfor
   set errorformat=%f\|%l\|%m
   cexpr l:Error_List
   cwindow
endfunction ada#List_Tag

" Section: ada#Jump_Tag (Word, Mode) {{{1
"
" Word tag - include '.' and if Ada make uppercase
"
function ada#Jump_Tag (Word, Mode)
   if a:Word == ''
      " Get current word
      let l:Word = ada#Word()
      if l:Word == ''
	 throw "NOT_FOUND: no identifier found."
      endif
   else
      let l:Word = a:Word
   endif

   echo "Searching for " . l:Word

   try
      execute a:Mode l:Word
   catch /.*:E426:.*/
      let ignorecase = &ignorecase
      set ignorecase
      execute a:Mode l:Word
      let &ignorecase = ignorecase
   endtry

   return
endfunction ada#Jump_Tag

" Section: ada#Insert_Backspace () {{{1
"
" Backspace at end of line after auto-inserted commentstring '-- ' wipes it
"
function ada#Insert_Backspace ()
   let l:Line = getline ('.')
   if col ('.') > strlen (l:Line) &&
    \ match (l:Line, '-- $') != -1 &&
    \ match (&comments,'--') != -1
      return "\<bs>\<bs>\<bs>"
   else
      return "\<bs>"
   endif

   return
endfunction ada#InsertBackspace

" Section: Insert Completions {{{1
"
" Section: ada#User_Complete(findstart, base) {{{2
"
" This function is used for the 'complete' option.
"
function! ada#User_Complete(findstart, base)
   if a:findstart == 1
      "
      " locate the start of the word
      "
      let line = getline ('.')
      let start = col ('.') - 1
      while start > 0 && line[start - 1] =~ '\i\|'''
	 let start -= 1
      endwhile
      return start
   else
      "
      " look up matches
      "
      let l:Pattern = '^' . a:base . '.*$'
      "
      " add keywords
      "
      for Tag_Item in g:ada#Keywords
	 if l:Tag_Item['word'] =~? l:Pattern
	    if complete_add (l:Tag_Item) == 0
	       return []
	    endif
	    if complete_check ()
	       return []
	    endif
	 endif
      endfor
      return []
   endif
endfunction ada#User_Complete

" Section: ada#Completion (cmd) {{{2
"
" Word completion (^N/^R/^X^]) - force '.' inclusion
function ada#Completion (cmd)
   set iskeyword+=46
   return a:cmd . "\<C-R>=ada#Completion_End ()\<CR>"
endfunction ada#Completion

" Section: ada#Completion_End () {{{2
"
function ada#Completion_End ()
   set iskeyword-=46
   return ''
endfunction ada#Completion_End

" Section: ada#Create_Tags {{{1
"
function ada#Create_Tags (option)
   if a:option == 'file'
      let l:Filename = fnamemodify (bufname ('%'), ':p')
   elseif a:option == 'dir'
      let l:Filename =
	 \ fnamemodify (bufname ('%'), ':p:h') . "*.ada " .
	 \ fnamemodify (bufname ('%'), ':p:h') . "*.adb " .
	 \ fnamemodify (bufname ('%'), ':p:h') . "*.ads"
   else
      let l:Filename = a:option
   endif
   execute '!ctags --excmd=number ' . l:Filename
endfunction ada#Create_Tags

" Section: ada#Switch_Session {{{1
"
function ada#Switch_Session (New_Session)
   " 
   " you should not save to much date into the seession since they will
   " be sourced
   "
   set sessionoptions=buffers,curdir,folds,globals,resize,slash,tabpages,tabpages,unix,winpos,winsize

   if a:New_Session != v:this_session
      "
      "  We actualy got a new session - otherwise there
      "  is nothing to do.
      "
      if strlen (v:this_session) > 0
	 execute 'mksession! ' . v:this_session
      endif

      let v:this_session = a:New_Session

      "if filereadable (v:this_session)
	 "execute 'source ' . v:this_session
      "endif

      augroup ada_session
	 autocmd!
	 autocmd VimLeavePre * execute 'mksession! ' . v:this_session
      augroup END
      "
      "if exists ("g:Tlist_Auto_Open") && g:Tlist_Auto_Open
	 "TlistOpen
      "endif

   endif

   return
endfunction ada#Switch_Session	

" Section: GNAT Pretty Printer folding {{{1
"
if exists('g:ada_folding') && g:ada_folding[0] == 'g'
   "
   " Lines consisting only of ')' ';' are due to a gnat pretty bug and
   " have the same level as the line above (can't happen in the first
   " line).
   "
   let s:Fold_Collate = '^\([;)]*$\|'

   "
   " some lone statements are folded with the line above
   "
   if stridx (g:ada_folding, 'i') >= 0
      let s:Fold_Collate .= '\s\+\<is\>$\|'
   endif
   if stridx (g:ada_folding, 'b') >= 0
      let s:Fold_Collate .= '\s\+\<begin\>$\|'
   endif
   if stridx (g:ada_folding, 'p') >= 0
      let s:Fold_Collate .= '\s\+\<private\>$\|'
   endif
   if stridx (g:ada_folding, 'x') >= 0
      let s:Fold_Collate .= '\s\+\<exception\>$\|'
   endif

   " We also handle empty lines and
   " comments here.
   let s:Fold_Collate .= '--\)'

   function ada#Pretty_Print_Folding (Line)			     " {{{2
      let l:Text = getline (a:Line)

      if l:Text =~ s:Fold_Collate
	 "
	 "  fold with line above
	 "
	 let l:Level = "="
      elseif l:Text =~ '^\s\+('
	 "
	 " gnat outdents a line which stards with a ( by one characters so
	 " that parameters which follow are aligned.
	 "
	 let l:Level = (indent (a:Line) + 1) / &shiftwidth
      else
	 let l:Level = indent (a:Line) / &shiftwidth
      endif

      return l:Level
   endfunction ada#Pretty_Print_Folding				     " }}}2
endif

" Section: Options and Menus {{{1
"
" Section: ada#Switch_Syntax_Options {{{2
"
function ada#Switch_Syntax_Option (option)
   syntax off
   if exists ('g:ada_' . a:option)
      unlet g:ada_{a:option}
      echo  a:option . 'now off'
   else
      let g:ada_{a:option}=1
      echo  a:option . 'now on'
   endif
   syntax on
endfunction ada#Switch_Syntax_Option

" Section: ada#Map_Menu {{{2
"
function ada#Map_Menu (Text, Keys, Command)
   if a:Keys[0] == ':'
      execute
	\ "50amenu " .
	\ "Ada."     . escape(a:Text, ' ') .
	\ "<Tab>"    . a:Keys .
	\ " :"	     . a:Command . "<CR>"
      execute
	\ "command -buffer " .
	\ a:Keys[1:] .
	\" :" . a:Command . "<CR>"
   elseif a:Keys[0] == '<'
      execute
	\ "50amenu " .
	\ "Ada."     . escape(a:Text, ' ') .
	\ "<Tab>"    . a:Keys .
	\ " :"	     . a:Command . "<CR>"
      execute
	\ "nnoremap <buffer> "	 .
	\ a:Keys		 .
	\" :" . a:Command . "<CR>"
      execute
	\ "inoremap <buffer> "	 .
	\ a:Keys		 .
	\" <C-O>:" . a:Command . "<CR>"
   else
      if exists("g:mapleader")
         let l:leader = g:mapleader
      else
         let l:leader = '\'
      endif
      execute
	\ "50amenu " .
	\ "Ada."  . escape(a:Text, ' ') .
	\ "<Tab>" . escape(g:mapleader . "a" . a:Keys , '\') .
	\ " :"	  . a:Command . "<CR>"
      execute
	\ "nnoremap <buffer>" .
	\ escape(g:mapleader . "a" . a:Keys , '\') .
	\" :" . a:Command
      execute
	\ "inoremap <buffer>" .
	\ escape(g:mapleader . "a" . a:Keys , '\') .
	\" <C-O>:" . a:Command
   endif
   return
endfunction

" Section: ada#Map_Popup {{{2
"
function ada#Map_Popup (Text, Keys, Command)
   if exists("g:mapleader")
      let l:leader = g:mapleader
   else
      let l:leader = '\'
   endif
   execute
     \ "50amenu " .
     \ "PopUp."   . escape(a:Text, ' ') .
     \ "<Tab>"	  . escape(l:leader . "a" . a:Keys , '\') .
     \ " :"	  . a:Command . "<CR>"

   call ada#Map_Menu (a:Text, a:Keys, a:Command)
   return
endfunction ada#Map_Popup

" }}}1

lockvar  g:ada#WordRegex
lockvar  g:ada#DotWordRegex
lockvar  g:ada#Comment
lockvar! g:ada#Keywords
lockvar! g:ada#Ctags_Kinds

finish " 1}}}

"------------------------------------------------------------------------------
"   Copyright (C) 2006	Martin Krischik
"
"   Vim is Charityware - see ":help license" or uganda.txt for licence details.
"------------------------------------------------------------------------------
" vim: textwidth=78 wrap tabstop=8 shiftwidth=3 softtabstop=3 noexpandtab
" vim: foldmethod=marker
autoload/adacomplete.vim	[[[1
109
"------------------------------------------------------------------------------
"  Description: Vim Ada omnicompletion file
"     Language:	Ada (2005)
"	   $Id: adacomplete.vim 745 2007-07-08 15:57:40Z krischik $
"   Maintainer:	Martin Krischik
"      $Author: krischik $
"	 $Date: 2007-07-08 17:57:40 +0200 (So, 08 Jul 2007) $
"      Version: 4.4
"    $Revision: 745 $
"     $HeadURL: http://gnuada.svn.sourceforge.net/svnroot/gnuada/trunk/tools/vim/autoload/adacomplete.vim $
"      History: 24.05.2006 MK Unified Headers
"		26.05.2006 MK improved search for begin of word.
"		16.07.2006 MK Ada-Mode as vim-ball
"		15.10.2006 MK Bram's suggestion for runtime integration
"		05.11.2006 MK Bram suggested not to use include protection for
"			      autoload
"		05.11.2006 MK Bram suggested agaist using setlocal omnifunc 
"		05.11.2006 MK Bram suggested to save on spaces
"    Help Page: ft-ada-omni
"------------------------------------------------------------------------------

if version < 700
   finish
endif

" Section: adacomplete#Complete () {{{1
"
" This function is used for the 'omnifunc' option.
"
function! adacomplete#Complete (findstart, base)
   if a:findstart == 1
      return ada#User_Complete (a:findstart, a:base)
   else
      "
      " look up matches
      "
      if exists ("g:ada_omni_with_keywords")
	 call ada#User_Complete (a:findstart, a:base)
      endif
      "
      "  search tag file for matches
      "
      let l:Pattern  = '^' . a:base . '.*$'
      let l:Tag_List = taglist (l:Pattern)
      "
      " add symbols
      "
      for Tag_Item in l:Tag_List
	 if l:Tag_Item['kind'] == ''
	    "
	    " Tag created by gnat xref
	    "
	    let l:Match_Item = {
	       \ 'word':  l:Tag_Item['name'],
	       \ 'menu':  l:Tag_Item['filename'],
	       \ 'info':  "Symbol from file " . l:Tag_Item['filename'] . " line " . l:Tag_Item['cmd'],
	       \ 'kind':  's',
	       \ 'icase': 1}
	 else
	    "
	    " Tag created by ctags
	    "
	    let l:Info	= 'Symbol		 : ' . l:Tag_Item['name']  . "\n"
	    let l:Info .= 'Of type		 : ' . g:ada#Ctags_Kinds[l:Tag_Item['kind']][1]  . "\n"
	    let l:Info .= 'Defined in File	 : ' . l:Tag_Item['filename'] . "\n"

	    if has_key( l:Tag_Item, 'package')
	       let l:Info .= 'Package		    : ' . l:Tag_Item['package'] . "\n"
	       let l:Menu  = l:Tag_Item['package']
	    elseif has_key( l:Tag_Item, 'separate')
	       let l:Info .= 'Separate from Package : ' . l:Tag_Item['separate'] . "\n"
	       let l:Menu  = l:Tag_Item['separate']
	    elseif has_key( l:Tag_Item, 'packspec')
	       let l:Info .= 'Package Specification : ' . l:Tag_Item['packspec'] . "\n"
	       let l:Menu  = l:Tag_Item['packspec']
	    elseif has_key( l:Tag_Item, 'type')
	       let l:Info .= 'Datetype		    : ' . l:Tag_Item['type'] . "\n"
	       let l:Menu  = l:Tag_Item['type']
	    else
	       let l:Menu  = l:Tag_Item['filename']
	    endif

	    let l:Match_Item = {
	       \ 'word':  l:Tag_Item['name'],
	       \ 'menu':  l:Menu,
	       \ 'info':  l:Info,
	       \ 'kind':  l:Tag_Item['kind'],
	       \ 'icase': 1}
	 endif
	 if complete_add (l:Match_Item) == 0
	    return []
	 endif
	 if complete_check ()
	    return []
	 endif
      endfor
      return []
   endif
endfunction adacomplete#Complete

finish " 1}}}

"------------------------------------------------------------------------------
"   Copyright (C) 2006	Martin Krischik
"
"   Vim is Charityware - see ":help license" or uganda.txt for licence details.
"------------------------------------------------------------------------------
" vim: textwidth=78 wrap tabstop=8 shiftwidth=3 softtabstop=3 noexpandtab
" vim: foldmethod=marker
autoload/decada.vim	[[[1
75
"------------------------------------------------------------------------------
"  Description: Vim Ada/Dec Ada compiler file
"     Language: Ada (Dec Ada)
"          $Id: decada.vim 745 2007-07-08 15:57:40Z krischik $
"    Copyright: Copyright (C) 2006 Martin Krischik
"   Maintainer:	Martin Krischik
"      $Author: krischik $
"        $Date: 2007-07-08 17:57:40 +0200 (So, 08 Jul 2007) $
"      Version: 4.4
"    $Revision: 745 $
"     $HeadURL: http://gnuada.svn.sourceforge.net/svnroot/gnuada/trunk/tools/vim/autoload/decada.vim $
"      History: 21.07.2006 MK New Dec Ada
"               15.10.2006 MK Bram's suggestion for runtime integration
"               05.11.2006 MK Bram suggested not to use include protection for
"                             autoload
"		05.11.2006 MK Bram suggested to save on spaces
"    Help Page: compiler-decada
"------------------------------------------------------------------------------

if version < 700
   finish
endif

function decada#Unit_Name () dict				     " {{{1
    "	Convert filename into acs unit:
    "	    1:  remove the file extenstion.
    "	    2:  replace all double '_' or '-' with an dot (which denotes a separate)
    "	    3:  remove a trailing '_' (wich denotes a specification)
    return substitute (substitute (expand ("%:t:r"), '__\|-', ".", "g"), '_$', "", '')
endfunction decada#Unit_Name					     " }}}1

function decada#Make () dict					     " {{{1
    let l:make_prg   = substitute (g:self.Make_Command, '%<', self.Unit_Name(), '')
    let &errorformat = g:self.Error_Format
    let &makeprg     = l:make_prg
    wall
    make
    copen
    set wrap
    wincmd W
endfunction decada#Build					     " }}}1

function decada#Set_Session (...) dict				     " {{{1
   if a:0 > 0
      call ada#Switch_Session (a:1)
   elseif argc() == 0 && strlen (v:servername) > 0
      call ada#Switch_Session (
	 \ expand('~')[0:-2] . ".vimfiles.session]decada_" .
	 \ v:servername . ".vim")
   endif
   return
endfunction decada#Set_Session					     " }}}1

function decada#New ()						     " }}}1
   let Retval = {
      \ 'Make'		: function ('decada#Make'),
      \ 'Unit_Name'	: function ('decada#Unit_Name'),
      \ 'Set_Session'   : function ('decada#Set_Session'),
      \ 'Project_Dir'   : '',
      \ 'Make_Command'  : 'ACS COMPILE /Wait /Log /NoPreLoad /Optimize=Development /Debug %<',
      \ 'Error_Format'  : '%+A%%ADAC-%t-%m,%C  %#%m,%Zat line number %l in file %f,' .
			\ '%+I%%ada-I-%m,%C  %#%m,%Zat line number %l in file %f'}

   return Retval 
endfunction decada#New						     " }}}1

finish " 1}}}

"------------------------------------------------------------------------------
"   Copyright (C) 2006  Martin Krischik
"
"   Vim is Charityware - see ":help license" or uganda.txt for licence details.
"------------------------------------------------------------------------------
" vim: textwidth=78 wrap tabstop=8 shiftwidth=3 softtabstop=3 noexpandtab
" vim: foldmethod=marker
autoload/gnat.vim	[[[1
145
"------------------------------------------------------------------------------
"  Description: Vim Ada/GNAT compiler file
"     Language: Ada (GNAT)
"          $Id: gnat.vim 745 2007-07-08 15:57:40Z krischik $
"    Copyright: Copyright (C) 2006 Martin Krischik
"   Maintainer:	Martin Krischik
"      $Author: krischik $
"        $Date: 2007-07-08 17:57:40 +0200 (So, 08 Jul 2007) $
"      Version: 4.4
"    $Revision: 745 $
"     $HeadURL: http://gnuada.svn.sourceforge.net/svnroot/gnuada/trunk/tools/vim/autoload/gnat.vim $
"      History: 24.05.2006 MK Unified Headers
"		16.07.2006 MK Ada-Mode as vim-ball
"		05.08.2006 MK Add session support
"               15.10.2006 MK Bram's suggestion for runtime integration
"               05.11.2006 MK Bram suggested not to use include protection for
"                             autoload
"		05.11.2006 MK Bram suggested to save on spaces
"    Help Page: compiler-gnat
"------------------------------------------------------------------------------

if version < 700
    finish
endif

function gnat#Make () dict					     " {{{1
   let &l:makeprg	 = self.Get_Command('Make')
   let &l:errorformat = self.Error_Format
   wall
   make
   copen
   set wrap
   wincmd W
endfunction gnat#Make						     " }}}1

function gnat#Pretty () dict					     " {{{1
   execute "!" . self.Get_Command('Pretty')
endfunction gnat#Make						     " }}}1

function gnat#Find () dict					     " {{{1
   execute "!" . self.Get_Command('Find')
endfunction gnat#Find						     " }}}1

function gnat#Tags () dict					     " {{{1
   execute "!" . self.Get_Command('Tags')
   edit tags
   call gnat#Insert_Tags_Header ()
   update
   quit
endfunction gnat#Tags						     " }}}1

function gnat#Set_Project_File (...) dict			     " {{{1
   if a:0 > 0
      let self.Project_File = a:1

      if ! filereadable (self.Project_File)
	 let self.Project_File = findfile (
	    \ fnamemodify (self.Project_File, ':r'),
	    \ $ADA_PROJECT_PATH,
	    \ 1)
      endif
   elseif strlen (self.Project_File) > 0
      let self.Project_File = browse (0, 'GNAT Project File?', '', self.Project_File)
   elseif expand ("%:e") == 'gpr'
      let self.Project_File = browse (0, 'GNAT Project File?', '', expand ("%:e"))
   else
      let self.Project_File = browse (0, 'GNAT Project File?', '', 'default.gpr')
   endif

   if strlen (v:this_session) > 0
      execute 'mksession! ' . v:this_session
   endif

   "if strlen (self.Project_File) > 0
      "if has("vms")
	 "call ada#Switch_Session (
	    "\ expand('~')[0:-2] . ".vimfiles.session]gnat_" .
	    "\ fnamemodify (self.Project_File, ":t:r") . ".vim")
      "else
	 "call ada#Switch_Session (
	    "\ expand('~') . "/vimfiles/session/gnat_" .
	    "\ fnamemodify (self.Project_File, ":t:r") . ".vim")
      "endif
   "else
      "call ada#Switch_Session ('')
   "endif

   return
endfunction gnat#Set_Project_File				     " }}}1

function gnat#Get_Command (Command) dict			     " {{{1
   let l:Command = eval ('self.' . a:Command . '_Command')
   return eval (l:Command)
endfunction gnat#Get_Command					     " }}}1

function gnat#Set_Session (...) dict				     " {{{1
   if argc() == 1 && fnamemodify (argv(0), ':e') == 'gpr'
      call self.Set_Project_File (argv(0))
   elseif  strlen (v:servername) > 0
      call self.Set_Project_File (v:servername . '.gpr')
   endif
endfunction gnat#Set_Session					     " }}}1

function gnat#New ()						     " {{{1
   let l:Retval = {
      \ 'Make'	      : function ('gnat#Make'),
      \ 'Pretty'	      : function ('gnat#Pretty'),
      \ 'Find'	      : function ('gnat#Find'),
      \ 'Tags'	      : function ('gnat#Tags'),
      \ 'Set_Project_File' : function ('gnat#Set_Project_File'),
      \ 'Set_Session'      : function ('gnat#Set_Session'),
      \ 'Get_Command'      : function ('gnat#Get_Command'),
      \ 'Project_File'     : '',
      \ 'Make_Command'     : '"gnat make -P " . self.Project_File . "  -F -gnatef  "',
      \ 'Pretty_Command'   : '"gnat pretty -P " . self.Project_File . " "',
      \ 'Find_Program'     : '"gnat find -P " . self.Project_File . " -F "',
      \ 'Tags_Command'     : '"gnat xref -P " . self.Project_File . " -v  *.AD*"',
      \ 'Error_Format'     : '%f:%l:%c: %trror: %m,'   .
			   \ '%f:%l:%c: %tarning: %m,' .
			   \ '%f:%l:%c: (%ttyle) %m'}

   return l:Retval
endfunction gnat#New						  " }}}1

function gnat#Insert_Tags_Header ()				  " {{{1
   1insert
!_TAG_FILE_FORMAT       1	 /extended format; --format=1 will not append ;" to lines/
!_TAG_FILE_SORTED       1	 /0=unsorted, 1=sorted, 2=foldcase/
!_TAG_PROGRAM_AUTHOR    AdaCore	 /info@adacore.com/
!_TAG_PROGRAM_NAME      gnatxref //
!_TAG_PROGRAM_URL       http://www.adacore.com  /official site/
!_TAG_PROGRAM_VERSION   5.05w   //
.
   return
endfunction gnat#Insert_Tags_Header				  " }}}1

finish " 1}}}

"------------------------------------------------------------------------------
"   Copyright (C) 2006  Martin Krischik
"
"   Vim is Charityware - see ":help license" or uganda.txt for licence details.
"------------------------------------------------------------------------------
" vim: textwidth=0 wrap tabstop=8 shiftwidth=3 softtabstop=3 noexpandtab
" vim: foldmethod=marker
compiler/decada.vim	[[[1
55
"------------------------------------------------------------------------------
"  Description: Vim Ada/Dec Ada compiler file
"     Language: Ada (Dec Ada)
"          $Id: decada.vim 745 2007-07-08 15:57:40Z krischik $
"    Copyright: Copyright (C) 2006 Martin Krischik
"   Maintainer:	Martin Krischik
"      $Author: krischik $
"        $Date: 2007-07-08 17:57:40 +0200 (So, 08 Jul 2007) $
"      Version: 4.4
"    $Revision: 745 $
"     $HeadURL: http://gnuada.svn.sourceforge.net/svnroot/gnuada/trunk/tools/vim/compiler/decada.vim $
"      History: 21.07.2006 MK New Dec Ada
"               15.10.2006 MK Bram's suggestion for runtime integration
"               08.09.2006 MK Correct double load protection.
"    Help Page: compiler-decada
"------------------------------------------------------------------------------

if (exists("current_compiler")	    &&
   \ current_compiler == "decada")  ||
   \ version < 700
   finish
endif

let current_compiler = "decada"

if !exists("g:decada")
   let g:decada = decada#New ()

   call ada#Map_Menu (
     \'Dec Ada.Build',
     \'<F7>',
     \'call decada.Make ()')

   call g:decada.Set_Session ()
endif

if exists(":CompilerSet") != 2
   "
   " plugin loaded by other means then the "compiler" command
   "
   command -nargs=* CompilerSet setlocal <args>
endif

execute "CompilerSet makeprg="     . escape (g:decada.Make_Command, ' ')
execute "CompilerSet errorformat=" . escape (g:decada.Error_Format, ' ')

finish " 1}}}

"------------------------------------------------------------------------------
"   Copyright (C) 2006  Martin Krischik
"
"   Vim is Charityware - see ":help license" or uganda.txt for licence details.
"------------------------------------------------------------------------------
" vim: textwidth=78 wrap tabstop=8 shiftwidth=3 softtabstop=3 noexpandtab
" vim: foldmethod=marker
compiler/gnat.vim	[[[1
71
"------------------------------------------------------------------------------
"  Description: Vim Ada/GNAT compiler file
"     Language: Ada (GNAT)
"          $Id: gnat.vim 745 2007-07-08 15:57:40Z krischik $
"    Copyright: Copyright (C) 2006 Martin Krischik
"   Maintainer:	Martin Krischik
"      $Author: krischik $
"        $Date: 2007-07-08 17:57:40 +0200 (So, 08 Jul 2007) $
"      Version: 4.4
"    $Revision: 745 $
"     $HeadURL: http://gnuada.svn.sourceforge.net/svnroot/gnuada/trunk/tools/vim/compiler/gnat.vim $
"      History: 24.05.2006 MK Unified Headers
"		16.07.2006 MK Ada-Mode as vim-ball
"               15.10.2006 MK Bram's suggestion for runtime integration
"    Help Page: compiler-gnat
"------------------------------------------------------------------------------

if (exists("current_compiler")	    &&
   \ current_compiler == "gnat")    ||
   \ version < 700
   finish
endif

let current_compiler = "gnat"

if !exists("g:gnat")
   let g:gnat = gnat#New ()

   call ada#Map_Menu (
      \ 'GNAT.Build',
      \ '<F7>',
      \ 'call gnat.Make ()')
   call ada#Map_Menu (
      \ 'GNAT.Pretty Print',
      \ ':GnatPretty',
      \ 'call gnat.Pretty ()')
   call ada#Map_Menu (
      \ 'GNAT.Tags',
      \ ':GnatTags',
      \ 'call gnat.Tags ()')
   call ada#Map_Menu (
      \ 'GNAT.Find',
      \ ':GnatFind',
      \ 'call gnat.Find ()')
   call ada#Map_Menu (
      \ 'GNAT.Set Projectfile\.\.\.',
      \ ':SetProject',
      \ 'call gnat.Set_Project_File ()')

   call g:gnat.Set_Session ()
endif

if exists(":CompilerSet") != 2
   "
   " plugin loaded by other means then the "compiler" command
   "
   command -nargs=* CompilerSet setlocal <args>
endif

execute "CompilerSet makeprg="     . escape (g:gnat.Get_Command('Make'), ' ')
execute "CompilerSet errorformat=" . escape (g:gnat.Error_Format, ' ')

finish " 1}}}

"------------------------------------------------------------------------------
"   Copyright (C) 2006  Martin Krischik
"
"   Vim is Charityware - see ":help license" or uganda.txt for licence details.
"------------------------------------------------------------------------------
" vim: textwidth=0 wrap tabstop=8 shiftwidth=3 softtabstop=3 noexpandtab
" vim: foldmethod=marker
doc/ft_ada.txt	[[[1
516
*ft_ada.txt*	    Ada File type Plug-ins	      Last change: 2006 Nov 08


		    ADA FILE TYPE PLUG-INS REFERENCE MANUAL~

ADA								      *ada.vim*

1.  Syntax Highlighting			    |ft-ada-syntax|
2.  Plug-in				    |ft-ada-plugin|
3.  Omni Completion			    |ft-ada-omni|
    3.1 Omni Completion with "gnat xref"	|gnat-xref|
    3.2 Omni Completion with "ctags"		|ada-ctags|
4.  Compiler Support			    |ada-compiler|
    4.1 GNAT					|compiler-gnat|
    4.1 Dec Ada					|compiler-decada|
5.  References				    |ada-reference|
    5.1 Options					|ft-ada-options|
    5.2 Functions				|ft-ada-functions|
    5.3 Commands				|ft-ada-commands|
    5.4 Variables				|ft-ada-variables|
    5.5 Constants				|ft-ada-contstants|
8.  Extra Plug-ins			    |ada-extra-plugins|

==============================================================================
1. Syntax Highlighting ~
							       *ft-ada-syntax*

This mode is designed for the 2005 edition of Ada ("Ada 2005"), which includes
support for objected-programming, protected types, and so on.  It handles code
written for the original Ada language ("Ada83", "Ada87", "Ada95") as well,
though code which uses Ada 2005-only keywords will be wrongly colored (such
code should be fixed anyway).  For more information about Ada, see
http://www.adapower.com.

The Ada mode handles a number of situations cleanly.

For example, it knows that the "-" in "-5" is a number, but the same character
in "A-5" is an operator.  Normally, a "with" or "use" clause referencing
another compilation unit is coloured the same way as C's "#include" is coloured.
If you have "Conditional" or "Repeat" groups coloured differently, then "end
if" and "end loop" will be coloured as part of those respective groups.

You can set these to different colours using vim's "highlight" command (e.g.,
to change how loops are displayed, enter the command ":hi Repeat" followed by
the colour specification; on simple terminals the colour specification
ctermfg=White often shows well).

There are several options you can select in this Ada mode. See|ft-ada-options|
for a complete list.

To enable them, assign a value to the option.  For example, to turn one on:
 >
    > let g:ada_standard_types = 1
>
To disable them use ":unlet".  Example:
>
    > unlet g:ada_standard_types

You can just use ":" and type these into the command line to set these
temporarily before loading an Ada file.  You can make these option settings
permanent by adding the "let" command(s), without a colon, to your "~/.vimrc"
file.

Even on a slow (90Mhz) PC this mode works quickly, but if you find the
performance unacceptable, turn on |g:ada_withuse_ordinary|.

Syntax folding instructions (|fold-syntax|) are added when |g:ada_folding| is
set.

==============================================================================
2. File type Plug-in ~
					       *ft-ada-indent* *ft-ada-plugin*

The Ada plug-in provides support for:

 - auto indenting	(|indent.txt|)
 - insert completion	(|i_CTRL-N|)
 - user completion	(|i_CTRL-X_CTRL-U|)
 - tag searches		(|tagsrch.txt|)
 - Quick Fix		(|quickfix.txt|)
 - backspace handling	(|'backspace'|)
 - comment handling	(|'comments'|, |'commentstring'|)

The plug-in only activates the features of the Ada mode whenever an Ada
files is opened and add adds Ada related entries to the main and pop-up menu.

==============================================================================
3. Omni Completion ~
								 *ft-ada-omni*

The Ada omni-completions (|i_CTRL-X_CTRL-O|) uses tags database created either
by "gnat xref -v" or the "exuberant Ctags (http://ctags.sourceforge.net).  The
complete function will automatically detect which tool was used to create the
tags file.

------------------------------------------------------------------------------
3.1 Omni Completion with "gnat xref" ~
								   *gnat-xref*

GNAT XREF uses the compiler internal informations (ali-files) to produce the
tags file. This has the advantage to be 100% correct and the option of deep
nested analysis. However the code must compile, the generator is quite
slow and the created tags file contains only the basic Ctags informations for
each entry - not enough for some of the more advanced Vim code browser
plug-ins.

NOTE: "gnat xref -v" is very tricky to use as it has almost no diagnostic
       output - If nothing is printed then usually the parameters are wrong.
       Here some important tips:

1)  You need to compile your code first and use the "-aO" option to point to
    your .ali files.
2)  "gnat xref -v ../Include/adacl.ads" won't work - use  the "gnat xref -v
    -aI../Include adacl.ads" instead.
3)  "gnat xref -v -aI../Include *.ad?" won't work - use "cd ../Include" and
    then "gnat xref -v *.ad?"
4)  Project manager support is completely broken - don't even try "gnat xref
    -Padacl.gpr".
5)  VIM is faster when the tags file is sorted - use "sort --unique
    --ignore-case --output=tags tags" .
6)  Remember to insert "!_TAG_FILE_SORTED 2 %sort ui" as first line to mark
    the file assorted.

------------------------------------------------------------------------------
3.2 Omni Completion with "ctags"~
								   *ada-ctags*

Exuberant Ctags uses it's own multi-language code parser. The parser is quite
fast, produces a lot of extra informations (hence the name "Exuberant Ctags")
and can run on files which currently do not compile.

There are also lots of other Vim-tools which use exuberant Ctags.

You will need to install a version of the Exuberant Ctags which has Ada
support patched in. Such a version is available from the GNU Ada Project
(http://gnuada.sourceforge.net).

The Ada parser for Exuberant Ctags is fairly new - don't expect complete
support yet.

==============================================================================
4.  Compiler Support ~
								*ada-compiler*

The Ada mode supports more then one Ada compiler and will automatically load the
compiler set in|g:ada_default_compiler|whenever an Ada source is opened. The
provided compiler plug-ins are split into the actual compiler plug-in and a
collection of support functions and variables. This allows the easy
development of specialized compiler plug-ins fine tuned to your development
environment.

------------------------------------------------------------------------------
4.1 GNAT ~
							       *compiler-gnat*

GNAT is the only free (beer and speech) Ada compiler available. There are
several version available which differentiate in the licence terms used.

The GNAT compiler plug-in will perform a compile on pressing <F7> and then
immediately shows the result. You can set the project file to be used by
setting:
 >
 > call g:gnat.Set_Project_File ('my_project.gpr')

Setting a project file will also create a Vim session (|views-sessions|) so -
like with the GPS - opened files, window positions etc. will remembered
separately for all projects.

								*gnat_members*
GNAT OBJECT ~

							       *g:gnat.Make()*
g:gnat.Make()
		Calls|g:gnat.Make_Command|and displays the result inside a
               |quickfix| window.

							     *g:gnat.Pretty()*
g:gnat.Pretty()
		Calls|g:gnat.Pretty_Command|

							       *g:gnat.Find()*
g:gnat.Find()
		Calls|g:gnat.Find_Command|

							       *g:gnat.Tags()*
g:gnat.Tags()
		Calls|g:gnat.Tags_Command|

						   *g:gnat.Set_Project_File()*
g:gnat.Set_Project_File([{file}])
		Set gnat project file and load associated session.  An open
		project will be closed and the session written.  If called
		without file name the file selector opens for selection of a
		project file. If called with an empty string then the project
		and associated session are closed.

							 *g:gnat.Project_File*
g:gnat.Project_File	string
		Current project file.

							 *g:gnat.Make_Command*
g:gnat.Make_Command	string
		External command used for|g:gnat.Make()| (|'makeprg'|).

						       *g:gnat.Pretty_Program*
g:gnat.Pretty_Program	string
		External command used for|g:gnat.Pretty()|

							 *g:gnat.Find_Program*
g:gnat.Find_Program	string
		External command used for|g:gnat.Find()|

							 *g:gnat.Tags_Command*
g:gnat.Tags_Command	string
		External command used for|g:gnat.Tags()|

							 *g:gnat.Error_Format*
g:gnat.Error_Format	string
		Error format (|'errorformat'|)

------------------------------------------------------------------------------
4.2 Dec Ada ~
					    *compiler-hpada* *compiler-decada*
					*compiler-vaxada* *compiler-compaqada*

Dec Ada (also known by - in chronological order - VAX Ada, Dec Ada, Compaq Ada
and HP Ada) is a fairly dated Ada 83 compiler. Support is basic: <F7> will
compile the current unit.

The Dec Ada compiler expects the package name and not the file name to be
passed a parameter. The compiler plug-in supports the usual file name
convention to convert the file into a unit name. For separates both '-' and
'__' are allowed.

							      *decada_members*
DEC ADA OBJECT ~

							     *g:decada.Make()*
g:decada.Make()		function
		Calls|g:decada.Make_Command|and displays the result inside a
               |quickfix| window.

							*g:decada.Unit_Name()*
g:decada.Unit_Name()	function
		Get the Unit name for the current file.

						       *g:decada.Make_Command*
g:decada.Make_Command	string
		External command used for|g:decadat.Make()| (|'makeprg'|).

						       *g:decada.Error_Format*
g:decada.Error_Format|	string
		Error format (|'errorformat'|).

==============================================================================
5. References ~
							       *ada-reference*

------------------------------------------------------------------------------
5.1 Options ~
							      *ft-ada-options*

							*g:ada_standard_types*
g:ada_standard_types	bool (true when exists)
		Highlight types in package Standard (e.g., "Float")

							  *g:ada_space_errors*
						  *g:ada_no_trail_space_error*
						    *g:ada_no_tab_space_error*
							 *g:ada_all_tab_usage*
g:ada_space_errors	 bool (true when exists)
		Highlight extraneous errors in spaces ...
		g:ada_no_trail_space_error
		    - but ignore trailing spaces at the end of a line
		g:ada_no_tab_space_error
		    - but ignore tabs after spaces
		g:ada_all_tab_usage
		    - highlight all tab use

							   *g:ada_line_errors*
g:ada_line_errors	  bool (true when exists)
		Highlight lines which are to long. Note: This highlighting
		option is quite CPU intensive.

							 *g:ada_rainbow_color*
g:ada_rainbow_color	  bool (true when exists)
		Use rainbow colours for '(' and ')'. You need the
		rainbow_parenthesis for this to work

							       *g:ada_folding*
g:ada_folding		  set ('sigpft')
		Use folding for Ada sources.
		    's':    activate syntax folding on load
			'p':    fold packages
			'f':    fold functions and procedures
			't':    fold types
			'c':    fold conditionals
		    'g':    activate gnat pretty print folding on load
			'i':    lone 'is' folded with line above
			'b':	lone 'begin' folded with line above
			'p':	lone 'private' folded with line above
			'x':	lone 'exception' folded with line above
		    'i':    activate indent folding on load

		Note: Syntax folding is in an early (unuseable) stage and
		      indent or gnat pretty folding is suggested.

		For gnat pretty folding to work the following settings are
		suggested: -cl3 -M79 -c2 -c3 -c4 -A1 -A2 -A3 -A4 -A5

		For indent folding to work the following settings are
		suggested: shiftwidth=3 softtabstop=3

								*g:ada_abbrev*
g:ada_abbrev		  bool (true when exists)
		Add some abbreviations. This feature more or less superseded
		by the various completion methods.

						      *g:ada_withuse_ordinary*
g:ada_withuse_ordinary	  bool (true when exists)
		Show "with" and "use" as ordinary keywords (when used to
		reference other compilation units they're normally highlighted
		specially).

							 *g:ada_begin_preproc*
g:ada_begin_preproc	  bool (true when exists)
		Show all begin-like keywords using the colouring of C
		preprocessor commands.

						    *g:ada_omni_with_keywords*
g:ada_omni_with_keywords
		Add Keywords, Pragmas, Attributes to omni-completions
		(|compl-omni|). Note: You can always complete then with user
		completion (|i_CTRL-X_CTRL-U|).

						      *g:ada_extended_tagging*
g:ada_extended_tagging	  enum ('jump', 'list')
		use extended tagging, two options are available
		    'jump': use tjump to jump.
		    'list': add tags quick fix list.
		Normal tagging does not support function or operator
		overloading as these features are not available in C and
		tagging was originally developed for C.

						   *g:ada_extended_completion*
g:ada_extended_completion
		Uses extended completion for <C-N> and <C-R> completions
		(|i_CTRL-N|). In this mode the '.' is used as part of the
		identifier so that 'Object.Method' or 'Package.Procedure' are
		completed together.

						       *g:ada_gnat_extensions*
g:ada_gnat_extensions	  bool (true when exists)
		 Support GNAT extensions.

					       *g:ada_with_gnat_project_files*
g:ada_with_gnat_project_files	 bool (true when exists)
		 Add gnat project file keywords and Attributes.

						      *g:ada_default_compiler*
g:ada_default_compiler	  string
		set default compiler. Currently supported is 'gnat' and
		'decada'.

An "exists" type is a boolean is considered true when the variable is defined
and false when the variable is undefined. The value which the variable is
set makes no difference.

------------------------------------------------------------------------------
5.3 Commands ~
							     *ft-ada-commands*

:AdaRainbow							 *:AdaRainbow*
		Toggles rainbow colour (|g:ada_rainbow_color|) mode for
		'(' and ')'

:AdaLines							   *:AdaLines*
		Toggles line error (|g:ada_line_errors|) display

:AdaSpaces							  *:AdaSpaces*
		Toggles space error (|g:ada_space_errors|) display.

:AdaTagDir							  *:AdaTagDir*
		Creates tags file for the directory of the current file.

:AdaTagFile							 *:AdaTagFile*
		Creates tags file for the current file.

:AdaTypes							   *:AdaTypes*
		Toggles standard types (|g:ada_standard_types|) colour.

:GnatFind							   *:GnatFind*
		Calls |g:gnat.Find()|

:GnatPretty							 *:GnatPretty*
		Calls |g:gnat.Pretty()|

:GnatTags							   *:GnatTags*
		Calls |g:gnat.Tags()|

------------------------------------------------------------------------------
5.3 Variables ~
							    *ft-ada-variables*

								      *g:gnat*
g:gnat			    object
		Control object which manages GNAT compiles.  The object
		is created when the first Ada source code is loaded provided
		that |g:ada_default_compiler|is set to 'gnat'. See|gnat_members|
		for details.

								    *g:decada*
g:decada		      object
		Control object which manages Dec Ada compiles.	The object
		is created when the first Ada source code is loaded provided
		that |g:ada_default_compiler|is set to 'decada'. See
	       |decada_members|for details.

------------------------------------------------------------------------------
5.4 Constants ~
							    *ft-ada-constants*
                                                             ft-ada-constants

All constants are locked. See |:lockvar| for details.

							     *g:ada#WordRegex*
g:ada#WordRegex		string
		Regular expression to search for Ada words

							  *g:ada#DotWordRegex*
g:ada#DotWordRegex	string
		Regular expression to search for Ada words separated by dots.

							       *g:ada#Comment*
g:ada#Comment		string
		Regular expression to search for Ada comments

							      *g:ada#Keywords*
g:ada#Keywords		list of dictionaries
		List of keywords, attributes etc. pp. in the format used by
		omni completion. See |complete-items| for details.

							   *g:ada#Ctags_Kinds*
g:ada#Ctags_Kinds	dictionary of lists
		Dictionary of the various kinds of items which the Ada support
		for Ctags generates.

------------------------------------------------------------------------------
5.2 Functions ~
							    *ft-ada-functions*

ada#Word([{line}, {col}])					  *ada#Word()*
		Return full name of Ada entity under the cursor (or at given
		line/column), stripping white space/newlines as necessary.

ada#List_Tag([{line}, {col}])				      *ada#Listtags()*
		List all occurrences of the Ada entity under the cursor (or at
		given line/column) inside the quick-fix window

ada#Jump_Tag ({ident}, {mode})				      *ada#Jump_Tag()*
		List all occurrences of the Ada entity under the cursor (or at
		given line/column) in the tag jump list. Mode can either be
		'tjump' or 'stjump'.

ada#Create_Tags ({option})				   *ada#Create_Tags()*
		Creates tag file using Ctags. The option can either be 'file'
		for the current file, 'dir' for the directory of the current
		file or a file name.

gnat#Insert_Tags_Header()			   *gnat#Insert_Tags_Header()*
		Adds the tag file header (!_TAG_) informations to the current
		file which are missing from the GNAT XREF output.

ada#Switch_Syntax_Option ({option})		  *ada#Switch_Syntax_Option()*
		Toggles highlighting options on or off. Used for the Ada menu.

								  *gnat#New()*
gnat#New ()
		Create a new gnat object. See |g:gnat| for details.


==============================================================================
8. Extra Plugins ~
							   *ada-extra-plugins*

You can optionally install the following extra plug-in. They work well with Ada
and enhance the ability of the Ada mode.:

backup.vim
	http://www.vim.org/scripts/script.php?script_id=1537
	Keeps as many backups as you like so you don't have to.

rainbow_parenthsis.vim
	http://www.vim.org/scripts/script.php?script_id=1561
	Very helpful since Ada uses only '(' and ')'.

nerd_comments.vim
	http://www.vim.org/scripts/script.php?script_id=1218
	Excellent commenting and uncommenting support for almost any
	programming language.

matchit.vim
	http://www.vim.org/scripts/script.php?script_id=39
	'%' jumping for any language. The normal '%' jump only works for '{}'
	style languages. The Ada mode will set the needed search patters.

taglist.vim
	http://www.vim.org/scripts/script.php?script_id=273
	Source code explorer sidebar. There is a patch for Ada available.

The GNU Ada Project distribution (http://gnuada.sourceforge.net) of Vim
contains all of the above.

==============================================================================
vim: textwidth=78 nowrap tabstop=8 shiftwidth=4 softtabstop=4 noexpandtab
vim: filetype=help
ftdetect/ada.vim	[[[1
39
"------------------------------------------------------------------------------
"  Description: Vim Ada detection file
"     Language: Ada (2005)
"	   $Id: ada.vim 467 2006-11-23 19:22:39Z krischik $
"    Copyright: Copyright (C) 2006 Martin Krischik
"   Maintainer: Martin Krischik
"      $Author: krischik $
"	 $Date: 2006-11-23 20:22:39 +0100 (Do, 23 Nov 2006) $
"      Version: 4.3
"    $Revision: 467 $
"     $HeadURL: https://svn.sourceforge.net/svnroot/gnuada/trunk/tools/vim/ftdetect/ada.vim $
"      History: 24.05.2006 MK Unified Headers
"		16.07.2006 MK Ada-Mode as vim-ball
"		15.10.2006 MK Bram's suggestion for runtime integration
"		05.11.2006 MK Bram suggested to save on spaces
"    Help Page: ft-ada-plugin
"------------------------------------------------------------------------------

if exists("s:loaded_ftdetect_ada")
    finish
endif

let s:loaded_ftdetect_ada=1

if has("vms")
   autocmd BufNewFile,BufRead *.gpr,*.ada_m,*.adc setfiletype ada
else
   autocmd BufNewFile,BufRead *.gpr setfiletype ada
endif

finish " 1}}}

"------------------------------------------------------------------------------
"   Copyright (C) 2006	Martin Krischik
"
"   Vim is Charityware - see ":help license" or uganda.txt for licence details.
"------------------------------------------------------------------------------
" vim: textwidth=78 nowrap tabstop=8 shiftwidth=3 softtabstop=3 noexpandtab
" vim: foldmethod=marker
ftplugin/ada.vim	[[[1
201
"------------------------------------------------------------------------------
"  Description: Perform Ada specific completion & tagging.
"     Language: Ada (2005)
"	   $Id: ada.vim 748 2007-07-15 18:11:29Z krischik $
"   Maintainer: Martin Krischik
"		Taylor Venable <taylor@metasyntax.net>
"		Neil Bird <neil@fnxweb.com>
"      $Author: krischik $
"	 $Date: 2007-07-15 20:11:29 +0200 (So, 15 Jul 2007) $
"      Version: 4.4
"    $Revision: 748 $
"     $HeadURL: http://gnuada.svn.sourceforge.net/svnroot/gnuada/trunk/tools/vim/ftplugin/ada.vim $
"      History: 24.05.2006 MK Unified Headers
"		26.05.2006 MK ' should not be in iskeyword.
"		16.07.2006 MK Ada-Mode as vim-ball
"		02.10.2006 MK Better folding.
"		15.10.2006 MK Bram's suggestion for runtime integration
"               05.11.2006 MK Bram suggested not to use include protection for
"                             autoload
"		05.11.2006 MK Bram suggested to save on spaces
"		08.07.2007 TV fix default compiler problems.
"    Help Page: ft-ada-plugin
"------------------------------------------------------------------------------
" Provides mapping overrides for tag jumping that figure out the current
" Ada object and tag jump to that, not the 'simple' vim word.
" Similarly allows <Ctrl-N> matching of full-length ada entities from tags.
"------------------------------------------------------------------------------

" Only do this when not done yet for this buffer
if exists ("b:did_ftplugin") || version < 700
   finish
endif

" Don't load another plugin for this buffer
let b:did_ftplugin = 44

"
" Temporarily set cpoptions to ensure the script loads OK
"
let s:cpoptions = &cpoptions
set cpoptions-=C

" Section: Comments {{{1
"
setlocal comments=O:--,:--\ \
setlocal commentstring=--\ \ %s
setlocal complete=.,w,b,u,t,i

" Section: Tagging {{{1
"
if exists ("g:ada_extended_tagging")
   " Make local tag mappings for this buffer (if not already set)
   if g:ada_extended_tagging == 'jump'
      if mapcheck('<C-]>','n') == ''
	 nnoremap <unique> <buffer> <C-]>    :call ada#Jump_Tag ('', 'tjump')<cr>
      endif
      if mapcheck('g<C-]>','n') == ''
	 nnoremap <unique> <buffer> g<C-]>   :call ada#Jump_Tag ('','stjump')<cr>
      endif
   elseif g:ada_extended_tagging == 'list'
      if mapcheck('<C-]>','n') == ''
	 nnoremap <unique> <buffer> <C-]>    :call ada#List_Tag ()<cr>
      endif
      if mapcheck('g<C-]>','n') == ''
	 nnoremap <unique> <buffer> g<C-]>   :call ada#List_Tag ()<cr>
      endif
   endif
endif

" Section: Completion {{{1
"
setlocal completefunc=ada#User_Complete
setlocal omnifunc=adacomplete#Complete

if exists ("g:ada_extended_completion")
   if mapcheck ('<C-N>','i') == ''
      inoremap <unique> <buffer> <C-N> <C-R>=ada#Completion("\<lt>C-N>")<cr>
   endif
   if mapcheck ('<C-P>','i') == ''
      inoremap <unique> <buffer> <C-P> <C-R>=ada#Completion("\<lt>C-P>")<cr>
   endif
   if mapcheck ('<C-X><C-]>','i') == ''
      inoremap <unique> <buffer> <C-X><C-]> <C-R>=<SID>ada#Completion("\<lt>C-X>\<lt>C-]>")<cr>
   endif
   if mapcheck ('<bs>','i') == ''
      inoremap <silent> <unique> <buffer> <bs> <C-R>=ada#Insert_Backspace ()<cr>
   endif
endif

" Section: Matchit {{{1
"
" Only do this when not done yet for this buffer & matchit is used
"
if !exists ("b:match_words")  &&
  \ exists ("loaded_matchit")
   "
   " The following lines enable the macros/matchit.vim plugin for
   " Ada-specific extended matching with the % key.
   "
   let s:notend      = '\%(\<end\s\+\)\@<!'
   let b:match_words =
      \ s:notend . '\<if\>:\<elsif\>:\<else\>:\<end\>\s\+\<if\>,' .
      \ s:notend . '\<case\>:\<when\>:\<end\>\s\+\<case\>,' .
      \ '\%(\<while\>.*\|\<for\>.*\|'.s:notend.'\)\<loop\>:\<end\>\s\+\<loop\>,' .
      \ '\%(\<do\>\|\<begin\>\):\<exception\>:\<end\>\s*\%($\|[;A-Z]\),' .
      \ s:notend . '\<record\>:\<end\>\s\+\<record\>'
endif


" Section: Compiler {{{1
"
if ! exists("g:ada_default_compiler")
   if has("vms")
      let g:ada_default_compiler = 'decada'
   else
      let g:ada_default_compiler = 'gnat'
   endif
endif

if ! exists("current_compiler")			||
   \ current_compiler != g:ada_default_compiler
   execute "compiler " . g:ada_default_compiler
endif

" Section: Folding {{{1
"
if exists("g:ada_folding")
   if g:ada_folding[0] == 'i'
      setlocal foldmethod=indent
      setlocal foldignore=--
      setlocal foldnestmax=5
   elseif g:ada_folding[0] == 'g'
      setlocal foldmethod=expr
      setlocal foldexpr=ada#Pretty_Print_Folding(v:lnum)
   elseif g:ada_folding[0] == 's'
      setlocal foldmethod=syntax
   endif
   setlocal tabstop=8
   setlocal softtabstop=3
   setlocal shiftwidth=3
endif

" Section: Abbrev {{{1
"
if exists("g:ada_abbrev")
   iabbrev ret	return
   iabbrev proc procedure
   iabbrev pack package
   iabbrev func function
endif

" Section: Commands, Mapping, Menus {{{1
"
call ada#Map_Popup (
   \ 'Tag.List',
   \  'l',
   \ 'call ada#List_Tag ()')
call ada#Map_Popup (
   \'Tag.Jump',
   \'j',
   \'call ada#Jump_Tag ()')
call ada#Map_Menu (
   \'Tag.Create File',
   \':AdaTagFile',
   \'call ada#Create_Tags (''file'')')
call ada#Map_Menu (
   \'Tag.Create Dir',
   \':AdaTagDir',
   \'call ada#Create_Tags (''dir'')')

call ada#Map_Menu (
   \'Highlight.Toggle Space Errors',
   \ ':AdaSpaces',
   \'call ada#Switch_Syntax_Option (''space_errors'')')
call ada#Map_Menu (
   \'Highlight.Toggle Lines Errors',
   \ ':AdaLines',
   \'call ada#Switch_Syntax_Option (''line_errors'')')
call ada#Map_Menu (
   \'Highlight.Toggle Rainbow Color',
   \ ':AdaRainbow',
   \'call ada#Switch_Syntax_Option (''rainbow_color'')')
call ada#Map_Menu (
   \'Highlight.Toggle Standard Types',
   \ ':AdaTypes',
   \'call ada#Switch_Syntax_Option (''standard_types'')')

" 1}}}
" Reset cpoptions
let &cpoptions = s:cpoptions
unlet s:cpoptions

finish " 1}}}

"------------------------------------------------------------------------------
"   Copyright (C) 2006	Martin Krischik
"
"   Vim is Charityware - see ":help license" or uganda.txt for licence details.
"------------------------------------------------------------------------------
" vim: textwidth=78 nowrap tabstop=8 shiftwidth=3 softtabstop=3 noexpandtab
" vim: foldmethod=marker
indent/ada.vim	[[[1
300
"------------------------------------------------------------------------------
"  Description: Vim Ada indent file
"     Language: Ada (2005)
"	   $Id: ada.vim 745 2007-07-08 15:57:40Z krischik $
"    Copyright: Copyright (C) 2006 Martin Krischik
"   Maintainer: Martin Krischik
"		Neil Bird <neil@fnxweb.com>
"      $Author: krischik $
"	 $Date: 2007-07-08 17:57:40 +0200 (So, 08 Jul 2007) $
"      Version: 4.4
"    $Revision: 745 $
"     $HeadURL: http://gnuada.svn.sourceforge.net/svnroot/gnuada/trunk/tools/vim/indent/ada.vim $
"      History: 24.05.2006 MK Unified Headers
"		16.07.2006 MK Ada-Mode as vim-ball
"		15.10.2006 MK Bram's suggestion for runtime integration
"		05.11.2006 MK Bram suggested to save on spaces
"    Help Page: ft-vim-indent
"------------------------------------------------------------------------------
" ToDo:
"  Verify handling of multi-line exprs. and recovery upon the final ';'.
"  Correctly find comments given '"' and "" ==> " syntax.
"  Combine the two large block-indent functions into one?
"------------------------------------------------------------------------------

" Only load this indent file when no other was loaded.
if exists("b:did_indent") || version < 700
   finish
endif

let b:did_indent = 44

setlocal indentexpr=GetAdaIndent()
setlocal indentkeys-=0{,0}
setlocal indentkeys+=0=~then,0=~end,0=~elsif,0=~when,0=~exception,0=~begin,0=~is,0=~record

" Only define the functions once.
if exists("*GetAdaIndent")
   finish
endif

if exists("g:ada_with_gnat_project_files")
   let s:AdaBlockStart = '^\s*\(if\>\|while\>\|else\>\|elsif\>\|loop\>\|for\>.*\<\(loop\|use\)\>\|declare\>\|begin\>\|type\>.*\<is\>[^;]*$\|\(type\>.*\)\=\<record\>\|procedure\>\|function\>\|accept\>\|do\>\|task\>\|package\>\|project\>\|then\>\|when\>\|is\>\)'
else
   let s:AdaBlockStart = '^\s*\(if\>\|while\>\|else\>\|elsif\>\|loop\>\|for\>.*\<\(loop\|use\)\>\|declare\>\|begin\>\|type\>.*\<is\>[^;]*$\|\(type\>.*\)\=\<record\>\|procedure\>\|function\>\|accept\>\|do\>\|task\>\|package\>\|then\>\|when\>\|is\>\)'
endif

" Section: s:MainBlockIndent {{{1
"
" Try to find indent of the block we're in
" prev_indent = the previous line's indent
" prev_lnum   = previous line (to start looking on)
" blockstart  = expr. that indicates a possible start of this block
" stop_at     = if non-null, if a matching line is found, gives up!
" No recursive previous block analysis: simply look for a valid line
" with a lesser or equal indent than we currently (on prev_lnum) have.
" This shouldn't work as well as it appears to with lines that are currently
" nowhere near the correct indent (e.g., start of line)!
" Seems to work OK as it 'starts' with the indent of the /previous/ line.
function s:MainBlockIndent (prev_indent, prev_lnum, blockstart, stop_at)
   let lnum = a:prev_lnum
   let line = substitute( getline(lnum), ada#Comment, '', '' )
   while lnum > 1
      if a:stop_at != ''  &&  line =~ '^\s*' . a:stop_at  &&  indent(lnum) < a:prev_indent
	 return a:prev_indent
      elseif line =~ '^\s*' . a:blockstart
	 let ind = indent(lnum)
	 if ind < a:prev_indent
	    return ind
	 endif
      endif

      let lnum = prevnonblank(lnum - 1)
      " Get previous non-blank/non-comment-only line
      while 1
	 let line = substitute( getline(lnum), ada#Comment, '', '' )
	 if line !~ '^\s*$' && line !~ '^\s*#'
	    break
	 endif
	 let lnum = prevnonblank(lnum - 1)
	 if lnum <= 0
	    return a:prev_indent
	 endif
      endwhile
   endwhile
   " Fallback - just move back one
   return a:prev_indent - &sw
endfunction MainBlockIndent

" Section: s:EndBlockIndent {{{1
"
" Try to find indent of the block we're in (and about to complete),
" including handling of nested blocks. Works on the 'end' of a block.
" prev_indent = the previous line's indent
" prev_lnum   = previous line (to start looking on)
" blockstart  = expr. that indicates a possible start of this block
" blockend    = expr. that indicates a possible end of this block
function s:EndBlockIndent( prev_indent, prev_lnum, blockstart, blockend )
   let lnum = a:prev_lnum
   let line = getline(lnum)
   let ends = 0
   while lnum > 1
      if getline(lnum) =~ '^\s*' . a:blockstart
	 let ind = indent(lnum)
	 if ends <= 0
	    if ind < a:prev_indent
	       return ind
	    endif
	 else
	    let ends = ends - 1
	 endif
      elseif getline(lnum) =~ '^\s*' . a:blockend
	 let ends = ends + 1
      endif

      let lnum = prevnonblank(lnum - 1)
      " Get previous non-blank/non-comment-only line
      while 1
	 let line = getline(lnum)
	 let line = substitute( line, ada#Comment, '', '' )
	 if line !~ '^\s*$'
	    break
	 endif
	 let lnum = prevnonblank(lnum - 1)
	 if lnum <= 0
	    return a:prev_indent
	 endif
      endwhile
   endwhile
   " Fallback - just move back one
   return a:prev_indent - &sw
endfunction EndBlockIndent

" Section: s:StatementIndent {{{1
"
" Return indent of previous statement-start
" (after we've indented due to multi-line statements).
" This time, we start searching on the line *before* the one given (which is
" the end of a statement - we want the previous beginning).
function s:StatementIndent( current_indent, prev_lnum )
   let lnum  = a:prev_lnum
   while lnum > 0
      let prev_lnum = lnum
      let lnum = prevnonblank(lnum - 1)
      " Get previous non-blank/non-comment-only line
      while 1
	 let line = substitute( getline(lnum), ada#Comment, '', '' )
	 if line !~ '^\s*$' && line !~ '^\s*#'
	    break
	 endif
	 let lnum = prevnonblank(lnum - 1)
	 if lnum <= 0
	    return a:current_indent
	 endif
      endwhile
      " Leave indent alone if our ';' line is part of a ';'-delineated
      " aggregate (e.g., procedure args.) or first line after a block start.
      if line =~ s:AdaBlockStart || line =~ '(\s*$'
	 return a:current_indent
      endif
      if line !~ '[.=(]\s*$'
	 let ind = indent(prev_lnum)
	 if ind < a:current_indent
	    return ind
	 endif
      endif
   endwhile
   " Fallback - just use current one
   return a:current_indent
endfunction StatementIndent


" Section: GetAdaIndent {{{1
"
" Find correct indent of a new line based upon what went before
"
function GetAdaIndent()
   " Find a non-blank line above the current line.
   let lnum = prevnonblank(v:lnum - 1)
   let ind = indent(lnum)
   let package_line = 0

   " Get previous non-blank/non-comment-only/non-cpp line
   while 1
      let line = substitute( getline(lnum), g:ada#Comment, '', '' )
      if line !~ '^\s*$' && line !~ '^\s*#'
	 break
      endif
      let lnum = prevnonblank(lnum - 1)
      if lnum <= 0
	 return ind
      endif
   endwhile

   " Get default indent (from prev. line)
   let ind = indent(lnum)
   let initind = ind

   " Now check what's on the previous line
   if line =~ s:AdaBlockStart  ||  line =~ '(\s*$'
      " Check for false matches to AdaBlockStart
      let false_match = 0
      if line =~ '^\s*\(procedure\|function\|package\)\>.*\<is\s*new\>'
	 " Generic instantiation
	 let false_match = 1
      elseif line =~ ')\s*;\s*$'  ||  line =~ '^\([^(]*([^)]*)\)*[^(]*;\s*$'
	 " forward declaration
	 let false_match = 1
      endif
      " Move indent in
      if ! false_match
	 let ind = ind + &sw
      endif
   elseif line =~ '^\s*\(case\|exception\)\>'
      " Move indent in twice (next 'when' will move back)
      let ind = ind + 2 * &sw
   elseif line =~ '^\s*end\s*record\>'
      " Move indent back to tallying 'type' preceeding the 'record'.
      " Allow indent to be equal to 'end record's.
      let ind = s:MainBlockIndent( ind+&sw, lnum, 'type\>', '' )
   elseif line =~ '\(^\s*new\>.*\)\@<!)\s*[;,]\s*$'
      " Revert to indent of line that started this parenthesis pair
      exe lnum
      exe 'normal! $F)%'
      if getline('.') =~ '^\s*('
	 " Dire layout - use previous indent (could check for ada#Comment here)
	 let ind = indent( prevnonblank( line('.')-1 ) )
      else
	 let ind = indent('.')
      endif
      exe v:lnum
   elseif line =~ '[.=(]\s*$'
      " A statement continuation - move in one
      let ind = ind + &sw
   elseif line =~ '^\s*new\>'
      " Multiple line generic instantiation ('package blah is\nnew thingy')
      let ind = s:StatementIndent( ind - &sw, lnum )
   elseif line =~ ';\s*$'
      " Statement end (but not 'end' ) - try to find current statement-start indent
      let ind = s:StatementIndent( ind, lnum )
   endif

   " Check for potential argument list on next line
   let continuation = (line =~ '[A-Za-z0-9_]\s*$')


   " Check current line; search for simplistic matching start-of-block
   let line = getline(v:lnum)
   if line =~ '^\s*#'
      " Start of line for ada-pp
      let ind = 0
   elseif continuation && line =~ '^\s*('
      " Don't do this if we've already indented due to the previous line
      if ind == initind
	 let ind = ind + &sw
      endif
   elseif line =~ '^\s*\(begin\|is\)\>'
      let ind = s:MainBlockIndent( ind, lnum, '\(procedure\|function\|declare\|package\|task\)\>', 'begin\>' )
   elseif line =~ '^\s*record\>'
      let ind = s:MainBlockIndent( ind, lnum, 'type\>\|for\>.*\<use\>', '' ) + &sw
   elseif line =~ '^\s*\(else\|elsif\)\>'
      let ind = s:MainBlockIndent( ind, lnum, 'if\>', '' )
   elseif line =~ '^\s*when\>'
      " Align 'when' one /in/ from matching block start
      let ind = s:MainBlockIndent( ind, lnum, '\(case\|exception\)\>', '' ) + &sw
   elseif line =~ '^\s*end\>\s*\<if\>'
      " End of if statements
      let ind = s:EndBlockIndent( ind, lnum, 'if\>', 'end\>\s*\<if\>' )
   elseif line =~ '^\s*end\>\s*\<loop\>'
      " End of loops
      let ind = s:EndBlockIndent( ind, lnum, '\(\(while\|for\)\>.*\)\?\<loop\>', 'end\>\s*\<loop\>' )
   elseif line =~ '^\s*end\>\s*\<record\>'
      " End of records
      let ind = s:EndBlockIndent( ind, lnum, '\(type\>.*\)\=\<record\>', 'end\>\s*\<record\>' )
   elseif line =~ '^\s*end\>\s*\<procedure\>'
      " End of procedures
      let ind = s:EndBlockIndent( ind, lnum, 'procedure\>.*\<is\>', 'end\>\s*\<procedure\>' )
   elseif line =~ '^\s*end\>\s*\<case\>'
      " End of case statement
      let ind = s:EndBlockIndent( ind, lnum, 'case\>.*\<is\>', 'end\>\s*\<case\>' )
   elseif line =~ '^\s*end\>'
      " General case for end
      let ind = s:MainBlockIndent( ind, lnum, '\(if\|while\|for\|loop\|accept\|begin\|record\|case\|exception\|package\)\>', '' )
   elseif line =~ '^\s*exception\>'
      let ind = s:MainBlockIndent( ind, lnum, 'begin\>', '' )
   elseif line =~ '^\s*then\>'
      let ind = s:MainBlockIndent( ind, lnum, 'if\>', '' )
   endif

   return ind
endfunction GetAdaIndent

finish " 1}}}

"------------------------------------------------------------------------------
"   Copyright (C) 2006	Martin Krischik
"
"   Vim is Charityware - see ":help license" or uganda.txt for licence details.
"------------------------------------------------------------------------------
" vim: textwidth=78 wrap tabstop=8 shiftwidth=3 softtabstop=3 noexpandtab
" vim: foldmethod=marker
syntax/ada.vim	[[[1
367
"----------------------------------------------------------------------------
"  Description: Vim Ada syntax file
"     Language: Ada (2005)
"	   $Id: ada.vim 745 2007-07-08 15:57:40Z krischik $
"    Copyright: Copyright (C) 2006 Martin Krischik
"   Maintainer: Martin Krischik
"		David A. Wheeler <dwheeler@dwheeler.com>
"		Simon Bradley <simon.bradley@pitechnology.com>
" Contributors: Preben Randhol.
"      $Author: krischik $
"	 $Date: 2007-07-08 17:57:40 +0200 (So, 08 Jul 2007) $
"      Version: 4.4
"    $Revision: 745 $
"     $HeadURL: http://gnuada.svn.sourceforge.net/svnroot/gnuada/trunk/tools/vim/syntax/ada.vim $
"		http://www.dwheeler.com/vim
"      History: 24.05.2006 MK Unified Headers
"		26.05.2006 MK ' should not be in iskeyword.
"		16.07.2006 MK Ada-Mode as vim-ball
"		02.10.2006 MK Better folding.
"		15.10.2006 MK Bram's suggestion for runtime integration
"		05.11.2006 MK Spell check for comments and strings only
"		05.11.2006 MK Bram suggested to save on spaces
"    Help Page: help ft-ada-syntax
"------------------------------------------------------------------------------
" The formal spec of Ada 2005 (ARM) is the "Ada 2005 Reference Manual".
" For more Ada 2005 info, see http://www.gnuada.org and http://www.adapower.com.
"
" This vim syntax file works on vim 7.0 only and makes use of most of Voim 7.0 
" advanced features.
"------------------------------------------------------------------------------

if exists("b:current_syntax") || version < 700
    finish
endif

let b:current_syntax = "ada"

" Section: Ada is entirely case-insensitive. {{{1
"
syntax   case ignore
setlocal nosmartcase
setlocal ignorecase

" Section: Highlighting commands {{{1
"
" There are 72 reserved words in total in Ada2005. Some keywords are
" used in more than one way. For example:
" 1. "end" is a general keyword, but "end if" ends a Conditional.
" 2. "then" is a conditional, but "and then" is an operator.
"
for b:Item in g:ada#Keywords
   " Standard Exceptions (including I/O).
   " We'll highlight the standard exceptions, similar to vim's Python mode.
   " It's possible to redefine the standard exceptions as something else,
   " but doing so is very bad practice, so simply highlighting them makes sense.
   if b:Item['kind'] == "x"
      execute "syntax keyword adaException " . b:Item['word']
   endif
   if b:Item['kind'] == "a"
      execute 'syntax match adaAttribute "\V' . b:Item['word'] . '"'
   endif
   " We don't normally highlight types in package Standard
   " (Integer, Character, Float, etc.).  I don't think it looks good
   " with the other type keywords, and many Ada programs define
   " so many of their own types that it looks inconsistent.
   " However, if you want this highlighting, turn on "ada_standard_types".
   " For package Standard's definition, see ARM section A.1.
   if b:Item['kind'] == "t" && exists ("g:ada_standard_types")
      execute "syntax keyword adaBuiltinType " . b:Item['word']
   endif
endfor

" Section: others {{{1
"
syntax keyword  adaLabel	others

" Section: Operatoren {{{1
"
syntax keyword  adaOperator abs mod not rem xor
syntax match    adaOperator "\<and\>"
syntax match    adaOperator "\<and\s\+then\>"
syntax match    adaOperator "\<or\>"
syntax match    adaOperator "\<or\s\+else\>"
syntax match    adaOperator "[-+*/<>&]"
syntax keyword  adaOperator **
syntax match    adaOperator "[/<>]="
syntax keyword  adaOperator =>
syntax match    adaOperator "\.\."
syntax match    adaOperator "="

" Section: <> {{{1
"
" Handle the box, <>, specially:
"
syntax keyword  adaSpecial	    <>

" Section: rainbow color {{{1
"
if exists("g:ada_rainbow_color")
    syntax match	adaSpecial	 "[:;.,]"
    runtime plugin/Rainbow_Parenthsis.vim
else
    syntax match	adaSpecial	 "[:;().,]"
endif

" Section: := {{{1
"
" We won't map "adaAssignment" by default, but we need to map ":=" to
" something or the "=" inside it will be mislabelled as an operator.
" Note that in Ada, assignment (:=) is not considered an operator.
"
syntax match adaAssignment		":="

" Section: Numbers, including floating point, exponents, and alternate bases. {{{1
"
syntax match   adaNumber		"\<\d[0-9_]*\(\.\d[0-9_]*\)\=\([Ee][+-]\=\d[0-9_]*\)\=\>"
syntax match   adaNumber		"\<\d\d\=#\x[0-9A-Fa-f_]*\(\.\x[0-9A-Fa-f_]*\)\=#\([Ee][+-]\=\d[0-9_]*\)\="

" Section: Identify leading numeric signs {{{1
"
" In "A-5" the "-" is an operator, " but in "A:=-5" the "-" is a sign. This
" handles "A3+-5" (etc.) correctly.  " This assumes that if you put a
" don't put a space after +/- when it's used " as an operator, you won't
" put a space before it either -- which is true " in code I've seen.
"
syntax match adaSign "[[:space:]<>=(,|:;&*/+-][+-]\d"lc=1,hs=s+1,he=e-1,me=e-1

" Section: Labels for the goto statement. {{{1
"
syntax region  adaLabel		start="<<"  end=">>"

" Section: Boolean Constants {{{1
" Boolean Constants.
syntax keyword adaBoolean	true false

" Section: Warn C/C++ {{{1
"
" Warn people who try to use C/C++ notation erroneously:
"
syntax match adaError "//"
syntax match adaError "/\*"
syntax match adaError "=="


" Section: Space Errors {{{1
"
if exists("g:ada_space_errors")
   if !exists("g:ada_no_trail_space_error")
       syntax match   adaSpaceError	 excludenl "\s\+$"
   endif
   if !exists("g:ada_no_tab_space_error")
      syntax match   adaSpaceError	 " \+\t"me=e-1
   endif
   if !exists("g:ada_all_tab_usage")
      syntax match   adaSpecial	 "\t"
   endif
endif

" Section: end {{{1
" Unless special ("end loop", "end if", etc.), "end" marks the end of a
" begin, package, task etc. Assiging it to adaEnd.
syntax match    adaEnd	/\<end\>/

syntax keyword  adaPreproc		 pragma

syntax keyword  adaRepeat	 exit for loop reverse while
syntax match    adaRepeat		   "\<end\s\+loop\>"

syntax keyword  adaStatement accept delay goto raise requeue return
syntax keyword  adaStatement terminate
syntax match    adaStatement  "\<abort\>"

" Section: Handle Ada's record keywords. {{{1
"
" 'record' usually starts a structure, but "with null record;" does not,
" and 'end record;' ends a structure.  The ordering here is critical -
" 'record;' matches a "with null record", so make it a keyword (this can
" match when the 'with' or 'null' is on a previous line).
" We see the "end" in "end record" before the word record, so we match that
" pattern as adaStructure (and it won't match the "record;" pattern).
"
syntax match adaStructure   "\<record\>"	contains=adaRecord
syntax match adaStructure   "\<end\s\+record\>"	contains=adaRecord
syntax match adaKeyword	    "\<record;"me=e-1

" Section: type classes {{{1
"
syntax keyword adaStorageClass	abstract access aliased array at constant delta
syntax keyword adaStorageClass	digits limited of private range tagged
syntax keyword adaStorageClass	interface synchronized
syntax keyword adaTypedef	subtype type

" Section: Conditionals {{{1
"
" "abort" after "then" is a conditional of its own.
"
syntax match    adaConditional  "\<then\>"
syntax match    adaConditional	"\<then\s\+abort\>"
syntax match    adaConditional	"\<else\>"
syntax match    adaConditional	"\<end\s\+if\>"
syntax match    adaConditional	"\<end\s\+case\>"
syntax match    adaConditional	"\<end\s\+select\>"
syntax keyword  adaConditional	if case select
syntax keyword  adaConditional	elsif when

" Section: other keywords {{{1
syntax match    adaKeyword	    "\<is\>" contains=adaRecord
syntax keyword  adaKeyword	    all do exception in new null out
syntax keyword  adaKeyword	    separate until overriding

" Section: begin keywords {{{1
"
" These keywords begin various constructs, and you _might_ want to
" highlight them differently.
"
syntax keyword  adaBegin	begin body declare entry generic
syntax keyword  adaBegin	protected renames task

syntax match    adaBegin	"\<function\>" contains=adaFunction
syntax match    adaBegin	"\<procedure\>" contains=adaProcedure
syntax match    adaBegin	"\<package\>" contains=adaPackage

if exists("ada_with_gnat_project_files")
   syntax keyword adaBegin	project
endif

" Section: with, use {{{1
"
if exists("ada_withuse_ordinary")
   " Don't be fancy. Display "with" and "use" as ordinary keywords in all cases.
   syntax keyword adaKeyword		with use
else
   " Highlight "with" and "use" clauses like C's "#include" when they're used
   " to reference other compilation units; otherwise they're ordinary keywords.
   " If we have vim 6.0 or later, we'll use its advanced pattern-matching
   " capabilities so that we won't match leading spaces.
   syntax match adaKeyword	"\<with\>"
   syntax match adaKeyword	"\<use\>"
   syntax match adaBeginWith	"^\s*\zs\(\(with\(\s\+type\)\=\)\|\(use\)\)\>" contains=adaInc
   syntax match adaSemiWith	";\s*\zs\(\(with\(\s\+type\)\=\)\|\(use\)\)\>" contains=adaInc
   syntax match adaInc		"\<with\>" contained contains=NONE
   syntax match adaInc		"\<with\s\+type\>" contained contains=NONE
   syntax match adaInc		"\<use\>" contained contains=NONE
   " Recognize "with null record" as a keyword (even the "record").
   syntax match adaKeyword	"\<with\s\+null\s\+record\>"
   " Consider generic formal parameters of subprograms and packages as keywords.
   syntax match adaKeyword	";\s*\zswith\s\+\(function\|procedure\|package\)\>"
   syntax match adaKeyword	"^\s*\zswith\s\+\(function\|procedure\|package\)\>"
endif

" Section: String and character constants. {{{1
"
syntax region  adaString	contains=@Spell start=+"+ skip=+""+ end=+"+ 
syntax match   adaCharacter "'.'"

" Section: Todo (only highlighted in comments) {{{1
"
syntax keyword adaTodo contained TODO FIXME XXX NOTE

" Section: Comments. {{{1
"
syntax region  adaComment 
    \ oneline 
    \ contains=adaTodo,adaLineError,@Spell
    \ start="--" 
    \ end="$"

" Section: line errors {{{1
"
" Note: Line errors have become quite slow with Vim 7.0
"
if exists("g:ada_line_errors")
    syntax match adaLineError "\(^.\{79}\)\@<=."  contains=ALL containedin=ALL
endif

" Section: syntax folding {{{1
"
"	Syntax folding is very tricky - for now I still suggest to use
"	indent folding
"
if exists("g:ada_folding") && g:ada_folding[0] == 's'
   if stridx (g:ada_folding, 'p') >= 0
      syntax region adaPackage
         \ start="\(\<package\s\+body\>\|\<package\>\)\s*\z(\k*\)"
         \ end="end\s\+\z1\s*;"
         \ keepend extend transparent fold contains=ALL
   endif
   if stridx (g:ada_folding, 'f') >= 0
      syntax region adaProcedure
         \ start="\<procedure\>\s*\z(\k*\)"
         \ end="\<end\>\s\+\z1\s*;"
         \ keepend extend transparent fold contains=ALL
      syntax region adaFunction
         \ start="\<procedure\>\s*\z(\k*\)"
         \ end="end\s\+\z1\s*;"
         \ keepend extend transparent fold contains=ALL
   endif
   if stridx (g:ada_folding, 'f') >= 0
      syntax region adaRecord
         \ start="\<is\s\+record\>"
         \ end="\<end\s\+record\>"
         \ keepend extend transparent fold contains=ALL
   endif
endif

" Section: The default methods for highlighting. Can be overridden later. {{{1
"
highlight def link adaCharacter	    Character
highlight def link adaComment	    Comment
highlight def link adaConditional   Conditional
highlight def link adaKeyword	    Keyword
highlight def link adaLabel	    Label
highlight def link adaNumber	    Number
highlight def link adaSign	    Number
highlight def link adaOperator	    Operator
highlight def link adaPreproc	    PreProc
highlight def link adaRepeat	    Repeat
highlight def link adaSpecial	    Special
highlight def link adaStatement	    Statement
highlight def link adaString	    String
highlight def link adaStructure	    Structure
highlight def link adaTodo	    Todo
highlight def link adaType	    Type
highlight def link adaTypedef	    Typedef
highlight def link adaStorageClass  StorageClass
highlight def link adaBoolean	    Boolean
highlight def link adaException	    Exception
highlight def link adaAttribute	    Tag
highlight def link adaInc	    Include
highlight def link adaError	    Error
highlight def link adaSpaceError    Error
highlight def link adaLineError	    Error
highlight def link adaBuiltinType   Type
highlight def link adaAssignment    Special

" Subsection: Begin, End {{{2
"
if exists ("ada_begin_preproc")
   " This is the old default display:
   highlight def link adaBegin   PreProc
   highlight def link adaEnd     PreProc
else
   " This is the new default display:
   highlight def link adaBegin   Keyword
   highlight def link adaEnd     Keyword
endif


" Section: formatoptions {{{1
"
setlocal formatoptions+=ron

" Section: sync {{{1
"
" We don't need to look backwards to highlight correctly;
" this speeds things up greatly.
syntax sync minlines=1 maxlines=1

finish " 1}}}

"------------------------------------------------------------------------------
"   Copyright (C) 2006	Martin Krischik
"
"   Vim is Charityware - see ":help license" or uganda.txt for licence details.
"------------------------------------------------------------------------------
"vim: textwidth=78 nowrap tabstop=8 shiftwidth=3 softtabstop=3 noexpandtab
"vim: foldmethod=marker
