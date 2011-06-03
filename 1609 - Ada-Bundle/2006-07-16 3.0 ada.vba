" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
ada_options.vim
47
"------------------------------------------------------------------------------
"  Description: Options setable by the Ada plugin
"          $Id: ada_options.vim 306 2006-07-16 15:06:00Z krischik $
"    Copyright: Copyright (C) 2006 Martin Krischik
"   Maintainer:	Martin Krischik
"      $Author: krischik $
"        $Date: 2006-07-16 17:06:00 +0200 (So, 16 Jul 2006) $
"      Version: 2.0 
"    $Revision: 306 $
"     $HeadURL: https://svn.sourceforge.net/svnroot/gnuada/trunk/tools/vim/ada_options.vim $
"      History:	24.05.2006 MK Unified Headers
"		16.07.2006 MK Ada-Mode as vim-ball
"	 Usage: copy content into your .vimrc and change options to your
"		likeing.
"    Help Page: ada-options
"------------------------------------------------------------------------------

    let g:ada_extended_tagging		= 'list'
    let g:ada_gnat_extensions		= 1
    let g:ada_standard_types		= 1
    let g:ada_space_errors		= 1
    let g:ada_line_errors		= 1
    let g:ada_folding			= 1
    let g:ada_abbrev			= 1
    let g:ada_with_gnat_project_files	= 1
    let g:ada_rainbow_color		= 1
    let g:ada_default_compiler          = 'gnat'

    let g:backup_directory		= '.backups'
    let g:backup_purge			= 10

finish

:.+2,.+10 MkVimball! ada

ada_options.vim
autoload/ada.vim
autoload/adacomplete.vim
compiler/gnat.vim
ftdetect/ada.vim
ftplugin/ada.vim
indent/ada.vim
syntax/ada.vim
doc/ada.txt

" vim: textwidth=0 nowrap tabstop=8 shiftwidth=4 softtabstop=4 noexpandtab
" vim: filetype=vim encoding=latin1 fileformat=unix
autoload/ada.vim
346
"------------------------------------------------------------------------------
"  Description: Perform Ada specific completion & tagging.
"     Language: Ada (2005)
"          $Id: ada.vim 306 2006-07-16 15:06:00Z krischik $
"   Maintainer:	Martin Krischik
"               Neil Bird <neil@fnxweb.com>
"      $Author: krischik $
"        $Date: 2006-07-16 17:06:00 +0200 (So, 16 Jul 2006) $
"      Version: 2.1 
"    $Revision: 306 $
"     $HeadURL: https://svn.sourceforge.net/svnroot/gnuada/trunk/tools/vim/autoload/ada.vim $
"      History: 24.05.2006 MK Unified Headers
"               26.05.2006 MK ' should not be in iskeyword.
"		16.07.2006 MK Ada-Mode as vim-ball
"    Help Page: ft-ada-functions
"------------------------------------------------------------------------------

if exists ('g:loaded_ada_autoload') || version < 700
   finish
else
   " Extract current Ada word across multiple lines
   " AdaWord ([line, column])\
   let g:loaded_ada_autoload  = 1
   let g:AdaWordRegex         = '\a\w*\(\_s*\.\_s*\a\w*\)*'
   let g:ada#Comment          = "\\v^(\"[^\"]*\"|'.'|[^\"']){-}\\zs\\s*--.*"
   let g:ada#Keywords         = []

   "--------------------------------------------------------------------------
   "
   "   add  Ada keywords
   "
   for Item in ['abort', 'else', 'new', 'return', 'abs', 'elsif', 'not', 'reverse', 'abstract', 'end', 'null', 'accept', 'entry', 'select', 'access', 'exception', 'of', 'separate', 'aliased', 'exit', 'or', 'subtype', 'all', 'others', 'synchronized', 'and', 'for', 'out', 'array', 'function', 'overriding', 'tagged', 'at', 'task', 'generic', 'package', 'terminate', 'begin', 'goto', 'pragma', 'then', 'body', 'private', 'type', 'if', 'procedure', 'case', 'in', 'protected', 'until', 'constant', 'interface', 'use', 'is', 'raise', 'declare', 'range', 'when', 'delay', 'limited', 'record', 'while', 'delta', 'loop', 'rem', 'with', 'digits', 'renames', 'do', 'mod', 'requeue', 'xor']
       let g:ada#Keywords += [{
               \ 'word':  Item,
               \ 'menu':  'keyword',
               \ 'info':  'Ada keyword.',
               \ 'kind':  'k',
               \ 'icase': 1}]
   endfor
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

   "--------------------------------------------------------------------------
   "
   "   add  standart exception
   "
   for Item in ['Constraint_Error', 'Program_Error', 'Storage_Error', 'Tasking_Error', 'Status_Error', 'Mode_Error', 'Name_Error', 'Use_Error', 'Device_Error', 'End_Error', 'Data_Error', 'Layout_Error', 'Length_Error', 'Pattern_Error', 'Index_Error', 'Translation_Error', 'Time_Error', 'Argument_Error', 'Tag_Error', 'Picture_Error', 'Terminator_Error', 'Conversion_Error', 'Pointer_Error', 'Dereference_Error', 'Update_Error']
       let g:ada#Keywords += [{
               \ 'word':  Item,
               \ 'menu':  'exception',
               \ 'info':  'Ada standart exception.',
               \ 'kind':  'x',
               \ 'icase': 1}]
   endfor
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

   "--------------------------------------------------------------------------
   "   
   "   add buildin types
   "
   for Item in ['Boolean', 'Integer', 'Natural', 'Positive', 'Float', 'Character', 'Wide_Character', 'Wide_Wide_Character', 'String', 'Wide_String', 'Wide_Wide_String', 'Duration']
       let g:ada#Keywords += [{
               \ 'word':  Item,
               \ 'menu':  'type',
               \ 'info':  'Ada buildin type.',
               \ 'kind':  't',
               \ 'icase': 1}]
   endfor
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

   "--------------------------------------------------------------------------
   "
   "   add  Ada Attributes
   "
   for Item in ['''Access', '''Address', '''Adjacent', '''Aft', '''Alignment', '''Base', '''Bit_Order', '''Body_Version', '''Callable', '''Caller', '''Ceiling', '''Class', '''Component_Size', '''Compose', '''Constrained', '''Copy_Sign', '''Count', '''Definite', '''Delta', '''Denorm', '''Digits', '''Emax', '''Exponent', '''External_Tag', '''Epsilon', '''First', '''First_Bit', '''Floor', '''Fore', '''Fraction', '''Identity', '''Image', '''Input', '''Large', '''Last', '''Last_Bit', '''Leading_Part', '''Length', '''Machine', '''Machine_Emax', '''Machine_Emin', '''Machine_Mantissa', '''Machine_Overflows', '''Machine_Radix', '''Machine_Rounding', '''Machine_Rounds', '''Mantissa', '''Max', '''Max_Size_In_Storage_Elements', '''Min', '''Mod', '''Model', '''Model_Emin', '''Model_Epsilon', '''Model_Mantissa', '''Model_Small', '''Modulus', '''Output', '''Partition_ID', '''Pos', '''Position', '''Pred', '''Priority', '''Range', '''Read', '''Remainder', '''Round', '''Rounding', '''Safe_Emax', '''Safe_First', '''Safe_Large', '''Safe_Last', '''Safe_Small', '''Scale', '''Scaling', '''Signed_Zeros', '''Size', '''Small', '''Storage_Pool', '''Storage_Size', '''Stream_Size', '''Succ', '''Tag', '''Terminated', '''Truncation', '''Unbiased_Rounding', '''Unchecked_Access', '''Val', '''Valid', '''Value', '''Version', '''Wide_Image', '''Wide_Value', '''Wide_Wide_Image', '''Wide_Wide_Value', '''Wide_Wide_Width', '''Wide_Width', '''Width', '''Write']
       let g:ada#Keywords += [{
               \ 'word':  Item,
               \ 'menu':  'attribute',
               \ 'info':  'Ada attribute.',
               \ 'kind':  'a',
               \ 'icase': 1}]
   endfor
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

   "--------------------------------------------------------------------------
   "
   "   add  Ada Pragmas
   "
   for Item in ['All_Calls_Remote', 'Assert', 'Assertion_Policy', 'Asynchronous', 'Atomic', 'Atomic_Components', 'Attach_Handler', 'Controlled', 'Convention', 'Detect_Blocking', 'Discard_Names', 'Elaborate', 'Elaborate_All', 'Elaborate_Body', 'Export', 'Import', 'Inline', 'Inspection_Point', 'Interface (Obsolescent)', 'Interrupt_Handler', 'Interrupt_Priority', 'Linker_Options', 'List', 'Locking_Policy', 'Memory_Size (Obsolescent)', 'No_Return', 'Normalize_Scalars', 'Optimize', 'Pack', 'Page', 'Partition_Elaboration_Policy', 'Preelaborable_Initialization', 'Preelaborate', 'Priority', 'Priority_Specific_Dispatching', 'Profile', 'Pure', 'Queueing_Policy', 'Relative_Deadline', 'Remote_Call_Interface', 'Remote_Types', 'Restrictions', 'Reviewable', 'Shared (Obsolescent)', 'Shared_Passive', 'Storage_Size', 'Storage_Unit (Obsolescent)', 'Suppress', 'System_Name (Obsolescent)', 'Task_Dispatching_Policy', 'Unchecked_Union', 'Unsuppress', 'Volatile', 'Volatile_Components']
       let g:ada#Keywords += [{
               \ 'word':  Item,
               \ 'menu':  'pragma',
               \ 'info':  'Ada pragma.',
               \ 'kind':  'p',
               \ 'icase': 1}]
   endfor
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

   "--------------------------------------------------------------------------
   "
   "
   function ada#Word (...)
      if a:0 > 1
         let linenr = a:1
         let colnr  = a:2 - 1
      else
         let linenr = line('.')
         let colnr  = col('.') - 1
      endif

      let line = substitute( getline(linenr), s:AdaComment, '', '' )

      " Cope with tag searching for items in comments; if we are, don't loop
      " backards looking for previous lines
      if colnr > strlen(line)
         " We were in a comment
         let line = getline(linenr)
         let search_prev_lines = 0
      else
         let search_prev_lines = 1
      endif

      " Go backwards until we find a match (Ada ID) that *doesn't* include our
      " location - i.e., the previous ID. This is because the current 'correct'
      " match will toggle matching/not matching as we traverse characters
      " backwards. Thus, we have to find the previous unrelated match, exclude
      " it, then use the next full match (ours).
      " Remember to convert vim column 'colnr' [1..n] to string offset [0..(n-1)]
      " ... but start, here, one after the required char.
      let newcol = colnr + 1
      while 1
         let newcol = newcol - 1
         if newcol < 0
            " Have to include previous line from file
            let linenr = linenr - 1
            if linenr < 1  ||  !search_prev_lines
               " Start of file or matching in a comment
               let linenr = 1
               let newcol = 0
               let ourmatch = match( line, s:AdaWordRegex )
               break
            endif
            " Get previous line, and prepend it to our search string
            let newline = substitute( getline(linenr), s:AdaComment, '', '' )
            let newcol  = strlen(newline) - 1
            let colnr   = colnr + newcol
            let line    = newline . line
         endif
         " Check to see if this is a match excluding 'us'
         let mend = newcol + matchend( strpart(line,newcol), s:AdaWordRegex ) - 1
         if mend >= newcol  &&  mend < colnr
            " Yes
            let ourmatch = mend+1 + match( strpart(line,mend+1), s:AdaWordRegex )
            break
         endif
      endwhile

      " Got anything?
      if ourmatch < 0
         return ''
      else
         let line = strpart( line, ourmatch)
      endif

      " Now simply add further lines until the match gets no bigger
      let matchstr = matchstr (line, s:AdaWordRegex)
      let lastline  = line('$')
      let linenr    = line('.') + 1
      while linenr <= lastline
         let lastmatch = matchstr
         let line = line . substitute (getline (linenr), s:AdaComment, '', '')
         let matchstr = matchstr (line, s:AdaWordRegex)
         if matchstr == lastmatch
            break
         endif
      endwhile

      " Strip whitespace & return
      return substitute (matchstr, '\s\+', '', 'g')
   endfunction ada#Word

   "--------------------------------------------------------------------------
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

      echo "Searching for " . l:Tag_Word

      let l:Tag_List = taglist (l:Tag_Word)
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
            \ l:Tag_Item['cmd']      . '|' .
            \ l:Tag_Item['kind']     . "\t" .
            \ l:Tag_Item['name'] ]
      endfor
      set errorformat=%f\|%l\|%m
      cexpr l:Error_List
      cwindow
   endfunction ada#List_Tag

   "--------------------------------------------------------------------------
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

   "--------------------------------------------------------------------------
   "
   " Backspace at end of line after auto-inserted commentstring '-- ' wipes it
   "
   function ada#Insert_Backspace ()
      let line = getline ('.')
      if col ('.') > strlen (line) &&
       \ match (line, '-- $') != -1 && 
       \ match (&comments,'--') != -1
         return "\<bs>\<bs>\<bs>"
      else
         return "\<bs>"
      endif

      return
   endfunction ada#InsertBackspace

   "--------------------------------------------------------------------------
   "
   " Word completion (^N/^R/^X^]) - force '.' inclusion
   function ada#Completion (cmd)
      set iskeyword+=46
      return a:cmd . "\<C-R>=ada#Completion_End ()\<CR>"
   endfunction ada#Completion

   function ada#Completion_End ()
      set iskeyword-=46
      return ''
   endfunction ada#Completion_End

   lockvar  g:AdaWordRegex
   lockvar  g:ada#Comment
   lockvar! g:ada#Keywords

   finish
endif

"------------------------------------------------------------------------------
"   Copyright (C) 2006  Martin Krischik
"
"   This program is free software; you can redistribute it and/or
"   modify it under the terms of the GNU General Public License
"   as published by the Free Software Foundation; either version 2
"   of the License, or (at your option) any later version.
"   
"   This program is distributed in the hope that it will be useful,
"   but WITHOUT ANY WARRANTY; without even the implied warranty of
"   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
"   GNU General Public License for more details.
"   
"   You should have received a copy of the GNU General Public License
"   along with this program; if not, write to the Free Software
"   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
"------------------------------------------------------------------------------
" vim: textwidth=78 wrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab
" vim: filetype=vim encoding=latin1 fileformat=unix
autoload/adacomplete.vim
114
"------------------------------------------------------------------------------
"  Description: Vim Ada omnicompletion file
"     Language:	Ada (2005)
"          $Id: adacomplete.vim 306 2006-07-16 15:06:00Z krischik $
"   Maintainer:	Martin Krischik 
"      $Author: krischik $
"        $Date: 2006-07-16 17:06:00 +0200 (So, 16 Jul 2006) $
"      Version: 3.0 
"    $Revision: 306 $
"     $HeadURL: https://svn.sourceforge.net/svnroot/gnuada/trunk/tools/vim/autoload/adacomplete.vim $
"      History: 24.05.2006 MK Unified Headers
"               26.05.2006 MK improved search for begin of word.
"		16.07.2006 MK Ada-Mode as vim-ball
"    Help Page: ft-ada-omni
"------------------------------------------------------------------------------

" Set completion with CTRL-X CTRL-O to autoloaded function.  This check is in
" place in case this script is sourced directly instead of using the autoload
" feature.  Do not set the option if already set since this results in an E117
" warning.
"
if exists ('+omnifunc') && &omnifunc == ""
    setlocal omnifunc=adacomplete#Complete
endif

if exists ('g:loaded_syntax_completion') || version < 700
    finish
else
   let g:loaded_syntax_completion = 20

   "--------------------------------------------------------------------------
   "
   "
   " This function is used for the 'omnifunc' option.
   "
   function! adacomplete#Complete(findstart, base)
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
         "
         "  search tag file for matches
         "
         let l:Tag_List = taglist (l:Pattern)
         " 
         " add symbols
         "
         for Tag_Item in l:Tag_List
	    if l:Tag_Item['kind'] == ''
	       let l:Tag_Item['kind'] = 's'
	    endif
	    let l:Match_Item = {
	       \ 'word':  l:Tag_Item['name'],
	       \ 'menu':  l:Tag_Item['filename'],
	       \ 'info':  "Symbol from file " . l:Tag_Item['filename'] . " line " . l:Tag_Item['cmd'],
	       \ 'kind':  l:Tag_Item['kind'],
	       \ 'icase': 1}
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

   finish
endif

"------------------------------------------------------------------------------
"   Copyright (C) 2006  Martin Krischik
"
"   This program is free software; you can redistribute it and/or
"   modify it under the terms of the GNU General Public License
"   as published by the Free Software Foundation; either version 2
"   of the License, or (at your option) any later version.
"   
"   This program is distributed in the hope that it will be useful,
"   but WITHOUT ANY WARRANTY; without even the implied warranty of
"   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
"   GNU General Public License for more details.
"   
"   You should have received a copy of the GNU General Public License
"   along with this program; if not, write to the Free Software
"   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
"------------------------------------------------------------------------------
" vim: textwidth=78 wrap tabstop=8 shiftwidth=3 softtabstop=3 noexpandtab
" vim: filetype=vim encoding=latin1 fileformat=unix
compiler/gnat.vim
72
"------------------------------------------------------------------------------
"  Description: Vim Ada/GNAT compiler file
"     Language: Ada (GNAT)
"          $Id: gnat.vim 306 2006-07-16 15:06:00Z krischik $
"    Copyright: Copyright (C) 2006 Martin Krischik
"   Maintainer:	Martin Krischik
"      $Author: krischik $
"        $Date: 2006-07-16 17:06:00 +0200 (So, 16 Jul 2006) $
"      Version: 2.0 
"    $Revision: 306 $
"     $HeadURL: https://svn.sourceforge.net/svnroot/gnuada/trunk/tools/vim/compiler/gnat.vim $
"      History: 24.05.2006 MK Unified Headers
"		16.07.2006 MK Ada-Mode as vim-ball
"    Help Page: compiler-gnat
"------------------------------------------------------------------------------

if exists("current_compiler") || version < 700
    finish
else
    function s:Make () dict
	let &makeprg	    = eval (self.Make_Command)
	let &errorformat    = self.Error_Format
	wall
	make
	copen
	set wrap
	wincmd W
    endfunction Make 

    function s:Find () dict
	execute "!" . eval (self.Find_Command)
    endfunction Make 
    
    let g:gnat = {
	\ 'Make'	    : function ('s:Make'),
	\ 'Find'	    : function ('s:Find'),
	\ 'Project_File'    : 'default.gpr',
	\ 'Make_Command'    : '"gnat make -P " . gnat.Project_File . " "',
	\ 'Find_Program'    : '"gnat find -P " . gnat.Project_File . " "',
	\ 'Tags_Command'    : '"gnat xref -P " . gnat.Project_File . " -v  *.AD*"',
	\ 'Error_Format'    : '%f:%l:%c: %trror: %m,'   .
			    \ '%f:%l:%c: %tarning: %m,' .
			    \ '%f:%l:%c: (%ttyle) %m'}

    let &makeprg	= eval (g:gnat.Make_Command)
    let &errorformat    = g:gnat.Error_Format

    command Make	:call g:gnat.Make ()

    let current_compiler = "gnat"
    finish    
endif

"------------------------------------------------------------------------------
"   Copyright (C) 2006  Martin Krischik
"
"   This program is free software; you can redistribute it and/or
"   modify it under the terms of the GNU General Public License
"   as published by the Free Software Foundation; either version 2
"   of the License, or (at your option) any later version.
"   
"   This program is distributed in the hope that it will be useful,
"   but WITHOUT ANY WARRANTY; without even the implied warranty of
"   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
"   GNU General Public License for more details.
"   
"   You should have received a copy of the GNU General Public License
"   along with this program; if not, write to the Free Software
"   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
"------------------------------------------------------------------------------
" vim: textwidth=78 wrap tabstop=8 shiftwidth=4 softtabstop=4 noexpandtab
" vim: filetype=vim encoding=latin1 fileformat=unix
ftdetect/ada.vim
47
"------------------------------------------------------------------------------
"  Description: Vim Ada filetype detection file
"     Language: Ada (2005)
"          $Id: ada.vim 306 2006-07-16 15:06:00Z krischik $
"    Copyright: Copyright (C) 2006 Martin Krischik
"   Maintainer:	Martin Krischik 
"      $Author: krischik $
"        $Date: 2006-07-16 17:06:00 +0200 (So, 16 Jul 2006) $
"      Version: 3.0
"    $Revision: 306 $
"     $HeadURL: https://svn.sourceforge.net/svnroot/gnuada/trunk/tools/vim/ftdetect/ada.vim $
"      History:	24.05.2006 MK Unified Headers
"		16.07.2006 MK Ada-Mode as vim-ball
"    Help Page: ft-ada-plugin
"------------------------------------------------------------------------------

if exists("s:loaded_ftdetect_ada")
    finish
else
    let s:loaded_ftdetect_ada=1
    if has("vms")
        autocmd BufNewFile,BufRead *.gpr,*.ada_m,*.adc setfiletype ada
    else
        autocmd BufNewFile,BufRead *.gpr setfiletype ada
    endif
    finish
endif

"------------------------------------------------------------------------------
"   Copyright (C) 2005,2006  Martin Krischik
"
"   This program is free software; you can redistribute it and/or
"   modify it under the terms of the GNU General Public License
"   as published by the Free Software Foundation; either version 2
"   of the License, or (at your option) any later version.
"   
"   This program is distributed in the hope that it will be useful,
"   but WITHOUT ANY WARRANTY; without even the implied warranty of
"   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
"   GNU General Public License for more details.
"   
"   You should have received a copy of the GNU General Public License
"   along with this program; if not, write to the Free Software
"   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
"------------------------------------------------------------------------------
" vim: textwidth=78 nowrap tabstop=8 shiftwidth=4 softtabstop=4 expandtab
" vim: filetype=vim encoding=utf8 fileformat=unix
ftplugin/ada.vim
139
"------------------------------------------------------------------------------
"  Description: Perform Ada specific completion & tagging.
"     Language: Ada (2005)
"          $Id: ada.vim 306 2006-07-16 15:06:00Z krischik $
"   Maintainer:	Martin Krischik
"               Neil Bird <neil@fnxweb.com>
"      $Author: krischik $
"        $Date: 2006-07-16 17:06:00 +0200 (So, 16 Jul 2006) $
"      Version: 3.0 
"    $Revision: 306 $
"     $HeadURL: https://svn.sourceforge.net/svnroot/gnuada/trunk/tools/vim/ftplugin/ada.vim $
"      History: 24.05.2006 MK Unified Headers
"               26.05.2006 MK ' should not be in iskeyword.
"		16.07.2006 MK Ada-Mode as vim-ball
"    Help Page: ft-ada-plugin
"------------------------------------------------------------------------------
" Provides mapping overrides for tag jumping that figure out the current
" Ada object and tag jump to that, not the 'simple' vim word.
" Similarly allows <Ctrl-N> matching of full-length ada entities from tags.
"------------------------------------------------------------------------------

" Only do this when not done yet for this buffer
if exists ("b:did_ftplugin") || version < 700
    finish
else
   " Don't load another plugin for this buffer
   let b:did_ftplugin = 1
   
   " Temporarily set cpoptions to ensure the script loads OK
   let s:cpoptions = &cpoptions
   set cpoptions-=C

   " Ada comments
   setlocal comments+=O:--
   setlocal complete=.,w,b,u,t,i
   setlocal completeopt=menuone

   if exists ('&omnifunc')
       setlocal omnifunc=adacomplete#Complete
   endif

   if exists ("g:ada_extended_tagging")
      " Make local tag mappings for this buffer (if not already set)
      if g:ada_extended_tagging = 'jump' then
         if mapcheck('<C-]>','n') == ''
            nnoremap <unique> <buffer> <C-]>    :call ada#Jump_Tag ('', 'tjump')<cr>
         endif
         if mapcheck('g<C-]>','n') == ''
            nnoremap <unique> <buffer> g<C-]>   :call ada#Jump_Tag ('','stjump')<cr>
         endif
      elseif g:ada_extended_tagging = 'list'
         if mapcheck('<C-]>','n') == ''
            nnoremap <unique> <buffer> <C-]>    :call ada#List_Tag ('')<cr>
         endif
         if mapcheck('g<C-]>','n') == ''
            nnoremap <unique> <buffer> g<C-]>   :call ada#List_Tag ('')<cr>
         endif
      endif
   endif

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

   " Only do this when not done yet for this buffer & matchit is used
   if ! exists ("b:match_words")  &&  exists ("loaded_matchit")
      " The following lines enable the macros/matchit.vim plugin for
      " Ada-specific extended matching with the % key.
      let s:notend = '\%(\<end\s\+\)\@<!'
      let b:match_words=
      \ s:notend . '\<if\>:\<elsif\>:\<else\>:\<end\>\s\+\<if\>,' .
      \ s:notend . '\<case\>:\<when\>:\<end\>\s\+\<case\>,' .
      \ '\%(\<while\>.*\|\<for\>.*\|'.s:notend.'\)\<loop\>:\<end\>\s\+\<loop\>,' .
      \ '\%(\<interface\>.*\|\<synchronized\>.*\|\<overriding\>,' .
      \ '\%(\<do\>\|\<begin\>\):\<exception\>:\<end\>\s*\%($\|[;A-Z]\),' .
      \ s:notend . '\<record\>:\<end\>\s\+\<record\>'
   endif

   if exists ("g:ada_default_compiler")
      execute "autocmd FileType ada compiler " . g:ada_default_compiler
   endif

   function s:Map_Menu (Text, Keys, Command)
      execute 
        \ "41menu " .
        \ "Plugin.Ada." . escape(a:Text, ' ') .
        \ "<Tab>"       . escape(g:mapleader . "a" . a:Keys , '\') .
        \ " :"          . a:Command . "<CR>"
      execute 
        \ "nnoremap " .
        \ escape(g:mapleader . "a" . a:Keys , '\') .
        \" :" . a:Command . "<CR>"
      execute 
        \ "inoremap " .
        \ escape(g:mapleader . "a" . a:Keys , '\') .
        \" <C-O>:" . a:Command . "<CR>"
      return
   endfunction

   call s:Map_Menu ('Tag List', 'l', 'call ada#List_Tag ()')
   call s:Map_Menu ('Tag Jump', 'j', 'call ada#Jump_Tag ()')

   delfunction s:Map_Menu

   " Reset cpoptions
   let &cpoptions = s:cpoptions
   unlet s:cpoptions

   finish
endif

"------------------------------------------------------------------------------
"   Copyright (C) 2006  Martin Krischik
"
"   This program is free software; you can redistribute it and/or
"   modify it under the terms of the GNU General Public License
"   as published by the Free Software Foundation; either version 2
"   of the License, or (at your option) any later version.
"   
"   This program is distributed in the hope that it will be useful,
"   but WITHOUT ANY WARRANTY; without even the implied warranty of
"   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
"   GNU General Public License for more details.
"   
"   You should have received a copy of the GNU General Public License
"   along with this program; if not, write to the Free Software
"   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
"------------------------------------------------------------------------------
" vim: textwidth=78 nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab
" vim: filetype=vim encoding=latin1 fileformat=unix
indent/ada.vim
301
"------------------------------------------------------------------------------
"  Description: Vim Ada indent file
"     Language:	Ada (2005)
"          $Id: ada.vim 306 2006-07-16 15:06:00Z krischik $
"    Copyright: Copyright (C) 2006 Martin Krischik
"   Maintainer:	Martin Krischik
"               Neil Bird <neil@fnxweb.com>
"      $Author: krischik $
"        $Date: 2006-07-16 17:06:00 +0200 (So, 16 Jul 2006) $
"      Version: 3.0
"    $Revision: 306 $
"     $HeadURL: https://svn.sourceforge.net/svnroot/gnuada/trunk/tools/vim/indent/ada.vim $
"      History: 24.05.2006 MK Unified Headers
"		16.07.2006 MK Ada-Mode as vim-ball
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
else
   let b:did_indent = 1

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


   " Find correct indent of a new line based upon what went before
   function GetAdaIndent()
      " Find a non-blank line above the current line.
      let lnum = prevnonblank(v:lnum - 1)
      let ind = indent(lnum)
      let package_line = 0

      " Get previous non-blank/non-comment-only/non-cpp line
      while 1
         let line = substitute( getline(lnum), ada#Comment, '', '' )
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

   finish
endif

"------------------------------------------------------------------------------
"   Copyright (C) 2006  Martin Krischik
"
"   This program is free software; you can redistribute it and/or
"   modify it under the terms of the GNU General Public License
"   as published by the Free Software Foundation; either version 2
"   of the License, or (at your option) any later version.
"   
"   This program is distributed in the hope that it will be useful,
"   but WITHOUT ANY WARRANTY; without even the implied warranty of
"   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
"   GNU General Public License for more details.
"   
"   You should have received a copy of the GNU General Public License
"   along with this program; if not, write to the Free Software
"   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
"------------------------------------------------------------------------------
" vim: textwidth=78 wrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab
" vim: filetype=vim encoding=latin1 fileformat=unix
syntax/ada.vim
333
"------------------------------------------------------------------------------
"  Description: Vim Ada syntax file
"     Language:	Ada (2005)
"          $Id: ada.vim 306 2006-07-16 15:06:00Z krischik $
"    Copyright: Copyright (C) 2006 Martin Krischik
"   Maintainer:	Martin Krischik 
"               David A. Wheeler <dwheeler@dwheeler.com>
"               Simon Bradley <simon.bradley@pitechnology.com>
" Contributors: Preben Randhol.
"      $Author: krischik $
"        $Date: 2006-07-16 17:06:00 +0200 (So, 16 Jul 2006) $
"      Version: 3.0 
"    $Revision: 306 $
"     $HeadURL: https://svn.sourceforge.net/svnroot/gnuada/trunk/tools/vim/syntax/ada.vim $
"               http://www.dwheeler.com/vim
"      History: 24.05.2006 MK Unified Headers
"               26.05.2006 MK ' should not be in iskeyword.
"		16.07.2006 MK Ada-Mode as vim-ball
"    Help Page: help ft-ada-syntax
"------------------------------------------------------------------------------
" The formal spec of Ada 2005 (ARM) is the "Ada 2005 Reference Manual".
" For more Ada 2005 info, see http://www.gnuada.org and http://www.adapower.com.
"
" This vim syntax file works on vim 5.6, 5.7, 5.8 6.x and 7.0
" It implements Bram Moolenaar's April 25, 2001 recommendations to make
" the syntax file maximally portable across different versions of vim.
" If vim 6.0+ is available,
" this syntax file takes advantage of the vim 6.0 advanced pattern-matching
" functions to avoid highlighting uninteresting leading spaces in
" some expressions containing "with" and "use".
"------------------------------------------------------------------------------

if exists("b:current_syntax") || version < 700
    finish
else
    " Ada is entirely case-insensitive.
    syntax case ignore
    setlocal nosmartcase
    setlocal ignorecase

    " Highlighting commands.  There are 72 reserved words in total in Ada2005.
    " Some keywords are used in more than one way. For example:
    " 1. "end" is a general keyword, but "end if" ends a Conditional.
    " 2. "then" is a conditional, but "and then" is an operator.

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

    syntax keyword  adaLabel	others

    syntax keyword  adaOperator	abs mod not rem xor
    syntax match    adaOperator	"\<and\>"
    syntax match    adaOperator	"\<and\s\+then\>"
    syntax match    adaOperator	"\<or\>"
    syntax match    adaOperator	"\<or\s\+else\>"
    syntax match    adaOperator	"[-+*/<>&]"
    syntax keyword  adaOperator	**
    syntax match    adaOperator	"[/<>]="
    syntax keyword  adaOperator	=>
    syntax match    adaOperator	"\.\."
    syntax match    adaOperator	"="

    " Handle the box, <>, specially:
    syntax keyword  adaSpecial      <>

    if exists("g:ada_rainbow_color")
        syntax match    adaSpecial	 "[:;.,]"
        runtime plugin/Rainbow_Parenthsis.vim
    else
        syntax match    adaSpecial	 "[:;().,]"
    endif

    " We won't map "adaAssignment" by default, but we need to map ":=" to
    " something or the "=" inside it will be mislabelled as an operator.
    " Note that in Ada, assignment (:=) is not considered an operator.
    syntax match adaAssignment		":="

    " Numbers, including floating point, exponents, and alternate bases.
    syntax match   adaNumber		"\<\d[0-9_]*\(\.\d[0-9_]*\)\=\([Ee][+-]\=\d[0-9_]*\)\=\>"
    syntax match   adaNumber		"\<\d\d\=#\x[0-9A-Fa-f_]*\(\.\x[0-9A-Fa-f_]*\)\=#\([Ee][+-]\=\d[0-9_]*\)\="

    " Identify leading numeric signs. In "A-5" the "-" is an operator,
    " but in "A:=-5" the "-" is a sign. This handles "A3+-5" (etc.) correctly.
    " This assumes that if you put a don't put a space after +/- when it's used
    " as an operator, you won't put a space before it either -- which is true
    " in code I've seen.
    syntax match adaSign "[[:space:]<>=(,|:;&*/+-][+-]\d"lc=1,hs=s+1,he=e-1,me=e-1

    " Labels for the goto statement.
    syntax region  adaLabel		start="<<"  end=">>"

    " Boolean Constants.
    syntax keyword adaBoolean	true false

    " Warn people who try to use C/C++ notation erroneously:
    syntax match adaError "//"
    syntax match adaError "/\*"
    syntax match adaError "=="


    if exists("g:ada_space_errors")
      if !exists("g:ada_no_trail_space_error")
        syntax match   adaSpaceError     excludenl "\s\+$"
      endif
      if !exists("g:ada_no_tab_space_error")
        syntax match   adaSpaceError     " \+\t"me=e-1
      endif
      if !exists("g:ada_all_tab_usage")
        syntax match   adaSpecial        "\t"
      endif  
    endif

    " Unless special ("end loop", "end if", etc.), "end" marks the end of a
    " begin, package, task etc. Assiging it to adaEnd.
    syntax match    adaEnd	"\<end\>"

    syntax keyword adaPreproc		pragma

    syntax keyword adaRepeat	exit for loop reverse while
    syntax match adaRepeat		"\<end\s\+loop\>"

    syntax keyword adaStatement	accept delay goto raise requeue return
    syntax keyword adaStatement	terminate
    syntax match adaStatement	"\<abort\>"

    " Handle Ada's record keywords.
    " 'record' usually starts a structure, but "with null record;" does not,
    " and 'end record;' ends a structure.  The ordering here is critical -
    " 'record;' matches a "with null record", so make it a keyword (this can
    " match when the 'with' or 'null' is on a previous line).
    " We see the "end" in "end record" before the word record, so we match that
    " pattern as adaStructure (and it won't match the "record;" pattern).
    syntax match adaStructure	 "\<record\>"
    syntax match adaStructure	 "\<end\s\+record\>"
    syntax match adaKeyword		 "\<record;"me=e-1

    syntax keyword adaStorageClass	abstract access aliased array at constant delta
    syntax keyword adaStorageClass	digits limited of private range tagged
    syntax keyword adaStorageClass	interface synchronized
    syntax keyword adaTypedef	subtype type

    " Conditionals. "abort" after "then" is a conditional of its own.
    syntax match    adaConditional  "\<then\>"
    syntax match    adaConditional	"\<then\s\+abort\>"
    syntax match    adaConditional	"\<else\>"
    syntax match    adaConditional	"\<end\s\+if\>"
    syntax match    adaConditional	"\<end\s\+case\>"
    syntax match    adaConditional	"\<end\s\+select\>"
    syntax keyword  adaConditional	if case select
    syntax keyword  adaConditional	elsif when

    syntax keyword  adaKeyword      all do exception in is new null out
    syntax keyword  adaKeyword      separate until overriding

    " These keywords begin various constructs, and you _might_ want to
    " highlight them differently.
    syntax keyword  adaBegin        begin body declare entry function generic 
    syntax keyword  adaBegin	package procedure protected renames task  

    if exists("ada_with_gnat_project_files")
       syntax keyword adaBegin	project 
    endif

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
       if version < 600
          syntax match adaBeginWith "^\s*\(\(with\(\s\+type\)\=\)\|\(use\)\)\>" contains=adaInc
          syntax match adaSemiWith	";\s*\(\(with\(\s\+type\)\=\)\|\(use\)\)\>"lc=1 contains=adaInc
       else
          syntax match adaBeginWith "^\s*\zs\(\(with\(\s\+type\)\=\)\|\(use\)\)\>" contains=adaInc
          syntax match adaSemiWith	";\s*\zs\(\(with\(\s\+type\)\=\)\|\(use\)\)\>" contains=adaInc
       endif
       syntax match adaInc	"\<with\>" contained contains=NONE
       syntax match adaInc	"\<with\s\+type\>" contained contains=NONE
       syntax match adaInc	"\<use\>" contained contains=NONE
       " Recognize "with null record" as a keyword (even the "record").
       syntax match adaKeyword	"\<with\s\+null\s\+record\>"
       " Consider generic formal parameters of subprograms and packages as keywords.
       if version < 600
          syntax match adaKeyword	";\s*with\s\+\(function\|procedure\|package\)\>"
          syntax match adaKeyword	"^\s*with\s\+\(function\|procedure\|package\)\>"
       else
          syntax match adaKeyword	";\s*\zswith\s\+\(function\|procedure\|package\)\>"
          syntax match adaKeyword	"^\s*\zswith\s\+\(function\|procedure\|package\)\>"
        endif
    endif


    " String and character constants.
    syntax region  adaString		start=+"+  skip=+""+  end=+"+
    syntax match   adaCharacter	"'.'"

    " Todo (only highlighted in comments)
    syntax keyword adaTodo contained TODO FIXME XXX

    " Comments.
    syntax region  adaComment oneline contains=adaTodo,adaLineError start="--"  end="$"

    if exists("g:ada_line_errors")
        syntax match adaLineError "\(^.\{79}\)\@<=."  contains=ALL containedin=ALL
    endif 

    " Define the default highlighting.
    " For version 5.7 and earlier: only when not done already
    " For version 5.8 and later: only when an item doesn't have highlighting yet
    if version >= 508 || !exists("g:did_ada_syn_inits")
       if version < 508
          let did_ada_syn_inits = 1
          command -nargs=+ HiLink highlight link <args>
       else
          command -nargs=+ HiLink highlight def link <args>
       endif

       " The default methods for highlighting. Can be overridden later.
       HiLink adaCharacter	   Character
       HiLink adaComment	   Comment
       HiLink adaConditional   Conditional
       HiLink adaKeyword	   Keyword
       HiLink adaLabel	   Label
       HiLink adaNumber	   Number
       HiLink adaSign	   Number
       HiLink adaOperator	   Operator
       HiLink adaPreproc	   PreProc
       HiLink adaRepeat	   Repeat
       HiLink adaSpecial	   Special
       HiLink adaStatement	   Statement
       HiLink adaString	   String
       HiLink adaStructure	   Structure
       HiLink adaTodo	   Todo
       HiLink adaType	   Type
       HiLink adaTypedef	   Typedef
       HiLink adaStorageClass  StorageClass
       HiLink adaBoolean	   Boolean
       HiLink adaException	   Exception
       HiLink adaAttribute	   Tag
       HiLink adaInc	   Include
       HiLink adaError	   Error
       HiLink adaSpaceError	   Error
       HiLink adaLineError	   Error
       HiLink adaBuiltinType   Type
       HiLink adaAssignment	   Special

       if exists ("ada_begin_preproc")
          " This is the old default display:
          HiLink adaBegin	PreProc
          HiLink adaEnd	PreProc
       else
          " This is the new default display:
          HiLink adaBegin	Keyword
          HiLink adaEnd	Keyword
       endif

       delcommand HiLink
    endif

    setlocal commentstring=--\ \ %s
    setlocal comments+=:--\ \ 
    setlocal formatoptions+=ron

    if exists("g:ada_folding")
        syntax sync minlines=1 maxlines=1
        setlocal foldmethod=indent
        setlocal foldignore=--
        setlocal tabstop=8
        setlocal softtabstop=3
        setlocal shiftwidth=3
    else
        " We don't need to look backwards to highlight correctly;
        " this speeds things up greatly.
        syntax sync minlines=1 maxlines=1
    endif

    if exists("g:ada_abbrev")
       iabbrev ret  return
       iabbrev proc procedure
       iabbrev pack package
       iabbrev func function
    endif

    let b:current_syntax = "ada"

    if exists("g:ada_default_compiler")
        execute "compiler" g:ada_default_compiler
    endif
    finish
endif

"------------------------------------------------------------------------------
"   Copyright (C) 2006  Martin Krischik
"
"   This program is free software; you can redistribute it and/or
"   modify it under the terms of the GNU General Public License
"   as published by the Free Software Foundation; either version 2
"   of the License, or (at your option) any later version.
"   
"   This program is distributed in the hope that it will be useful,
"   but WITHOUT ANY WARRANTY; without even the implied warranty of
"   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
"   GNU General Public License for more details.
"   
"   You should have received a copy of the GNU General Public License
"   along with this program; if not, write to the Free Software
"   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
"------------------------------------------------------------------------------
"vim: textwidth=78 nowrap tabstop=8 shiftwidth=3 softtabstop=3 noexpandtab
"vim: filetype=vim encoding=latin1 fileformat=unix
doc/ada.txt
218
*ada.txt*         Ada Filetype Plugins                Last change: 2006 Apr 30


                    ADA FILETYPE PLUGINS REFERENCE MANUAL~

ADA                                                                   *ada.vim*

1. Syntax Highlighting          |ft-ada-syntax|
2. Plugin                       |ft-ada-plugin|
3. Omni Completion              |ft-ada-omni|
4. Gnat Compiler                |compiler-gnat|
5. Options                      |ada-options|
6. Exported Functions           |ft-ada-functions|
7. Exported Contants            |ada-options|
8. Extra Plugins                |ada-extra-plugins|

==============================================================================
1. Syntax Highlighting                                          *ft-ada-syntax*


This mode is designed for the 2005 edition of Ada ("Ada 2005"), which includes
support for objected-programming, protected types, and so on.  It handles code
written for the original Ada language ("Ada83", "Ada87", "Ada95") as well,
though code which uses Ada 2005-only keywords will be wrongly colored (such
code should be fixed anyway).  For more information about Ada, see
http://www.adapower.com.

The Ada mode handles a number of situations cleanly.

For example, it knows that the "-" in "-5" is a number, but the same character
in "A-5" is an operator.  Normally, a "with" or "use" clause referencing
another compilation unit is colored the same way as C's "#include" is colored.
If you have "Conditional" or "Repeat" groups colored differently, then "end
if" and "end loop" will be colored as part of those respective groups.

You can set these to different colors using vim's "highlight" command (e.g.,
to change how loops are displayed, enter the command ":hi Repeat" followed by
the color specification; on simple terminals the color specification
ctermfg=White often shows well).

There are several options you can select in this Ada mode.

To enable them, assign a value to the option.  For example, to turn one on:
   let ada_standard_types = 1
To disable them use ":unlet".  Example:
   unlet ada_standard_types = 1

You can just use ":" and type these into the command line to set these
temporarily before loading an Ada file.  You can make these option settings
permanent by adding the "let" command(s), without a colon, to your "~/.vimrc"
file.

Here are the Ada mode options:

Variable                 Action ~

Even on a slow (90Mhz) PC this mode works quickly, but if you find the
performance unacceptable, turn on ada_withuse_ordinary.

==============================================================================
2. Plugin                                       *ft-vim-indent* *ft-ada-plugin*

The ada plugin provides various support functions as well as indent setup for
needed to indent Ada source.

==============================================================================
3. Omni Completion                                               *ft-ada-omni*

The Ada omni-completions which uses tags database created by "gnat xref -v
>tags".

It is a lot simpler then the other omnicompletion plugins which is quite
helpfull if you plan to write your own.

NOTE: "gnat xref -v" is very tricky to use as it has almost no diagnostic
       output - If nothing is printed then usualy the parameters are wrong.
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
6)  Remember to insert "!_TAG_FILE_SORTED 2 %sort ui" as first line.

==============================================================================
4. Gnat Compiler                                               *compiler-gnat*

==============================================================================
5. Options                                        *ada-variable* *ada-options*

                                                        *g:ada_standard_types*
g:ada_standard_types    bool (true when exists)
                Highlight types in package Standard (e.g., "Float")

                                                          *g:ada_space_errors*
                                                  *g:ada_no_trail_space_error*     
                                                    *g:ada_no_tab_space_error*     
                                                         *g:ada_all_tab_usage*            
g:ada_space_errors       bool (true when exists)            
                Highlight extraneous errors in spaces ...
                g:ada_no_trail_space_error        
                    - but ignore trailing spaces at the end of a line
                g:ada_no_tab_space_error          
                    - but ignore tabs after spaces
                g:ada_all_tab_usage               
                    - highligt all tab use

                                                           *g:ada_line_errors* 
g:ada_line_errors         bool (true when exists)
                Highlight lines which are to long

                                                         *g:ada_rainbow_color*   
g:ada_rainbow_color       bool (true when exists)
                Use rainbow color for '(' and ')'. You need the
                rainbow_parenthsis for this to work

                                                               *g:ada_folding*   
g:ada_folding             bool (true when exists)
                Use folding for Ada sources.

                                                                *g:ada_abbrev* 
g:ada_abbrev              bool (true when exists)
                Add some abbrevs

                                                      *g:ada_withuse_ordinary*  
g:ada_withuse_ordinary    bool (true when exists)
                Show "with" and "use" as ordinary keywords (when used to
                reference other compilation units they're normally highlighted
                specially).

                                                         *g:ada_begin_preproc*      
g:ada_begin_preproc       bool (true when exists)
                Show all begin-like keywords using the coloring
                of C preprocessor commands.

                                                      *g:ada_extended_tagging*  
g:ada_extended_tagging    enum ('jump', 'list')
                use exteded tagging, two options are available 
                    'jump': use tjump to jump.
                    'list': add tags quickfix list.
                Normal tagging does not support function or operator
                overloading as this is available in C and tagging was
                originaly developed for C.

                                                       *g:ada_gnat_extensions*    
g:ada_gnat_extensions     bool (true when exists)
                 Support GNAT extensions.
                                               *g:ada_with_gnat_project_files*
g:ada_with_gnat_project_files    bool (true when exists)
                 Add gnat project file keywords

                                                      *g:ada_default_compiler*  
g:ada_default_compiler    string
                 set default compiler. Currently supported is 'gnat'

An "exists" type is a boolean is considered true when the variable is defined
and false when the variable is undefined. The value ti which the variable is
set makes no difference. 

==============================================================================
6. Exported Functions                                       *ft-ada-functions*
		
ada#Word([{line}, {col}])                                         *ada#Word()*
		return full name of Ada entity under the cursor (or at given
                line/column), stripping whitespace/newlines as necessary.

ada#List_Tag([{line}, {col}])                                 *ada#Listtags()*
                list all ocurencies of the Ada entity under the cursor (or at
                given line/column) inside the quick-fix window

ada#Jump_Tag ({ident}, {mode})                                *ada#Jump_Tag()*
                list all ocurencies of the Ada entity under the cursor (or at
                given line/column) in the tag jump list. Mode can either be
                'tjump' or 'stjump'.
     
==============================================================================
7. Exported Contants                                        *ft-ada-constants*

                                                              *g:AdaWordRegex*
g:AdaWordRegex          string
                Regular expression to search for ada words

                                                               *g:ada#Comment*
g:ada#Comment           string
                Regular expression to search for ada comments

                                                              *g:ada#Keywords*
g:ada#Keywords          list of dictionaries
                List of keywords, attributes etc. pp. in onmicompletion format

==============================================================================
8. Extra Plugins                                           *ada-extra-plugins*

You can optionaly install the following extra plugins. They work well with Ada
and enhance the ability of the Ada mode.:

backup.vim
        http://www.vim.org/scripts/script.php?script_id=1537
        Keeps as many backups as you like so you don't have to.

rainbow_parenthsis.vim
        http://www.vim.org/scripts/script.php?script_id=1561
        Very helpfull since Ada uses only '(' and ')'.

nerd_comments.vim
        http://www.vim.org/scripts/script.php?script_id=1218
        Excelent commenting and uncommenting support for allmost any
        programming language.

==============================================================================
vim: textwidth=78 nowrap tabstop=8 shiftwidth=4 softtabstop=4 expandtab
vim: filetype=help encoding=latin1
