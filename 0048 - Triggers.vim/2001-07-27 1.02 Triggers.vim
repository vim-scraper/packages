"===========================================================================
" Vim script file
"
" File:		Triggers.vim -- v1.02
" Author:	Luc Hermitte <EMAIL:hermitte@free.fr>
" 		<URL:http://hermitte.free.fr/vim/>
" Last Update:	09th May 2001
"
" Purpose:	Help to map a sequence of keys to activate and desactivate
" 		either a mapping, a setting or an abbreviation.
"               
" Exemples:      
"  * |call Trigger_Define( '<F7>', 'set ai' )
"    When pressing <F7> a first time, 'set ai!' is executed. Pressing <F7>
"    a second time executes 'set ai'. Of course, for sets, it is not really
"    interresting thanks to "map <F7> :set ai!<CR>"... I must admit that I 
"    haven't seen any interrested (yet) in switching the numerical value of
"    settings, but it is handled ->
"  * |call Trigger_Define( '<F4>', 'set tw=120 sw*=2' ) 
"    ... works fine
"  * |call Trigger_Define( '<F9>', 'inoremap { {}<Left>' )
"    I'm sure you have allready seen an interrest in this one.
"  * |call Trigger_Define( '<F3>', 'iab LU Last Updated' )
"    Fine abbrev, but a little bit annoying when doing linear algebra...
"  * |source myAbbrevAndMap.vim
"    |call Trigger_Function('<F3>', 'MyAbbrevs', 'myAbbrevAndMap.vim' )
"    This one calls by turns MyAbbrevs(), and its undoing counterpart.
"    The undoing function does not exists ? No problem, its creates it
"    in the file "$VIMRUNTIME/settings/switch/MyAbbrevs.vim" and calls it
"    "Switched_MyAbbrevs()".
"    !!! BTW, do not forget to prepare the folder !!!
"    
"
" Inspiration:	buffoptions.vim
" TODO: automatically rebuilds switch file if the original file has been
" updated.
"---------------------------------------------------------------------------
" Defines the following function:
" * Internal functions: [not for end user]
"  -> <i> function: Trigger_DoSwitch( key, action, opposite [, verbose] )
"  -> <i> function: Trigger_BuildInv4Set( action )
"  -> <i> function: Trigger_BuildInv4Map_n_Abbr( action [, CheckOldValue] )
"  -> <i> function: Trigger_BuildInv( action [, CheckOldValue] )
"  -> i<> function: Trigger_File(funcname)
"  -> i<> function: TRIGGER(action, opposite)
" * "exported" functions: [yes! Use these ones]
"  -> <e> function: Trigger_Define( keys, action [, verbose] )
"  -> <e> function: Trigger_Function(keys, funcname, fileassociated)
"  -> <e> command:  TRIGGER "action", "opposite"
"===========================================================================
"
"---------------------------------------------------------------------------
" Avoid reinclusion
if !exists('g:Triggers_loaded')
  let g:Triggers_loaded = 1
"
"---------------------------------------------------------------------------
"---------------------------------------------------------------------------
" Function: TRIGGER(action, opposite)				<internal>
" Command:  TRIGGER "action", "opposite"			<exported>
"
" This little tool will enable to define switchable triggers that are
" neither mappins, settings nor abbreviations. 
" When called, the function executes the command that should be in the first
" parameter and totally ignores the second parameter. 
" The function Trigger_File() swap the two parameters of the TRIGGER command.
" The parameters must be separated by an unique comma -- or else update the 
" pattern ! I only use it for echoings !
"
" Note:It is not designed to be used directly. Use instead the corresponding 
" command : TRIGGER.
" Ex: TRIGGER "echo 'ON'", "echo 'OFF'"
" Attention: In order to work correctly, the double quotes must be around each 
" parameter while the single ones are inside in order to protect the constant 
" parameters of echo.
function TRIGGER(a, b)
  exe a:a
endfunction

command! TRIGGER call TRIGGER(<args>)
"---------------------------------------------------------------------------
"---------------------------------------------------------------------------
" Function: Trigger_DoSwitch( keys, action, opposite [,verbose])	
" 								<internal>
" Maps a sequence of "keys" to execute turn after turn "action" and its
" "opposite"
" 
" I suppose that the NoVerbose is equivalent to the wish to see the
" "action" not executed.
function Trigger_DoSwitch( ... )
  if (a:0 < 3) || (a:0 > 4)
    echohl ErrorMsg
    echo "'Trigger_DoSwitch(keys, action, opposite [,verbose] )' incorrect number of arguments..."
    echohl None
    return
  endif
  if (a:0 == 3) && (a:3 != "0")
    exe "noremap ".a:1." :call Trigger_DoSwitch('".a:1."','".a:3."','".a:2."')<CR>"
    exe a:2
    echo a:2
  else
    " delay the execution
    exe "noremap ".a:1." :call Trigger_DoSwitch('".a:1."','".a:2."','".a:3."')<CR>"
  endif
endfunction
"
"---------------------------------------------------------------------------
" Function: Trigger_BuildInv4Set( action )			<internal>
" Builds the opposite action of the option assignment "action"
"
" The set format is the one accepted by vim, except that it authorizes white 
" spaces after the affectation signs.
"
function Trigger_BuildInv4Set( action )
  if a:action =~ '^\s*set\s\+'
    " purge <action> from the "set" string
    let sets = substitute( a:action, '^\s*set\s\+', '', '' )
    " extract all the options to set
    let opp = 'set'
      let pattern_set = '\s*\(\w\+\)\(\(\s*\(:\|[+^-]\==\)\s*\)\(\(\\ \|\w\)\+\)\)\=\s*\(.*\)$'
    while 1
      let var  = substitute( sets, pattern_set, '\1', '')
      let sign = substitute( sets, pattern_set, '\4', '')
      let val  = substitute( sets, pattern_set, '\5', '')
      let sets = substitute( sets, pattern_set, '\7', '')
      "echo var . sign . val . "-- " . sets
      if val == "" 
        let opp = opp . ' ' . var . "!"
      elseif sign =~ "+="
        let opp = opp . " ". var . "-=" . val2
      elseif sign =~ "-="
        let opp = opp . " ". var . "-=" . val2
      else
        exe "let val2 = &" .var 
        let opp = opp . " ". var . "=" . val2
      endif
      if sets == ""
        break
      endif
    endwhile
    return opp
  else
    echohl ErrorMsg
    echo "Incorect option definition : " . a:action
    echohl None
  endif
endfunction
"
"---------------------------------------------------------------------------
" Function: Trigger_BuildInv4Map_n_Abbr( action [,CheckOldValue])	
" 								<internal>
" Builds the opposite action of the Mapping or abbreviation : "action"
"
" If the format, we want at least 'map' or 'ab'
"
function Trigger_BuildInv4Map_n_Abbr( ... )
  if (a:0==0) || (a:0>2)
    echohl ErrorMsg
    echo 'Trigger_BuildInv4Map_n_Abbr(action [,CheckOldValue]) : Incorect number of arguments...'
    echohl None
    return 
  endif
    let p_abbrv = '\([ic]\=\(nore\)\=ab\(br\)\=\)'
    let p_map   = '\(!\=[nvoic]\=\(nore\)\=map\)'
    let p_name  = '\s\+\(\(\\ \|\S\)\+\)'
    let pattern = '^\s*\('.p_abbrv.'\|'.p_map.'\)' . p_name . '\s\+\(.*\)$'
  if a:1 =~ '^\s*\(' . p_abbrv . '\|' . p_map . '\)\s\+'
    let cmd  = substitute( a:1, pattern, '\1', '' )
    let ctx  = matchstr( cmd, '^!\=[nvoic]\=' )
    let ctx  = matchstr( ctx, '[nvoic]\=' )
    let name = substitute( a:1, pattern, '\7', '' )
    "let val  = substitute( a:1, pattern, '\9', '' )
    "echo '- '. ctx . ' - ' . cmd . ' - ' . name . ' - ' . val . ' -'
    " Map ----------------------------------------------------------------
    if cmd =~ p_map
      if (a:0==2)&&(a:2!=0)
        let rhs = maparg( name, ctx )
        if rhs != ""
          " check for 'nore' feature -- equiv 16th char from map is '*'
          " Message will be echo
          redir @a
            exe ctx . 'map '.name
          redir END
          if @a[16] == '*'
            let opp = ctx . 'noremap ' .name . ' ' . rhs 
          else
            let opp = ctx . 'map ' .name . ' ' . rhs 
          endif
        else
          let opp = ctx . 'unmap ' . name 
        endif
      else
        let opp = ctx . 'unmap ' . name 
      endif
    " Abbreviation -------------------------------------------------------
    elseif cmd =~ p_abbrv
      if (a:0==2)&&(a:2!=0)
        redir @a
          exe ctx . 'abbr ' . name
        redir END
        if @a =~ "No abbreviation found"
          let opp = ctx . 'unabbr ' . name
        else
          " first char matched is the nul char from @a
          let val = substitute( @a, '.[ic]\s*\(\\ \|\S\)\+\s\+\*\=\(.*\)$', '\2', '' )
          let va0 = substitute( @a, '.[ic]\s*\(\(\\ \|\S\)\+\)\s\+\*\=\(.*\)$', '\1', '' )
          if va0 != name
            echohl ErrorMsg
            echo 'Abbreviation [' . name . '] inconsistant with a previous one [' . va0 . ']...'
            echohl None
            return 
          endif
          if @a[16] == '*'
            let opp = ctx . 'noreabbr ' . name . ' ' . val
          else
            let opp = ctx . 'abbr ' . name . ' ' . val
          endif
        endif
      else
        let opp = ctx . 'unabbr ' . name
      endif
    endif
    return opp
  else
    echohl ErrorMsg
    echo "Incorect mapping/abbreviation definition : " . a:1
    echohl None
  endif
endfunction
"
"---------------------------------------------------------------------------
" Function: Trigger_BuildInv( action [, CheckOldValue] )	<internal>
" 
" Builds the opposite action for the Setting, Mapping or abbreviation :
" "action"
" 
" Rem.1: Setting CheckOldValue to something different to 0, prevent the
" function to checks for previous values of the switch mapping or
" abbreviation. The main purpose is to prevent the execution of "abbrv foo"
" or "map foo" which cause the display of unwanted messages. The main
" application comes when loading for the first time the new mapping. 
"
" Rem.2: CheckOldValue doesn't apply to "set" actions. Moreover, for hard
" settings of non boolean options, the value pushed is the one that were
" effective at the time the call to Trigger_BuildInv() has been made ; not
" the one effective at the moment of the first switch.
" Ex: set tw=78 | call Trigger_Define('<F4>', 'set tw=120') | set tw=72
" If now we hit <F4>, &tw will equals 78. <F4> again, this time it equals
" 120, <F4> -> 78, etc. We have lost 72
" On an other hand, 'set tw+=40' works fine and 'set tw^=2' works strange
" because there no '/='.
"
" May be the functions could be extended to supports mechanisms like \def
" and \edef in TeX...
"
function Trigger_BuildInv( ... )
  if (a:0==0) || (a:0>2)
    echohl ErrorMsg
    echo 'Trigger_BuildInv(action [,CheckOldValue]) : Incorect number of arguments...'
    echohl None
    return 
  endif
  if a:1 =~ '^\s*set\s\+'
    return Trigger_BuildInv4Set( a:1 )
  else
    if (a:0==2) && (a:2==0)
      return Trigger_BuildInv4Map_n_Abbr( a:1, a:2 )
    else
      return Trigger_BuildInv4Map_n_Abbr( a:1 )
    endif
  endif
endfunction
"
"---------------------------------------------------------------------------
"---------------------------------------------------------------------------
" Function: Trigger_Define( keys, action [, verbose] )		<exported>
" Ex: Trigger_Define( '<F4>', 'set hlsearch' )
"
function Trigger_Define( ... )
  if (a:0 < 2) || (a:0 > 3)
    echohl ErrorMsg
    echo "'Trigger_Define(keys, action [,verbose] )' incorrect number of arguments..."
    echohl None
    return 
  endif
  let opp = Trigger_BuildInv( a:2 )
  if opp == ""
    return 
    "return a:2
  else
    if a:0 == 2
      call Trigger_DoSwitch( a:1, a:2, opp )
    elseif a:0 == 3
      call Trigger_DoSwitch( a:1, a:2, opp, a:3 )
    endif
  endif
endfunction
"
"
"---------------------------------------------------------------------------
" Function: Trigger_File(funcname)				<internal>
"
" Builds thafile containing all the opposite (undefinitions) macros than
" those defined in "funcname".
" Note: This macro should be apply while editing the file containing the
" function to transform. 
" Called by: |Trigger_Function()|
" Assumes: no function embeded, no group, etc
function Trigger_File(funcname)
"1- Find the range of definition for a:funcname
  exe 'normal 1G'
  exe '/^\s*fu[nction]*!\=\s*' . a:funcname . '\s*()'
  let b=line(".") 
  exe '/^\s*endf'
  let e=line(".") + 1
  if line(".") < line("$") 
    let eff = line("$") - line(".")
    exe "normal j" . eff . "dd"
    "exe "normal " .e . ',$g/.*/d' 
  endif
  exe '1,' . b .'g/.*/d'
"2- Apply Trigger_DoSwitch() on all the corresponding lines 
    let p_set   = '\(set\)'
    let p_abbrv = '\([ic]\=\(nore\)\=ab\(br\)\=\)'
    let p_map   = '\(!\=[nvoic]\=\(nore\)\=map\)'
    let pattern = '^\s*\('.p_abbrv.'\|'.p_map.'\|'.p_set.'\)'
  exe 'g/'.pattern.'/ call setline(line("."),Trigger_BuildInv(getline(line(".")), 0))'
    let p_trig  = '^\(\s*TRIGGER\s*\)\([^,]*\)\s*,\s*\([^,]*\)'
  exe '%s/'.p_trig.'/\1\3, \2/'
  call append( "0", 'function! Switched_' . a:funcname . '()' )
"3- And wq!
  let filename = $VIMRUNTIME . '/settings/switch/' . a:funcname . '.switch'
  exe "w! " . filename
  q
endfunction
"
"---------------------------------------------------------------------------
" Function: Trigger_Function(keys, funcname, fileassoc)		<exported>
"
" Set a switch mapping on "keys" that executes in turn "funcname"() 
" then its opposite/negate. "funcname"() is defined in <"fileassoc">.
" This function search for Switched_"funcname"() in 
" <$VIMRUNTIME/settings/switch/"funcname".switch>. If the file does not exists, 
" it is build thanks to Trigger_File().
function Trigger_Function(keys, funcname, fileassoc)
"1- Checks wheither the function has allready been computed to its opposite or not.
  let filename = $VIMRUNTIME . '/settings/switch/' . a:funcname . '.switch'
  if !filereadable( filename )
    " Then build it !
    if Trigger_RebuildFile( a:funcname, a:fileassoc ) != ""
      return
    endif 
    echo filename . ' created...'
  else
    exe "so " . filename
  endif
"2- Loads the opposite function .. 
"  .. embeded in Trigger_RebuildFile()
"3- Defines the switch
  let call1 = 'call ' . a:funcname . '()'
  let call2 = 'call Switched_' . a:funcname . '()'
  call Trigger_DoSwitch( a:keys, call1, call2, 0 )
endfunction
"
"
"---------------------------------------------------------------------------
function Trigger_RebuildFile(funcname, fileassoc)
  let this     = $VIMRUNTIME . '/macros/Triggers.vim' 
  if has("unix")
    call system( "vim ".a:fileassoc." -R -u ". this ." -c \"call Trigger_File('".a:funcname."')\"")
  elseif has("win32")
    call system( "gvim ".a:fileassoc." -R -u ". this ." -c \"call Trigger_File('".a:funcname."')\"")
  endif
  if v:shell_error
    echohl ErrorMsg
    echohl "Can't execute vim with current environment..."
    echohl None
    return
  endif
  let filename = $VIMRUNTIME . '/settings/switch/' . a:funcname . '.switch'
  exe "so " . filename
endfunction
"---------------------------------------------------------------------------
"---------------------------------------------------------------------------
" Avoid reinclusion
endif
