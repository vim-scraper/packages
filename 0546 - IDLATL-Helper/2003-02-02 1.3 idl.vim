" IDL functions
" Author: Michael Geddes <michaelrgeddes@optushome.com.au>
" Date: Jan 2003
" Version: 1.3
"
" <localleader>si - Stub Interface
"   Stub an interface given an interface name. Supports the following lines:
"   IMyInterface
"   IMyInterface : IUnknown
"   dispinterface IMyInterface
"   _IMyInterfaceEvent
"
" <localleader>p  - Make a property
"   Makes a property from a c++ style declaration (const at the end does read-only
"   property). Supports the following lines:
"     long MyProp
"     BSTR MyProp const
"     VARIANT MyArrayProp( BSTR prop)
"     VARIANT MyArrayProp( BSTR prop, bool prop2)
"
" <localleader>P - Make method
"   Makes a method from a c++ style declaration. Changes common types to
"   VARIANT types - supports SAFEARRAY.  Adds [in], [in,out] as appropriate.
"   Makes the function return HRESULT, and puts the return value as the last
"   paramater.  Supports void or functions already returning HRESULT or CError.
"
"   Supports the following lines:
"       void MyFunc( bool arg1, long arg2)
"       long MyFunc( SAFEARRAY(string) arg2)
"       bool MyFunc()
"
" <localleader>i - Add an id()
"   Adds an id to a method or to a property.  Adds 1 to the highest id in the
"   current interface.
"
" in C++ files
"   <localleader>C - Convert idl to cpp
"   Mangles an idl method to a cpp definition.
"
" History:
" 1.1:
"   Hide mediacommand stuff by default.
"   Add support for SAFEARRAY
"   Extend support for classes
" 1.2:
"   Fix id() support
" 1.3:
"   Fix support for adding [in] [in,out] to parameters
"

" if !exists('DoingSOURCE')
"  SO <sfile>
"  finish
" endif

if !exists('g:idl_mc_gs_stuff')
    let idl_mc_gs_stuff=($USERDOMAIN == 'MEDIACOMMAND')
endif

if &filetype=='cpp'
    map <buffer> <localleader>C :call <SID>IDLToCpp()<cr>
else
    map <buffer> <localleader>si :call <SID>IDLGenInterface()<cr>
    map <buffer> <localleader>p :call <SID>IDLProperty()<cr>
    map <buffer> <localleader>P :call <SID>IDLGenerateIDLStub()<CR>
    map <buffer> <localleader>i :call <SID>PutId()<cr>
endif

if exists(':Bmenu')
    exe FindBufferSID()
    if &filetype=='cpp'
        if BufferOneShot('idlcommands_cpp')
            " FileType cpp
            Bamenu 42.10 &ATL.&IDL\ to\ CPP<tab><localleader>C :call <SID>IDLToCpp()<cr>
        endif
    else
        if BufferOneShot('idlcommands')
            " FileType idl,odl
            Bamenu 42.10 I&DL.&Stub\ Interface<tab><localleader>si :call <SID>IDLGenInterface()<cr>
            Bamenu 42.20 I&DL.&Property<tab><localleader>p :call <SID>IDLProperty()<cr>
            Bamenu 42.30 I&DL.&Cpp\ to\ IDL<tab><localleader>P :call <SID>IDLGenerateIDLStub()<cr>
            Bamenu 42.40 I&DL.&Id\ Generate<tab><localleader>i :call <SID>PutId()<cr>
            Bamenu 42.50 I&DL.&Modify\ Guid :call <SID>ModifyGUID()<cr>
            "FileType:
            menu 42.45 I&DL.-aa- <nul>
            " FileType: idl,odl
            Bamenu 42.50 I&DL.Add\ Class\ &Definition :call <SID>IDLAddClassDefinition()<cr>
            Bamenu 42.60 IDL.Add\ &Library :call <SID>IDLDeclareLibrary(input("Library Name:"))<CR>
        endif
    endif
endif
"fun! AddCoClass(class,interface)
"echo CreateRGS(progid,class)
"fun! InsertRGS(resfile,rgsfile)
"fun! CreateATLClass( classname, class, interface, library, resource )

"if exists('s:IDLAddInterface')
"   finish
"endif

" Add a new interface to the file
fun! s:IDLAddInterface(interface)
  let here=winbufnr('.')
  exe 'grep -r "interface\ '.a:interface.'.:" *.idl'
  if getline('.')=~ 'interface '.a:interface.' :' 
    /{/+1
    norm "xyi}
    exe 'buf '.here
    put ='/// \name '.a:interface.' Interface'
    put ='//@{'
    put ='//@}'
    mark y
    norm =3k
    +2
    put x
    while line('.') < line("'y")
      if getline('.') !~ '^\s*$'
        call s:IDLToCpp()
      endif
      +
    endwhile
  endif
  colder
endfun

" Interactive -  Add a new coclass to the file

fun! s:IDLAddClassDefinition()
  let coclass=''
  let colist=s:GetCoClassList()
  if colist!=""
    let res=confirm("CoClass:","&New\n".colist,1)
    if res > 1
      let colist="x\n".colist."\n"
      let n=matchend(colist,"^\\([^\n]*\n\\)\\{".(res-1).'}')
      let e=matchend(colist,"^\\([^\n]*\n\\)\\{".(res).'}')
      let coclass=strpart(colist,n,e-(n+1))
    elseif res==0
      return
    endif
  endif
  if coclass==""
    let coclass=input("CoClass:")
    if(coclass=="")
      return
    endif
  endif

  let interface=input("Interface:")
  if interface==""
    return
  endif

  if !s:QueryCheckout()
    return "NotWriteable"
  endif

  let gsdep=0
  let component_impl=0
  if g:idl_mc_gs_stuff
      let res=confirm("Is this a GS Dependent class?", "&Independent\n&Dependent",1)
      if res==0 | return | endif
      let gsdep=(res==2)

      let res = confirm("Type of Implentation:","&Normal\n&Component",1)
      if res==0 | return | endif
      let component_impl=(res==2)
  endif
  let noncreateable=0
  let aggregateable=component_impl
  if !component_impl
      let res=confirm("Mark as Createable?", "&Createable\n&Noncreateable",1)
      if res==0
          return
      endif
      let noncreateable=(res==2)
      if !noncreateable
        let res=confirm("Mark as aggregateable?", "&Yes\n&No",2)
        if res ==0
            return
        endif
        let aggregateable=res==1
      endif
  endif

  if s:AddCoClass(coclass,interface,gsdep,noncreateable,aggregateable) =='created'
    let res = confirm("Implementation wrapper:", "&Comet\n&Atl",((g:idl_mc_gs_stuff)?1:2))
    if res==0
        return
    elseif res==2
        return s:IDLImplementClass(coclass,interface,component_impl,gsdep)
    else
        return s:CometImplementClass(coclass,interface,component_impl, gsdep)
    endif
  endif
endfun

fun! s:CometImplementClass(coclass,interface,component_impl, gsdep)
  let curbuf=winbufnr(winnr())
  let dirs=".\\\nObjects\\\nOther.."
  let index=confirm("Choose Directory:", dirs, 2)
  if index==1
    let objdir=""
  elseif index==2
    let objdir='Objects\'
  elseif index==3
    let objdir=input("Directory:")
    if objdir!="" && objdir !~ '[/\\]$'
      let objdir=objdir.'\'
    endif
  else
    return ""
  endif
  if &modified
    w
  endif
  let fname=inputdialog('Filename:', objdir.a:coclass.'.hpp')
  if fname==''
    return ''
  endif

  call system( g:MIDL.' /nologo /Oicf /win32 /tlb '.expand('%:r').'.tlb '.expand('%') )


  exe '!'.g:TLB2H.' -w '.a:coclass.' -o "'.fname.'" '.expand('%:r').'.tlb'

 " exe "norm ".bufwinnr(curbuf)."\<c-w>w"
  "let multithread=confirm("Threading Model:", "&Single\n&Free\n&Both",1)-1
  "if multithread==-1 
  "  return
  "endif
endfun

fun! s:IDLImplementClass(coclass,interface,component,gsdep)
  let curbuf=winbufnr(winnr())
  let coclass=a:coclass
  let interface=a:interface

    let resfile=FindRCFile()
    if resfile!=""
      let res=confirm("Use:".resfile,"&Yes\n&No",1)
      if res==0
        return
      endif
      if res==2
        let resfile=""
      endif
    endif  
    if resfile==""
      if has('gui_running')
        let resfile=browse(0, "Resource File", '.', '*.rc')
      else
        let resfile=input("Enter RC resfile:")
      endif
      if resfile==""
        return
      endif
    endif
    let dirs=".\\\nObjects\\\nOther.."
    let index=confirm("Choose Directory:", dirs, 2)
    if index==1
      let objdir=""
    elseif index==2
      let objdir='Objects\'
    elseif index==3
      let objdir=input("Directory:")
      if objdir!="" && objdir !~ '[/\\]$'
        let objdir=objdir.'\'
      endif
    else
      return ""
    endif
    let projname=substitute(system('grep IDS_PROJNAME '.resfile),'^.*"\(.*\)".*$','\1','')
    if !a:component && !a:gsdep
      let compfile=objdir.coclass.'.rgs'
      if s:CreateRGS(projname,coclass,compfile)=='Cancelled'
        return
      endif
      let resource=substitute(coclass, '^\([^.]*\)$', 'IDR_\U\1', '')
      echo s:InsertRGS(resfile,compfile,resource)
    else
      let rgsname=(a:gsdep?'object.rgs' : 'component.rgs')
      let rgsdesc=(a:gsdep?'Object' : 'Component')
      let compfile=substitute(system('dir /s/b '.rgsname ),"[   \r\n]*$",'','')
      if compfile=="" || compfile =~? 'file not found'
        if has('gui_running')
          let compfile=browse(0, rgsdesc.' Resource File', '.', '*.rgs')
        else
          let compfile=input('Enter '.rgsdesc.' rgs file:')
        endif
        if compfile==""
          return
        endif
      endif
      let compfile=fnamemodify(compfile,':.')
      let resource=substitute(fnamemodify(compfile,':t:r'),'^.*$','IDR_\U&','')
"     let resource=substitute(compfile,'^\([^/\\]*\)\.[rR][gG][sS]$','\U\1','')
      echo s:InsertRGS(resfile,compfile,resource)
    endif

    exe "norm ".bufwinnr(curbuf)."\<c-w>w"

    let impclass=substitute(coclass,'^\(.\)\(.*\)$','C\u\1\2','')
    let res=confirm("Implement ".coclass.' as '.impclass.'?',"&Yes\n&No\n&Rename",1)
    if res==0 || res==2
      return
    endif
    if res==3
      let impclass=input('Implement as :')
      if impclass==""
        return
      endif
    endif
    1 mark y
    exe '/\<coclass\s\+'.coclass.'/?^library\>?mark y'
    if line("'y") == 1
      echo "Unable to find library"
      return
    endif
    let libname=substitute(getline("'y"),'^library\s\+\(\k\+\)\s*$','\1','')

    1 mark y
    let findinterface=interface
    let cont=1
    let dispatch=-1
    while cont
      let cont=0
      let mx='\<interface\s\+'.findinterface.'\s*:\s*\(\<\k\+\>\)'
      silent! exe '/'.mx.'/ mark y'
      let found=line("'y")
      if found>1
        let line=getline(found)
        let inherit=substitute(matchstr(line,mx),mx,'\1','')
        if inherit=='IUnknown'
          let dispatch=0
        elseif inherit=='IDispatch'
          let dispatch=1
        else
          let findinterface=inherit
          let cont=1
        endif
      endif
      if dispatch==-1
        let dispatch=2-confirm("Support IDispatch?","&Yes\n&No",1)
        if dispatch==2
          return
        endif
      endif
    endwhile
    
    let multithread=confirm("Threading Model:", "&Single\n&Multiple",1)-1
    if multithread==-1 
      return
    endif
    let implfile=objdir.coclass.'.hpp'

    if confirm("CreateATLClass('".coclass."', '".impclass."', '".interface."', '".libname."', '".resource."',".dispatch.",".a:component.",'".compfile.",'".multithread."' )","&Yes\n&No",1)==1
      exe 'new '.implfile
      call s:CreateATLClass( impclass, coclass, interface, libname, projname, resource ,dispatch ,a:component, multithread, a:gsdep )
      let mx='^\(\s*[^:]\{-}\>\)\s*:'
      let projectfile=substitute(matchstr(system('grep BEGIN_OBJECT_MAP *.cpp'),mx),mx,'\1','')
      if projectfile ==""
        echo confirm('Unable to find BEGIN_OBJECT_MAP','&OK',1)
      else
        exe 'new '.projectfile
        if !s:QueryCheckout()
          return "NotWriteable"
        endif
        if @% ==? projectfile
          /^END_OBJECT_MAP/mark y
          call append(line("'y")-1, 'OBJECT_ENTRY(CLSID_'.coclass.', '.impclass.' )')
        endif
      endif
    endif
endfun

let idllibrary="
\[
\\<CR>  uuid(%LIBID%),
\\<CR>  version(1.0),
\\<CR>  helpstring(\"%LIBRARY% 1.0 Type Library\")
\\<CR>]
\\<CR>library %LIBRARYLib%
\\<CR>{
\\<CR>  importlib(\"stdole2.tlb\");
\\<CR>};
\\<CR>"

fun! s:IDLDeclareLibrary( libraryname )
  if a:libraryname==''
    return
  endif
  let liblib=substitute(a:libraryname,'^\(\k\{-}\)\([lL][iI][bB]\)\=$','\U\1','').'Lib'
  let b:IDLLibName=liblib
  let text=substitute(g:idllibrary,'%LIBRARY%',a:libraryname,'g')
  let text=substitute(text,'%LIBRARYLib%',liblib, 'g')
  let libid=s:IDLGetUUID()
  let text=substitute(text,'%LIBID%',libid,'g')
  $
  let paste=&paste
  set paste
  exe 'norm o'.text."\<ESC>"
  let &paste=paste
endfun


fun! s:GetCoClassList()
  let cur=1
  let end=line('$')
  let mx='\s*coclass\s\+\(\k\+\>\)'
  let lst=''
  while cur < end
    let here=getline(cur)
    if here=~mx 
      if lst!=""
        let lst=lst."\n"
      endif
      let lst=lst.substitute(matchstr(here,mx),mx,'\1','')
    endif
    let cur=cur+1
  endwhile
  return lst
endfun

if !exists('$MSDEVDIR')
    let $MSDEVDIR='C:\Program Files\Microsoft Visual Studio'
endif
if !exists('g:UUIDGEN')
    let g:UUIDGEN=fnamemodify($MSDEVDIR,':h').'\Tools\uuidgen.exe'
endif
if !exists('g:MIDL')
    let g:MIDL=fnamemodify($MSDEVDIR,':h:h').'\vc98\bin\midl.exe'
endif
if !exists('g:TLB2H')
  let g:TLB2H='tlb2h'
  if filereadable('t:\comet\bin\tlb2h.exe') | let g:TLB2H='t:\comet\bin\tlb2h' | endif
endif

"Returns a UUID
function! s:IDLGetUUID()
  let guid=system('"'.g:UUIDGEN.'"')
  let guid=matchstr(guid,'[-0-9a-zA-Z]\+')
  let guid=substitute(guid,'^.*$','\U&','')
  return guid
endfun

" Generate an IDL Interface stub
function! s:IDLGenInterface()
  let curline=getline('.')
  let dual="\<tab>dual,\<cr>"
  let object="\<tab>object,\<cr>"
  let nonextensible="\<tab>nonextensible,\<cr>"
  let pointer=",\<cr>\<tab>pointer_default(unique)"
  let deftype="interface"
  let defaultbody=""
  " Check for two words
  let reA='^\s*\(\k\+\>\)\s\+\(\k\+\>\)'
  let helpdesc=" Interface"
  if curline =~ reA
    let cur=matchstr(curline,reA)
    let interface=substitute(cur,reA,'\2','')
    let type=substitute(cur,reA,'\1','')
    if type == 'enum'
      let dual = ''
      let object = ''
      let nonextensible= ''
      let pointer = ''
      let namedinterface = 'enum'
      let helpdesc=" Enumeration"
    elseif type == 'dispinterface'
      let dual = ''
      let object = ''
      let nonextensible= ''
      let pointer = ''
      let namedinterface = 'NONE'
      let helpdesc=" Event Dispatch"
      let defaultbody="properties:\rmethods:\r"
    endif
  else
    norm 0eb
    let interface=expand('<cword>')
    if interface =~ '^_I\k*Events\=$'
      let dual = ''
      let object = ''
      let nonextensible= ''
      let pointer = ''
      let namedinterface = 'NONE'
      let helpdesc=" Event Dispatch"
      let deftype="dispinterface"
      let defaultbody="properties:\rmethods:\r"
    endif
  endif
  if (!exists('type') || type != 'enum') && curline =~ ':\s*I'
    let reA=':\s*\(I\k\+\)\>'
    let namedinterface=substitute(matchstr(curline,reA),reA,'\1','')
    if namedinterface!='IDispatch'
      let dual=''
      let nonextensible=''
    endif
  endif
  let guid=s:IDLGetUUID()
  set paste
  exe "norm viwo\<esc>mzO[\<cr>".object.nonextensible."\<tab>uuid(".guid."\<esc>0f(v$UA),\<cr>".dual."\<tab>helpstring(\""
\.interface.helpdesc."\")".pointer."\<cr>\<c-d>]\<esc>`z"
  if !exists('type')
    exe "norm 0ebi".deftype." \<esc>"
  endif
  if !exists('namedinterface')
    exe "norm A : IDispatch\<esc>"
  endif
  if getline(line('.')+1) !~ '{'
    exe "norm o{\<cr>".defaultbody."};\<esc>"
  endif
  set nopaste
endfun


" Define an IDL Property - 
" line should be of the form:
" <type> <Property>
function! s:IDLProperty()
  call s:ConvertTypes()
  let here=getline('.')
  let y='\(\<\(const\|func\)\>\)\s*$'
  let type=substitute(matchstr(here,y),y,'\1','')


  if type!=""
    let here=substitute(here,y,'','')
  endif
  " Set up the Expression
  let reA='^\s*\<\(\%(SAFEARRAY([^)]*)\|[^(]\{-1,}\)\(\s*\*\)\=\)\=\s*\(\<\k\+\>\)\(\s*(\s*\([^)]*\)\s*)\)\='
"  let reA='^\s*\<\(SAFEARRAY([^)]*)\|\%([^(]\{-1,}\)\(\s*\*\)\=\)\=\s*\(\<\k\+\>\)\(\s*(\s*\([^)]*\)\s*)\)\='
  let p=matchstr(here,reA)
  if p!=""

    let propdesc=" property"
    let b=substitute(p,reA,'\1','')
    let a=substitute(p,reA,'\3','')
    let c=substitute(p,reA,'\5','')

    let a=substitute(a,'^.','\u&','')
    "let type=substitute(p,reA,'\5','')
"   echo "b='".b."'"
"   echo " a=".a
"   echo " c='".c."'"
"   echo " type=".type
"   echo ""
    let putref=''
    if b=="void"
      let type="func"
      let getb=""
    elseif b=="" &&  type == "func"
      let getb=""
    else
      if b=="" 
        let b="long"
      endif
      if b=~'[&*]$'
        let getb=b."*"
        let putb=b
        let putref='ref'
      else
        let getb=b." *"
        let putb=b." "
      endif
    endif

    let comment=GetComment(line('.'))
    if comment !~ '^\s*$'
      let help='helpstring("'.comment.'")'
    else
      if type=="func"
        let propdesc=" function"
      elseif c!=""
        let n=strlen(substitute(c,'[^,]','','g')) " Count ','s
        if n==0 
          let propdesc=" Array".propdesc
  "   elseif n==1
  "     let propdesc=" Coord".propdesc
        else
          let propdesc=" ".(n+1)."D Array".propdesc
        endif
        if match(c,'\[')==-1
          let c=substitute(c,'\(^\|,\)','&[in] ','g')
        endif
        let c=c." ,"
      endif
      let ic=&ic
      set noic
      let help='helpstring("'.substitute(a,'\C\([a-z]\)\([A-Z]\)','\1 \l\2',"g").propdesc."\")"
      let &ic=ic
    endif
    if type!=""
      let pput=""
    else
      let pput="\[propput".putref.",".help."\] HRESULT ".a."(".c."[in] ".putb."newval);"
    endif
    if type=="func"
      if !exists("getb") || getb == ""
        let pget="\[".help."\] HRESULT ".a."(".c.");"
      else
        if c!=""
          let c=c.", "
        endif
        let pget="\[".help."\] HRESULT ".a."(".c."[out,retval] ".getb."pval);"
      endif
    else
      let pget="\[propget,".help."\] HRESULT ".a."(".c."[out,retval] ".getb."pval);"
    endif
    if( strlen(pget) + &shiftwidth + 2 >  &textwidth)
      let pput=substitute(pput,'\] ',"]\<cr>",'')
      let pget=substitute(pget,'\] ',"]\<cr>",'')
    endif
    exe 'norm ^Da'.pput."\<cr>".pget."\<esc>"
  endif
endfun


" Convert IDL style lines to CPP style.
function! s:IDLToCpp()
    let reA='^\s*\[\([^]]\+\)\]\s*'
    let match=matchstr(getline('.'),reA)
    if getline('.') !~ 'HRESULT' && getline(line('.')+1) !~ 'HRESULT'
      return ""
    endif
    if match=="" 
      let matchx=getline(line('.')-1)
      if matchx != ""
        let match=matchx
        -
      endif
    endif
    let props=substitute(match,reA,'\1','')
    let y='helpstring\s*(\s*"\(.\{-}\)"\s*)'
    let match=matchstr(match,y)
    if match==""
      let comment="TODO: Comment"
    else
      let comment=substitute(match,y,'\1','')
      let comment=substitute(comment,'\\\(.\)','.','g')
    endif
    let prefix=""
    if props=~"propputref"
      let prefix="putref_"
      let comment="Put ".comment
    elseif props=~"propput"
      let prefix="put_"
      let comment="Put ".comment
    elseif props =~"propget"
      let prefix="get_"
      let comment="Get ".comment
    endif
    exe 's/'.reA.'//'
    let max=3
    while line('.') != line('$') && getline('.')!~ ')\s*;' &&  max >0
      join
      let max=max-1
    endwhile
    s/\<SAFEARRAY(\([^)]*\))\s*\(\*\=\)/SAFEARRAY\/*(\1)*\/ *\2/ge
    exe 's/\<HRESULT\s\+\(\k\+\)\s*(/STDMETHOD('.prefix.'\1)('
    s/\[\s*in\s*\]\s*//ge
"   s/\(\[[^]]\{-}\)\s*in\s*\([^]]*]\)/\1\2/ge
"   s/\([[,]\)\s*,/\1/ge
    s/,]\s*/]/ge
    s/\<out\s*,\s*retval\>/retval/ge
    s/\[\([^]]\{-}\)\s*]/\/*\1*\//ge
    exe "norm o\<tab>// ".comment."\<esc>=k"
endfun

" Put the next available id() tag in a property.
fun! s:PutId()
  let here=line(".")
  norm 0
  let orig=here."norm 0"
  if(col(".")> 2)
    let orig=orig.col(".")-1.'l'
  endif
  let hline=getline(here)

  if hline !~'^\s*[' && getline(here-1)=~ '^\s*['
    let here=here-1
    let hline=getline(here)
  endif
  if hline=~'\[[^]]*id(.*]' | return | endif

  let property=(hline=~ 'prop..t\(ref\)\=')
  let top=search( '^\s*\(disp\)\=interface\s\+_\=I', 'b')+1
  if top==0 | exe orig | echoe "Can't find top" | return| endif
  exe top
  let bottom=search('^\s*}')
  if bottom ==0 |  exe orig |echoe 'Bottom not found'|return |endif
  exe here

  let reA='\<id(\([0-9]\+\))'
  let i=top
  let idno=1
  while i< bottom
    if getline(i)=~reA
      let idhere=1+substitute(matchstr(getline(i),reA),reA,'\1','')
      if idhere > idno
        let idno=idhere
      endif
    endif
    let i=i+1
  endwhile

  if property
    let cmd= 's/\(prop\%(put\|get\)\%(ref\)\=\)\([],]\)\s*/\1, id('.idno.')\2 /'
  else
    let cmd='/[id('.idno.')'
    if hline =~ '^\s*\['
      exe 's/\['.cmd.', /'
    else
      exe 's/\<'.cmd.'] /'
    endif
  endif
  exe cmd
  if property
    let reA='HRESULT \(\k\+\)('
    let c=3
    let word=""
    while c> 0
      if getline(here) =~ reA
        let word=substitute(matchstr(getline(here),reA),reA,'\1','')
        break
      endif
      let here=here+1
      let c=c-1
    endwhile
    if word != ""
      let reA='\<'.word.'('
      exe here+1.'/'.reA.'/ mark z'
      let nextone=line("'z")
    
      if nextone!=here && nextone > top && nextone < here+10
        exe nextone.'?prop..t\(ref\)\=? mark z'
        let nextprop=line("'z")
        if (nextone > here && nextprop > here) || (nextone < here && nextprop > top )
          exe nextprop.cmd
        endif
      endif
    endif
  endif
  exe orig
endfun

fun! s:GetTypelibID()
  /^library/?uuid?mark y
  if line("'y")!=0
    let reA='^.*uuid(\s*\([^)]*\)\s*).*$'
    return substitute(matchstr(getline("'y"),reA),reA,'\1','')
  endif
  return "UNKNOWN"
endfun

fun! s:GetCoClassID(class)
    let reA='coclass\s\+'.a:class
    exe '/'.reA."/mark y"
    if line("'y")>0 && getline("'y")=~reA
      let reA='^.*uuid(\s*\([^)]*\)\s*).*$'
      exe "'y?".reA."? mark y"
      return substitute(matchstr(getline("'y"),reA),reA,'\1','')
    endif
endfun

" Add an interface to a co-class
" \retval: added Interface was added to existing CoClass 
" \retval: created CoClass with specified Interface was created

fun! s:AddCoClass(class,interface,gsdep,noncreateable, aggregateable)
    norm mz
    let x='coclass\s\+'.a:class
    exe 'norm :s/'.x."/&/e\<cr>"
    if getline(".") =~ x
      " Find brace
      /\s*}/mark y
      let ln=line("'y")
      norm my
      let text="interface ".a:interface.";"
      exe  ln
      exe 'norm O'.text."\<ESC>"
      norm `y
      return 'added'
    else
      let text=substitute(g:coclass,'%CLASS%',a:class,'g')
      let text=substitute(text,'%INTERFACE%',a:interface,'g')
      let clsid=s:IDLGetUUID()
      let uuidspec=substitute(clsid,g:uuidstr_sub,( (a:gsdep) ? (g:uuidstr_gs) : (g:uuidstr) ), '')
      let text=substitute(text,'%UUIDSPEC%',uuidspec,'')
      let extras=''
      if a:noncreateable
        let extras=",\nnoncreatable"
      endif
      if a:aggregateable
        let extras=",\naggregatable"
      endif
      let text=substitute(text,'%EXTRAS%',extras,'')
      /^library/mark y
      'y/\s*{/
      "'
      exe 'norm %O'.text."\<ESC>"
      return 'created'
    endif
    norm `z
endfun

let uuidstr='uuid(\1\2)'
let uuidstr_gs='GS_UUID(\2)'
let uuidstr_sub='^\(....\)\(.*\)$'

".idl
let coclass="
\[
\\<cr>  %UUIDSPEC%,
\\<cr>helpstring(\"%CLASS% Class\")%EXTRAS%
\\<cr>\<c-d>]
\\<cr>coclass %CLASS%
\\<cr>{
\\<cr>[default] interface %INTERFACE%;
\\<cr>};
\\<cr>"

" Create an RGS file for the given named program identifier and class
fun! s:CreateRGS(progid,class,filename)
  if filereadable(a:filename)
      return "Exists"
  else
    let coclass=GetCoClassID(a:class)
    let tlibid=GetTypelibID()
    if coclass=="" || tlibid==""
      return "Cancelled"
    endif
    let text=substitute(g:rgsfile,'%PROGID%',a:progid,'g')
    let text=substitute(text,'%CLASS%',a:class,'g')
    let text=substitute(text,'%CLSID%',coclass,'g')
    let text=substitute(text,'%TYPELIBID%',tlibid,'g')

    exe 'new '.a:filename
    set paste
    exe "norm i".text."\<esc>"
    set nopaste
    
    return a:filename." Created"
  endif 
endfun
fun! s:ToLower( str)
  return substitute(a:str,'^.*$','\L&','')
endfun

" find MSRCFile
fun! s:FindRCFile()
  let rcs=expand('*.rc')
  while rcs !~ '^\s*$'
    let rcfi=match(rcs,"\n")
    if rcfi >= 0
      let rcf=strpart(rcs,0,rcfi)
      let rcs=strpart(rcs,rcfi+1, 0xffff)
    else
      let rcf=rcs
      let rcs=""
    endif
    if system('head -1 '.rcf) =~ '^//.*generated resource script' 
      return rcf
    endif
  endwhile
  return ""
endfun

fun! s:QueryCheckout()
  if &ro 
    let res=confirm("File is ReadOnly, Check Out of SourceSafe?","&Yes\n&Cancel",1,"Question")
    if res==1
      norm ,sk
      return !&ro
    endif
    return 0
  endif
  return 1
endfun

" Insert new RGSFile into the RESFile
fun! s:InsertRGS(resfile,rgsfile,resource)
  if filereadable(a:resfile)
    exe 'new '.a:resfile
    let end=line("$")
    let cur=1
    let found=0
    let foundreg=0
    let foundres=0
    let laststudinvoke=0
    let lastrgs=0
    if search('^\s*'.a:resource.'\>') > 0
      let found=1
      call input('found')
    else
      call input('inserting')
      while cur<end && !found
        let l=getline(cur)
        if l =~ '#endif.*resources$' 
          let foundres=1
          if foundreg
            let found=1
          endif
        elseif s:ToLower(l) =~ ToLower(a:rgsfile)
          return "Already there"
        elseif s:ToLower(l) =~ '\.rgs\>'
          let lastrgs=cur
        elseif l =~ '#ifndef APSTUDIO'
          let laststudinvoke=cur
        elseif l =~ '^VS_VERSION_INFO'
          let foundres=0
        elseif !foundreg && l =~ '^// REGISTRY'
          let foundreg=1
          let regline=cur+1
        elseif foundreg && l =~ '^IDR\k\+\s\+REGISTRY'
          let regline=cur
        endif
        let cur=cur+1
      endwhile
      if !s:QueryCheckout()
        return "NotWriteable"
      endif
        
      if !found
        if laststudinvoke > 0
          let cur=laststudinvoke
          let found=1
        endif
        if !foundres
          exe cur
          set paste
          exe "norm O".g:AUSHeader."\<esc>"
          set nopaste
          let cur=line(".")-2
          let found=1
        endif
        if found
          exe cur
          set paste
          exe "norm O/////////////////////////////////////////////////////////////////////////////\<cr>"
          \."//\<CR>// REGISTRY\<CR>//\<cr>\<cr>\<esc>"
          set nopaste
          let cur=line('.')
        endif
      else
        if lastrgs > 0
          let cur=lastrgs+1
        elseif foundreg
          let cur=regline+1
        endif
      endif
      if found
        exe cur
        set paste
        exe "norm O".a:resource." REGISTRY DISCARDABLE \"".a:rgsfile."\"\<esc>"
        set nopaste
        w
      endif
    endif
    if found
      e resource.h
      if search('#define\s\+'.a:resource.'\>')>0
        return "OK"
      endif
      if !s:QueryCheckout()
        return "NotWriteable"
      endif
      set paste
      let x='\(^#define\s\+_APS_NEXT_SYMED_VALUE\s\+\)\([0-9]\+\)'
      exe '/'.x.'/'
      let newresource=100
      if getline('.') =~ x
        let newresource=substitute(matchstr(getline('.'),x),x,'\2','')
        exe 's/'.x.'/\1'.(newresource+1)
        exe '/^#define[^0-9]*\<'.(newresource-1).'\>'
      else
        /^\([^/]\|$\)/
      endif
      exe "norm o#define ".a:resource." ".newresource."\<ESC>"
      set nopaste
      return "OK"
    endif
    return "Error"
  else
    call confirm("Can't find resource","OK",1,"Error")
    return "Can't find resource"
  endif
endfun

fun! s:CreateATLClass( classname, class, interface, library, projname, resource, dispatch, component, multithread, gsdep  )
  if a:dispatch
    let impl=g:ATLDispImpl
    let dmap=g:ATLDispImplEntry
  else
    let impl='%INTERFACE%'
    let dmap=''
  endif 
  if a:component 
    let text=substitute(g:ATLComponentStub,'%COMPONENT_MAP%',g:ATLComponentMapStub,'')
  elseif a:gsdep
    let text=substitute(g:ATLComponentStub,'%COMPONENT_MAP%','','')
  else
    let text=g:ATLStub
  endif
  if !exists("text")
    return
  endif
  if a:multithread
    let thread='Multi'
    let threading='Both'
  else
    let thread='Single'
    let threading='Apartment'
  endif
  let text=substitute(text,'%INTERFACEIMPL%',impl,'g')
  let text=substitute(text,'%DISPATCH%',dmap,'g')
  let text=substitute(text,'%CLASSNAME%',a:classname,'g')
  let text=substitute(text,'%INTERFACE%',a:interface ,'g')
  let text=substitute(text,'%CLASS%',a:class ,'g')
  let text=substitute(text,'%LIBRARY%',a:library,'g')
  let text=substitute(text,'%RESOURCEID%',a:resource,'g')
  let text=substitute(text,'%PROGID%',a:projname,'g')
  let text=substitute(text,'%THREAD%',thread,'g')
  let text=substitute(text,'%THREADING%',threading,'g')
  let here=line('.')
  set paste
  exe 'norm o'.text."\<esc>"
  set nopaste
  exe here
  /<IMPL_INTERFACE>/ d |call s:ATLAddInterface( a:interface )
  
endfun

" AUS resource header
let AUSHeader="
\\<CR>/////////////////////////////////////////////////////////////////////////////
\\<CR>// English (Australia) resources
\\<CR>
\\<CR>#if !defined(AFX_RESOURCE_DLL) || defined(AFX_TARG_ENA)
\\<CR>#ifdef _WIN32
\\<CR>LANGUAGE LANG_ENGLISH, SUBLANG_ENGLISH_AUS
\\<CR>#pragma code_page(1252)
\\<CR>#endif //_WIN32
\\<CR>\<CR>#endif    // English (Australia) resources
\\<CR>/////////////////////////////////////////////////////////////////////////////\<CR>"

"%INTERFACE%.rgs
let rgsfile="
\\<cr>HKCR
\\<cr>{
\\<cr>  %PROGID%.%CLASS%.1 = s '%CLASS% Class'
\\<cr>  {
\\<cr>      CLSID = s '{%CLSID%}'
\\<cr>  }
\\<cr>  %PROGID%.%CLASS% = s '%CLASS% Class'
\\<cr>  {
\\<cr>      CLSID = s '{%CLSID%}'
\\<cr>      CurVer = s '%PROGID%.%CLASS%.1'
\\<cr>  }
\\<cr>  NoRemove CLSID
\\<cr>  {
\\<cr>      ForceRemove {%CLSID%} = s '%CLASS% Class'
\\<cr>      {
\\<cr>          ProgID = s '%PROGID%.%CLASS%.1'
\\<cr>          VersionIndependentProgID = s '%PROGID%.%CLASS%'
\\<cr>          ForceRemove 'Programmable'
\\<cr>          InprocServer32 = s '%MODULE%'
\\<cr>          {
\\<cr>              val ThreadingModel = s 'Apartment'
\\<cr>          }
\\<cr>          'TypeLib' = s '{%TYPELIBID%}'
\\<cr>      }
\\<cr>  }
\\<cr>}
\\<cr>"


"ATL Class stub
" %CLASSNAME%   Name of new class
" %INTERFACE%   Interface to implement
" %LIBRARY%     Library id
" %RESOURCEID%  ID of registry resources
" %CLASS"       Name of Class being implemented
let ATLDispImpl='IDispatchImpl<%INTERFACE%, \&IID_%INTERFACE%, \&LIBID_%LIBRARY%>'
let ATLDispImplEntry="\<cr>         COM_INTERFACE_ENTRY(IDispatch)"
let ATLStub="
\\<cr>#include \"resource.h\"       // main symbols
\\<cr>
\\<cr>/////////////////////////////////////////////////////////////////////////////
\\<cr>// %CLASSNAME%
\\<cr>
\\<cr>/** TODO: Comment.
\\<cr>  */
\\<cr>class ATL_NO_VTABLE %CLASSNAME% : 
\\<cr>  public CComObjectRootEx<CCom%THREAD%ThreadModel>,
\\<cr>  public ISupportErrorInfoImpl< &IID_%INTERFACE%>,
\\<cr>  public CComCoClass<%CLASSNAME%, \&CLSID_%CLASS%>,
\\<cr>  public %INTERFACEIMPL%
\\<cr>{
\\<cr>  public:
\\<cr>      %CLASSNAME%();
\\<cr>          ///< default constructor.
\\<cr>
\\<cr>      HRESULT FinalConstruct();
\\<cr>          ///< Called to finalise construction.
\\<cr>
\\<cr>      <IMPL_INTERFACE>
\\<cr>
\\<cr>      DECLARE_REGISTRY_RESOURCEID(%RESOURCEID%)
\\<cr>
\\<cr>      DECLARE_PROTECT_FINAL_CONSTRUCT()
\\<cr>
\\<cr>      BEGIN_COM_MAP(%CLASSNAME%)
\\<cr>          COM_INTERFACE_ENTRY(%INTERFACE%)%DISPATCH%
\\<cr>          COM_INTERFACE_ENTRY(ISupportErrorInfo)
\\<cr>      END_COM_MAP()
\\<cr>};
\\<cr>"


if idl_mc_gs_stuff

let ATLComponentMapStub="
\\<cr>
\\<cr>          REGMAP_UUID (\"COMPONENT_UUID\", CLSID_NULL) // TODO: Complete
\\<cr>          REGMAP_ENTRY(\"COMPONENT_DESC\", \"All\")   // TODO: Complete
\\<cr>          REGMAP_ENTRY(\"OPTIONS\", \"\")
\"
let ATLComponentStub="
\\<cr>#include \"resource.h\"       // main symbols
\\<cr>#include \"comlib/registrymap.hpp\"       // For extended registry functions
\\<cr>
\\<cr>/////////////////////////////////////////////////////////////////////////////
\\<cr>// %CLASSNAME%
\\<cr>
\\<cr>/** TODO: Comment.
\\<cr>  */
\\<cr>class ATL_NO_VTABLE %CLASSNAME% : 
\\<cr>  public CComObjectRootEx<CCom%THREAD%ThreadModel>,
\\<cr>  public ISupportErrorInfoImpl< &IID_%INTERFACE%>,
\\<cr>  public CComCoClass<%CLASSNAME%, \&CLSID_%CLASS%>,
\\<cr>  public %INTERFACEIMPL%
\\<cr>{
\\<cr>  public:
\\<cr>      %CLASSNAME%();
\\<cr>          ///< default constructor.
\\<cr>
\\<cr>      HRESULT FinalConstruct();
\\<cr>          ///< Called to finalise construction.
\\<cr>
\\<cr>      <IMPL_INTERFACE>
\\<cr>
\\<cr>      DECLARE_PROTECT_FINAL_CONSTRUCT()
\\<cr>
\\<cr>      BEGIN_COM_MAP(%CLASSNAME%)
\\<cr>          COM_INTERFACE_ENTRY(%INTERFACE%)%DISPATCH%
\\<cr>          COM_INTERFACE_ENTRY(ISupportErrorInfo)
\\<cr>      END_COM_MAP()
\\<cr>
\\<cr>      DECLARE_REGISTRY_RESOURCEID_EX(%RESOURCEID%)
\\<cr>      BEGIN_REGISTRY_MAP(%CLASSNAME%)
\\<cr>          REGMAP_ENTRY(\"PROGID\",      \"%PROGID%.%CLASS%\")
\\<cr>          REGMAP_ENTRY(\"VERSION\",     GS_VERSTR(01))
\\<cr>          REGMAP_ENTRY(\"DESCRIPTION\", \"%CLASS% Class\")
\\<cr>          REGMAP_UUID (\"CLSID\",       CLSID_%CLASS%)
\\<cr>          REGMAP_UUID (\"IID\",         IID_%INTERFACE%)
\\<cr>          REGMAP_UUID (\"LIBID\",       LIBID_%LIBRARY%)%COMPONENT_MAP%
\\<cr>
\\<cr>          REGMAP_ENTRY(\"THREADING\",   \"%THREADING%\")
\\<cr>      END_REGISTRY_MAP()
\\<cr>
\\<cr>};
\\<cr>"
endif

fun! s:ConvertTypes()
  s/virtual\s*//e
  s/\<const\>\s\+\(\<\k\+\>\(\s*<\s*\k\+>\s*\)\=\)\s*&\s*/\1 /ge
  s/&/*/ge
  s/\<cFoton\>\s*/long /ge
  s/\<\(bool\|BOOL\)\>\s*/VARIANT_BOOL /ge
  s/\<\(cString\|bstr_t\|string\|CComBSTR\|_bstr_t\)\>\s*/BSTR /ge
  s/\<\(cDate\|cDateTime\|datetime_t\)\>\s*/DATE /ge
  s/\<\(cVariant\|variant_t\|CComVariant\)\>\s*/VARIANT /ge
  s/\<\(com_ptr\|CComPtr\|_com_ptr\)<\s*\<\(\k\+\)\>\s*>\s*/\2* /ge
  s/\<_\=I\(\k\+\)Ptr\>/\1 */ge
  s/\<int\>/long/ge
  s/\<safearray_t<\s*\<\(\k\+\>[^>]\{-}\)\s*>\s*/SAFEARRAY(\1) /ge

endfun


fun! s:IDLGenerateIDLStub()
  call s:ConvertTypes()
  let x='\([(,]\)\s*[^[]\(\<\(SAFEARRAY([^)]*)\)\=[^,)]\+\)'
  let done=0
  while !done
    let arg=substitute(matchstr(getline('.'),x),x,'\2','')
    if arg==""
      let done=1
      break
    else
      if arg=~'^I.*\*\s*\k\+$'
        if arg=~'\*\*\s*\k\+$'
          let type='[out]'
        else
          let type='[in]'
        endif
      elseif arg=~'\*\s*\k\+$'
        let type='[out]'
      else
        let type='[in]'
      endif
      exe 's/'.x.'/\1 '.type.' \2'
    endif
  endwhile
  s/\[out\]/[in,out]/ge

"  let reA='^\s*\<\(\%(SAFEARRAY([^)]*)\|[^(]\{-1,}\)\(\s*\*\)\=\)\=\s*\(\<\k\+\>\)\(\s*(\s*\([^)]*\)\s*)\)\='
  let x='^\(\s*\)\<\(\%(SAFEARRAY([^)]*)\|\k\+\(\s*[*&]\)\=\)\)\=\s\+\(\(\k\+\)\s*(\(.*\)\))[^)]*;\=\s*$'
  let txt=getline('.') 
  if match(txt,x)== -1
      echoe 'Did not recognise line as a function'
      return
  endif

  let type=substitute(txt,x,'\2','')
  let arguments=substitute(txt,x,'\6','')
  if type=="" || type=="void" || type=="[cC]Error" || type=="HRESULT" || type=="VOID"
    let lastarg=""
  else
    if type !~ '[*&]$'
      let type=type.' '
    endif
    let lastarg="[out,retval] ".type."*ret"
    if arguments !~'^\s*$'
      let lastarg=', '.lastarg
    endif
  endif
  exe 's/'.x.'/\1HRESULT \u\4'.lastarg.');'
  let comment=GetComment(line('.'))
  if comment==""
    let comment = substitute(substitute(txt,x,'\5',''),'\C\([a-z]\)\([A-Z]\)','\1 \l\2','g').' method'
  endif
  if comment!="" && getline(line('.')-1) !~ '\['
    set paste
    exe 'norm O[helpstring("'.comment.'")]'."\<esc>=="
    set nopaste
  endif
endfun

fun! s:ModifyGUID()
  let guid = s:IDLGetUUID()
  let guidlen=strlen(guid)
  let mx='\<\x\{4,8}-\(\x\{4}-\)\{3}\x\{12}\>'
  let match=matchstr(getline('.'),mx)
  exe 's/'.mx.'/'.strpart(guid,guidlen-strlen(match), guidlen  ).'/'
endfun
" vim:sw=2 ts=2 et
