" Vim syntax file
" Language:	Nant build file (xml)
" Maintainer:	Juan Chanco <jchanco@fastmail.fm>
" Last Change:	Sat Mar 10 10:50:55 CST 2007
" Filenames:	*.build
" License:	This file is placed in the public domain.

" Quit when a syntax file was already loaded
if exists("b:current_syntax")
    finish
endif

let s:nant_cpo_save = &cpo
set cpo&vim

runtime! syntax/xml.vim

syn case ignore

" this started from the ANT sytax file, which supports inline script.
" i'll add that when/if i figure it out and/or have time
"if !exists('*NantSyntaxScript')
"    fun NantSyntaxScript(tagname, synfilename)
"        unlet b:current_syntax
"        let s:include = expand("<sfile>:p:h").'/'.a:synfilename
"        if filereadable(s:include)
"            exe 'syn include @nant'.a:tagname.' '.s:include
"        else
"            exe 'syn include @nant'.a:tagname." $VIMRUNTIME/syntax/".a:synfilename
"        endif

"        exe 'syn region nant'.a:tagname
"                    \." start=#<script[^>]\\{-}language\\s*=\\s*['\"]".a:tagname."['\"]\\(>\\|[^>]*[^/>]>\\)#"
"                    \.' end=#</script>#'
"                    \.' fold'
"                    \.' contains=@nant'.a:tagname.',xmlCdataStart,xmlCdataEnd,xmlTag,xmlEndTag'
"                    \.' keepend'
"        exe 'syn cluster xmlRegionHook add=nant'.a:tagname
"    endfun
"endif

" TODO: add more script languages here ?
" call NantSyntaxScript('javascript', 'javascript.vim')
" call NantSyntaxScript('jpython', 'python.vim')


syn cluster xmlTagHook add=nantElement
syn cluster xmlAttribHook add=nantAttribute

syn keyword nantElement display al arg asminfo assemblies assemblyfileset assemblyfolders attachments attrib
syn keyword nantElement display attribute attributes available aximp buildfiles call categories certificates cl
syn keyword nantElement display code comparefiles copy credential credentials csc cvs cvs-changelog cvs-checkout
syn keyword nantElement display cvs-export cvs-pass cvs-rtag cvs-tag cvs-update cvsfileset define defines
syn keyword nantElement display delay-sign delayloaded delete description dirset do documenters echo
syn keyword nantElement display elementTest1 embeddedresources environment exclude excludeprojects excludes
syn keyword nantElement display excludesfile exec extensionobject extensionobjects fail files fileset filter
syn keyword nantElement display filterchain forcedusingfiles foreach formatter get gunzip if ifnot
syn keyword nantElement display ignorelibraries ilasm ildasm import imports in include includedirs includes
syn keyword nantElement display includesList includesfile infiles items jsc lib libdirs library license link
syn keyword nantElement display loadfile loadtasks mail map mc metadataincludedirs midl mkdir modules move
syn keyword nantElement display namespace namespaceimports namespaces nant nantschema ndoc nowarn nunit nunit2
syn keyword nantElement display option options package parameter parameters path pathelement pkg-references
syn keyword nantElement display project projects properties property proxy rc readregistry referencepaths
syn keyword nantElement display referenceprojects references regasm regex regsvcs resgen resourcefileset
syn keyword nantElement display resources script servicecontroller setenv sleep solution sourcefiles sources
syn keyword nantElement display style summaries symbol symbols sysinfo tar tarfileset target targetfiles targets
syn keyword nantElement display test testtask tlbexp tlbimp touch tstamp undefine undefines unzip uptodate
syn keyword nantElement display uptodatefiles variable vbc vjc warnaserror warning webmap xmlpeek xmlpoke zip
syn keyword nantElement display zipfileset

syn keyword nantAttribute display acf act-on-date act-on-tag action algid align alignment all app_config
syn keyword nantAttribute display appconfig append application archive asis asmversion assembly assemblyname base
syn keyword nantAttribute display baseaddress basedir bcclist builddirs buildfile bypassonlocal bytes cascade
syn keyword nantAttribute display casesensitive cclist char characterset checked class client clock codebase
syn keyword nantAttribute display codepage command commandline comment company comparefile componentsonly
syn keyword nantAttribute display compression compressionlevel configuration copyright cstub culture cvsfullpath
syn keyword nantAttribute display cvsroot cvsrsh date datetime debug default defaultexcludes define delaysign
syn keyword nantAttribute display delim depends description dest destdir destfile destination dir dirmode dlldata
syn keyword nantAttribute display doc domain duplicate dynamic dynamicprefix enablewebdav encoding end env error
syn keyword nantAttribute display evidence existingapp existingtlb extension fail fail-if-modified failonempty
syn keyword nantAttribute display failonerror file filealign filemode filename fileversion flags flatten force
syn keyword nantAttribute display force-head fork format from frompath generatesource gid groupname haltonerror
syn keyword nantAttribute display haltonfailure header headerpath hidden hive host hours httpproxy id if
syn keyword nantAttribute display ignoreerrors iid imports in includeemptydirs includevsfolders inheritall
syn keyword nantAttribute display inheritrefs input inputencoding item jcpa key keycontainer keyfile keysource
syn keyword nantAttribute display langid language langversion level libpath licensetarget line linenumbers listing
syn keyword nantAttribute display machine mailhost main mainclass managedextensions mcfile message millis
syn keyword nantAttribute display milliseconds minutes module moduledefinition move-if-exists name names namespace
syn keyword nantAttribute display namespaceuri no-shortening noconfig nodeindex noil noreconfig normal nostdlib
syn keyword nantAttribute display nowarn number objectfile ocx optimize optioncompare optionexplicit
syn keyword nantAttribute display optionoptimize options optionstrict out outfile output outputdir outputencoding
syn keyword nantAttribute display override-directory overridedir overwrite overwritelocal partition passfile
syn keyword nantAttribute display password path pattern pchfile pchmode pchthroughfile pdbfile platform port
syn keyword nantAttribute display prefix primary product productversion program property propertyexists
syn keyword nantAttribute display propertytrue proxy pruneempty publickey publiconly quiet quoteallnames
syn keyword nantAttribute display rawexceptionhandling rcfile rcpath rcw readonly readwrite reallyquiet rebuild
syn keyword nantAttribute display recursive refid regfile registered remove removeintchecks required
syn keyword nantAttribute display requirednotempty resource resourcefile resultproperty revision rootnamespace
syn keyword nantAttribute display seconds securescoping service solutionfile source src stampdatetime start
syn keyword nantAttribute display sticky-tag strictref style subject subsystem sysarray system tag target
syn keyword nantAttribute display target-ns targetexists template test testname timeout title tlb todir tofile
syn keyword nantAttribute display tokens tolist trademark transform transformfile trim type typelib typename uid
syn keyword nantAttribute display unicode unless unregister unsafe uptodatefile uri url usecvsignore usefile
syn keyword nantAttribute display username useruntimeengine usesharpcvslib usesourcepath usetimestamp utf8 value
syn keyword nantAttribute display verbose version visibility warnaserror warninglevel win32icon win32res
syn keyword nantAttribute display workingdir x xmlfile xpath zipfile ziplevel

hi def link nantElement Statement
hi def link nantAttribute Special

let b:current_syntax = "nant"

let &cpo = s:nant_cpo_save
unlet s:nant_cpo_save

" vim: ts=8
