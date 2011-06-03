"
" GoogleSearch
"
" Author: Jack Atkinson
" Contact: doxalogos AT gmail DOT com
" Version: 0.2
" Last Updated: 25 July 2008

" This plugin allows you to use Google Desktop Search engine to search text
" files from within VIM. 
" This requires python, pygoogledesktop script, and Google Desktop Search engine installed.
" Right now, this is a Windows only program, until I find time to modify it
" for other OS's
"
" Instructions:
"
" Make sure the correct version of Python is installed on your machine that
" VIM supports.  Type :version and you should see a "+python" or "+python/dyn"
" If it is not there, then you cannot use this script.
" Also in that same version command, check for "DFEAT_PYTHON" to see
" where python is installed for VIM to use and make sure your python is
" located there.
"
" Make sure you have a copy of PyGoogleDesktop python package installed
" as well. http://code.google.com/p/pythongoogledesktop or if you have
" setuptools, do "easy_install pygoogledesktop" to install
"
" Install googlesearch.vim into your plugin directory
"
" To run use :GDS text
" where text is what you want to search for
"
" There are a couple of global variables you can set in your vimrc file
" to help with the search.
" 
" g:gds_numresults - 
" This variable handles the number of results returned from a search.  If 
" you want all results returned, specify a 0 as the value.  Specifying
" a 0 could result in long search times.
"
" g:gds_extensions -
" This variable holds the file extensions you want searched and returned
" to you.  Specify the extensions separated by a comma (e.g. 'cpp,java,hpp')
"
" The search will create a buffer with the results called "GDSResults". And put
" you cursor in that window.  From there you can usually just press "gf" to
" open the file you want to check out then do a normal search command
" on the search string i.e "/text"
"
" Changes
" v0.1: Initial revision
" v0.2: fixed buffer quirks, updated debug capability
"
if !has('python')
    echo "Error: Required vim compiled with +python"
    finish
endif

"Check if gds_numresults is defined
"Note if its defined as 0, then that means return all the results that are 
"avialable. !!This could take a long time!!
if !exists('g:gds_numresults')
    let g:gds_numresults = 10
endif
"Check if gds_extensions is defined
"Make sure you seperate with commas
"Note if its defined as nothing, then that means return all the results no
"matter the extension
if !exists('g:gds_extensions')
    let g:gds_extensions = 'cpp,hpp,h,py,java,c,rb'
endif
"create buffer for results
"bad GDSResults
"setlocal nobuflisted
"setlocal bt="nofile"

function! s:SearchInGDS(text)
    "build the argument string
"    let i = 0
"    while i < argc()
"        let text = text + argv(i)
"        let i = i + 1
"    endwhile
    execute "python searchGDS('" . a:text . "', " . g:gds_numresults . ", '" . g:gds_extensions . "')"
    setlocal bt=nofile
    "sbuf! GDSResults
    setlocal nobl
"    write! GDSResults
    execute "/" . a:text . ""
endfunction

"Warning: This can slow things down big time!
function! s:GDSEnableDebug()
    execute "python EnableDebug()"
endfunction

function! s:GDSDisableDebug()
    execute "python DisableDebug()"
endfunction

"
"Commands
"
command! -nargs=+ GDS call s:SearchInGDS(<q-args>)
command! -nargs=0 EDGDS call s:GDSEnableDebug()
command! -nargs=0 DDGDS call s:GDSDisableDebug()


function! s:DefPython()
python << PYTHONEOF
import sys,pygoogledesktop
import vim
pyGDS = pygoogledesktop.PyGDS() 

def EnableDebug():
    pyGDS.EnableDebug()
    print "GDS debugging enabled"
    return 
   
def DisableDebug():
    pyGDS.DisableDebug()
    print "GDS debugging disabled"
    return 

def searchGDS(searchText,numresults,extensions):
    print "Querying " + searchText
    count,results = pyGDS.DoQueryHTTP(searchText,numResults=numresults,category='file', extension=extensions)

    b = None
    for buf in vim.buffers:
        if(buf.name.find('GDSResults') != -1):
            b = buf

    #did buffer exist?
    if(b == None):
        print 'buffer not found'
        #add it back in
        vim.command('bad GDSResults')
        vim.command('setlocal nobl')
        vim.command('setlocal bt=\"nofile\"')
        for buf in vim.buffers:
            if(buf.name.find('GDSResults') != -1):
                b = buf

    vim.command("sbuf! GDSResults")
    vim.command("setlocal nobuflisted") 
    vim.command("setlocal bt=\"nofile\"") 
    cb = vim.current.buffer
    if(cb.name.find('GDSResults') == -1):
       print("Error finding buffer")
       return 

    #clear the buffer
    cb[:] = None
    cw = vim.current.window
    cw.height = 20
    #place the path and file name in the buffer
    cb[0] = ('Google Desktop Search found ' + str(count) + ' results for ' + searchText)
    if(len(results)):
        cb.append(str(len(results)) + " results returned for your category and extension")
        for result in results:
            cb.append(result[2])
    else:
        cb.append('But no results found with your given category or extension')

    return

PYTHONEOF
endfunction

call s:DefPython()
" vim: set et ts=4:
