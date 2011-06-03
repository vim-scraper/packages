" File    :wrap.vim
" Required:fmt
" Language:Vim 7.2 ex script
" Platform:Any
" Title   :Word Wrap for Vim
" Author  :Roy Mathur
" Licence :Freeware, (C)Roy Mathur 2008
" Contact :Roy.Mathur@gmail.com
"
" Notes   :Installation/Usage
"          ------------------
"
"          Win32
"          -----
"          Download fmt.exe from http://sourceforge.net/projects/unxutils
"          Add path to where you unzipped these utilities.
"          Place this script in the same directory as vim.exe
"          Run using-
"
"                     :run wrap.vim
"
"          UNIX OSs
"          --------
"          You can miss out installing 'fmt', 
"          as you already have it- just replace 'del' with 'rm' in the
"          script below.
"          
"          GVim
"          ----
"          Also is you are using gvim, change 'vim' to 'gvim'
"
"          Motivation
"          ----------
"          When ordinary and novice users (myself included) request that
"          something is useful (e.g. word wrap), why do the sodding 
"          'experts' blabber on about how it isn't needed instead of 
"          getting off their collective, lazy behinds and actually 
"          contributing useful information?  I did this so that I could
"          use vim to read Neal Stephenson's esaay 'In the Begining was
"          the Command Line'.  Just google to find it- its a good read IF
"          YOU HAVE WORD WRAP!  
"
" Version :1.0 19/08/2008
"          --------------
"          Very simple script using fmt. I wanted to copy the way cream
"          does word wrap, but I'm too stupid to understand this and I am
"          fed up of waiting for someone to add word wrap to vim, so I 
"          wrote this little thingy myself.  It does require downloading 
"          fmt and a few minor changes if you are using a non UNIX type 
"          platform like Win32 or you use gvim.  
"
"*************************************************************************************
"*************************************************************************************
"
"                         START OF CODE
"                         -------------
"create wrapped tmp file from current file
:!fmt -s % > wrap.tmp
"start another instance of vim to open it
:!gvim wrap.tmp
"delete the tmp wrapped file when you are finished and return to the
"orginal file
:!del wrap.tmp
"
"                          END OF CODE
"                          -----------
"
"*************************************************************************************
"*************************************************************************************
