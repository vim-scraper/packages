" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin/twitvim.vim	[[[1
156
" ==============================================================
" TwitVim - Post to Twitter from Vim
" Based on Twitter Vim script by Travis Jeffery <eatsleepgolf@gmail.com>
"
" Version: 0.1.2
" License: Vim license. See :help license
" Language: Vim script
" Maintainer: Po Shan Cheah <morton@mortonfox.com>
" Created: March 28, 2008
" Last updated: April 3, 2008
"
" GetLatestVimScripts: 2204 1 twitvim.vim
" ==============================================================

" Load this module only once.
if exists('loaded_twitvim')
    finish
endif
let loaded_twitvim = 1

" Avoid side-effects from cpoptions setting.
let s:save_cpo = &cpo
set cpo&vim

let s:proxy = ""
let s:login = ""

" The extended character limit is 246. Twitter will display a tweet longer than
" 140 characters in truncated form with a link to the full tweet. If that is
" undesirable, set s:char_limit to 140.
let s:char_limit = 246

let s:twupdate = "http://twitter.com/statuses/update.xml?source=vim"

function! s:get_config_proxy()
    " Get proxy setting from twitvim_proxy in .vimrc or _vimrc.
    " Format is proxysite:proxyport
    if exists('g:twitvim_proxy')
	let s:proxy = "-x " . g:twitvim_proxy
    else
	let s:proxy = ""
    endif
endfunction

" Get user-config variables twitvim_proxy and twitvim_login.
function! s:get_config()
    call s:get_config_proxy()

    " Get Twitter login info from twitvim_login in .vimrc or _vimrc.
    " Format is username:password
    if exists('g:twitvim_login') && g:twitvim_login != ''
	let s:login = "-u " . g:twitvim_login
    else
	" Beep and error-highlight 
	execute "normal \<Esc>"
	echohl ErrorMsg
	echomsg 'Twitter login not set.'
	    \ 'Please add to .vimrc: let twitvim_login="USER:PASS"'
	echohl None
	return -1
    endif
    return 0
endfunction

" Common code to post a message to Twitter.
function! s:post_twitter(mesg)
    " Get user-config variables twitvim_proxy and twitvim_login.
    " We get these variables every time before posting to Twitter so that the
    " user can change them on the fly.
    let rc = s:get_config()
    if rc < 0
	return -1
    endif

    let mesg = a:mesg

    " Remove trailing newline. You see that when you visual-select an entire
    " line. Don't let it count towards the tweet length.
    let mesg = substitute(mesg, '\n$', '', "")

    " Convert internal newlines to spaces.
    let mesg = substitute(mesg, '\n', ' ', "g")

    " Check tweet length. Note that the tweet length should be checked before
    " URL-encoding the special characters because URL-encoding increases the
    " string length.
    if strlen(mesg) > s:char_limit
	echohl WarningMsg
	echo "Your tweet has" strlen(mesg) - s:char_limit
	    \ "too many characters. It was not sent."
	echohl None
    elseif strlen(mesg) < 1
	echohl WarningMsg
	echo "Your tweet was empty. It was not sent."
	echohl None
    else
	" URL-encode some special characters so they show up verbatim.
	let mesg = substitute(mesg, '%', '%25', "g")
	let mesg = substitute(mesg, '"', '%22', "g")
	let mesg = substitute(mesg, '&', '%26', "g")

	let output = system("curl ".s:proxy." ".s:login.' -d status="'.
		    \mesg.'" '.s:twupdate)
	if v:shell_error != 0
	    echohl ErrorMsg
	    echomsg "Error posting your tweet. Result code: ".v:shell_error
	    echomsg "Output:"
	    echomsg output
	    echohl None
	else
	    echo "Your tweet was sent. You used" strlen(mesg) "characters."
	endif
    endif
endfunction

function! s:CmdLine_Twitter()
    " Do this here too to check for twitvim_login. This is to avoid having the
    " user type in the message only to be told that his configuration is
    " incomplete.
    let rc = s:get_config()
    if rc < 0
	return -1
    endif

    call inputsave()
    let mesg = input("Your Twitter: ")
    call inputrestore()
    call s:post_twitter(mesg)
endfunction

" Prompt user for tweet.
if !exists(":PosttoTwitter")
    command PosttoTwitter :call <SID>CmdLine_Twitter()
endif

" Post current line to Twitter.
if !exists(":CPosttoTwitter")
    command CPosttoTwitter :call <SID>post_twitter(getline('.'))
endif

" Post entire buffer to Twitter.
if !exists(":BPosttoTwitter")
    command BPosttoTwitter :call <SID>post_twitter(join(getline(1, "$")))
endif

" Post visual selection to Twitter.
noremap <SID>Visual y:call <SID>post_twitter(@")<cr>
noremap <unique> <script> <Plug>TwitvimVisual <SID>Visual
if !hasmapto('<Plug>TwitvimVisual')
    vmap <unique> T <Plug>TwitvimVisual
endif

let &cpo = s:save_cpo
finish

" vim:set tw=0:
doc/twitvim.txt	[[[1
125
*twitvim.txt*  Post to Twitter from Vim

		      ---------------------------------
		      TwitVim: A Twitter client for Vim
		      ---------------------------------

Author: Po Shan Cheah <morton@mortonfox.com> 
	http://twitter.com/mortonfox

License: The Vim License applies to twitvim.vim and twitvim.txt (see
	|copyright|) except use "TwitVim" instead of "Vim". No warranty,
	express of implied. Use at your own risk.


==============================================================================
1. Contents					*TwitVim* *TwitVim-contents*

	1. Contents................................: |TwitVim-contents|
	2. Introduction............................: |TwitVim-intro|
	3. Installation............................: |TwitVim-install|
	   cURL....................................: |TwitVim-cURL|
	   twitvim.vim.............................: |TwitVim-add|
	   twitvim_login...........................: |twitvim_login|
	   twitvim_proxy...........................: |twitvim_proxy|
	4. Manual..................................: |TwitVim-manual|
	4.1. Commands..............................: |TwitVim-commands|
	     :PosttoTwitter........................: |:PosttoTwitter|
	     :CPosttoTwitter.......................: |:CPosttoTwitter|
	     :BPosttoTwitter.......................: |:BPosttoTwitter|
	4.2. Mapping...............................: |TwitVim-mapping|
	     T.....................................: |TwitVim-v_T|
	5. History.................................: |TwitVim-history|
	6. Credits.................................: |TwitVim-credits|


==============================================================================
2. Introduction						*TwitVim-intro*

TwitVim is a plugin that allows you to post to Twitter, a microblogging
service at http://www.twitter.com.


==============================================================================
3. Installation						*TwitVim-install*

	1. Install cURL.				*TwitVim-cURL*

	If you don't already have cURL on your system, download it from
	http://curl.haxx.se/. Make sure that the curl executable is in a
	directory listed in your PATH environment variable, or the equivalent
	for your system.


	2. twitvim.vim					*TwitVim-add*

	Add twitvim.vim to your plugins directory. The location depends on
	your operating system. See |add-global-plugin| for details.


	3. twitvim_login				*twitvim_login*

	Add the following to your vimrc:

		let twitvim_login = "USER:PASS"

	Replace USER with your Twitter user name and PASS with your Twitter
	password.


	4. twitvim_proxy				*twitvim_proxy*

	This step is only needed if you access the web through a HTTP proxy.
	If you use a HTTP proxy, add the following to your vimrc:

		let twitvim_proxy = "proxyserver:proxyport"

	Replace proxyserver with the address of the HTTP proxy and proxyport
	with the port number of the HTTP proxy.


==============================================================================
4. TwitVim Manual					*TwitVim-manual*

------------------------------------------------------------------------------
4.1. TwitVim Commands					*TwitVim-commands*

	:PosttoTwitter					*:PosttoTwitter*

	This command will prompt you for a message and post it to Twitter.

	:CPosttoTwitter					*:CPosttoTwitter*

	This command posts the current line in the current buffer to Twitter.

	:BPosttoTwitter					*:BPosttoTwitter*

	This command posts the contents of the current buffer to Twitter.

------------------------------------------------------------------------------
4.2. TwitVim Mapping					*TwitVim-mapping*

	T						*TwitVim-v_T*

	In visual mode, the T key posts the highlighted text to Twitter.


==============================================================================
5. TwitVim History					*TwitVim-history*

	0.1.2 : 2008-04-03 * Make plugin conform to guidelines in
    			    |write-plugin|.
			   * Add help documentation.
	0.1.1 : 2008-04-01 * Add error reporting for cURL problems.
	0.1   : 2008-03-28 * Initial release.

==============================================================================
6. TwitVim Credits					*TwitVim-credits*

	Thanks to Travis Jeffery, the author of the original VimTwitter script
	(vimscript #2124), who came up with the idea of running cURL from Vim
	to access the Twitter API.


==============================================================================
vim:tw=78:ts=8:ft=help:norl:
