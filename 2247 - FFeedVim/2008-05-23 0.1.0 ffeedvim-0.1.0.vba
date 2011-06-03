" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin/ffeedvim.vim	[[[1
293
" ==============================================================
" FFeedVim - Post to Friendfeed from Vim
"
" Version: 0.1.0
" License: Vim license. See :help license
" Language: Vim script
" Maintainer: Po Shan Cheah <morton@mortonfox.com>
" Created: May 20, 2008
" Last updated: May 23, 2008
"
" GetLatestVimScripts: 2247 1 ffeedvim.vim
" ==============================================================

" Load this module only once.
if exists('loaded_ffeedvim')
    finish
endif
let loaded_ffeedvim = 1

" Avoid side-effects from cpoptions setting.
let s:save_cpo = &cpo
set cpo&vim

let s:proxy = ""
let s:login = ""

let s:ffupdate = '-F "via=ffeedvim" http://friendfeed.com/api/share'

function! s:get_config_proxy()
    " Get proxy setting from ffeed_proxy in .vimrc or _vimrc.
    " Format is proxysite:proxyport
    let s:proxy = exists('g:ffeed_proxy') ? '-x "'.g:ffeed_proxy.'"': ""
    " If ffeed_proxy_login exists, use that as the proxy login.
    " Format is proxyuser:proxypassword
    "
    " If ffeed_proxy_login_b64 exists, use that instead. This is the proxy
    " user:password in base64 encoding.
    if exists('g:ffeed_proxy_login_b64')
	let s:proxy .= ' -H "Proxy-Authorization: Basic '.g:ffeed_proxy_login_b64.'"'
    else
	let s:proxy .= exists('g:ffeed_proxy_login') ? ' -U "'.g:ffeed_proxy_login.'"' : ''
    endif
endfunction

" Get user-config variables ffeed_proxy and ffeed_login.
function! s:get_config()
    call s:get_config_proxy()

    " Get Friendfeed login info from ffeed_login in .vimrc or _vimrc.
    " Format is username:remotekey
    "
    " Note: Get remotekey from http://friendfeed.com/remotekey
    " It is not the same as your Friendfeed login password.
    "
    " If ffeed_login_b64 exists, use that instead. This is the user:remotekey
    " in base64 encoding.
    if exists('g:ffeed_login_b64')
	let s:login = '-H "Authorization: Basic '.g:ffeed_login_b64.'"'	
    elseif exists('g:ffeed_login') && g:ffeed_login != ''
	let s:login = '-u "'.g:ffeed_login.'"'
    else
	" Beep and error-highlight 
	execute "normal \<Esc>"
	redraw
	echohl ErrorMsg
	echomsg 'Friendfeed login not set.'
	    \ 'Please add to .vimrc: let ffeed_login="USER:PASS"'
	echohl None
	return -1
    endif
    return 0
endfunction

" URL-encode a string.
function! s:url_encode(str)
    return substitute(a:str, '[^a-zA-Z_-]', '\=printf("%%%02X", char2nr(submatch(0)))', 'g')
endfunction

" Parse link and message from a user-supplied string.
" Returns a list with the message in the first element and the link in the
" second.
function! s:parse_link(mesg)
    let linkre = '\%(http\|https\|ftp\)://\S\+'
    let mesg = a:mesg

    " Try matching with link at the start of the string.
    let matchres = matchlist(mesg, '^\('.linkre.'\)\s\+\(.*\)$')
    if matchres != []
	return [ matchres[2], matchres[1] ]
    endif
    
    " Try matching with link at the end of the string.
    let matchres = matchlist(mesg, '^\(.*\)\s\+\('.linkre.'\)$')
    if matchres != []
	return [ matchres[1], matchres[2] ]
    endif

    return [ mesg, '' ]
endfunction

" Parse comment from a message.
function! s:parse_comment(mesg)
    let matchres = matchlist(a:mesg, '^\(.*\)//\(.*\)$')
    return matchres == [] ? [ a:mesg, '' ] : [ matchres[1], matchres[2] ]
endfunction

" Parse room ID from a message.
function! s:parse_room(mesg)
    let matchres = matchlist(a:mesg, '\c^room=\(\S\+\)\s\+\(.*\)$')
    return matchres == [] ? [ a:mesg, '' ] : [ matchres[2], matchres[1] ]
endfunction

" Remove leading and trailing whitespace.
function! s:chomp(s)
    let s = substitute(a:s, '\s\+$', '', '')
    return substitute(s, '^\s\+', '', '')
endfunction

" Escape double quotes.
function! s:escape(s)
    return substitute(a:s, '"', '\\&', 'g')
endfunction

" Post a message to Friendfeed.
function! s:post_ffeed(mesg, imgfile)
    " Get user-config variables ffeed_proxy and ffeed_login.
    " We get these variables every time before posting to Friendfeed so
    " that the user can change them on the fly.
    let rc = s:get_config()
    if rc < 0
	return -1
    endif

    " Convert internal newlines to spaces.
    " Remove leading and trailing whitespace.
    let mesg = s:chomp(substitute(a:mesg, '\n', ' ', 'g'))

    " Parse out the link if user supplied one.
    let [ mesg, link ] = s:parse_link(mesg)

    let link = s:escape(link)
    let linkparm = link == '' ? '' : '-F "link='.link.'"'

    " Parse out the comment if user supplied one.
    let [ mesg, comment ] = s:parse_comment(mesg)

    let comment = s:escape(s:chomp(comment))
    let commentparm = comment == '' ? '' : '-F "comment='.comment.'"'

    " Parse out the room name if user supplied one.
    let [ mesg, room ] = s:parse_room(mesg)

    let mesg = s:escape(s:chomp(mesg))
    let mesgparm = '-F "title='.mesg.'"'

    let roomparm = room == '' ? '' : '-F "room='.room.'"'

    " Upload an image file if user supplied one.
    let imgfile = s:escape(a:imgfile)
    let imgparm = imgfile == '' ? '' : '-F "img=@'.imgfile.'"'

    if strlen(mesg) < 1
	redraw
	echohl WarningMsg
	echo "Your message was empty. It was not sent."
	echohl None
    else
	redraw
	echo "Sending update to Friendfeed..."
	let output = system('curl -s '.s:proxy.' '.s:login.' '.mesgparm.' '.imgparm.' '.linkparm.' '.commentparm.' '.roomparm.' '.s:ffupdate)
	if v:shell_error != 0
	    redraw
	    echohl ErrorMsg
	    echomsg "Error posting your Friendfeed message. Result code: ".v:shell_error
	    echomsg "Output:"
	    echomsg output
	    echohl None
	else
	    " Check for errors from Friendfeed.
	    " On errors, Friendfeed simply returns a web page with the error
	    " title in a h1 tags.
	    let matchres = matchlist(output, '<h1>\(.*\)</h1>')
	    if matchres == []
		redraw
		echo "Your Friendfeed message was sent."
	    else
		redraw
		echohl ErrorMsg
		echomsg "Friendfeed Error: ".matchres[1]
		echohl None
	    endif
	endif
    endif
endfunction

" Prompt user for Friendfeed message if not supplied and then post it.
function! s:prompt_ffeed(mesg, imgfile)
    " Do this here too to check for ffeed_login. This is to avoid having the
    " user type in the message only to be told that his configuration is
    " incomplete.
    let rc = s:get_config()
    if rc < 0
	return -1
    endif

    let mesg = a:mesg

    if mesg == ""
	call inputsave()
	let mesg = input("Message: ")
	call inputrestore()
    endif

    if mesg == ""
	redraw
	echohl WarningMsg
	echo "No message provided. Not posted to Friendfeed."
	echohl None
	return
    endif

    call s:post_ffeed(mesg, a:imgfile)
endfunction

" Prompt user for image file if not supplied and then post to Friendfeed.
function! s:prompt_image_ffeed(mesg, imgfile)
    " Do this here too to check for ffeed_login. This is to avoid having the
    " user type in the message only to be told that his configuration is
    " incomplete.
    let rc = s:get_config()
    if rc < 0
	return -1
    endif

    let imgfile = a:imgfile

    if imgfile == ""
	call inputsave()
	let imgfile = input("Image file: ", '', 'file')
	call inputrestore()
    endif

    if imgfile == ""
	redraw
	echohl WarningMsg
	echo "No image file provided. Not posted to Friendfeed."
	echohl None
	return
    endif

    call s:prompt_ffeed(a:mesg, imgfile)
endfunction


" Prompt user for Friendfeed message or take message from command line.
if !exists(":PostFfeed")
    command -nargs=? PostFfeed :call <SID>prompt_ffeed(<q-args>, '')
endif

" Post current line to Friendfeed.
if !exists(":CPostFfeed")
    command -range CPostFfeed :call <SID>post_ffeed(join(getline(<line1>,<line2>), ' '), '')
endif

" Post visual selection to Friendfeed.
noremap <SID>Visual y:call <SID>post_ffeed(@", '')<cr>
noremap <unique> <script> <Plug>FfeedVisual <SID>Visual
if !hasmapto('<Plug>FfeedVisual')
    vmap <unique> <Leader>f <Plug>FfeedVisual
endif

" Prompt user for Friendfeed image file name or get it from command line.
if !exists(":PostImageFfeed")
    command -complete=file -nargs=? PostImageFfeed :call <SID>prompt_image_ffeed('', <q-args>)
endif

" Post current line to Friendfeed but prompt user for Friendfeed image file
" name or get it from command line.
if !exists(":CPostImageFfeed")
    command -range -complete=file -nargs=? CPostImageFfeed :call <SID>prompt_image_ffeed(join(getline(<line1>,<line2>), ' '), <q-args>)
endif

" Post visual selection to Friendfeed with prompt for image file.
noremap <SID>ImageVisual y:call <SID>prompt_image_ffeed(@", '')<cr>
noremap <unique> <script> <Plug>FfeedImageVisual <SID>ImageVisual
if !hasmapto('<Plug>FfeedImageVisual')
    vmap <unique> <Leader>F <Plug>FfeedImageVisual
endif

let &cpo = s:save_cpo
finish

" vim:set tw=0:
doc/ffeedvim.txt	[[[1
276
*ffeedvim.txt*  Post to Friendfeed from Vim

		    -------------------------------------
		    FFeedVim: A Friendfeed client for Vim
		    -------------------------------------

Author: Po Shan Cheah <morton@mortonfox.com> 
	http://friendfeed.com/mortonfox

License: The Vim License applies to ffeedvim.vim and ffeedvim.txt (see
	|copyright|) except use "FFeedVim" instead of "Vim". No warranty,
	express or implied. Use at your own risk.


==============================================================================
1. Contents					*FFeedVim* *FFeedVim-contents*

	1. Contents...............................: |FFeedVim-contents|
	2. Introduction...........................: |FFeedVim-intro|
	3. Installation...........................: |FFeedVim-install|
	   cURL...................................: |FFeedVim-cURL|
	   ffeedvim.vim...........................: |FFeedVim-add|
	   ffeed_login............................: |ffeed_login|
	   ffeed_proxy............................: |ffeed_proxy|
	   ffeed_proxy_login......................: |ffeed_proxy_login|
	3.1. Base64-Encoded Login.................: |FFeedVim-login-base64|
	     ffeed_login_b64......................: |ffeed_login_b64|
	     ffeed_proxy_login_b64................: |ffeed_proxy_login_b64|
	4. FFeedVim Manual........................: |FFeedVim-manual|
	4.1. Sharing Commands.....................: |FFeedVim-share|
	     :PostFfeed...........................: |:PostFfeed|
	     :CPostFfeed..........................: |:CPostFfeed|
	     <Leader>f............................: |FFeedVim-Leader-f|
	     :PostImageFfeed......................: |:PostImageFfeed|
	     :CPostImageFfeed.....................: |:CPostImageFfeed|
	     <Leader>F............................: |FFeedVim-Leader-S-f|
	4.2. Links, rooms, and initial comment....: |FFeedVim-links|
	5. History................................: |FFeedVim-history|


==============================================================================
2. Introduction						*FFeedVim-intro*

FFeedVim is a plugin that allows you to share messages, links, and images on
Friendfeed, a social network aggregator at http://www.friendfeed.com.


==============================================================================
3. Installation						*FFeedVim-install*

	1. Install cURL.				*FFeedVim-cURL*

	If you don't already have cURL on your system, download it from
	http://curl.haxx.se/. Make sure that the curl executable is in a
	directory listed in your PATH environment variable, or the equivalent
	for your system.


	2. ffeedvim.vim					*FFeedVim-add*

	Add ffeedvim.vim to your plugins directory. The location depends on
	your operating system. See |add-global-plugin| for details.

	If you installed from the Vimball (.vba) file, ffeedvim.vim should
	already be in its correct place.


	3. ffeed_login					*ffeed_login*

	Add the following to your vimrc:

		let ffeed_login = "USER:REMOTEKEY"

	Replace USER with your Friendfeed user name and REMOTEKEY with your
	Friendfeed remote key. Your remote key is not the same as your
	password. You can get it by logging into Friendfeed and then visiting
	http://friendfeed.com/remotekey

	It is possible to avoid having your Friendfeed remote key in plaintext
	in your vimrc. See |FFeedVim-login-base64| for details.


	4. ffeed_proxy					*ffeed_proxy*

	This step is only needed if you access the web through a HTTP proxy.
	If you use a HTTP proxy, add the following to your vimrc:

		let ffeed_proxy = "proxyserver:proxyport"

	Replace proxyserver with the address of the HTTP proxy and proxyport
	with the port number of the HTTP proxy.


	5. ffeed_proxy_login				*ffeed_proxy_login*

	If the HTTP proxy requires authentication, add the following to your
	vimrc:

		let ffeed_proxy_login = "proxyuser:proxypassword"

	Where proxyuser is your proxy user and proxypassword is your proxy
	password.

	It is possible to avoid having your proxy password in plaintext in
	your vimrc. See |FFeedVim-login-base64| for details.


------------------------------------------------------------------------------
3.1. Base64-Encoded Login				*FFeedVim-login-base64*

	For safety purposes, FFeedVim allows you to specify your Friendfeed
	login and proxy login information preencoded in base64. This is not
	truly secure as it is not encryption but it can stop casual onlookers
	from reading off your password when you edit your vimrc.

						*ffeed_login_b64*
	To do that, set the following in your vimrc:

		let ffeed_login_b64 = "base64string"
	
	Where base64string is your username:remotekey encoded in base64.

						*ffeed_proxy_login_b64*
	If your HTTP proxy needs authentication, set the following:

		let ffeed_proxy_login_b64 = "base64string"

	Where base64string is your username:password encoded in base64.


	An example:

	Let's say Joe User has a Friendfeed login of "joeuser" and a remote
	key of
	"something123stuff". His first step is to encode
	"joeuser:something123stuff" in
	Base64. He can either use a standalone utility to do that or, in a
	pinch, he can do the encoding at websites such as the following:
	http://makcoder.sourceforge.net/demo/base64.php
	http://www.opinionatedgeek.com/dotnet/tools/Base64Encode/

	The result is: am9ldXNlcjpzb21ldGhpbmcxMjNzdHVmZg==

	Then he adds the following to his vimrc:

		let ffeed_login_b64 = "am9ldXNlcjpzb21ldGhpbmcxMjNzdHVmZg=="

	And his setup is ready.


==============================================================================
4. FFeedVim Manual					*FFeedVim-manual*

------------------------------------------------------------------------------
4.1. Sharing Commands					*FFeedVim-share*

	These commands share a message, link, or image to your Friendfeed
	stream.

	:PostFfeed					*:PostFfeed*
	:PostFfeed {message}

	Share {message} to Friendfeed. If {message} is not supplied on the
	command line, FFeedVim will prompt you for it.


	:CPostFfeed					*:CPostFfeed*

	Share the current line to Friendfeed. 
	
	This command also accepts a range, so you can select a range of lines
	before invoking it or prepend a line range to the command. See
	|cmdline-ranges| If you give this command a range, it will share all
	the lines in the range.


	<Leader>f					*FFeedVim-Leader-f*

	In visual mode, this mapping shares the highlighted text to
	Friendfeed. The <Leader> character defaults to \ (backslash) but see
	|mapleader| for information on customizing that.


	:PostImageFfeed					*:PostImageFfeed*
	:PostImageFfeed {imagefile}

	Share the image file {imagefile} to Friendfeed. If {imagefile} is not
	supplied on the command line, FFeedVim will prompt you for it.

	In either case, FFeedVim will prompt you for a message to go with the
	image.


	:CPostImageFfeed				*:CPostImageFfeed*
	:CPostImageFfeed {imagefile}

	Share the image file {imagefile} to Friendfeed. If {imagefile} is not
	supplied on the command line, FFeedVim will prompt you for it.

	The difference between this command and :PostImageFfeed is this
	command takes the message text from the current line or line range.


	<Leader>F					*FFeedVim-Leader-S-f*

	In visual mode, this mapping (Note: Uppercase F) prompts you for an
	image file name and shares both the highlighted text and image file to
	Friendfeed. The <Leader> character defaults to \ (backslash) but see
	|mapleader| for information on customizing that.


------------------------------------------------------------------------------
4.2. Links, rooms, and initial comment			*FFeedVim-links*
							*FFeedVim-rooms*
							*FFeedVim-comment*

	You can augment a message to add links, an initial comment, and a room
	specifier.

	Here's a plain message:

		Hello, world!

	FFeedVim will share that as a simple message.


	Here's a message with a link:

		http://en.wikipedia.org/wiki/Hello_world Hello, world!
	
	FFeedVim will share to Friendfeed that as a link to
	http://en.wikipedia.org/wiki/Hello_world with "Hello, world!" as the
	link name.


	FFeedVim also supports links at the end of a message, so the following
	message will post the same thing as the previous example:

		Hello, world! http://en.wikipedia.org/wiki/Hello_world


	You can also add an initial comment to the message:

		Hello, world! // hello?
	
	This will post the message "Hello, world!" with an initial comment of
	"hello?".


	You can post all 3 items together:

		Hello, world! // hello?
		http://en.wikipedia.org/wiki/Hello_world

	This will post the link with "Hello, world!" as the link name and
	"hello?" as the initial comment. Note also that the message can be on
	as many lines as you want. Newline characters within the message are
	treated as spaces.


	And finally, you can specify the room to which the shared item will
	go. Just add room=NICKNAME to the beginning of the message, where
	NICKNAME is the room's nickname. Example:

		room=hello Hello, world!

	This will post "Hello, world!" to the hello room.


==============================================================================
5. FFeedVim History					*FFeedVim-history*

	0.1.0 : 2008-05-22 * Initial release.

==============================================================================
vim:tw=78:ts=8:ft=help:norl:
