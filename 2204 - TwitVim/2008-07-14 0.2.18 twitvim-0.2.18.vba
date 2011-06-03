" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin/twitvim.vim	[[[1
1069
" ==============================================================
" TwitVim - Post to Twitter from Vim
" Based on Twitter Vim script by Travis Jeffery <eatsleepgolf@gmail.com>
"
" Version: 0.2.18
" License: Vim license. See :help license
" Language: Vim script
" Maintainer: Po Shan Cheah <morton@mortonfox.com>
" Created: March 28, 2008
" Last updated: July 11, 2008
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

" If true, disable the Perl code that simplifies and localizes Twitter
" timestamps.
if !exists('g:twitvim_disable_simple_time')
    let g:twitvim_disable_simple_time = 0
endif

" The extended character limit is 246. Twitter will display a tweet longer than
" 140 characters in truncated form with a link to the full tweet. If that is
" undesirable, set s:char_limit to 140.
let s:char_limit = 246

let s:twupdate = "http://twitter.com/statuses/update.xml?source=twitvim"

function! s:get_config_proxy()
    " Get proxy setting from twitvim_proxy in .vimrc or _vimrc.
    " Format is proxysite:proxyport
    let s:proxy = exists('g:twitvim_proxy') ? '-x "'.g:twitvim_proxy.'"': ""
    " If twitvim_proxy_login exists, use that as the proxy login.
    " Format is proxyuser:proxypassword
    " If twitvim_proxy_login_b64 exists, use that instead. This is the proxy
    " user:password in base64 encoding.
    if exists('g:twitvim_proxy_login_b64')
	let s:proxy .= ' -H "Proxy-Authorization: Basic '.g:twitvim_proxy_login_b64.'"'
    else
	let s:proxy .= exists('g:twitvim_proxy_login') ? ' -U "'.g:twitvim_proxy_login.'"' : ''
    endif
endfunction

" Get user-config variables twitvim_proxy and twitvim_login.
function! s:get_config()
    call s:get_config_proxy()

    " Get Twitter login info from twitvim_login in .vimrc or _vimrc.
    " Format is username:password
    " If twitvim_login_b64 exists, use that instead. This is the user:password
    " in base64 encoding.
    if exists('g:twitvim_login_b64')
	let s:login = '-H "Authorization: Basic '.g:twitvim_login_b64.'"'	
    elseif exists('g:twitvim_login') && g:twitvim_login != ''
	let s:login = '-u "'.g:twitvim_login.'"'
    else
	" Beep and error-highlight 
	execute "normal \<Esc>"
	redraw
	echohl ErrorMsg
	echomsg 'Twitter login not set.'
	    \ 'Please add to .vimrc: let twitvim_login="USER:PASS"'
	echohl None
	return -1
    endif
    return 0
endfunction

" === XML helper functions ===

" Get the content of the n'th element in a series of elements.
function! s:xml_get_nth(xmlstr, elem, n)
    let matchres = matchlist(a:xmlstr, '<'.a:elem.'>\(.\{-}\)</'.a:elem.'>', -1, a:n)
    return matchres == [] ? "" : matchres[1]
endfunction

" Get the content of the specified element.
function! s:xml_get_element(xmlstr, elem)
    return s:xml_get_nth(a:xmlstr, a:elem, 1)
endfunction

" Remove any number of the specified element from the string. Used for removing
" sub-elements so that you can parse the remaining elements safely.
function! s:xml_remove_elements(xmlstr, elem)
    return substitute(a:xmlstr, '<'.a:elem.'>.\{-}</'.a:elem.'>', '', "g")
endfunction

" === End of XML helper functions ===

" === Perl time string parser ===

if has('perl') && !g:twitvim_disable_simple_time
    function s:def_perl_time_funcs()
	perl <<EOF
eval {
    require Time::Local; Time::Local->import;
    require POSIX; POSIX->import(qw(strftime));
};
if ($@) {
    # Play it safe and disable this feature if modules fail to load.
    VIM::DoCommand('let g:twitvim_disable_simple_time = 1');
}

# Convert abbreviated month name to month number.
sub twitvim_conv_month {
    my $monthstr = shift;
    my @months = qw(jan feb mar apr may jun jul aug sep oct nov dec);
    for my $mon (0..11) {
	$months[$mon] eq lc($monthstr) and return $mon;
    }
    undef;
}

# Parse time string in Twitter RSS or Summize Atom format.
sub twitvim_parse_time {
    my $timestr = shift;
    # This timestamp format is used by Twitter in timelines.
    if ($timestr =~ /^\w+,\s+(\d+)\s+(\w+)\s+(\d+)\s+(\d+):(\d+):(\d+)\s+\+0000$/) {
	my $mon = twitvim_conv_month($2);
	defined $mon or return undef;
	return timegm($6, $5, $4, $1, $mon, $3);
    }
    # This timestamp format is used by Twitter in response to an update.
    elsif ($timestr =~ /^\w+\s+(\w+)\s+(\d+)\s+(\d+):(\d+):(\d+)\s+\+0000\s+(\d+)$/) {
	my $mon = twitvim_conv_month($1);
	defined $mon or return undef;
	return timegm($5, $4, $3, $2, $mon, $6);
    }
    # This timestamp format is used by Summize.
    elsif ($timestr =~ /^(\d+)-(\d+)-(\d+)T(\d+):(\d+):(\d+)Z$/) {
	return timegm($6, $5, $4, $3, $2 - 1, $1);
    }
    else {
	return undef;
    }
}

# Convert the Twitter timestamp to local time and simplify it.
sub twitvim_new_time {
    my $timestr = shift;
    my $time = twitvim_parse_time($timestr);
    defined $time ? strftime("%I:%M %p %b %d, %Y", localtime($time)) : $timestr;
}
EOF
    endfunction

    call s:def_perl_time_funcs()

    " Wrapper for the Twitter timestamp converter.
    function s:perl_time(timestr)
	execute 'perl VIM::DoCommand("let newtime = \"".twitvim_new_time("'.a:timestr.'")."\"")'
	return newtime
    endfunction
endif

" Simplify the time string. Do this only if the Perl interface is enabled and
" if we have not disabled the feature.
function s:time_filter(timestr)
    let s = a:timestr
    if has('perl') && !g:twitvim_disable_simple_time
	let s = s:perl_time(s)
    endif
    return s
endfunction

" === End of Perl time string parser ===

" Add update to Twitter buffer if public, friends, or user timeline.
function! s:add_update(output)
    if s:twit_buftype == "public" || s:twit_buftype == "friends" || s:twit_buftype == "user"

	" Parse the output from the Twitter update call.
	let date = s:time_filter(s:xml_get_element(a:output, 'created_at'))
	let text = s:xml_get_element(a:output, 'text')
	let name = s:xml_get_element(a:output, 'screen_name')

	if text == ""
	    return
	endif

	let twit_bufnr = bufwinnr('^'.s:twit_winname.'$')
	if twit_bufnr > 0
	    execute twit_bufnr . "wincmd w"
	    call append(2, name.': '.s:convert_entity(text).' |'.date.'|')
	    normal 1G
	    wincmd p
	endif
    endif
endfunction

" URL-encode a string.
function! s:url_encode(str)
    return substitute(a:str, '[^a-zA-Z_-]', '\=printf("%%%02X", char2nr(submatch(0)))', 'g')
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
	redraw
	echohl WarningMsg
	echo "Your tweet has" strlen(mesg) - s:char_limit
	    \ "too many characters. It was not sent."
	echohl None
    elseif strlen(mesg) < 1
	redraw
	echohl WarningMsg
	echo "Your tweet was empty. It was not sent."
	echohl None
    else
	redraw
	echo "Sending update to Twitter..."
	let output = system("curl -s ".s:proxy." ".s:login.' -d status="'.s:url_encode(mesg).'" '.s:twupdate)
	if v:shell_error != 0
	    redraw
	    echohl ErrorMsg
	    echomsg "Error posting your tweet. Result code: ".v:shell_error
	    echomsg "Output:"
	    echomsg output
	    echohl None
	else
	    call s:add_update(output)
	    redraw
	    echo "Your tweet was sent. You used" strlen(mesg) "characters."
	endif
    endif
endfunction

" Prompt user for tweet and then post it.
" If initstr is given, use that as the initial input.
function! s:CmdLine_Twitter(initstr)
    " Do this here too to check for twitvim_login. This is to avoid having the
    " user type in the message only to be told that his configuration is
    " incomplete.
    let rc = s:get_config()
    if rc < 0
	return -1
    endif

    call inputsave()
    let mesg = input("Your Twitter: ", a:initstr)
    call inputrestore()
    call s:post_twitter(mesg)
endfunction

" Extract the user name from a line in the timeline.
function! s:get_user_name(line)
    let matchres = matchlist(a:line, '^\(\w\+\):')
    return matchres != [] ? matchres[1] : ""
endfunction

" This is for a local mapping in the timeline. Start an @-reply on the command
" line to the author of the tweet on the current line.
function! s:Quick_Reply()
    let username = s:get_user_name(getline('.'))
    if username != ""
	call s:CmdLine_Twitter('@'.username.' ')
    endif
endfunction

" This is for a local mapping in the timeline. Start a direct message on the
" command line to the author of the tweet on the current line.
function! s:Quick_DM()
    let username = s:get_user_name(getline('.'))
    if username != ""
	call s:CmdLine_Twitter('d '.username.' ')
    endif
endfunction


" Prompt user for tweet.
if !exists(":PosttoTwitter")
    command PosttoTwitter :call <SID>CmdLine_Twitter('')
endif

nnoremenu Plugin.TwitVim.Post\ from\ cmdline :call <SID>CmdLine_Twitter('')<cr>

" Post current line to Twitter.
if !exists(":CPosttoTwitter")
    command CPosttoTwitter :call <SID>post_twitter(getline('.'))
endif

nnoremenu Plugin.TwitVim.Post\ current\ line :call <SID>post_twitter(getline('.'))<cr>

" Post entire buffer to Twitter.
if !exists(":BPosttoTwitter")
    command BPosttoTwitter :call <SID>post_twitter(join(getline(1, "$")))
endif

" Post visual selection to Twitter.
noremap <SID>Visual y:call <SID>post_twitter(@")<cr>
noremap <unique> <script> <Plug>TwitvimVisual <SID>Visual
if !hasmapto('<Plug>TwitvimVisual')
    vmap <unique> <A-t> <Plug>TwitvimVisual

    " Allow Ctrl-T as an alternative to Alt-T.
    " Alt-T pulls down the Tools menu if the menu bar is enabled.
    vmap <unique> <C-t> <Plug>TwitvimVisual
endif

vmenu Plugin.TwitVim.Post\ selection <Plug>TwitvimVisual

" Launch web browser with the given URL.
function! s:launch_browser(url)
    if !exists('g:twitvim_browser_cmd') || g:twitvim_browser_cmd == ''
	" Beep and error-highlight 
	execute "normal \<Esc>"
	redraw
	echohl ErrorMsg
	echomsg 'Browser cmd not set.'
	    \ 'Please add to .vimrc: let twitvim_browser_cmd="browsercmd"'
	echohl None
	return -1
    endif

    let startcmd = has("win32") || has("win64") ? "!start " : "! "
    let endcmd = has("unix") ? "&" : ""

    " Escape characters that have special meaning in the :! command.
    let url = substitute(a:url, '!\|#\|%', '\\&', 'g')

    redraw
    echo "Launching web browser..."
    silent execute startcmd g:twitvim_browser_cmd url endcmd
    redraw
    echo "Web browser launched."
endfunction

" Launch web browser with the URL at the cursor position. If possible, this
" function will try to recognize a URL within the current word. Otherwise,
" it'll just use the whole word.
" If the cWORD happens to be @user or user:, show that user's timeline.
function! s:launch_url_cword()
    let s = expand("<cWORD>")

    let matchres = matchlist(s, '^@\(\w\+\)')
    if matchres != []
	call s:get_timeline("user", matchres[1], 1)
	return
    endif

    let matchres = matchlist(s, '^\(\w\+\):$')
    if matchres != []
	call s:get_timeline("user", matchres[1], 1)
	return
    endif

    let s = substitute(s, '.*\<\(\(http\|https\|ftp\)://\S\+\)', '\1', "")
    call s:launch_browser(s)
endfunction

" Decode HTML entities. Twitter gives those to us a little weird. For example,
" a '<' character comes to us as &amp;lt;
function! s:convert_entity(str)
    let s = a:str
    let s = substitute(s, '&amp;', '\&', 'g')
    let s = substitute(s, '&lt;', '<', 'g')
    let s = substitute(s, '&gt;', '>', 'g')
    let s = substitute(s, '&#\(\d\+\);','\=nr2char(submatch(1))', 'g')
    return s
endfunction

let s:twit_winname = "Twitter_".localtime()
let s:twit_buftype = ""

" Switch to the Twitter window if there is already one or open a new window for
" Twitter.
function! s:twitter_win()
    let twit_bufnr = bufwinnr('^'.s:twit_winname.'$')
    if twit_bufnr > 0
	execute twit_bufnr . "wincmd w"
    else
	execute "new " . s:twit_winname
	setlocal noswapfile
	setlocal buftype=nofile
	setlocal bufhidden=delete 
	setlocal foldcolumn=0
	setlocal nobuflisted
	setlocal nospell

	" Quick reply feature for replying from the timeline.
	nnoremap <buffer> <silent> <A-r> :call <SID>Quick_Reply()<cr>
	nnoremap <buffer> <silent> <Leader>r :call <SID>Quick_Reply()<cr>

	" Quick DM feature for direct messaging from the timeline.
	nnoremap <buffer> <silent> <A-d> :call <SID>Quick_DM()<cr>
	nnoremap <buffer> <silent> <Leader>d :call <SID>Quick_DM()<cr>

	" Launch browser with URL in visual selection or at cursor position.
	nnoremap <buffer> <silent> <A-g> :call <SID>launch_url_cword()<cr>
	nnoremap <buffer> <silent> <Leader>g :call <SID>launch_url_cword()<cr>
	vnoremap <buffer> <silent> <A-g> y:call <SID>launch_browser(@")<cr>
	vnoremap <buffer> <silent> <Leader>g y:call <SID>launch_browser(@")<cr>

	" Beautify the Twitter window with syntax highlighting.
	if has("syntax") && exists("g:syntax_on") && !has("syntax_items")

	    " Twitter user name: from start of line to first colon.
	    syntax match twitterUser /^.\{-1,}:/

	    " Use the bars to recognize the time but hide the bars.
	    syntax match twitterTime /|[^|]\+|$/ contains=twitterTimeBar
	    syntax match twitterTimeBar /|/ contained

	    " Highlight links in tweets.
	    syntax match twitterLink "\<http://\S\+"
	    syntax match twitterLink "\<https://\S\+"
	    syntax match twitterLink "\<ftp://\S\+"

	    " An @-reply must be preceded by whitespace and ends at a non-word
	    " character.
	    syntax match twitterReply "\S\@<!@\w\+"

	    " Use the extra star at the end to recognize the title but hide the
	    " star.
	    syntax match twitterTitle /^.\+\*$/ contains=twitterTitleStar
	    syntax match twitterTitleStar /\*$/ contained

	    highlight default link twitterUser Identifier
	    highlight default link twitterTime String
	    highlight default link twitterTimeBar Ignore
	    highlight default link twitterTitle Title
	    highlight default link twitterTitleStar Ignore
	    highlight default link twitterLink Underlined
	    highlight default link twitterReply Label
	endif
    endif
endfunction

" Get a Twitter window and stuff text into it.
function! s:twitter_wintext(text)
    call s:twitter_win()

    " Overwrite the entire buffer.
    " Need to use 'silent' or a 'No lines in buffer' message will appear.
    " Delete to the blackhole register "_ so that we don't affect registers.
    silent %delete _
    call setline('.', a:text)
    normal 1G

    wincmd p
endfunction

" Show a timeline.
function! s:show_timeline(timeline, page)
    let matchcount = 1
    let text = []

    let channel = s:xml_remove_elements(a:timeline, 'item')
    let title = s:xml_get_element(channel, 'title')

    if a:page > 1
	let title .= ' (page '.a:page.')'
    endif

    " The extra stars at the end are for the syntax highlighter to recognize
    " the title. Then the syntax highlighter hides the stars by coloring them
    " the same as the background. It is a bad hack.
    call add(text, title.'*')
    call add(text, repeat('=', strlen(title)).'*')

    while 1
	let item = s:xml_get_nth(a:timeline, 'item', matchcount)
	if item == ""
	    break
	endif

	let title = s:xml_get_element(item, 'title')
	let pubdate = s:time_filter(s:xml_get_element(item, 'pubDate'))

	call add(text, s:convert_entity(title).' |'.pubdate.'|')

	let matchcount += 1
    endwhile
    call s:twitter_wintext(text)
endfunction

" Generic timeline retrieval function.
function! s:get_timeline(tline_name, username, page)
    let login = ""
    if a:tline_name == "public"
	" No authentication is needed for public timeline so just get the proxy
	" info.
	call s:get_config_proxy()
    else
	let rc = s:get_config()
	if rc < 0
	    return -1
	endif
	let login = s:login
    endif

    " Twitter API allows you to specify a username for user timeline and
    " friends timeline to retrieve another user's timeline.
    let user = a:username == '' ? '' : '/'.a:username

    let url_fname = a:tline_name == "replies" ? "replies.rss" : a:tline_name."_timeline".user.".rss"

    " Support pagination.
    if a:page > 1
	let url_fname .= '?page='.a:page
    endif

    redraw
    echo "Sending" a:tline_name "timeline request to Twitter..."
    let output = system("curl -s ".s:proxy." ".login." http://twitter.com/statuses/".url_fname)
    if v:shell_error != 0
	redraw
	echohl ErrorMsg
	echomsg "Error getting Twitter" a:tline_name "timeline. Result code: ".v:shell_error
	echomsg "Output:"
	echomsg output
	echohl None
	return
    endif

    call s:show_timeline(output, a:page)
    let s:twit_buftype = a:tline_name
    redraw

    let foruser = a:username == '' ? '' : ' for user '.a:username

    " Uppercase the first letter in the timeline name.
    echo substitute(a:tline_name, '^.', '\u&', '') "timeline updated".foruser."."
endfunction

" Show direct message sent or received by user. First argument should be 'sent'
" or 'received' depending on which timeline we are displaying.
function! s:show_dm_xml(sent_or_recv, timeline, page)
    let matchcount = 1
    let text = []

    let title = 'Direct messages '.a:sent_or_recv

    if a:page > 1
	let title .= ' (page '.a:page.')'
    endif

    " The extra stars at the end are for the syntax highlighter to recognize
    " the title. Then the syntax highlighter hides the stars by coloring them
    " the same as the background. It is a bad hack.
    call add(text, title.'*')
    call add(text, repeat('=', strlen(title)).'*')

    while 1
	let item = s:xml_get_nth(a:timeline, 'direct_message', matchcount)
	if item == ""
	    break
	endif

	let user = s:xml_get_element(item, a:sent_or_recv == 'sent' ? 'recipient_screen_name' : 'sender_screen_name')
	let mesg = s:xml_get_element(item, 'text')
	let date = s:time_filter(s:xml_get_element(item, 'created_at'))

	call add(text, user.": ".s:convert_entity(mesg).' |'.date.'|')

	let matchcount += 1
    endwhile
    call s:twitter_wintext(text)
endfunction

" Get direct messages sent to user.
function! s:Direct_Messages(page)
    let rc = s:get_config()
    if rc < 0
	return -1
    endif

    " Support pagination.
    let pagearg = ''
    if a:page > 1
	let pagearg = '?page='.a:page
    endif

    redraw
    echo "Sending direct message timeline request to Twitter..."
    let output = system("curl -s ".s:proxy." ".s:login." http://twitter.com/direct_messages.xml".pagearg)
    if v:shell_error != 0
	redraw
	echohl ErrorMsg
	echomsg "Error getting Twitter direct messages. Result code: ".v:shell_error
	echomsg "Output:"
	echomsg output
	echohl None
	return
    endif

    call s:show_dm_xml('received', output, a:page)
    let s:twit_buftype = "directmessages"
    redraw
    echo "Direct message timeline updated."
endfunction

" Get direct messages sent by user.
function! s:Direct_Messages_Sent(page)
    let rc = s:get_config()
    if rc < 0
	return -1
    endif

    " Support pagination.
    let pagearg = ''
    if a:page > 1
	let pagearg = '?page='.a:page
    endif

    redraw
    echo "Sending direct messages sent timeline request to Twitter..."
    let output = system("curl -s ".s:proxy." ".s:login." http://twitter.com/direct_messages/sent.xml".pagearg)
    if v:shell_error != 0
	redraw
	echohl ErrorMsg
	echomsg "Error getting Twitter direct messages sent timeline. Result code: ".v:shell_error
	echomsg "Output:"
	echomsg output
	echohl None
	return
    endif

    call s:show_dm_xml('sent', output, a:page)
    let s:twit_buftype = "directmessages"
    redraw
    echo "Direct messages sent timeline updated."
endfunction

if !exists(":PublicTwitter")
    command PublicTwitter :call <SID>get_timeline("public", '', 1)
endif
if !exists(":FriendsTwitter")
    command -count=1 -nargs=? FriendsTwitter :call <SID>get_timeline("friends", <q-args>, <count>)
endif
if !exists(":UserTwitter")
    command -count=1 -nargs=? UserTwitter :call <SID>get_timeline("user", <q-args>, <count>)
endif
if !exists(":RepliesTwitter")
    command -count=1 RepliesTwitter :call <SID>get_timeline("replies", '', <count>)
endif
if !exists(":DMTwitter")
    command -count=1 DMTwitter :call <SID>Direct_Messages(<count>)
endif
if !exists(":DMSentTwitter")
    command -count=1 DMSentTwitter :call <SID>Direct_Messages_Sent(<count>)
endif

nnoremenu Plugin.TwitVim.-Sep1- :
nnoremenu Plugin.TwitVim.&Friends\ Timeline :call <SID>get_timeline("friends", '', 1)<cr>
nnoremenu Plugin.TwitVim.&User\ Timeline :call <SID>get_timeline("user", '', 1)<cr>
nnoremenu Plugin.TwitVim.&Replies\ Timeline :call <SID>get_timeline("replies", '', 1)<cr>
nnoremenu Plugin.TwitVim.&Direct\ Messages :call <SID>Direct_Messages(1)<cr>
nnoremenu Plugin.TwitVim.Direct\ Messages\ &Sent :call <SID>Direct_Messages_Sent(1)<cr>
nnoremenu Plugin.TwitVim.&Public\ Timeline :call <SID>get_timeline("public", '', 1)<cr>

" Call Tweetburner API to shorten a URL.
function! s:call_tweetburner(url)
    call s:get_config_proxy()
    redraw
    echo "Sending request to Tweetburner..."
    let output = system('curl -s '.s:proxy.' -d link[url]="'.s:url_encode(a:url).'" http://tweetburner.com/links')
    if v:shell_error != 0
	redraw
	echohl ErrorMsg
	echomsg "Error calling Tweetburner API. Result code: ".v:shell_error
	echomsg "Output:"
	echomsg output
	echohl None
	return ""
    else
	redraw
	echo "Received response from Tweetburner."
	return output
    endif
endfunction

" Call SnipURL API to shorten a URL.
function! s:call_snipurl(url)
    call s:get_config_proxy()
    redraw
    echo "Sending request to SnipURL..."
    let output = system('curl -s '.s:proxy.' "http://snipr.com/site/snip?r=simple&link='.s:url_encode(a:url).'"')
    if v:shell_error != 0
	redraw
	echohl ErrorMsg
	echomsg "Error calling SnipURL API. Result code: ".v:shell_error
	echomsg "Output:"
	echomsg output
	echohl None
	return ""
    else
	redraw
	echo "Received response from SnipURL."
	" Get rid of extraneous newline at the beginning of SnipURL's output.
	return substitute(output, '^\n', '', '')
    endif
endfunction

" Call Metamark API to shorten a URL.
function! s:call_metamark(url)
    call s:get_config_proxy()
    redraw
    echo "Sending request to Metamark..."
    let output = system('curl -s '.s:proxy.' -d long_url="'.s:url_encode(a:url).'" http://metamark.net/api/rest/simple')
    if v:shell_error != 0
	redraw
	echohl ErrorMsg
	echomsg "Error calling Metamark API. Result code: ".v:shell_error
	echomsg "Output:"
	echomsg output
	echohl None
	return ""
    else
	redraw
	echo "Received response from Metamark."
	return output
    endif
endfunction

" Call TinyURL API to shorten a URL.
function! s:call_tinyurl(url)
    call s:get_config_proxy()
    redraw
    echo "Sending request to TinyURL..."
    let output = system('curl -s '.s:proxy.' "http://tinyurl.com/api-create.php?url='.a:url.'"')
    if v:shell_error != 0
	redraw
	echohl ErrorMsg
	echomsg "Error calling TinyURL API. Result code: ".v:shell_error
	echomsg "Output:"
	echomsg output
	echohl None
	return ""
    else
	redraw
	echo "Received response from TinyURL."
	return output
    endif
endfunction

" Call urlTea API to shorten a URL.
function! s:call_urltea(url)
    call s:get_config_proxy()
    redraw
    echo "Sending request to urlTea..."
    let output = system('curl -s '.s:proxy.' "http://urltea.com/api/text/?url='.s:url_encode(a:url).'"')
    if v:shell_error != 0
	redraw
	echohl ErrorMsg
	echomsg "Error calling urlTea API. Result code: ".v:shell_error
	echomsg "Output:"
	echomsg output
	echohl None
	return ""
    else
	redraw
	echo "Received response from urlTea."
	return output
    endif
endfunction

" Call bit.ly API to shorten a URL.
function! s:call_bitly(url)
    call s:get_config_proxy()
    redraw
    echo "Sending request to bit.ly..."
    let output = system('curl -s '.s:proxy.' "http://bit.ly/api?url='.s:url_encode(a:url).'"')
    if v:shell_error != 0
	redraw
	echohl ErrorMsg
	echomsg "Error calling bit.ly API. Result code: ".v:shell_error
	echomsg "Output:"
	echomsg output
	echohl None
	return ""
    else
	redraw
	echo "Received response from bit.ly."
	return output
    endif
endfunction

" Call is.gd API to shorten a URL.
function! s:call_isgd(url)
    call s:get_config_proxy()
    redraw
    echo "Sending request to is.gd..."
    let output = system('curl -s '.s:proxy.' "http://is.gd/api.php?longurl='.s:url_encode(a:url).'"')
    if v:shell_error != 0
	redraw
	echohl ErrorMsg
	echomsg "Error calling is.gd API. Result code: ".v:shell_error
	echomsg "Output:"
	echomsg output
	echohl None
	return ""
    else
	redraw
	echo "Received response from is.gd."
	return output
    endif
endfunction


" Get urlBorg API key if configured by the user. Otherwise, use a default API
" key.
function! s:get_urlborg_key()
    return exists('g:twitvim_urlborg_key') ? g:twitvim_urlborg_key : '26361-80ab'
endfunction

" Call urlBorg API to shorten a URL.
function! s:call_urlborg(url)
    call s:get_config_proxy()
    let key = s:get_urlborg_key()
    redraw
    echo "Sending request to urlBorg..."
    let output = system('curl -s '.s:proxy.' "http://urlborg.com/api/'.key.'/create/'.s:url_encode(a:url).'"')
    if v:shell_error != 0
	redraw
	echohl ErrorMsg
	echomsg "Error calling urlBorg API. Result code: ".v:shell_error
	echomsg "Output:"
	echomsg output
	echohl None
	return ""
    else
	let matchres = matchlist(output, '^http')
	if matchres == []
	    redraw
	    echohl ErrorMsg
	    echomsg "urlBorg error: ".output
	    echohl None
	    return ""
	else
	    redraw
	    echo "Received response from urlBorg."
	    return output
	endif
    endif
endfunction


" Invoke URL shortening service to shorten a URL and insert it at the current
" position in the current buffer.
function! s:GetShortURL(tweetmode, url, shortfn)
    let url = a:url

    " Prompt the user to enter a URL if not provided on :Tweetburner command
    " line.
    if url == ""
	call inputsave()
	let url = input("URL to shorten: ")
	call inputrestore()
    endif

    if url == ""
	redraw
	echohl WarningMsg
	echo "No URL provided."
	echohl None
	return
    endif

    let shorturl = call(function("s:".a:shortfn), [url])
    if shorturl != ""
	if a:tweetmode == "cmdline"
	    call s:CmdLine_Twitter(shorturl." ")
	elseif a:tweetmode == "append"
	    execute "normal a".shorturl."\<esc>"
	else
	    execute "normal i".shorturl." \<esc>"
	endif
    endif
endfunction

if !exists(":Tweetburner")
    command -nargs=? Tweetburner :call <SID>GetShortURL("insert", <q-args>, "call_tweetburner")
endif
if !exists(":ATweetburner")
    command -nargs=? ATweetburner :call <SID>GetShortURL("append", <q-args>, "call_tweetburner")
endif
if !exists(":PTweetburner")
    command -nargs=? PTweetburner :call <SID>GetShortURL("cmdline", <q-args>, "call_tweetburner")
endif

if !exists(":Snipurl")
    command -nargs=? Snipurl :call <SID>GetShortURL("insert", <q-args>, "call_snipurl")
endif
if !exists(":ASnipurl")
    command -nargs=? ASnipurl :call <SID>GetShortURL("append", <q-args>, "call_snipurl")
endif
if !exists(":PSnipurl")
    command -nargs=? PSnipurl :call <SID>GetShortURL("cmdline", <q-args>, "call_snipurl")
endif

if !exists(":Metamark")
    command -nargs=? Metamark :call <SID>GetShortURL("insert", <q-args>, "call_metamark")
endif
if !exists(":AMetamark")
    command -nargs=? AMetamark :call <SID>GetShortURL("append", <q-args>, "call_metamark")
endif
if !exists(":PMetamark")
    command -nargs=? PMetamark :call <SID>GetShortURL("cmdline", <q-args>, "call_metamark")
endif

if !exists(":TinyURL")
    command -nargs=? TinyURL :call <SID>GetShortURL("insert", <q-args>, "call_tinyurl")
endif
if !exists(":ATinyURL")
    command -nargs=? ATinyURL :call <SID>GetShortURL("append", <q-args>, "call_tinyurl")
endif
if !exists(":PTinyURL")
    command -nargs=? PTinyURL :call <SID>GetShortURL("cmdline", <q-args>, "call_tinyurl")
endif

if !exists(":UrlTea")
    command -nargs=? UrlTea :call <SID>GetShortURL("insert", <q-args>, "call_urltea")
endif
if !exists(":AUrlTea")
    command -nargs=? AUrlTea :call <SID>GetShortURL("append", <q-args>, "call_urltea")
endif
if !exists(":PUrlTea")
    command -nargs=? PUrlTea :call <SID>GetShortURL("cmdline", <q-args>, "call_urltea")
endif

if !exists(":BitLy")
    command -nargs=? BitLy :call <SID>GetShortURL("insert", <q-args>, "call_bitly")
endif
if !exists(":ABitLy")
    command -nargs=? ABitLy :call <SID>GetShortURL("append", <q-args>, "call_bitly")
endif
if !exists(":PBitLy")
    command -nargs=? PBitLy :call <SID>GetShortURL("cmdline", <q-args>, "call_bitly")
endif

if !exists(":IsGd")
    command -nargs=? IsGd :call <SID>GetShortURL("insert", <q-args>, "call_isgd")
endif
if !exists(":AIsGd")
    command -nargs=? AIsGd :call <SID>GetShortURL("append", <q-args>, "call_isgd")
endif
if !exists(":PIsGd")
    command -nargs=? PIsGd :call <SID>GetShortURL("cmdline", <q-args>, "call_isgd")
endif

if !exists(":UrlBorg")
    command -nargs=? UrlBorg :call <SID>GetShortURL("insert", <q-args>, "call_urlborg")
endif
if !exists(":AUrlBorg")
    command -nargs=? AUrlBorg :call <SID>GetShortURL("append", <q-args>, "call_urlborg")
endif
if !exists(":PUrlBorg")
    command -nargs=? PUrlBorg :call <SID>GetShortURL("cmdline", <q-args>, "call_urlborg")
endif

" Parse and format search results from Summize API.
function! s:show_summize(searchres)
    let text = []
    let matchcount = 1

    let channel = s:xml_remove_elements(a:searchres, 'entry')
    let title = s:xml_get_element(channel, 'title')

    " The extra stars at the end are for the syntax highlighter to recognize
    " the title. Then the syntax highlighter hides the stars by coloring them
    " the same as the background. It is a bad hack.
    call add(text, title.'*')
    call add(text, repeat('=', strlen(title)).'*')

    while 1
	let item = s:xml_get_nth(a:searchres, 'entry', matchcount)
	if item == ""
	    break
	endif

	let title = s:xml_get_element(item, 'title')
	let pubdate = s:time_filter(s:xml_get_element(item, 'updated'))
	let sender = substitute(s:xml_get_element(item, 'uri'), 'http://twitter.com/', '', '')

	call add(text, sender.": ".s:convert_entity(title).' |'.pubdate.'|')

	let matchcount += 1
    endwhile
    call s:twitter_wintext(text)
endfunction

" Query Summize API and retrieve results
function! s:get_summize(query)
    call s:get_config_proxy()

    redraw
    echo "Sending search request to Summize..."

    let output = system("curl -s ".s:proxy.' "http://summize.com/search.atom?rpp=25&q='.s:url_encode(a:query).'"')
    if v:shell_error != 0
	redraw
	echohl ErrorMsg
	echomsg "Error getting search results from Summize. Result code: ".v:shell_error
	echomsg "Output:"
	echomsg output
	echohl None
	return
    endif

    call s:show_summize(output)
    let s:twit_buftype = "summize"
    redraw
    echo "Received search results from Summize."
endfunction

" Prompt user for Summize query string if not entered on command line.
function! s:Summize(query)
    let query = a:query

    " Prompt the user to enter a query if not provided on :Summize command
    " line.
    if query == ""
	call inputsave()
	let query = input("Search Summize: ")
	call inputrestore()
    endif

    if query == ""
	redraw
	echohl WarningMsg
	echo "No query provided for Summize search."
	echohl None
	return
    endif

    call s:get_summize(query)
endfunction

if !exists(":Summize")
    command -nargs=? Summize :call <SID>Summize(<q-args>)
endif

let &cpo = s:save_cpo
finish

" vim:set tw=0:
doc/twitvim.txt	[[[1
675
*twitvim.txt*  Twitter client for Vim

		      ---------------------------------
		      TwitVim: A Twitter client for Vim
		      ---------------------------------

Author: Po Shan Cheah <morton@mortonfox.com> 
	http://twitter.com/mortonfox

License: The Vim License applies to twitvim.vim and twitvim.txt (see
	|copyright|) except use "TwitVim" instead of "Vim". No warranty,
	express or implied. Use at your own risk.


==============================================================================
1. Contents					*TwitVim* *TwitVim-contents*

	1. Contents...............................: |TwitVim-contents|
	2. Introduction...........................: |TwitVim-intro|
	3. Installation...........................: |TwitVim-install|
	   cURL...................................: |TwitVim-cURL|
	   twitvim.vim............................: |TwitVim-add|
	   twitvim_login..........................: |twitvim_login|
	   twitvim_proxy..........................: |twitvim_proxy|
	   twitvim_proxy_login....................: |twitvim_proxy_login|
	3.1. Base64-Encoded Login.................: |TwitVim-login-base64|
	     twitvim_login_b64....................: |twitvim_login_b64|
	     twitvim_proxy_login_b64..............: |twitvim_proxy_login_b64|
	4. Manual.................................: |TwitVim-manual|
	4.1. Update Commands......................: |TwitVim-update-commands|
	     :PosttoTwitter.......................: |:PosttoTwitter|
	     :CPosttoTwitter......................: |:CPosttoTwitter|
	     :BPosttoTwitter......................: |:BPosttoTwitter|
	4.2. Timeline Commands....................: |TwitVim-timeline-commands|
	     :UserTwitter.........................: |:UserTwitter|
	     :FriendsTwitter......................: |:FriendsTwitter|
	     :RepliesTwitter......................: |:RepliesTwitter|
	     :PublicTwitter.......................: |:PublicTwitter|
	     :DMTwitter...........................: |:DMTwitter|
	     :DMSentTwitter.......................: |:DMSentTwitter|
	4.3. Mappings.............................: |TwitVim-mappings|
	     Alt-T................................: |TwitVim-A-t|
	     Ctrl-T...............................: |TwitVim-C-t|
	     Alt-R................................: |TwitVim-A-r|
	     <Leader>r............................: |TwitVim-Leader-r|
	     Alt-D................................: |TwitVim-A-d|
	     <Leader>d............................: |TwitVim-Leader-d|
	     Alt-G................................: |TwitVim-A-g|
	     <Leader>g............................: |TwitVim-Leader-g|
	     twitvim_browser_cmd..................: |twitvim_browser_cmd|
	4.4. Utility Commands.....................: |TwitVim-utility|
	     :Tweetburner.........................: |:Tweetburner|
	     :ATweetburner........................: |:ATweetburner|
	     :PTweetburner........................: |:PTweetburner|
	     :Snipurl.............................: |:Snipurl|
	     :ASnipurl............................: |:ASnipurl|
	     :PSnipurl............................: |:PSnipurl|
	     :Metamark............................: |:Metamark|
	     :AMetamark...........................: |:AMetamark|
	     :PMetamark...........................: |:PMetamark|
	     :TinyURL.............................: |:TinyURL|
	     :ATinyURL............................: |:ATinyURL|
	     :PTinyURL............................: |:PTinyURL|
	     :UrlTea..............................: |:UrlTea|
	     :AUrlTea.............................: |:AUrlTea|
	     :PUrlTea.............................: |:PUrlTea|
	     :BitLy...............................: |:BitLy|
	     :ABitLy..............................: |:ABitLy|
	     :PBitLy..............................: |:PBitLy|
	     :IsGd................................: |:IsGd|
	     :AIsGd...............................: |:AIsGd|
	     :PIsGd...............................: |:PIsGd|
	     :UrlBorg.............................: |:UrlBorg|
	     twitvim_urlborg_key..................: |twitvim_urlborg_key|
	     :AUrlBorg............................: |:AUrlBorg|
	     :PUrlBorg............................: |:PUrlBorg|
	     :Summize.............................: |:Summize|
	5. Timeline Highlighting..................: |TwitVim-highlight|
	   twitterUser............................: |hl-twitterUser|
	   twitterTime............................: |hl-twitterTime|
	   twitterTitle...........................: |hl-twitterTitle|
	   twitterLink............................: |hl-twitterLink|
	   twitterReply...........................: |hl-twitterReply|
	6. History................................: |TwitVim-history|
	7. Credits................................: |TwitVim-credits|


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

	If you installed from the Vimball (.vba) file, twitvim.vim should
	already be in its correct place.


	3. twitvim_login				*twitvim_login*

	Add the following to your vimrc:

		let twitvim_login = "USER:PASS"

	Replace USER with your Twitter user name and PASS with your Twitter
	password.

	It is possible to avoid having your Twitter password in plaintext in
	your vimrc. See |TwitVim-login-base64| for details.


	4. twitvim_proxy				*twitvim_proxy*

	This step is only needed if you access the web through a HTTP proxy.
	If you use a HTTP proxy, add the following to your vimrc:

		let twitvim_proxy = "proxyserver:proxyport"

	Replace proxyserver with the address of the HTTP proxy and proxyport
	with the port number of the HTTP proxy.


	5. twitvim_proxy_login				*twitvim_proxy_login*

	If the HTTP proxy requires authentication, add the following to your
	vimrc:

		let twitvim_proxy_login = "proxyuser:proxypassword"

	Where proxyuser is your proxy user and proxypassword is your proxy
	password.

	It is possible to avoid having your proxy password in plaintext in
	your vimrc. See |TwitVim-login-base64| for details.


------------------------------------------------------------------------------
3.1. Base64-Encoded Login				*TwitVim-login-base64*

	For safety purposes, TwitVim allows you to specify your Twitter login
	and proxy login information preencoded in base64. This is not truly
	secure as it is not encryption but it can stop casual onlookers
	from reading off your password when you edit your vimrc.

						*twitvim_login_b64*
	To do that, set the following in your vimrc:

		let twitvim_login_b64 = "base64string"
	
						*twitvim_proxy_login_b64*
	If your HTTP proxy needs authentication, set the following:

		let twitvim_proxy_login_b64 = "base64string"


	Where base64string is your username:password encoded in base64.


	An example:

	Let's say Joe User has a Twitter login of "joeuser" and a password of
	"joepassword". His first step is to encode "joeuser:joepassword" in
	Base64. He can either use a standalone utility to do that or, in a
	pinch, he can do the encoding at websites such as the following:
	http://makcoder.sourceforge.net/demo/base64.php
	http://www.opinionatedgeek.com/dotnet/tools/Base64Encode/

	The result is: am9ldXNlcjpqb2VwYXNzd29yZA==

	Then he adds the following to his vimrc:

		let twitvim_login_b64 = "am9ldXNlcjpqb2VwYXNzd29yZA=="

	And his setup is ready.


==============================================================================
4. TwitVim Manual					*TwitVim-manual*

------------------------------------------------------------------------------
4.1. Update Commands				*TwitVim-update-commands*

	These commands post an update to your Twitter account. If the friends,
	user, or public timeline is visible, TwitVim will insert the update
	into the timeline view after posting it.

	:PosttoTwitter					*:PosttoTwitter*

	This command will prompt you for a message and post it to Twitter.

	:CPosttoTwitter					*:CPosttoTwitter*

	This command posts the current line in the current buffer to Twitter.

	:BPosttoTwitter					*:BPosttoTwitter*

	This command posts the contents of the current buffer to Twitter.

------------------------------------------------------------------------------
4.2. Timeline Commands				*TwitVim-timeline-commands*

	These commands retrieve a Twitter timeline and display it in a special
	Twitter buffer. TwitVim applies syntax highlighting to highlight
	certain elements in the timeline view. See |TwitVim-highlight| for a
	list of highlighting groups it uses.


	:[count]UserTwitter				*:UserTwitter*
	:[count]UserTwitter {username}

	This command displays your Twitter timeline.

	If you specify a {username}, this command displays the timeline for
	that user.

	If you specify [count], that number is used as the page number. For
	example, :2UserTwitter displays the second page from your user
	timeline.


	:[count]FriendsTwitter				*:FriendsTwitter*
	:[count]FriendsTwitter {username}

	This command displays your Twitter timeline with updates from friends
	merged in.

	If you specify a {username}, this command displays the friends
	timeline for that user.

	If you specify [count], that number is used as the page number. For
	example, :2FriendsTwitter displays the second page from your friends
	timeline.


	:[count]RepliesTwitter				*:RepliesTwitter*

	This command displays a timeline of @-replies that you've received
	from other Twitter users.

	If you specify [count], that number is used as the page number. For
	example, :2RepliesTwitter displays the second page from your replies
	timeline.


	:PublicTwitter					*:PublicTwitter*

	This command displays the public timeline.


	:[count]DMTwitter				*:DMTwitter*

	This command displays direct messages that you've received.

	If you specify [count], that number is used as the page number. For
	example, :2DMTwitter displays the second page from your direct
	messages timeline.


	:[count]DMSentTwitter				*:DMSentTwitter*

	This command displays direct messages that you've sent.

	If you specify [count], that number is used as the page number. For
	example, :2DMSentTwitter displays the second page from your direct
	messages sent timeline.


------------------------------------------------------------------------------
4.3. Mappings						*TwitVim-mappings*

	Alt-T						*TwitVim-A-t*

	In visual mode, Alt-T posts the highlighted text to Twitter.

	Ctrl-T						*TwitVim-C-t*

	This is an alternative to the Alt-T mapping. If the menu bar is
	enabled, Alt-T pulls down the Tools menu. So use this mapping instead.

	Alt-R						*TwitVim-A-r*
	<Leader>r					*TwitVim-Leader-r*

	This mapping is local to the timeline buffer. In the timeline buffer,
	it starts composing an @-reply on the command line to the author of
	the tweet on the current line.

	Under Cygwin, Alt-R is not recognized so you can use <Leader>r as an
	alternative. The <Leader> character defaults to \ (backslash) but see
	|mapleader| for information on customizing that.

	Alt-D						*TwitVim-A-d*
	<Leader>d					*TwitVim-Leader-d*

	This mapping is local to the timeline buffer. In the timeline buffer,
	it starts composing a direct message on the command line to the author
	of the tweet on the current line.

	Under Cygwin, Alt-D is not recognized so you can use <Leader>d as an
	alternative. The <Leader> character defaults to \ (backslash) but see
	|mapleader| for information on customizing that.

	Alt-G						*TwitVim-A-g*
	<Leader>g					*TwitVim-Leader-g*

	This mapping is local to the timeline buffer. It launches the web
	browser with the URL at the cursor position. If you visually select
	text before invoking this mapping, it launches the web browser with
	the selected text.

	As a special case, if the cursor is on a word of the form @user or
	user:, TwitVim will display that user's timeline in the timeline
	buffer. This will not launch the web browser.

							*twitvim_browser_cmd*
	Before using this command, you need to tell TwitVim how to launch your
	browser. For example, you can add the following to your vimrc:

		let twitvim_browser_cmd = 'firefox.exe'

	Of course, replace firefox.exe with the browser of your choice.


------------------------------------------------------------------------------
4.4. Utility Commands					*TwitVim-utility*

	:Tweetburner					*:Tweetburner*
	:Tweetburner {url}

	Tweetburner is a URL forwarding and shortening service. See
	http://tweetburner.com/

	This command calls the Tweetburner API to get a short URL in place of
	<url>. If {url} is not provided on the command line, the command will
	prompt you to enter a URL. The short URL is then inserted into the
	current buffer at the current position.

	:ATweetburner					*:ATweetburner*
	:ATweetburner {url}

	Same as :Tweetburner but appends, i.e. inserts after the current
	position instead of at the current position,  the short URL instead.

	:PTweetburner					*:PTweetburner*
	:PTweetburner {url}
	
	Same as :Tweetburner but prompts for a tweet on the command line with
	the short URL already inserted.


	:Snipurl					*:Snipurl*
	:Snipurl {url}

	SnipURL is a URL forwarding and shortening service. See
	http://www.snipurl.com/

	This command calls the SnipURL API to get a short URL in place of
	<url>. If {url} is not provided on the command line, the command will
	prompt you to enter a URL. The short URL is then inserted into the
	current buffer at the current position.

	:ASnipurl					*:ASnipurl*
	:ASnipurl {url}

	Same as :Snipurl but appends, i.e. inserts after the current
	position instead of at the current position,  the short URL instead.

	:PSnipurl					*:PSnipurl*
	:PSnipurl {url}
	
	Same as :Snipurl but prompts for a tweet on the command line with
	the short URL already inserted.


	:Metamark					*:Metamark*
	:Metamark {url}

	Metamark is a URL forwarding and shortening service. See
	http://metamark.net/

	This command calls the Metamark API to get a short URL in place of
	<url>. If {url} is not provided on the command line, the command will
	prompt you to enter a URL. The short URL is then inserted into the
	current buffer at the current position.

	:AMetamark					*:AMetamark*
	:AMetamark {url}

	Same as :Metamark but appends, i.e. inserts after the current
	position instead of at the current position,  the short URL instead.

	:PMetamark					*:PMetamark*
	:PMetamark {url}
	
	Same as :Metamark but prompts for a tweet on the command line with
	the short URL already inserted.


	:TinyURL					*:TinyURL*
	:TinyURL {url}

	TinyURL is a URL forwarding and shortening service. See
	http://tinyurl.com

	This command calls the TinyURL API to get a short URL in place of
	<url>. If {url} is not provided on the command line, the command will
	prompt you to enter a URL. The short URL is then inserted into the
	current buffer at the current position.

	:ATinyURL					*:ATinyURL*
	:ATinyURL {url}

	Same as :TinyURL but appends, i.e. inserts after the current
	position instead of at the current position,  the short URL instead.

	:PTinyURL					*:PTinyURL*
	:PTinyURL {url}
	
	Same as :TinyURL but prompts for a tweet on the command line with
	the short URL already inserted.


	:UrlTea						*:UrlTea*
	:UrlTea {url}

	UrlTea is a URL forwarding and shortening service. See
	http://urltea.com

	This command calls the UrlTea API to get a short URL in place of
	<url>. If {url} is not provided on the command line, the command will
	prompt you to enter a URL. The short URL is then inserted into the
	current buffer at the current position.

	:AUrlTea					*:AUrlTea*
	:AUrlTea {url}

	Same as :UrlTea but appends, i.e. inserts after the current
	position instead of at the current position,  the short URL instead.

	:PUrlTea					*:PUrlTea*
	:PUrlTea {url}
	
	Same as :UrlTea but prompts for a tweet on the command line with
	the short URL already inserted.


	:BitLy						*:BitLy*
	:BitLy {url}

	bit.ly is a URL forwarding and shortening service. See
	http://bit.ly/go

	This command calls the bit.ly API to get a short URL in place of
	<url>. If {url} is not provided on the command line, the command will
	prompt you to enter a URL. The short URL is then inserted into the
	current buffer at the current position.

	:ABitLy						*:ABitLy*
	:ABitLy {url}

	Same as :BitLy but appends, i.e. inserts after the current
	position instead of at the current position, the short URL instead.

	:PBitLy						*:PBitLy*
	:PBitLy {url}
	
	Same as :BitLy but prompts for a tweet on the command line with
	the short URL already inserted.


	:IsGd						*:IsGd*
	:IsGd {url}

	is.gd is a URL forwarding and shortening service. See
	http://is.gd

	This command calls the is.gd API to get a short URL in place of <url>.
	If {url} is not provided on the command line, the command will prompt
	you to enter a URL. The short URL is then inserted into the current
	buffer at the current position.

	:AIsGd						*:AIsGd*
	:AIsGd {url}

	Same as :IsGd but appends, i.e. inserts after the current position
	instead of at the current position, the short URL instead.

	:PIsGd						*:PIsGd*
	:PIsGd {url}
	
	Same as :IsGd but prompts for a tweet on the command line with the
	short URL already inserted.


	:UrlBorg					*:UrlBorg*
	:UrlBorg {url}

	urlBorg is a URL forwarding and shortening service. See
	http://urlborg.com

	This command calls the urlBorg API to get a short URL in place of
	<url>. If {url} is not provided on the command line, the command will
	prompt you to enter a URL. The short URL is then inserted into the
	current buffer at the current position.

	The urlBorg API requires an API key. A default API key is provided
	with TwitVim and no configuration is required. However, if you wish to
	supply your own key in order to track your urlBorg history and stats,
	visit http://urlborg.com/a/account/ to retrieve your API key and then
	add the following to your vimrc:

							*twitvim_urlborg_key*
		let twitvim_urlborg_key = "12345-6789"

	Replace 12345-6789 with your API key.

	:AUrlBorg					*:AUrlBorg*
	:AUrlBorg {url}

	Same as :UrlBorg but appends, i.e. inserts after the current position
	instead of at the current position, the short URL instead.

	:PUrlBorg					*:PUrlBorg*
	:PUrlBorg {url}
	
	Same as :UrlBorg but prompts for a tweet on the command line with the
	short URL already inserted.


	:Summize					*:Summize*
	:Summize {query}
	
	Summize is a website that searches Twitter in realtime. See
	http://summize.com/

	This command calls the Summize API to search for {query}. If {query}
	is not provided on the command line, the command will prompt you for
	it. Search results are then displayed in the timeline buffer.

	All of the Summize search operators are supported implicitly. See
	http://summize.com/operators for a list of search operators.


==============================================================================
5. Timeline Highlighting				*TwitVim-highlight*

	TwitVim uses a number of highlighting groups to highlight certain
	elements in the Twitter timeline views. See |:highlight| for details
	on how to customize these highlighting groups.

	twitterUser					*hl-twitterUser*
	
	The Twitter user name at the beginning of each line.

	twitterTime					*hl-twitterTime*

	The time a Twitter update was posted.

	twitterTitle					*hl-twitterTitle*

	The header at the top of the timeline view.

	twitterLink					*hl-twitterLink*

	Link URLs in a Twitter status.

	twitterReply					*hl-twitterReply*

	@-reply in a Twitter status.


==============================================================================
6. TwitVim History					*TwitVim-history*

	0.2.18 : 2008-07-14 * Added support for urlBorg API.
	0.2.17 : 2008-07-11 * Added command to show DM Sent Timeline.
			    * Added support for pagination in Friends, User,
			      Replies, DM, and DM Sent timelines.
			    * Added support for bit.ly API and is.gd API.
	0.2.16 : 2008-05-16 * Removed quotes around browser launch URL.
			    * Escape ! character in browser launch URL.
	0.2.15 : 2008-05-13 * Extend :UserTwitter and :FriendsTwitter to show
			      another user's timeline if argument supplied.
			    * Extend Alt-G mapping to jump to another user's
			      timeline if invoked over @user or user:
			    * Escape special Vim shell characters in URL when
			      launching web browser.
	0.2.14 : 2008-05-12 * Added support for Summize search API.
	0.2.13 : 2008-05-07 * Added mappings to launch web browser on URLs in
			      timeline.
	0.2.12 : 2008-05-05 * Allow user to specify Twitter login info and
			      proxy login info preencoded in base64.
	0.2.11 : 2008-05-02 * Scroll to top in timeline window after adding
			      an update line.
			    * Add <Leader>r and <Leader>d mappings as
			      alternative to Alt-R and Alt-D because the
			      latter are not valid key combos under Cygwin.
	0.2.10 : 2008-04-25 * Shortened snipurl.com to snipr.com
			    * Added support for proxy authentication.
			    * Handle Perl module load failure. Not that I
			      expect those modules to ever be missing.
	0.2.9 : 2008-04-23 * Added some status messages.
			   * Added menu items under Plugin menu.
			   * Allow Ctrl-T as an alternative to Alt-T to avoid
			     conflict with the menu bar.
			   * Added support for UrlTea API.
			   * Generalize URL encoding to all non-alpha chars.
	0.2.8 : 2008-04-22 * Encode URLs sent to URL-shortening services.
	0.2.7 : 2008-04-21 * Add support for TinyURL API.
			   * Add quick direct message feature.
	0.2.6 : 2008-04-15 * Delete Twitter buffer to the blackhole register
			     to avoid stepping on registers unnecessarily.
			   * Quote login and proxy arguments before sending to
			     cURL.
			   * Added support for SnipURL API and Metamark API.
	0.2.5 : 2008-04-14 * Escape the "+" character in sent tweets.
			   * Added Perl code to convert Twitter timestamps to
			     local time and simplify them.
			   * Fix for timestamp highlight when the "|"
			     character appears in a tweet.
	0.2.4 : 2008-04-13 * Use <q-args> in Tweetburner commands.
			   * Improve XML parsing so that order of elements
			     does not matter.
			   * Changed T mapping to Alt-T to avoid overriding
			     the |T| command.
	0.2.3 : 2008-04-12 * Added more Tweetburner commands.
	0.2.2 : 2008-04-11 * Added quick reply feature.
			   * Added Tweetburner support.
			   * Changed client ident to "from twitvim".
	0.2.1 : 2008-04-10 * Bug fix for Chinese characters in timeline.
			     Thanks to Leiyue.
			   * Scroll up to newest tweet after refreshing
			     timeline.
			   * Changed Twitter window name to avoid unsafe
			     special characters and clashes with file names.
	0.2.0 : 2008-04-09 * Added views for public, friends, user timelines,
			     replies, and direct messages. 
			   * Automatically insert user's posts into
			     public, friends, or user timeline, if visible.
			   * Added syntax highlighting for timeline view.
	0.1.2 : 2008-04-03 * Make plugin conform to guidelines in
    			    |write-plugin|.
			   * Add help documentation.
	0.1.1 : 2008-04-01 * Add error reporting for cURL problems.
	0.1   : 2008-03-28 * Initial release.


==============================================================================
7. TwitVim Credits					*TwitVim-credits*

	Thanks to Travis Jeffery, the author of the original VimTwitter script
	(vimscript #2124), who came up with the idea of running cURL from Vim
	to access the Twitter API.

	Techniques for managing the Twitter buffer were adapted from the NERD
	Tree plugin (vimscript #1658) by Marty Grenfell.


==============================================================================
vim:tw=78:ts=8:ft=help:norl:
