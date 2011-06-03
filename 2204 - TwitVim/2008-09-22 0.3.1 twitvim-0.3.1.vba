" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin/twitvim.vim	[[[1
1576
" ==============================================================
" TwitVim - Post to Twitter from Vim
" Based on Twitter Vim script by Travis Jeffery <eatsleepgolf@gmail.com>
"
" Version: 0.3.1
" License: Vim license. See :help license
" Language: Vim script
" Maintainer: Po Shan Cheah <morton@mortonfox.com>
" Created: March 28, 2008
" Last updated: September 18, 2008
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

" Allow the user to override the API root, e.g. for identi.ca, which offers a
" Twitter-compatible API.
function! s:get_api_root()
    return exists('g:twitvim_api_root') ? g:twitvim_api_root : "http://twitter.com"
endfunction

" Allow user to set the format for retweets.
function! s:get_retweet_fmt()
    return exists('g:twitvim_retweet_format') ? g:twitvim_retweet_format : "Retweeting %s: %t"
endfunction

" Allow user to enable Python networking code by setting twitvim_enable_python.
function! s:get_enable_python()
    return exists('g:twitvim_enable_python') ? g:twitvim_enable_python : 0
endfunction

" Allow user to enable Perl networking code by setting twitvim_enable_perl.
function! s:get_enable_perl()
    return exists('g:twitvim_enable_perl') ? g:twitvim_enable_perl : 0
endfunction

" Allow user to enable Ruby code by setting twitvim_enable_ruby.
function! s:get_enable_ruby()
    return exists('g:twitvim_enable_ruby') ? g:twitvim_enable_ruby : 0
endfunction

" Allow user to enable Tcl code by setting twitvim_enable_tcl.
function! s:get_enable_tcl()
    return exists('g:twitvim_enable_tcl') ? g:twitvim_enable_tcl : 0
endfunction

" Get proxy setting from twitvim_proxy in .vimrc or _vimrc.
" Format is proxysite:proxyport
function! s:get_proxy()
    return exists('g:twitvim_proxy') ? g:twitvim_proxy : ''
endfunction

" If twitvim_proxy_login exists, use that as the proxy login.
" Format is proxyuser:proxypassword
" If twitvim_proxy_login_b64 exists, use that instead. This is the proxy
" user:password in base64 encoding.
function! s:get_proxy_login()
    if exists('g:twitvim_proxy_login_b64') && g:twitvim_proxy_login_b64 != ''
	return g:twitvim_proxy_login_b64
    else
	return exists('g:twitvim_proxy_login') ? g:twitvim_proxy_login : ''
    endif
endfunction

" Display an error message in the message area.
function! s:errormsg(msg)
    redraw
    echohl ErrorMsg
    echomsg a:msg
    echohl None
endfunction

" Display a warning message in the message area.
function! s:warnmsg(msg)
    redraw
    echohl WarningMsg
    echo a:msg
    echohl None
endfunction

" Get Twitter login info from twitvim_login in .vimrc or _vimrc.
" Format is username:password
" If twitvim_login_b64 exists, use that instead. This is the user:password
" in base64 encoding.
function! s:get_twitvim_login()
    if exists('g:twitvim_login_b64') && g:twitvim_login_b64 != ''
	return g:twitvim_login_b64
    elseif exists('g:twitvim_login') && g:twitvim_login != ''
	return g:twitvim_login
    else
	" Beep and error-highlight 
	execute "normal \<Esc>"
	call s:errormsg('Twitter login not set. Please add to .vimrc: let twitvim_login="USER:PASS"')
	return ''
    endif
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

" === Time parser ===

" Convert date to Julian date.
function! s:julian(year, mon, mday)
    let month = (a:mon - 1 + 10) % 12
    let year = a:year - month / 10
    return a:mday + 365 * year + year / 4 - year / 100 + year / 400 + ((month * 306) + 5) / 10
endfunction

" Calculate number of days since UNIX Epoch.
function! s:daygm(year, mon, mday)
    return s:julian(a:year, a:mon, a:mday) - s:julian(1970, 1, 1)
endfunction

" Convert date/time to UNIX time. (seconds since Epoch)
function! s:timegm(year, mon, mday, hour, min, sec)
    return a:sec + a:min * 60 + a:hour * 60 * 60 + s:daygm(a:year, a:mon, a:mday) * 60 * 60 * 24
endfunction

" Convert abbreviated month name to month number.
function! s:conv_month(s)
    let monthnames = ['jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec']
    for mon in range(len(monthnames))
	if monthnames[mon] == tolower(a:s)
	    return mon + 1
	endif	
    endfor
    return 0
endfunction

function! s:timegm2(matchres, indxlist)
    let args = []
    for i in a:indxlist
	if i < 0
	    let mon = s:conv_month(a:matchres[-i])
	    if mon == 0
		return -1
	    endif
	    let args = add(args, mon)
	else
	    let args = add(args, a:matchres[i] + 0)
	endif
    endfor
    return call('s:timegm', args)
endfunction

" Parse a Twitter time string.
function! s:parse_time(str)
    " This timestamp format is used by Twitter in timelines.
    let matchres = matchlist(a:str, '^\w\+,\s\+\(\d\+\)\s\+\(\w\+\)\s\+\(\d\+\)\s\+\(\d\+\):\(\d\+\):\(\d\+\)\s\++0000$')
    if matchres != []
	return s:timegm2(matchres, [3, -2, 1, 4, 5, 6])
    endif

    " This timestamp format is used by Twitter in response to an update.
    let matchres = matchlist(a:str, '^\w\+\s\+\(\w\+\)\s\+\(\d\+\)\s\+\(\d\+\):\(\d\+\):\(\d\+\)\s\++0000\s\+\(\d\+\)$')
    if matchres != []
	return s:timegm2(matchres, [6, -1, 2, 3, 4, 5])
    endif
	
    " This timestamp format is used by Twitter Search.
    let matchres = matchlist(a:str, '^\(\d\+\)-\(\d\+\)-\(\d\+\)T\(\d\+\):\(\d\+\):\(\d\+\)Z$')
    if matchres != []
	return s:timegm2(matchres, range(1, 6))
    endif

    return -1
endfunction

" Convert the Twitter timestamp to local time and simplify it.
function s:time_filter(str)
    if !exists("*strftime")
	return a:str
    endif
    let t = s:parse_time(a:str)
    return t < 0 ? a:str : strftime('%I:%M %p %b %d, %Y', t)
endfunction

" === End of time parser ===

" === Networking code ===

function! s:url_encode_char(c)
    let utf = iconv(a:c, &encoding, "utf-8")
    if utf == ""
	return a:c
    else
	let s = ""
	for i in range(strlen(utf))
	    let s .= printf("%%%02X", char2nr(utf[i]))
	endfor
	return s
    endif
endfunction

" URL-encode a string.
function! s:url_encode(str)
    return substitute(a:str, '[^a-zA-Z0-9_-]', '\=s:url_encode_char(submatch(0))', 'g')
endfunction

" Use curl to fetch a web page.
function! s:curl_curl(url, login, proxy, proxylogin, parms)
    let error = ""
    let output = ""

    let curlcmd = "curl -s -f -S "

    if a:proxy != ""
	let curlcmd .= '-x "'.a:proxy.'" '
    endif

    if a:proxylogin != ""
	if stridx(a:proxylogin, ':') != -1
	    let curlcmd .= '-U "'.a:proxylogin.'" '
	else
	    let curlcmd .= '-H "Proxy-Authorization: Basic '.a:proxylogin.'" '
	endif
    endif

    if a:login != ""
	if stridx(a:login, ':') != -1
	    let curlcmd .= '-u "'.a:login.'" '
	else
	    let curlcmd .= '-H "Authorization: Basic '.a:login.'" '
	endif
    endif

    for [k, v] in items(a:parms)
	let curlcmd .= '-d "'.s:url_encode(k).'='.s:url_encode(v).'" '
    endfor

    let curlcmd .= '"'.a:url.'"'

    let output = system(curlcmd)
    if v:shell_error != 0
	let error = output
    endif

    return [ error, output ]
endfunction

" Check if we can use Python.
function! s:check_python()
    let can_python = 1
    python <<EOF
import vim
try:
    import urllib
    import urllib2
    import base64
except:
    vim.command('let can_python = 0')
EOF
    return can_python
endfunction

" Use Python to fetch a web page.
function! s:python_curl(url, login, proxy, proxylogin, parms)
    let error = ""
    let output = ""
    python <<EOF
import urllib
import urllib2
import base64
import vim

def make_base64(s):
    if s.find(':') != -1:
	s = base64.b64encode(s)
    return s

try:
    url = vim.eval("a:url")
    parms = vim.eval("a:parms")
    req = parms == {} and urllib2.Request(url) or urllib2.Request(url, urllib.urlencode(parms))

    login = vim.eval("a:login")
    if login != "":
	req.add_header('Authorization', 'Basic %s' % make_base64(login))

    proxy = vim.eval("a:proxy")
    if proxy != "":
	req.set_proxy(proxy, 'http')

    proxylogin = vim.eval("a:proxylogin")
    if proxylogin != "":
	req.add_header('Proxy-Authorization', 'Basic %s' % make_base64(proxylogin))

    f = urllib2.urlopen(req)
    out = ''.join(f.readlines())
except urllib2.HTTPError, (httperr):
    vim.command("let error='%s'" % str(httperr).replace("'", "''"))
else:
    vim.command("let output='%s'" % out.replace("'", "''"))
EOF

    return [ error, output ]
endfunction

" Check if we can use Perl.
function! s:check_perl()
    let can_perl = 1
    perl <<EOF
eval {
    require MIME::Base64;
    MIME::Base64->import;

    require LWP::UserAgent;
    LWP::UserAgent->import;
};

if ($@) {
    VIM::DoCommand('let can_perl = 0');
}
EOF
    return can_perl
endfunction

" Use Perl to fetch a web page.
function! s:perl_curl(url, login, proxy, proxylogin, parms)
    let error = ""
    let output = ""

    perl <<EOF
require MIME::Base64;
MIME::Base64->import;

require LWP::UserAgent;
LWP::UserAgent->import;

sub make_base64 {
    my $s = shift;
    $s =~ /:/ ? encode_base64($s) : $s;
}

my $ua = LWP::UserAgent->new;

my $url = VIM::Eval('a:url');

my $login = VIM::Eval('a:login');
$login ne '' and $ua->default_header('Authorization' => 'Basic '.make_base64($login));

my $proxy = VIM::Eval('a:proxy');
$proxy ne '' and $ua->proxy('http', "http://$proxy");

my $proxylogin = VIM::Eval('a:proxylogin');
$proxylogin ne '' and $ua->default_header('Proxy-Authorization' => 'Basic '.make_base64($proxylogin));

my %parms = ();
my $keys = VIM::Eval('keys(a:parms)');
for $k (split(/\n/, $keys)) {
    $parms{$k} = VIM::Eval("a:parms['$k']");
}

my $response = %parms ? $ua->post($url, \%parms) : $ua->get($url);
if ($response->is_success) {
    my $output = $response->content;
    $output =~ s/'/''/g;
    VIM::DoCommand("let output ='$output'");
}
else {
    my $error = $response->status_line;
    $error =~ s/'/''/g;
    VIM::DoCommand("let error ='$error'");
}
EOF

    return [ error, output ]
endfunction

" Check if we can use Ruby.
"
" Note: Before the networking code will function in Ruby under Windows, you
" need the patch from here:
" http://www.mail-archive.com/vim_dev@googlegroups.com/msg03693.html
"
" and Bram's correction to the patch from here:
" http://www.mail-archive.com/vim_dev@googlegroups.com/msg03713.html
"
function! s:check_ruby()
    let can_ruby = 1
    ruby <<EOF
begin
    require 'net/http'
    require 'uri'
    require 'Base64'
rescue LoadError
    VIM.command('let can_ruby = 0')
end
EOF
    return can_ruby
endfunction

" Use Ruby to fetch a web page.
function! s:ruby_curl(url, login, proxy, proxylogin, parms)
    let error = ""
    let output = ""

    ruby <<EOF
require 'net/http'
require 'uri'
require 'Base64'

def make_base64(s)
    s =~ /:/ ? Base64.encode64(s) : s
end

proxy = VIM.evaluate('a:proxy')
if proxy != ''
    prox = URI.parse("http://#{proxy}")
    net = Net::HTTP::Proxy(prox.host, prox.port)
else
    net = Net::HTTP
end

parms = {}
keys = VIM.evaluate('keys(a:parms)')
keys.split(/\n/).each { |k|
    parms[k] = VIM.evaluate("a:parms['#{k}']")
}

url = URI.parse(VIM.evaluate('a:url'))
res = net.start(url.host, url.port) { |http| 
    path = "#{url.path}?#{url.query}"
    if parms == {}
	req = Net::HTTP::Get.new(path)
    else
	req = Net::HTTP::Post.new(path)
	req.set_form_data(parms)
    end

    login = VIM.evaluate('a:login')
    if login != ''
	req.add_field 'Authorization', "Basic #{make_base64(login)}"
    end

    proxylogin = VIM.evaluate('a:proxylogin')
    if proxylogin != ''
	req.add_field 'Proxy-Authorization', "Basic #{make_base64(proxylogin)}"
    end

    http.request(req)
}
case res
when Net::HTTPSuccess
    output = res.body.gsub("'", "''")
    VIM.command("let output='#{output}'")
else
    error = "#{res.code} #{res.message}".gsub("'", "''")
    VIM.command("let error='#{error}'")
end
EOF

    return [error, output]
endfunction

" Check if we can use Tcl.
"
" Note: ActiveTcl 8.5 doesn't include Tcllib in the download. You need to run the following after installing ActiveTcl:
"
"    teacup install tcllib
"
function! s:check_tcl()
    let can_tcl = 1
    tcl <<EOF
if [catch {
    package require http
    package require uri
    package require base64
} result] {
    ::vim::command "let can_tcl = 0"
}
EOF
    return can_tcl
endfunction

" Use Tcl to fetch a web page.
function! s:tcl_curl(url, login, proxy, proxylogin, parms)
    let error = ""
    let output = ""

    tcl << EOF
package require http
package require uri
package require base64

proc make_base64 {s} {
    if { [string first : $s] >= 0 } {
	return [base64::encode $s]
    }
    return $s
}

set url [::vim::expr a:url]

set headers [list]

set proxy [::vim::expr a:proxy]
if { $proxy != "" } {
    array set prox [uri::split "http://$proxy"]
    ::http::config -proxyhost $prox(host)
    ::http::config -proxyport $prox(port)
}

set proxylogin [::vim::expr a:proxylogin]
if { $proxylogin != "" } {
    lappend headers "Proxy-Authorization" "Basic [make_base64 $proxylogin]"
}

set login [::vim::expr a:login]
if { $login != "" } {
    lappend headers "Authorization" "Basic [make_base64 $login]"
}

set parms [list]
set keys [split [::vim::expr "keys(a:parms)"] "\n"]
if { [llength $keys] > 0 } {
    foreach key $keys {
	lappend parms $key [::vim::expr "a:parms\['$key']"]
    }
    set query [eval [concat ::http::formatQuery $parms]]
    set res [::http::geturl $url -headers $headers -query $query]
} else {
    set res [::http::geturl $url -headers $headers]
}

upvar #0 $res state

if { $state(status) == "ok" } {
    set output [string map {' ''} $state(body)]
    ::vim::command "let output = '$output'"
} else {
    set error [string map {' ''} $state(error)]
    ::vim::command "let error = '$error'"
}
EOF

    return [error, output]
endfunction

" Find out which method we can use to fetch a web page.
function! s:get_curl_method()
    if !exists('s:curl_method')
	let s:curl_method = 'curl'

	if s:get_enable_perl() && has('perl')
	    if s:check_perl()
		let s:curl_method = 'perl'
	    endif
	elseif s:get_enable_python() && has('python')
	    if s:check_python()
		let s:curl_method = 'python'
	    endif
	elseif s:get_enable_ruby() && has('ruby')
	    if s:check_ruby()
		let s:curl_method = 'ruby'
	    endif
	elseif s:get_enable_tcl() && has('tcl')
	    if s:check_tcl()
		let s:curl_method = 'tcl'
	    endif
	endif
    endif

    return s:curl_method
endfunction

function! s:run_curl(url, login, proxy, proxylogin, parms)
    return s:{s:get_curl_method()}_curl(a:url, a:login, a:proxy, a:proxylogin, a:parms)
endfunction

function! s:reset_curl_method()
    if exists('s:curl_method')	
	unlet s:curl_method
    endif
endfunction

function! s:show_curl_method()
    echo 'Method:' s:get_curl_method()
endfunction

" For debugging. Reset networking method.
if !exists(":TwitVimResetMethod")
    command TwitVimResetMethod :call <SID>reset_curl_method()
endif

" For debugging. Show current networking method.
if !exists(":TwitVimShowMethod")
    command TwitVimShowMethod :call <SID>show_curl_method()
endif

" === End of networking code ===

" Add update to Twitter buffer if public, friends, or user timeline.
function! s:add_update(output)
    if s:twit_buftype == "public" || s:twit_buftype == "friends" || s:twit_buftype == "user"

	" Parse the output from the Twitter update call.
	let date = s:time_filter(s:xml_get_element(a:output, 'created_at'))
	let text = s:xml_get_element(a:output, 'text')
	let name = s:xml_get_element(a:output, 'screen_name')

	" Add the status ID to the current buffer's statuses list.
	call insert(s:statuses, s:xml_get_element(a:output, 'id'), 3)

	if text == ""
	    return
	endif

	let twit_bufnr = bufwinnr('^'.s:twit_winname.'$')
	if twit_bufnr > 0
	    execute twit_bufnr . "wincmd w"
	    set modifiable
	    call append(2, name.': '.s:convert_entity(text).' |'.date.'|')
	    normal 1G
	    set nomodifiable
	    wincmd p
	endif
    endif
endfunction

" Count number of characters in a multibyte string. Use technique from
" :help strlen().
function! s:mbstrlen(s)
    return strlen(substitute(a:s, ".", "x", "g"))
endfunction

" Common code to post a message to Twitter.
function! s:post_twitter(mesg, inreplyto)
    let login = s:get_twitvim_login()
    if login == ''
	return -1
    endif

    let parms = {}

    " Add in_reply_to_status_id if status ID is available.
    if a:inreplyto != 0
	let parms["in_reply_to_status_id"] = a:inreplyto
    endif

    let mesg = a:mesg

    " Remove trailing newline. You see that when you visual-select an entire
    " line. Don't let it count towards the tweet length.
    let mesg = substitute(mesg, '\n$', '', "")

    " Convert internal newlines to spaces.
    let mesg = substitute(mesg, '\n', ' ', "g")

    let mesglen = s:mbstrlen(mesg)

    " Check tweet length. Note that the tweet length should be checked before
    " URL-encoding the special characters because URL-encoding increases the
    " string length.
    if mesglen > s:char_limit
	call s:warnmsg("Your tweet has ".(mesglen - s:char_limit)." too many characters. It was not sent.")
    elseif mesglen < 1
	call s:warnmsg("Your tweet was empty. It was not sent.")
    else
	redraw
	echo "Sending update to Twitter..."

	let url = s:get_api_root()."/statuses/update.xml?source=twitvim"
	let parms["status"] = mesg

	let [error, output] = s:run_curl(url, login, s:get_proxy(), s:get_proxy_login(), parms)

	if error != ''
	    call s:errormsg("Error posting your tweet: ".error)
	else
	    call s:add_update(output)
	    redraw
	    echo "Your tweet was sent. You used ".mesglen." characters."
	endif
    endif
endfunction

" Prompt user for tweet and then post it.
" If initstr is given, use that as the initial input.
function! s:CmdLine_Twitter(initstr, inreplyto)
    " Do this here too to check for twitvim_login. This is to avoid having the
    " user type in the message only to be told that his configuration is
    " incomplete.
    let login = s:get_twitvim_login()
    if login == ''
	return -1
    endif

    call inputsave()
    let mesg = input("Your Twitter: ", a:initstr)
    call inputrestore()
    call s:post_twitter(mesg, a:inreplyto)
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
	" If the status ID is not available, get() will return 0 and
	" post_twitter() won't add in_reply_to_status_id to the update.
	call s:CmdLine_Twitter('@'.username.' ', get(s:statuses, line('.')))
    endif
endfunction

" This is for a local mapping in the timeline. Start a direct message on the
" command line to the author of the tweet on the current line.
function! s:Quick_DM()
    let username = s:get_user_name(getline('.'))
    if username != ""
	call s:CmdLine_Twitter('d '.username.' ', 0)
    endif
endfunction

" Extract the tweet text from a timeline buffer line.
function! s:get_tweet(line)
    let line = substitute(a:line, '^\w\+:\s\+', '', '')
    let line = substitute(line, '\s\+|[^|]\+|$', '', '')

    " Remove newlines.
    let line = substitute(line, "\n", '', 'g')

    return line
endfunction

" Retweet is for replicating a tweet from another user.
function! s:Retweet()
    let line = getline('.')
    let username = s:get_user_name(line)
    if username != ""
	let retweet = substitute(s:get_retweet_fmt(), '%s', '@'.username, '')
	let retweet = substitute(retweet, '%t', s:get_tweet(line), '')
	call s:CmdLine_Twitter(retweet, 0)
    endif
endfunction

" Prompt user for tweet.
if !exists(":PosttoTwitter")
    command PosttoTwitter :call <SID>CmdLine_Twitter('', 0)
endif

nnoremenu Plugin.TwitVim.Post\ from\ cmdline :call <SID>CmdLine_Twitter('', 0)<cr>

" Post current line to Twitter.
if !exists(":CPosttoTwitter")
    command CPosttoTwitter :call <SID>post_twitter(getline('.'), 0)
endif

nnoremenu Plugin.TwitVim.Post\ current\ line :call <SID>post_twitter(getline('.'), 0)<cr>

" Post entire buffer to Twitter.
if !exists(":BPosttoTwitter")
    command BPosttoTwitter :call <SID>post_twitter(join(getline(1, "$")), 0)
endif

" Post visual selection to Twitter.
noremap <SID>Visual y:call <SID>post_twitter(@", 0)<cr>
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
	call s:errormsg('Browser cmd not set. Please add to .vimrc: let twitvim_browser_cmd="browsercmd"')
	return -1
    endif

    let startcmd = has("win32") || has("win64") ? "!start " : "! "
    let endcmd = has("unix") ? "&" : ""

    " Escape characters that have special meaning in the :! command.
    let url = substitute(a:url, '!\|#\|%', '\\&', 'g')

    redraw
    echo "Launching web browser..."
    let v:errmsg = ""
    silent! execute startcmd g:twitvim_browser_cmd url endcmd
    if v:errmsg == ""
	redraw
	echo "Web browser launched."
    else
	call s:errormsg('Error launching browser: '.v:errmsg)
    endif
endfunction

" Launch web browser with the URL at the cursor position. If possible, this
" function will try to recognize a URL within the current word. Otherwise,
" it'll just use the whole word.
" If the cWORD happens to be @user or user:, show that user's timeline.
function! s:launch_url_cword()
    let s = expand("<cWORD>")

    " Handle @-replies by showing that user's timeline.
    let matchres = matchlist(s, '^@\(\w\+\)')
    if matchres != []
	call s:get_timeline("user", matchres[1], 1)
	return
    endif

    " Handle username: at the beginning of the line by showing that user's
    " timeline.
    let matchres = matchlist(s, '^\(\w\+\):$')
    if matchres != []
	call s:get_timeline("user", matchres[1], 1)
	return
    endif

    " Handle #-hashtags by showing the Twitter Search for that hashtag.
    let matchres = matchlist(s, '^\(#\w\+\)')
    if matchres != []
	call s:get_summize(matchres[1])
	return
    endif

    let s = substitute(s, '.*\<\(\(http\|https\|ftp\)://\S\+\)', '\1', "")
    call s:launch_browser(s)
endfunction

" Call LongURL API on a shorturl to expand it.
function! s:call_longurl(url)
    redraw
    echo "Sending request to LongURL..."

    let url = 'http://api.longurl.org/v1/expand?url='.s:url_encode(a:url)
    let [error, output] = s:run_curl(url, '', s:get_proxy(), s:get_proxy_login(), {})
    if error != ''
	call s:errormsg("Error calling LongURL API: ".error)
	return ""
    else
	redraw
	echo "Received response from LongURL."

	let longurl = s:xml_get_element(output, 'long_url')
	if longurl != ""
	    return longurl
	endif

	let errormsg = s:xml_get_element(output, 'error')
	if errormsg != ""
	    call s:errormsg("LongURL error: ".errormsg)
	    return ""
	endif

	call s:errormsg("Unknown response from LongURL: ".output)
	return ""
    endif
endfunction

" Call LongURL API on the given string. If no string is provided, use the
" current word. In the latter case, this function will try to recognize a URL
" within the word. Otherwise, it'll just use the whole word.
function! s:do_longurl(s)
    let s = a:s
    if s == ""
	let s = expand("<cWORD>")
	let s = substitute(s, '.*\<\(\(http\|https\|ftp\)://\S\+\)', '\1', "")
    endif
    let result = s:call_longurl(s)
    if result != ""
	redraw
	echo s.' expands to '.result
    endif
endfunction

" Decode HTML entities. Twitter gives those to us a little weird. For example,
" a '<' character comes to us as &amp;lt;
function! s:convert_entity(str)
    let s = a:str
    let s = substitute(s, '&amp;', '\&', 'g')
    let s = substitute(s, '&lt;', '<', 'g')
    let s = substitute(s, '&gt;', '>', 'g')
    let s = substitute(s, '&quot;', '"', 'g')
    let s = substitute(s, '&#\(\d\+\);','\=nr2char(submatch(1))', 'g')
    return s
endfunction

let s:twit_winname = "Twitter_".localtime()
let s:twit_buftype = ""

" Set syntax highlighting in timeline window.
function! s:twitter_win_syntax()
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

	" A #-hashtag must be preceded by whitespace and ends at a non-word
	" character.
	syntax match twitterLink "\S\@<!#\w\+"

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
endfunction

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

	" Retweet feature for replicating another user's tweet.
	nnoremap <buffer> <silent> <Leader>R :call <SID>Retweet()<cr>

	" Call LongURL API on current word or selection.
	nnoremap <buffer> <silent> <Leader>e :call <SID>do_longurl("")<cr>
	vnoremap <buffer> <silent> <Leader>e y:call <SID>do_longurl(@")<cr>
    endif

    call s:twitter_win_syntax()
endfunction

" Get a Twitter window and stuff text into it.
function! s:twitter_wintext(text)
    call s:twitter_win()

    set modifiable

    " Overwrite the entire buffer.
    " Need to use 'silent' or a 'No lines in buffer' message will appear.
    " Delete to the blackhole register "_ so that we don't affect registers.
    silent %delete _
    call setline('.', a:text)
    normal 1G

    set nomodifiable

    wincmd p
endfunction

" Show a timeline.
function! s:show_timeline(timeline, page)
    let matchcount = 1
    let text = []

    " Index of first status will be 3 to match line numbers in timeline display.
    let s:statuses = [0, 0, 0]

    let channel = s:xml_remove_elements(a:timeline, 'item')
    let title = s:xml_get_element(channel, 'title')

    if a:page > 1
	let title .= ' (page '.a:page.')'
    endif

    " The extra stars at the end are for the syntax highlighter to recognize
    " the title. Then the syntax highlighter hides the stars by coloring them
    " the same as the background. It is a bad hack.
    call add(text, title.'*')
    call add(text, repeat('=', s:mbstrlen(title)).'*')

    while 1
	let item = s:xml_get_nth(a:timeline, 'item', matchcount)
	if item == ""
	    break
	endif

	let title = s:xml_get_element(item, 'title')
	let pubdate = s:time_filter(s:xml_get_element(item, 'pubDate'))

	" Parse and save the status ID.
	let status = substitute(s:xml_get_element(item, 'guid'), '^.*/', '', '')
	call add(s:statuses, status)

	call add(text, s:convert_entity(title).' |'.pubdate.'|')

	let matchcount += 1
    endwhile
    call s:twitter_wintext(text)
endfunction

" For debugging. Show list of status IDs.
if !exists(":TwitVimShowStatuses")
    command TwitVimShowStatuses :echo s:statuses
endif

" Generic timeline retrieval function.
function! s:get_timeline(tline_name, username, page)
    if a:tline_name == "public"
	" No authentication is needed for public timeline.
	let login = ''
    else
	let login = s:get_twitvim_login()
	if login == ''
	    return -1
	endif
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

    let url = s:get_api_root()."/statuses/".url_fname

    let [error, output] = s:run_curl(url, login, s:get_proxy(), s:get_proxy_login(), {})

    if error != ''
	call s:errormsg("Error getting Twitter ".a:tline_name." timeline: ".error)
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

    "No status IDs in direct messages.
    let s:statuses = []

    let title = 'Direct messages '.a:sent_or_recv

    if a:page > 1
	let title .= ' (page '.a:page.')'
    endif

    " The extra stars at the end are for the syntax highlighter to recognize
    " the title. Then the syntax highlighter hides the stars by coloring them
    " the same as the background. It is a bad hack.
    call add(text, title.'*')
    call add(text, repeat('=', s:mbstrlen(title)).'*')

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
    let login = s:get_twitvim_login()
    if login == ''
	return -1
    endif

    " Support pagination.
    let pagearg = ''
    if a:page > 1
	let pagearg = '?page='.a:page
    endif

    redraw
    echo "Sending direct message timeline request to Twitter..."

    let url = s:get_api_root()."/direct_messages.xml".pagearg

    let [error, output] = s:run_curl(url, login, s:get_proxy(), s:get_proxy_login(), {})

    if error != ''
	call s:errormsg("Error getting Twitter direct messages: ".error)
	return
    endif

    call s:show_dm_xml('received', output, a:page)
    let s:twit_buftype = "directmessages"
    redraw
    echo "Direct message timeline updated."
endfunction

" Get direct messages sent by user.
function! s:Direct_Messages_Sent(page)
    let login = s:get_twitvim_login()
    if login == ''
	return -1
    endif

    " Support pagination.
    let pagearg = ''
    if a:page > 1
	let pagearg = '?page='.a:page
    endif

    redraw
    echo "Sending direct messages sent timeline request to Twitter..."

    let url = s:get_api_root()."/direct_messages/sent.xml".pagearg

    let [error, output] = s:run_curl(url, login, s:get_proxy(), s:get_proxy_login(), {})

    if error != ''
	call s:errormsg("Error getting Twitter direct messages sent timeline: ".error)
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
    redraw
    echo "Sending request to Tweetburner..."

    let [error, output] = s:run_curl('http://tweetburner.com/links', '', s:get_proxy(), s:get_proxy_login(), {'link[url]' : a:url})

    if error != ''
	call s:errormsg("Error calling Tweetburner API: ".error)
	return ""
    else
	redraw
	echo "Received response from Tweetburner."
	return output
    endif
endfunction

" Call SnipURL API to shorten a URL.
function! s:call_snipurl(url)
    redraw
    echo "Sending request to SnipURL..."

    let url = 'http://snipr.com/site/snip?r=simple&link='.s:url_encode(a:url)

    let [error, output] = s:run_curl(url, '', s:get_proxy(), s:get_proxy_login(), {})

    if error != ''
	call s:errormsg("Error calling SnipURL API: ".error)
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
    redraw
    echo "Sending request to Metamark..."

    let [error, output] = s:run_curl('http://metamark.net/api/rest/simple', '', s:get_proxy(), s:get_proxy_login(), {'long_url' : a:url})

    if error != ''
	call s:errormsg("Error calling Metamark API: ".error)
	return ""
    else
	redraw
	echo "Received response from Metamark."
	return output
    endif
endfunction

" Call TinyURL API to shorten a URL.
function! s:call_tinyurl(url)
    redraw
    echo "Sending request to TinyURL..."

    let url = 'http://tinyurl.com/api-create.php?url='.a:url
    let [error, output] = s:run_curl(url, '', s:get_proxy(), s:get_proxy_login(), {})

    if error != ''
	call s:errormsg("Error calling TinyURL API: ".error)
	return ""
    else
	redraw
	echo "Received response from TinyURL."
	return output
    endif
endfunction

" Call bit.ly API to shorten a URL.
function! s:call_bitly(url)
    redraw
    echo "Sending request to bit.ly..."

    let url = 'http://bit.ly/api?url='.s:url_encode(a:url)
    let [error, output] = s:run_curl(url, '', s:get_proxy(), s:get_proxy_login(), {})

    if error != ''
	call s:errormsg("Error calling bit.ly API: ".error)
	return ""
    else
	redraw
	echo "Received response from bit.ly."
	return output
    endif
endfunction

" Call is.gd API to shorten a URL.
function! s:call_isgd(url)
    redraw
    echo "Sending request to is.gd..."

    let url = 'http://is.gd/api.php?longurl='.s:url_encode(a:url)
    let [error, output] = s:run_curl(url, '', s:get_proxy(), s:get_proxy_login(), {})

    if error != ''
	call s:errormsg("Error calling is.gd API: ".error)
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
    let key = s:get_urlborg_key()
    redraw
    echo "Sending request to urlBorg..."

    let url = 'http://urlborg.com/api/'.key.'/create_or_reuse/'.s:url_encode(a:url)
    let [error, output] = s:run_curl(url, '', s:get_proxy(), s:get_proxy_login(), {})

    if error != ''
	call s:errormsg("Error calling urlBorg API: ".error)
	return ""
    else
	let matchres = matchlist(output, '^http')
	if matchres == []
	    call s:errormsg("urlBorg error: ".output)
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
	call s:warnmsg("No URL provided.")
	return
    endif

    let shorturl = call(function("s:".a:shortfn), [url])
    if shorturl != ""
	if a:tweetmode == "cmdline"
	    call s:CmdLine_Twitter(shorturl." ", 0)
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

" Parse and format search results from Twitter Search API.
function! s:show_summize(searchres)
    let text = []
    let matchcount = 1

    " Index of first status will be 3 to match line numbers in timeline display.
    let s:statuses = [0, 0, 0]

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

	" Parse and save the status ID.
	let status = substitute(s:xml_get_element(item, 'id'), '^.*:', '', '')
	call add(s:statuses, status)

	call add(text, sender.": ".s:convert_entity(title).' |'.pubdate.'|')

	let matchcount += 1
    endwhile
    call s:twitter_wintext(text)
endfunction

" Query Twitter Search API and retrieve results
function! s:get_summize(query)
    redraw
    echo "Sending search request to Twitter Search..."

    let url = 'http://search.twitter.com/search.atom?rpp=25&q='.s:url_encode(a:query)
    let [error, output] = s:run_curl(url, '', s:get_proxy(), s:get_proxy_login(), {})

    if error != ''
	call s:errormsg("Error querying Twitter Search: ".error)
	return
    endif

    call s:show_summize(output)
    let s:twit_buftype = "summize"
    redraw
    echo "Received search results from Twitter Search."
endfunction

" Prompt user for Twitter Search query string if not entered on command line.
function! s:Summize(query)
    let query = a:query

    " Prompt the user to enter a query if not provided on :SearchTwitter
    " command line.
    if query == ""
	call inputsave()
	let query = input("Search Twitter: ")
	call inputrestore()
    endif

    if query == ""
	call s:warnmsg("No query provided for Twitter Search.")
	return
    endif

    call s:get_summize(query)
endfunction

if !exists(":Summize")
    command -nargs=? Summize :call <SID>Summize(<q-args>)
endif
if !exists(":SearchTwitter")
    command -nargs=? SearchTwitter :call <SID>Summize(<q-args>)
endif

let &cpo = s:save_cpo
finish

" vim:set tw=0:
doc/twitvim.txt	[[[1
921
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
	   twitvim_api_root.......................: |twitvim_api_root|
	   twitvim-identi.ca......................: |twitvim-identi.ca|
	3.1. Base64-Encoded Login.................: |TwitVim-login-base64|
	     twitvim_login_b64....................: |twitvim_login_b64|
	     twitvim_proxy_login_b64..............: |twitvim_proxy_login_b64|
	3.2. Alternatives to cURL.................: |TwitVim-non-cURL|
	     twitvim_enable_perl..................: |twitvim_enable_perl|
	     twitvim_enable_python................: |twitvim_enable_python|
	     twitvim_enable_ruby..................: |twitvim_enable_ruby|
	     twitvim_enable_tcl...................: |twitvim_enable_tcl|
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
	     Reply Feature........................: |TwitVim-reply|
	     Alt-R................................: |TwitVim-A-r|
	     <Leader>r............................: |TwitVim-Leader-r|
	     Retweet Feature......................: |TwitVim-retweet|
	     <Leader>R............................: |TwitVim-Leader-S-r|
	     twitvim_retweet_format...............: |twitvim_retweet_format|
	     Direct Message Feature...............: |TwitVim-direct-message|
	     Alt-D................................: |TwitVim-A-d|
	     <Leader>d............................: |TwitVim-Leader-d|
	     Goto Feature.........................: |TwitVim-goto|
	     Alt-G................................: |TwitVim-A-g|
	     <Leader>g............................: |TwitVim-Leader-g|
	     twitvim_browser_cmd..................: |twitvim_browser_cmd|
	     LongURL Feature......................: |TwitVim-LongURL|
	     <Leader>e............................: |TwitVim-Leader-e|
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
	     :SearchTwitter.......................: |:SearchTwitter|
	5. Timeline Highlighting..................: |TwitVim-highlight|
	   twitterUser............................: |hl-twitterUser|
	   twitterTime............................: |hl-twitterTime|
	   twitterTitle...........................: |hl-twitterTitle|
	   twitterLink............................: |hl-twitterLink|
	   twitterReply...........................: |hl-twitterReply|
	6. Tips and Tricks........................: |TwitVim-tips|
	6.1. Timeline Hotkeys.....................: |TwitVim-hotkeys|
	6.2. Switching between services...........: |TwitVim-switch|
	6.3. Line length in status line...........: |TwitVim-line-length|
	7. History................................: |TwitVim-history|
	8. Credits................................: |TwitVim-credits|


==============================================================================
2. Introduction						*TwitVim-intro*

	TwitVim is a plugin that allows you to post to Twitter, a
	microblogging service at http://www.twitter.com.

	Since version 0.2.19, TwitVim also supports other microblogging
	services, such as identi.ca, that offer Twitter-compatible APIs. See
	|twitvim_api_root| for information on configuring TwitVim for those
	services.


==============================================================================
3. Installation						*TwitVim-install*

	1. Install cURL.				*TwitVim-cURL*

	If you don't already have cURL on your system, download it from
	http://curl.haxx.se/. Make sure that the curl executable is in a
	directory listed in your PATH environment variable, or the equivalent
	for your system.

	If you have the Perl, Python, Ruby, or Tcl interfaces, you may use one
	of those instead of installing cURL. See |TwitVim-non-cURL| for
	setup details.


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


	6. twitvim_api_root				*twitvim_api_root*

	This setting allows you to configure TwitVim to communicate with
	servers other than twitter.com that implement a Twitter-compatible
	API.

							*twitvim-identi.ca*
	For instance, to use identi.ca instead of Twitter, add this to your
	vimrc:

		let twitvim_api_root = "http://identi.ca/api"
	
	A server implementing a Twitter-compatible API may not support all of
	Twitter's features, so some TwitVim commands may not work.


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


------------------------------------------------------------------------------
3.2. Alternatives to cURL				*TwitVim-non-cURL*

	TwitVim supports http networking through Vim's Perl, Python, Ruby, and
	Tcl interfaces, so if you have any of those interfaces compiled into
	your Vim program, you can use that instead of cURL.
	
	Generally, it is slightly faster to use one of those scripting
	interfaces for networking because it avoids running an external
	program. On Windows, it also avoids a brief taskbar flash when cURL
	runs.

	To find out if you have those interfaces, use the |:version| command
	and check the |+feature-list|. Then to enable this special http
	networking code in TwitVim, add one of the following lines to your
	vimrc:

		let twitvim_enable_perl = 1
		let twitvim_enable_python = 1
		let twitvim_enable_ruby = 1
		let twitvim_enable_tcl = 1

	You can enable more than one scripting language but TwitVim will only
	use the first one it finds.


	1. Perl interface				*twitvim_enable_perl*

	To enable TwitVim's Perl networking code, add the following to your
	vimrc:

		let twitvim_enable_perl = 1

	TwitVim requires the MIME::Base64 and LWP::UserAgent modules. If you
	have ActivePerl, these modules are included in the default
	installation.


	2. Python interface				*twitvim_enable_python*

	To enable TwitVim's Python networking code, add the following to your
	vimrc:

		let twitvim_enable_python = 1

	TwitVim requires the urllib, urllib2, and base64 modules. These
	modules are in the Python standard library.


	3. Ruby interface				*twitvim_enable_ruby*

	To enable TwitVim's Ruby networking code, add the following to your
	vimrc:

		let twitvim_enable_ruby = 1

	TwitVim requires the net/http, uri, and Base64 modules. These modules
	are in the Ruby standard library.

	In addition, TwitVim requires a Vim patch to fix an if_ruby networking
	problem. See the following message:

	http://www.mail-archive.com/vim_dev@googlegroups.com/msg03693.html

	and also Bram's correction to the patch:

	http://www.mail-archive.com/vim_dev@googlegroups.com/msg03713.html


	3. Tcl interface				*twitvim_enable_tcl*

	To enable TwitVim's Tcl networking code, add the following to your
	vimrc:

		let twitvim_enable_tcl = 1

	TwitVim requires the http, uri, and base64 modules. uri and base64 are
	in the Tcllib library so you may need to install that. See
	http://tcllib.sourceforge.net/

	If you have ActiveTcl 8.5, the default installation does not include
	Tcllib. Run the following command from the shell to add Tcllib:

		teacup install tcllib


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
	Ctrl-T						*TwitVim-C-t*

	In visual mode, Alt-T posts the highlighted text to Twitter.

	Ctrl-T is an alternative to the Alt-T mapping. If the menu bar is
	enabled, Alt-T pulls down the Tools menu. So use Ctrl-T instead.


							*TwitVim-reply*
	Alt-R						*TwitVim-A-r*
	<Leader>r					*TwitVim-Leader-r*

	This mapping is local to the timeline buffer. In the timeline buffer,
	it starts composing an @-reply on the command line to the author of
	the tweet on the current line.

	Under Cygwin, Alt-R is not recognized so you can use <Leader>r as an
	alternative. The <Leader> character defaults to \ (backslash) but see
	|mapleader| for information on customizing that.


							*TwitVim-retweet*	
	<Leader>R					*TwitVim-Leader-S-r*

	This mapping (Note: uppercase 'R' instead of 'r'.) is local to the
	timeline buffer. It is similar to the retweet feature in popular
	Twitter clients. In the timeline buffer, it sends the current line to
	the command line so that you can repost this line as a new tweet.

						    *twitvim_retweet_format*
	By default, TwitVim retweets tweets in the following format:

		Retweeting @user: text of the tweet

	You can customize the retweet format by adding the following to your
	vimrc, for example:

		let twitvim_retweet_format = 'Retweet from %s: %t'

	or:

		let twitvim_retweet_format = '%t (retweeted from %s)'

	When you retweet a tweet, TwitVim will replace "%s" in
	twitvim_retweet_format with the user name of the original poster and
	"%t" with the text of the tweet.

	The default setting of twitvim_retweet_format is "Retweeting %s: %t"


							*TwitVim-direct-message*
	Alt-D						*TwitVim-A-d*
	<Leader>d					*TwitVim-Leader-d*

	This mapping is local to the timeline buffer. In the timeline buffer,
	it starts composing a direct message on the command line to the author
	of the tweet on the current line.

	Under Cygwin, Alt-D is not recognized so you can use <Leader>d as an
	alternative. The <Leader> character defaults to \ (backslash) but see
	|mapleader| for information on customizing that.


							*TwitVim-goto*
	Alt-G						*TwitVim-A-g*
	<Leader>g					*TwitVim-Leader-g*

	This mapping is local to the timeline buffer. It launches the web
	browser with the URL at the cursor position. If you visually select
	text before invoking this mapping, it launches the web browser with
	the selected text as is.

	As a special case, if the cursor is on a word of the form @user or
	user:, TwitVim will display that user's timeline in the timeline
	buffer. This will not launch the web browser.

	In addition, if the cursor is on a word of the form #hashtag, TwitVim
	will do a Twitter Search for that #hashtag. This too will not launch
	the web browser.

							*twitvim_browser_cmd*
	Before using this command, you need to tell TwitVim how to launch your
	browser. For example, you can add the following to your vimrc:

		let twitvim_browser_cmd = 'firefox.exe'

	Of course, replace firefox.exe with the browser of your choice.


							*TwitVim-LongURL*
	<Leader>e					*TwitVim-Leader-e*

	This mapping is local to the timeline buffer. It calls the LongURL API
	(see http://longurl.org/) to expand the short URL at the cursor
	position. A short URL is a URL from a URL shortening service such as
	TinyURL, SnipURL, etc. Use this feature if you wish to preview a URL
	before browsing to it with |TwitVim-goto|.
	
	If you visually select text before invoking this mapping, it calls the
	LongURL API with the selected text as is.

	If successful, TwitVim will display the result from LongURL in the
	message area.


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


	:SearchTwitter					*:SearchTwitter*
	:SearchTwitter {query}
	
	This command calls the Twitter Search API to search for {query}. If
	{query} is not provided on the command line, the command will prompt
	you for it. Search results are then displayed in the timeline buffer.

	All of the Twitter Search operators are supported implicitly. See
	http://search.twitter.com/operators for a list of search operators.


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

	Link URLs and #hashtags in a Twitter status.

	twitterReply					*hl-twitterReply*

	@-reply in a Twitter status.


==============================================================================
6. Tips and Tricks					*TwitVim-tips*

	Here are a few tips for using TwitVim more efficiently.


------------------------------------------------------------------------------
6.1. Timeline Hotkeys					*TwitVim-hotkeys*

	TwitVim does not autorefresh. However, you can make refreshing your
	timeline easier by mapping keys to the timeline commands. For example,
	I use the <F8> key for that:

		nnoremap <F8> :FriendsTwitter<cr>
		nnoremap <S-F8> :UserTwitter<cr>
		nnoremap <A-F8> :RepliesTwitter<cr>
		nnoremap <C-F8> :DMTwitter<cr>


------------------------------------------------------------------------------
6.2. Switching between services				*TwitVim-switch*

	I have user accounts on both Twitter and identi.ca. Here is what I
	added to my vimrc to make it easy to switch between the two services
	within the same TwitVim session:

		function! Switch_to_twitter()
		    let g:twitvim_api_root = "http://twitter.com"

		    let g:twitvim_login_b64 = "Twitter Base64 login"

		    FriendsTwitter
		endfunction

		function! Switch_to_identica()
		    let g:twitvim_api_root = "http://identi.ca/api"

		    let g:twitvim_login_b64 = "identi.ca Base64 login"

		    FriendsTwitter
		endfunction

		command! ToTwitter :call Switch_to_twitter()
		command! ToIdentica :call Switch_to_identica()
	
	With that in place, I can use :ToTwitter and :ToIdentica to switch
	between services. I added a call to FriendsTwitter at the end of each
	function so that I'll have a fresh timeline view after switching. You
	may also use this technique to switch between different user accounts
	on the same service.


------------------------------------------------------------------------------
6.3. Line length in status line				*TwitVim-line-length*

	Add the following to your |'statusline'| to display the length of the
	current line:

		%{strlen(getline('.'))}
	
	This is useful if you compose tweets in a separate buffer and post
	them with |:CPosttoTwitter|. With the line length in your status line,
	you will know when you've reached the 140-character boundary.

==============================================================================
7. TwitVim History					*TwitVim-history*

	0.3.1 : 2008-09-18 * Added support for LongURL.
			   * Added support for posting multibyte/Unicode
			     tweets in cURL mode.
			   * Remove newlines from text before retweeting.
	0.3.0 : 2008-09-12 * Added support for http networking through Vim's
			     Perl, Python, Ruby, and Tcl interfaces, as
			     alternatives to cURL.
			   * Removed UrlTea support.
	0.2.24 : 2008-08-28 * Added retweet feature. See |TwitVim-retweet|
	0.2.23 : 2008-08-25 * Support in_reply_to_status_id parameter.
			    * Added tip on line length in statusline.
			    * Report browser launch errors.
			    * Set syntax highlighting on every timeline refresh.
	0.2.22 : 2008-08-13 * Rewrote time conversion code in Vim script
			      so we don't need Perl or Python any more.
			    * Do not URL-encode digits 0 to 9.
	0.2.21 : 2008-08-12 * Added tips section to documentation.
			    * Use create_or_reuse instead of create in UrlBorg
			      API so that it will always generate the same
			      short URL for the same long URL.
			    * Added support for highlighting #hashtags and
			      jumping to Twitter Searches for #hashtags.
			    * Added Python code to convert Twitter timestamps
			      to local time and simplify them.
	0.2.20 : 2008-07-24 * Switched from Summize to Twitter Search.
	0.2.19 : 2008-07-23 * Added support for non-Twitter servers
			      implementing the Twitter API. This is for
			      identi.ca support. See |twitvim-identi.ca|.
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
8. TwitVim Credits					*TwitVim-credits*

	Thanks to Travis Jeffery, the author of the original VimTwitter script
	(vimscript #2124), who came up with the idea of running cURL from Vim
	to access the Twitter API.

	Techniques for managing the Twitter buffer were adapted from the NERD
	Tree plugin (vimscript #1658) by Marty Grenfell.


==============================================================================
vim:tw=78:ts=8:ft=help:norl:
