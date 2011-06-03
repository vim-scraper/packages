" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
plugin/twitvim.vim	[[[1
3400
" ==============================================================
" TwitVim - Post to Twitter from Vim
" Based on Twitter Vim script by Travis Jeffery <eatsleepgolf@gmail.com>
"
" Version: 0.5.5
" License: Vim license. See :help license
" Language: Vim script
" Maintainer: Po Shan Cheah <morton@mortonfox.com>
" Created: March 28, 2008
" Last updated: August 16, 2010
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

" Twitter character limit. Twitter used to accept tweets up to 246 characters
" in length and display those in truncated form, but that is no longer the
" case. So 140 is now the hard limit.
let s:char_limit = 140

" Allow the user to override the API root, e.g. for identi.ca, which offers a
" Twitter-compatible API.
function! s:get_api_root()
    return exists('g:twitvim_api_root') ? g:twitvim_api_root : "http://api.twitter.com/1"
endfunction

" Allow user to set the format for retweets.
function! s:get_retweet_fmt()
    return exists('g:twitvim_retweet_format') ? g:twitvim_retweet_format : "RT %s: %t"
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

" Get twitvim_count, if it exists. This will be the number of tweets returned
" by :FriendsTwitter, :UserTwitter, and :SearchTwitter.
function! s:get_count()
    if exists('g:twitvim_count')
	if g:twitvim_count < 1
	    return 1
	elseif g:twitvim_count > 200
	    return 200
	else
	    return g:twitvim_count
	endif
    endif
    return 0
endfunction

" User setting to show/hide header in the buffer. Default: show header.
function! s:get_show_header()
    return exists('g:twitvim_show_header') ? g:twitvim_show_header : 1
endfunction

" User config for name of OAuth access token file.
function! s:get_token_file()
    return exists('g:twitvim_token_file') ? g:twitvim_token_file : $HOME . "/.twitvim.token"
endfunction

" User config to disable the OAuth access token file.
function! s:get_disable_token_file()
    return exists('g:twitvim_disable_token_file') ? g:twitvim_disable_token_file : 0
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

" Get Twitter login info from twitvim_login in vimrc.
" Format is username:password
" If twitvim_login_b64 exists, use that instead. This is the user:password
" in base64 encoding.
"
" This function is for services with Twitter-compatible APIs that use Basic
" authentication, e.g. identi.ca
function! s:get_twitvim_login_noerror()
    if exists('g:twitvim_login_b64') && g:twitvim_login_b64 != ''
	return g:twitvim_login_b64
    elseif exists('g:twitvim_login') && g:twitvim_login != ''
	return g:twitvim_login
    else
	return ''
    endif
endfunction

" Dummy login string to force OAuth signing in run_curl_oauth().
let s:ologin = "oauth:oauth"

" Reset login info.
function! s:reset_twitvim_login()
    let s:access_token = ""
    let s:access_token_secret = ""
    call delete(s:get_token_file())

    let s:cached_username = ""
endfunction

" Verify user credentials. This function is actually used to do an OAuth
" handshake after deleting the access token.
"
" Returns 1 if login succeeded, 0 if login failed, <0 for other errors.
function! s:check_twitvim_login()
    redraw
    echo "Logging into Twitter..."

    let url = s:get_api_root()."/account/verify_credentials.xml"
    let [error, output] = s:run_curl_oauth(url, s:ologin, s:get_proxy(), s:get_proxy_login(), {})
    if error =~ '401'
	return 0
    endif

    if error != ''
	let errormsg = s:xml_get_element(output, 'error')
	call s:errormsg("Error logging into Twitter: ".(errormsg != '' ? errormsg : error))
	return -1
    endif

    " The following check should not be required because Twitter is supposed to
    " return a 401 HTTP status on login failure, but you never know with
    " Twitter.
    let error = s:xml_get_element(output, 'error')
    if error =~ '\ccould not authenticate'
	return 0
    endif

    if error != ''
	call s:errormsg("Error logging into Twitter: ".error)
	return -1
    endif

    redraw
    echo "Twitter login succeeded."

    return 1
endfunction

" Throw away OAuth access tokens and log in again. This is meant to allow the
" user to switch to a different Twitter account.
function! s:prompt_twitvim_login()
    call s:reset_twitvim_login()
    call s:check_twitvim_login()
endfunction

let s:cached_login = ''
let s:cached_username = ''

" See if we can save time by using the cached username.
function! s:get_twitvim_cached_username()
    if s:get_api_root() =~ 'twitter\.com'
	if s:cached_username == ''
	    return ''
	endif
    else
	" In Twitter-compatible services that use Basic authentication, the
	" user may have changed the login info on the fly. So we have to watch
	" out for that.
	let login = s:get_twitvim_login_noerror()
	if login == '' || login != s:cached_login
	    return ''
	endif
    endif
    return s:cached_username
endfunction

" Get Twitter user name by verifying login credentials
function! s:get_twitvim_username()
    " If we already have the info, no need to get it again.
    let username = s:get_twitvim_cached_username()
    if username != ''
	return username
    endif

    redraw
    echo "Verifying login credentials with Twitter..."

    let url = s:get_api_root()."/account/verify_credentials.xml"
    let [error, output] = s:run_curl_oauth(url, s:ologin, s:get_proxy(), s:get_proxy_login(), {})
    if error != ''
	let errormsg = s:xml_get_element(output, 'error')
	call s:errormsg("Error verifying login credentials: ".(errormsg != '' ? errormsg : error))
	return
    endif

    redraw
    echo "Twitter login credentials verified."

    let username = s:xml_get_element(output, 'screen_name')

    " Save it so we don't have to do it again unless the user switches to
    " a different login.
    let s:cached_username = username
    let s:cached_login = s:get_twitvim_login_noerror()

    return username
endfunction

" If set, twitvim_cert_insecure turns off certificate verification if using
" https Twitter API over cURL or Ruby.
function! s:get_twitvim_cert_insecure()
    return exists('g:twitvim_cert_insecure') ? g:twitvim_cert_insecure : 0
endfunction

" === XML helper functions ===

" Get the content of the n'th element in a series of elements.
function! s:xml_get_nth(xmlstr, elem, n)
    let matchres = matchlist(a:xmlstr, '<'.a:elem.'\%( [^>]*\)\?>\(.\{-}\)</'.a:elem.'>', -1, a:n)
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

" Get the attributes of the n'th element in a series of elements.
function! s:xml_get_attr_nth(xmlstr, elem, n)
    let matchres = matchlist(a:xmlstr, '<'.a:elem.'\s\+\([^>]*\)>', -1, a:n)
    if matchres == []
	return {}
    endif

    let matchcount = 1
    let attrstr = matchres[1]
    let attrs = {}

    while 1
	let matchres = matchlist(attrstr, '\(\w\+\)="\([^"]*\)"', -1, matchcount)
	if matchres == []
	    break
	endif

	let attrs[matchres[1]] = matchres[2]
	let matchcount += 1
    endwhile

    return attrs
endfunction

" Get attributes of the specified element.
function! s:xml_get_attr(xmlstr, elem)
    return s:xml_get_attr_nth(a:xmlstr, a:elem, 1)
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

    " This timestamp format is used by Twitter Rate Limit.
    let matchres = matchlist(a:str, '^\(\d\+\)-\(\d\+\)-\(\d\+\)T\(\d\+\):\(\d\+\):\(\d\+\)+00:00$')
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

" === OAuth code ===

" Check if we can use Perl for HMAC-SHA1 digests.
function! s:check_perl_hmac()
    let can_perl = 1
    perl <<EOF
eval {
    require Digest::HMAC_SHA1;
    Digest::HMAC_SHA1->import;
};
if ($@) {
    VIM::DoCommand('let can_perl = 0');
}
EOF
    return can_perl
endfunction

" Compute HMAC-SHA1 digest. (Perl version)
function! s:perl_hmac_sha1_digest(key, str)
    perl <<EOF
require Digest::HMAC_SHA1;
Digest::HMAC_SHA1->import;

my $key = VIM::Eval('a:key');
my $str = VIM::Eval('a:str');

my $hmac = Digest::HMAC_SHA1->new($key);

$hmac->add($str);
my $signature = $hmac->b64digest; # Length of 27

VIM::DoCommand("let signature = '$signature'");
EOF

    return signature
endfunction

" Check if we can use Python for HMAC-SHA1 digests.
function! s:check_python_hmac()
    let can_python = 1
    python <<EOF
import vim
try:
    import base64
    import hashlib
    import hmac
except:
    vim.command('let can_python = 0')
EOF
    return can_python
endfunction

" Compute HMAC-SHA1 digest. (Python version)
function! s:python_hmac_sha1_digest(key, str)
    python <<EOF
import base64
import hashlib
import hmac
import vim

key = vim.eval("a:key")
mstr = vim.eval("a:str")

digest = hmac.new(key, mstr, hashlib.sha1).digest()
signature = base64.encodestring(digest)[0:-1]

vim.command("let signature='%s'" % signature)
EOF
    return signature
endfunction

" Check if we can use Ruby for HMAC-SHA1 digests.
function! s:check_ruby_hmac()
    let can_ruby = 1
    ruby <<EOF
begin
    require 'openssl'
    require 'base64'
rescue LoadError
    VIM.command('let can_ruby = 0')
end
EOF
    return can_ruby
endfunction

" Compute HMAC-SHA1 digest. (Ruby version)
function! s:ruby_hmac_sha1_digest(key, str)
    ruby <<EOF
require 'openssl'
require 'base64'

key = VIM.evaluate('a:key')
str = VIM.evaluate('a:str')

digest = OpenSSL::HMAC.digest(OpenSSL::Digest::Digest.new('sha1'), key, str)
signature = Base64.encode64(digest).chomp

VIM.command("let signature='#{signature}'")
EOF
    return signature
endfunction

" Check if we can use Tcl for HMAC-SHA1 digests.
function! s:check_tcl_hmac()
    let can_tcl = 1
    tcl <<EOF
if [catch {
    package require sha1
    package require base64
} result] {
    ::vim::command "let can_tcl = 0"
}
EOF
    return can_tcl
endfunction

" Compute HMAC-SHA1 digest. (Tcl version)
function! s:tcl_hmac_sha1_digest(key, str)
    tcl <<EOF
package require sha1
package require base64

set key [::vim::expr a:key]
set str [::vim::expr a:str]

set signature [base64::encode [sha1::hmac -bin $key $str]]

::vim::command "let signature = '$signature'"
EOF
    return signature
endfunction

" Compute HMAC-SHA1 digest by running openssl command line utility.
function! s:openssl_hmac_sha1_digest(key, str)
    let output = system('openssl dgst -binary -sha1 -hmac "'.a:key.'" | openssl base64', a:str)
    if v:shell_error != 0
	call s:errormsg("Error running openssl command: ".output)
	return ""
    endif

    " Remove trailing newlines.
    let output = substitute(output, '\n\+$', '', '')

    return output
endfunction

" Find out which method we can use to compute a HMAC-SHA1 digest.
function! s:get_hmac_method()
    if !exists('s:hmac_method')
	let s:hmac_method = 'openssl'
	if s:get_enable_perl() && has('perl') && s:check_perl_hmac()
	    let s:hmac_method = 'perl'
	elseif s:get_enable_python() && has('python') && s:check_python_hmac()
	    let s:hmac_method = 'python'
	elseif s:get_enable_ruby() && has('ruby') && s:check_ruby_hmac()
	    let s:hmac_method = 'ruby'
	elseif s:get_enable_tcl() && has('tcl') && s:check_tcl_hmac()
	    let s:hmac_method = 'tcl'
	endif
    endif
    return s:hmac_method
endfunction

function! s:hmac_sha1_digest(key, str)
    return s:{s:get_hmac_method()}_hmac_sha1_digest(a:key, a:str)
endfunction

function! s:reset_hmac_method()
    unlet! s:hmac_method
endfunction

function! s:show_hmac_method()
    echo 'Hmac Method:' s:get_hmac_method()
endfunction

" For debugging. Reset Hmac method.
if !exists(":TwitVimResetHmacMethod")
    command TwitVimResetHmacMethod :call <SID>reset_hmac_method()
endif

" For debugging. Show current Hmac method.
if !exists(":TwitVimShowHmacMethod")
    command TwitVimShowHmacMethod :call <SID>show_hmac_method()
endif


let s:gc_consumer_key = "HyshEU8SbcsklPQ6ouF0g"
let s:gc_consumer_secret = "U1uvxLjZxlQAasy9Kr5L2YAFnsvYTOqx1bk7uJuezQ"

let s:gc_req_url = "http://api.twitter.com/oauth/request_token"
let s:gc_access_url = "http://api.twitter.com/oauth/access_token"
let s:gc_authorize_url = "http://api.twitter.com/oauth/authorize"

" Simple nonce value generator. This needs to be randomized better.
function s:nonce()
    if !exists("s:nonce_val") || s:nonce_val < 1
	let s:nonce_val = localtime() + 109
    endif

    let retval = s:nonce_val
    let s:nonce_val += 109

    return retval
endfunction

" Split a URL into base and params.
function s:split_url(url)
    let urlarray = split(a:url, '?')
    let baseurl = urlarray[0]
    let parms = {}
    if len(urlarray) > 1
	for pstr in split(urlarray[1], '&')
	    let [key, value] = split(pstr, '=')
	    let parms[key] = value
	endfor
    endif
    return [baseurl, parms]
endfunction

" Produce signed content using the parameters provided via parms using the
" chosen method, url and provided token secret. Note that in the case of
" getting a new Request token, the secret will be ""
function s:getOauthResponse(url, method, parms, token_secret)
    let parms = copy(a:parms)

    " Add some constants to hash
    let parms["oauth_consumer_key"] = s:gc_consumer_key
    let parms["oauth_signature_method"] = "HMAC-SHA1"
    let parms["oauth_version"] = "1.0"

    " Get the timestamp and add to hash
    let parms["oauth_timestamp"] = localtime()

    let parms["oauth_nonce"] = s:nonce()

    let [baseurl, urlparms] = s:split_url(a:url)
    call extend(parms, urlparms)

    " Alphabetically sort by key and form a string that has
    " the format key1=value1&key2=value2&...
    " Must UTF8 encode and then URL encode the values.
    let content = ""

    for key in sort(keys(parms))
	let value = s:url_encode(parms[key])
	let content .= key . "=" . value . "&"
    endfor
    let content = content[0:-2]

    " Form the signature base string which is comprised of 3
    " pieces, with each piece URL encoded.
    " [METHOD_UPPER_CASE]&[url]&content
    let signature_base_str = a:method . "&" . s:url_encode(baseurl) . "&" . s:url_encode(content)
    let hmac_sha1_key = s:url_encode(s:gc_consumer_secret) . "&" . s:url_encode(a:token_secret)
    let signature = s:hmac_sha1_digest(hmac_sha1_key, signature_base_str)

    " Add padding character to make a multiple of 4 per the
    " requirement of OAuth.
    if strlen(signature) % 4
	let signature .= "="
    endif

    let content = "OAuth "

    for key in keys(parms)
	if key =~ "oauth"
	    let value = s:url_encode(parms[key])
	    let content .= key . '="' . value . '", '
	endif
    endfor
    let content .= 'oauth_signature="' . s:url_encode(signature) . '"'
    return content
endfunction

" Perform the OAuth dance to authorize this client with Twitter.
function! s:do_oauth()
    " Call oauth/request_token to get request token from Twitter.

    let parms = { "oauth_callback": "oob", "dummy" : "1" }
    let oauth_hdr = s:getOauthResponse(s:gc_req_url, "POST", parms, "")

    let [error, output] = s:run_curl(s:gc_req_url, oauth_hdr, s:get_proxy(), s:get_proxy_login(), { "dummy" : "1" })

    if error != ''
	call s:errormsg("Error from oauth/request_token: ".error)
	return [-1, '', '']
    endif

    let matchres = matchlist(output, 'oauth_token=\([^&]\+\)&')
    if matchres != []
	let request_token = matchres[1]
    endif

    let matchres = matchlist(output, 'oauth_token_secret=\([^&]\+\)&')
    if matchres != []
	let token_secret = matchres[1]
    endif

    " Launch web browser to let user allow or deny the authentication request.
    let auth_url = s:gc_authorize_url . "?oauth_token=" . request_token

    " If user has not set up twitvim_browser_cmd, just display the
    " authentication URL and ask the user to visit that URL.
    if !exists('g:twitvim_browser_cmd') || g:twitvim_browser_cmd == ''

	" Attempt to shorten the auth URL.
	let newurl = s:call_isgd(auth_url)
	if newurl != ""
	    let auth_url = newurl
	else
	    let newurl = s:call_bitly(auth_url)
	    if newurl != ""
		let auth_url = newurl
	    endif
	endif

	echo "Visit the following URL in your browser to authenticate TwitVim:"
	echo auth_url
    else
	if s:launch_browser(auth_url) < 0
	    return [-2, '', '']
	endif
    endif

    call inputsave()
    let pin = input("Enter Twitter OAuth PIN: ")
    call inputrestore()

    if pin == ""
	call s:warnmsg("No OAuth PIN entered")
	return [-3, '', '']
    endif

    " Call oauth/access_token to swap request token for access token.
    
    let parms = { "dummy" : 1, "oauth_token" : request_token, "oauth_verifier" : pin }
    let oauth_hdr = s:getOauthResponse(s:gc_access_url, "POST", parms, token_secret)

    let [error, output] = s:run_curl(s:gc_access_url, oauth_hdr, s:get_proxy(), s:get_proxy_login(), { "dummy" : 1 })

    if error != ''
	call s:errormsg("Error from oauth/access_token: ".error)
	return [-4, '', '']
    endif

    let matchres = matchlist(output, 'oauth_token=\([^&]\+\)&')
    if matchres != []
	let request_token = matchres[1]
    endif

    let matchres = matchlist(output, 'oauth_token_secret=\([^&]\+\)&')
    if matchres != []
	let token_secret = matchres[1]
    endif

    return [ 0, request_token, token_secret ]
endfunction

" Sign a request with OAuth and send it.
function! s:run_curl_oauth(url, login, proxy, proxylogin, parms)
    if a:login != '' && a:url =~ 'twitter\.com'
	if !exists('s:access_token') || s:access_token == ''

	    let tokens = []

	    if !s:get_disable_token_file() && filereadable(s:get_token_file())
		" Try to read access tokens from token file.
		let tokens = readfile(s:get_token_file(), "t", 3)
	    endif

	    if tokens == []

		" If unsuccessful at reading token file, do the OAuth handshake.
		let [ retval, s:access_token, s:access_token_secret ] = s:do_oauth()
		if retval < 0
		    return [ "Error from do_oauth(): ".retval, '' ]
		endif

		if !s:get_disable_token_file()
		    " Save access tokens to the token file.
		    let v:errmsg = ""
		    if writefile([ s:access_token, s:access_token_secret ], s:get_token_file()) < 0
			call s:errormsg('Error writing token file: '.v:errmsg)
		    endif
		endif
	    else
		let [s:access_token, s:access_token_secret] = tokens
	    endif
	endif

	let parms = copy(a:parms)
	let parms["oauth_token"] = s:access_token
	let oauth_hdr = s:getOauthResponse(a:url, a:parms == {} ? 'GET' : 'POST', parms, s:access_token_secret)

	return s:run_curl(a:url, oauth_hdr, a:proxy, a:proxylogin, a:parms)
    else
	if a:login != ''
	    let login = s:get_twitvim_login_noerror()
	    if login == ''
		return [ 'Login info not set. Please add to vimrc: let twitvim_login="USER:PASS"', '' ]
	    endif
	else
	    let login = a:login
	endif
	return s:run_curl(a:url, login, a:proxy, a:proxylogin, a:parms)
    endif
endfunction

" === End of OAuth code ===

" === Networking code ===

function! s:url_encode_char(c)
    let utf = iconv(a:c, &encoding, "utf-8")
    if utf == ""
	let utf = a:c
    endif
    let s = ""
    for i in range(strlen(utf))
	let s .= printf("%%%02X", char2nr(utf[i]))
    endfor
    return s
endfunction

" URL-encode a string.
function! s:url_encode(str)
    return substitute(a:str, '[^a-zA-Z0-9_.~-]', '\=s:url_encode_char(submatch(0))', 'g')
endfunction

" Use curl to fetch a web page.
function! s:curl_curl(url, login, proxy, proxylogin, parms)
    let error = ""
    let output = ""

    let curlcmd = "curl -s -S "

    if s:get_twitvim_cert_insecure()
	let curlcmd .= "-k "
    endif

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
	if a:login =~ "^OAuth "
	    let curlcmd .= '-H "Authorization: '.a:login.'" '
	elseif stridx(a:login, ':') != -1
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
    let errormsg = s:xml_get_element(output, 'error')
    if v:shell_error != 0
	let error = output
    elseif errormsg != ''
	let error = errormsg
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
	if login[0:6] == "OAuth ":
	    req.add_header('Authorization', login)
	else:
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
    vim.command("let output='%s'" % httperr.read().replace("'", "''"))
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

my $proxy = VIM::Eval('a:proxy');
$proxy ne '' and $ua->proxy('http', "http://$proxy");

my $proxylogin = VIM::Eval('a:proxylogin');
$proxylogin ne '' and $ua->default_header('Proxy-Authorization' => 'Basic '.make_base64($proxylogin));

my %parms = ();
my $keys = VIM::Eval('keys(a:parms)');
for $k (split(/\n/, $keys)) {
    $parms{$k} = VIM::Eval("a:parms['$k']");
}

my $login = VIM::Eval('a:login');
if ($login ne '') {
    if ($login =~ /^OAuth /) {
	$ua->default_header('Authorization' => $login);
	# VIM::Msg($login, "ErrorMsg");
    }
    else {
	$ua->default_header('Authorization' => 'Basic '.make_base64($login));
    }
}

# VIM::Msg($url, "ErrorMsg");
# VIM::Msg(join(' ', keys(%parms)), "ErrorMsg");
my $response = %parms ? $ua->post($url, \%parms) : $ua->get($url);
if ($response->is_success) {
    my $output = $response->content;
    $output =~ s/'/''/g;
    VIM::DoCommand("let output ='$output'");
}
else {
    my $output = $response->content;
    $output =~ s/'/''/g;
    VIM::DoCommand("let output ='$output'");

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
    require 'net/https'
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
require 'net/https'
require 'uri'
require 'Base64'

def make_base64(s)
    s =~ /:/ ? Base64.encode64(s) : s
end

def parse_user_password(s)
    (s =~ /:/ ? s : Base64.decode64(s)).split(':', 2)    
end

url = URI.parse(VIM.evaluate('a:url'))
httpargs = [ url.host, url.port ]

proxy = VIM.evaluate('a:proxy')
if proxy != ''
    prox = URI.parse("http://#{proxy}")
    httpargs += [ prox.host, prox.port ]
end

proxylogin = VIM.evaluate('a:proxylogin')
if proxylogin != ''
    httpargs += parse_user_password(proxylogin)
end

net = Net::HTTP.new(*httpargs)

net.use_ssl = (url.scheme == 'https')

# Disable certificate verification if user sets this variable.
cert_insecure = VIM.evaluate('s:get_twitvim_cert_insecure()')
if cert_insecure != '0'
    net.verify_mode = OpenSSL::SSL::VERIFY_NONE
end

parms = {}
keys = VIM.evaluate('keys(a:parms)')

# Vim patch 7.2.374 adds support to if_ruby for Vim types. So keys() will
# actually return a Ruby array instead of a newline-delimited string.
# So we only need to split the string if VIM.evaluate returns a string.
# If it's already an array, leave it alone.

keys = keys.split(/\n/) if keys.is_a? String

keys.each { |k|
    parms[k] = VIM.evaluate("a:parms['#{k}']")
}

begin
    res = net.start { |http| 
	path = "#{url.path}?#{url.query}"
	if parms == {}
	    req = Net::HTTP::Get.new(path)
	else
	    req = Net::HTTP::Post.new(path)
	    req.set_form_data(parms)
	end

	login = VIM.evaluate('a:login')
	if login != ''
	    if login =~ /^OAuth /
		req.add_field 'Authorization', login
	    else
		req.add_field 'Authorization', "Basic #{make_base64(login)}"
	    end
	end

	#    proxylogin = VIM.evaluate('a:proxylogin')
	#    if proxylogin != ''
	#	req.add_field 'Proxy-Authorization', "Basic #{make_base64(proxylogin)}"
	#    end

	http.request(req)
    }
    case res
    when Net::HTTPSuccess
	output = res.body.gsub("'", "''")
	VIM.command("let output='#{output}'")
    else
	error = "#{res.code} #{res.message}".gsub("'", "''")
	VIM.command("let error='#{error}'")

	output = res.body.gsub("'", "''")
	VIM.command("let output='#{output}'")
    end
rescue => exc
    VIM.command("let error='#{exc.message}'")
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

if {[string tolower [string range $url 0 7]] == "https://"} {
    # Load and register support for https URLs.
    package require tls
    ::http::register https 443 ::tls::socket
}

set headers [list]

::http::config -proxyhost ""
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
    if {[string range $login 0 5] == "OAuth "} {
	lappend headers "Authorization" $login
    } else {
	lappend headers "Authorization" "Basic [make_base64 $login]"
    }
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
    if { [ ::http::ncode $res ] >= 400 } {
	set error $state(http)
	::vim::command "let error = '$error'"
	set output [string map {' ''} $state(body)]
	::vim::command "let output = '$output'"
    } else {
	set output [string map {' ''} $state(body)]
	::vim::command "let output = '$output'"
    }
} else {
    if { [ info exists state(error) ] } {
	set error [string map {' ''} $state(error)]
    } else {
	set error "$state(status) error"
    }
    ::vim::command "let error = '$error'"
}

::http::cleanup $res
EOF

    return [error, output]
endfunction

" Find out which method we can use to fetch a web page.
function! s:get_curl_method()
    if !exists('s:curl_method')
	let s:curl_method = 'curl'
	if s:get_enable_perl() && has('perl') && s:check_perl()
	    let s:curl_method = 'perl'
	elseif s:get_enable_python() && has('python') && s:check_python()
	    let s:curl_method = 'python'
	elseif s:get_enable_ruby() && has('ruby') && s:check_ruby()
	    let s:curl_method = 'ruby'
	elseif s:get_enable_tcl() && has('tcl') && s:check_tcl()
	    let s:curl_method = 'tcl'
	endif
    endif
    return s:curl_method
endfunction

function! s:run_curl(url, login, proxy, proxylogin, parms)
    return s:{s:get_curl_method()}_curl(a:url, a:login, a:proxy, a:proxylogin, a:parms)
endfunction

function! s:reset_curl_method()
    unlet! s:curl_method
endfunction

function! s:show_curl_method()
    echo 'Net Method:' s:get_curl_method()
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

" === Buffer stack code ===

" Each buffer record holds the following fields:
"
" buftype: Buffer type = dmrecv, dmsent, search, public, friends, user, replies, list
" user: For user buffers if other than current user
" list: List slug if displaying a Twitter list.
" page: Keep track of pagination.
" statuses: Tweet IDs. For use by in_reply_to_status_id
" inreplyto: IDs of predecessor messages for @-replies.
" dmids: Direct Message IDs.
" buffer: The buffer text.
" showheader: 1 if header is shown in this buffer, 0 if header is hidden.

let s:curbuffer = {}

let s:bufstack = []

" Maximum items in the buffer stack. Adding a new item after this limit will
" get rid of the first item.
let s:bufstackmax = 10

" Buffer stack pointer. -1 if no items yet. May not point to the end of the
" list if user has gone back one or more buffers.
let s:bufstackptr = -1

" Add current buffer to the buffer stack at the next position after current.
" Remove all buffers after that.
function! s:add_buffer()

    " If stack is already full, remove the buffer at the bottom of the stack to
    " make room.
    if s:bufstackptr >= s:bufstackmax
	call remove(s:bufstack, 0)
	let s:bufstackptr -= 1
    endif

    let s:bufstackptr += 1

    " Suppress errors because there may not be anything to remove after current
    " position.
    silent! call remove(s:bufstack, s:bufstackptr, -1)

    call add(s:bufstack, s:curbuffer)
endfunction

" If current buffer is same type as the buffer at the buffer stack pointer then
" just copy it into the buffer stack. Otherwise, add it to buffer stack.
function! s:save_buffer()
    if s:curbuffer == {}
	return
    endif

    " Save buffer contents and cursor position.
    let twit_bufnr = bufwinnr('^'.s:twit_winname.'$')
    if twit_bufnr > 0
	let curwin = winnr()
	execute twit_bufnr . "wincmd w"
	let s:curbuffer.buffer = getline(1, '$')
	let s:curbuffer.view = winsaveview()
	execute curwin .  "wincmd w"
    else
	let s:curbuffer.view = {}
    endif

    " If current buffer is the same type as buffer at the top of the stack,
    " then just copy it.
    if s:bufstackptr >= 0 && s:curbuffer.buftype == s:bufstack[s:bufstackptr].buftype && s:curbuffer.list == s:bufstack[s:bufstackptr].list && s:curbuffer.user == s:bufstack[s:bufstackptr].user && s:curbuffer.page == s:bufstack[s:bufstackptr].page

	let s:bufstack[s:bufstackptr] = deepcopy(s:curbuffer)
	return
    endif

    " Otherwise, push the current buffer onto the stack.
    call s:add_buffer()
endfunction

" Go back one buffer in the buffer stack.
function! s:back_buffer()
    call s:save_buffer()

    if s:bufstackptr < 1
	call s:warnmsg("Already at oldest buffer. Can't go back further.")
	return -1
    endif

    let s:bufstackptr -= 1
    let s:curbuffer = deepcopy(s:bufstack[s:bufstackptr])

    call s:twitter_wintext_view(s:curbuffer.buffer, "timeline", s:curbuffer.view)
    return 0
endfunction

" Go forward one buffer in the buffer stack.
function! s:fwd_buffer()
    call s:save_buffer()

    if s:bufstackptr + 1 >= len(s:bufstack)
	call s:warnmsg("Already at newest buffer. Can't go forward.")
	return -1
    endif

    let s:bufstackptr += 1
    let s:curbuffer = deepcopy(s:bufstack[s:bufstackptr])

    call s:twitter_wintext_view(s:curbuffer.buffer, "timeline", s:curbuffer.view)
    return 0
endfunction

if !exists(":BackTwitter")
    command BackTwitter :call <SID>back_buffer()
endif
if !exists(":ForwardTwitter")
    command ForwardTwitter :call <SID>fwd_buffer()
endif

" For debugging. Show the buffer stack.
function! s:show_bufstack()
    for i in range(len(s:bufstack) - 1, 0, -1)
	echo i.':' 'type='.s:bufstack[i].buftype 'user='.s:bufstack[i].user 'page='.s:bufstack[i].page
    endfor
endfunction

if !exists(":TwitVimShowBufstack")
    command TwitVimShowBufstack :call <SID>show_bufstack()
endif

" For debugging. Show curbuffer variable.
if !exists(":TwitVimShowCurbuffer")
    command TwitVimShowCurbuffer :echo s:curbuffer
endif

" === End of buffer stack code ===

" Add update to Twitter buffer if public, friends, or user timeline.
function! s:add_update(output)
    if has_key(s:curbuffer, 'buftype') && (s:curbuffer.buftype == "public" || s:curbuffer.buftype == "friends" || s:curbuffer.buftype == "user" || s:curbuffer.buftype == "replies" || s:curbuffer.buftype == "list" || s:curbuffer.buftype == "retweeted_by_me" || s:curbuffer.buftype == "retweeted_to_me")

	" Parse the output from the Twitter update call.
	let line = s:format_status_xml(a:output)

	" Line number where new tweet will be inserted. It should be 3 if
	" header is shown and 1 if header is hidden.
	let insline = s:curbuffer.showheader ? 3 : 1

	" Add the status ID to the current buffer's statuses list.
	call insert(s:curbuffer.statuses, s:xml_get_element(a:output, 'id'), insline)

	" Add in-reply-to ID to current buffer's in-reply-to list.
	call insert(s:curbuffer.inreplyto, s:xml_get_element(a:output, 'in_reply_to_status_id'), insline)

	let twit_bufnr = bufwinnr('^'.s:twit_winname.'$')
	if twit_bufnr > 0
	    let curwin = winnr()
	    execute twit_bufnr . "wincmd w"
	    setlocal modifiable
	    call append(insline - 1, line)
	    execute "normal! ".insline."G"
	    setlocal nomodifiable
	    let s:curbuffer.buffer = getline(1, '$')
	    execute curwin .  "wincmd w"
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

	let url = s:get_api_root()."/statuses/update.xml"
	let parms["status"] = mesg
	let parms["source"] = "twitvim"

	let [error, output] = s:run_curl_oauth(url, s:ologin, s:get_proxy(), s:get_proxy_login(), parms)

	if error != ''
	    let errormsg = s:xml_get_element(output, 'error')
	    call s:errormsg("Error posting your tweet: ".(errormsg != '' ? errormsg : error))
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
    call inputsave()
    redraw
    let mesg = input("Your Twitter: ", a:initstr)
    call inputrestore()
    call s:post_twitter(mesg, a:inreplyto)
endfunction

" Extract the user name from a line in the timeline.
function! s:get_user_name(line)
    let line = substitute(a:line, '^+ ', '', '')
    let matchres = matchlist(line, '^\(\w\+\):')
    return matchres != [] ? matchres[1] : ""
endfunction

" This is for a local mapping in the timeline. Start an @-reply on the command
" line to the author of the tweet on the current line.
function! s:Quick_Reply()
    let username = s:get_user_name(getline('.'))
    if username != ""
	" If the status ID is not available, get() will return 0 and
	" post_twitter() won't add in_reply_to_status_id to the update.
	call s:CmdLine_Twitter('@'.username.' ', get(s:curbuffer.statuses, line('.')))
    endif
endfunction

" Extract all user names from a line in the timeline. Return the poster's name as well as names from all the @replies.
function! s:get_all_names(line)
    let names = []
    let dictnames = {}

    let username = s:get_user_name(getline('.'))
    if username != ""
	" Add this to the beginning of the list because we want the tweet
	" author to be the main addressee in the reply to all.
	let names = [ username ]
	let dictnames[tolower(username)] = 1
    endif

    let matchcount = 1
    while 1
	let matchres = matchlist(a:line, '@\(\w\+\)', -1, matchcount)
	if matchres == []
	    break
	endif
	let name = matchres[1]
	" Don't add duplicate names.
	if !has_key(dictnames, tolower(name))
	    call add(names, name)
	    let dictnames[tolower(name)] = 1
	endif
	let matchcount += 1
    endwhile

    return names
endfunction

" Reply to everyone mentioned on a line in the timeline.
function! s:Reply_All()
    let names = s:get_all_names(getline('.'))

    " Remove the author from the reply list so that he doesn't end up replying
    " to himself.
    let user = s:get_twitvim_username()
    let names2 = []
    for name in names
	if name != user
	    call add(names2, name)
	endif
    endfor

    let replystr = '@'.join(names2, ' @').' '

    if names != []
	" If the status ID is not available, get() will return 0 and
	" post_twitter() won't add in_reply_to_status_id to the update.
	call s:CmdLine_Twitter(replystr, get(s:curbuffer.statuses, line('.')))
    endif
endfunction

" This is for a local mapping in the timeline. Start a direct message on the
" command line to the author of the tweet on the current line.
function! s:Quick_DM()
    let username = s:get_user_name(getline('.'))
    if username != ""
	" call s:CmdLine_Twitter('d '.username.' ', 0)
	call s:send_dm(username, '')
    endif
endfunction

" Allow user to switch to old-style retweets by setting twitvim_old_retweet.
function! s:get_old_retweet()
    return exists('g:twitvim_old_retweet') ? g:twitvim_old_retweet : 0
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

" Use new-style retweet API to retweet a tweet from another user.
function! s:Retweet_2()

    " Do an old-style retweet if user has set twitvim_old_retweet.
    if s:get_old_retweet()
	call s:Retweet()
	return
    endif

    let status = get(s:curbuffer.statuses, line('.'))
    if status == 0
	" Fall back to old-style retweeting if we can't get this tweet's status
	" ID.
	call s:Retweet()
	return
    endif

    let parms = {}

    " Force POST instead of GET.
    let parms["dummy"] = "dummy1"

    let url = s:get_api_root()."/statuses/retweet/".status.".xml"

    redraw
    echo "Retweeting..."

    let [error, output] = s:run_curl_oauth(url, s:ologin, s:get_proxy(), s:get_proxy_login(), parms)
    if error != ''
	let errormsg = s:xml_get_element(output, 'error')
	call s:errormsg("Error retweeting: ".(errormsg != '' ? errormsg : error))
    else
	call s:add_update(output)
	redraw
	echo "Retweeted."
    endif
endfunction

" Show which tweet this one is replying to below the current line.
function! s:show_inreplyto()
    let lineno = line('.')

    let inreplyto = get(s:curbuffer.inreplyto, lineno)
    if inreplyto == 0
	call s:warnmsg("No in-reply-to information for current line.")
	return
    endif

    redraw
    echo "Querying Twitter for in-reply-to tweet..."

    let url = s:get_api_root()."/statuses/show/".inreplyto.".xml"
    let [error, output] = s:run_curl_oauth(url, s:ologin, s:get_proxy(), s:get_proxy_login(), {})
    if error != ''
	let errormsg = s:xml_get_element(output, 'error')
	call s:errormsg("Error getting in-reply-to tweet: ".(errormsg != '' ? errormsg : error))
	return
    endif

    let line = s:format_status_xml(output)

    " Add the status ID to the current buffer's statuses list.
    call insert(s:curbuffer.statuses, s:xml_get_element(output, 'id'), lineno + 1)

    " Add in-reply-to ID to current buffer's in-reply-to list.
    call insert(s:curbuffer.inreplyto, s:xml_get_element(output, 'in_reply_to_status_id'), lineno + 1)

    " Already in the correct buffer so no need to search or switch buffers.
    setlocal modifiable
    call append(lineno, '+ '.line)
    setlocal nomodifiable
    let s:curbuffer.buffer = getline(1, '$')

    redraw
    echo "In-reply-to tweet found."
endfunction

" Truncate a string. Add '...' to the end of string was longer than
" the specified number of characters.
function! s:strtrunc(s, len)
    let slen = strlen(substitute(a:s, ".", "x", "g"))
    let s = substitute(a:s, '^\(.\{,'.a:len.'}\).*$', '\1', '')
    if slen > a:len
	let s .= '...'
    endif
    return s
endfunction

" Delete tweet or DM on current line.
function! s:do_delete_tweet()
    let lineno = line('.')

    let isdm = (s:curbuffer.buftype == "dmrecv" || s:curbuffer.buftype == "dmsent")
    let obj = isdm ? "message" : "tweet"
    let uobj = isdm ? "Message" : "Tweet"

    let id = get(isdm ? s:curbuffer.dmids : s:curbuffer.statuses, lineno)

    " The delete API call requires POST, not GET, so we supply a fake parameter
    " to force run_curl() to use POST.
    let parms = {}
    let parms["id"] = id

    let url = s:get_api_root().'/'.(isdm ? "direct_messages" : "statuses")."/destroy/".id.".xml"
    let [error, output] = s:run_curl_oauth(url, s:ologin, s:get_proxy(), s:get_proxy_login(), parms)
    if error != ''
	let errormsg = s:xml_get_element(output, 'error')
	call s:errormsg("Error deleting ".obj.": ".(errormsg != '' ? errormsg : error))
	return
    endif

    if isdm
	call remove(s:curbuffer.dmids, lineno)
    else
	call remove(s:curbuffer.statuses, lineno)
	call remove(s:curbuffer.inreplyto, lineno)
    endif

    " Already in the correct buffer so no need to search or switch buffers.
    setlocal modifiable
    normal! dd
    setlocal nomodifiable
    let s:curbuffer.buffer = getline(1, '$')

    redraw
    echo uobj "deleted."
endfunction

" Delete tweet or DM on current line.
function! s:delete_tweet()
    let lineno = line('.')

    let isdm = (s:curbuffer.buftype == "dmrecv" || s:curbuffer.buftype == "dmsent")
    let obj = isdm ? "message" : "tweet"
    let uobj = isdm ? "Message" : "Tweet"

    let id = get(isdm ? s:curbuffer.dmids : s:curbuffer.statuses, lineno)
    if id == 0
	call s:warnmsg("No erasable ".obj." on current line.")
	return
    endif

    call inputsave()
    let answer = input('Delete "'.s:strtrunc(getline('.'), 40).'"? (y/n) ')
    call inputrestore()
    if answer == 'y' || answer == 'Y'
	call s:do_delete_tweet()
    else
	redraw
	echo uobj "not deleted."
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
	execute "normal! \<Esc>"
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
	return -2
    endif

    return 0
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
	call s:get_summize(matchres[1], 1)
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
	    return substitute(longurl, '<!\[CDATA\[\(.*\)]]>', '\1', '')
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

" Get info on the given user. If no user is provided, use the current word and
" strip off the @ or : if the current word is @user or user:. 
function! s:do_user_info(s)
    let s = a:s
    if s == ''
	let s = expand("<cword>")
	
	" Handle @-replies.
	let matchres = matchlist(s, '^@\(\w\+\)')
	if matchres != []
	    let s = matchres[1]
	else
	    " Handle username: at the beginning of the line.
	    let matchres = matchlist(s, '^\(\w\+\):$')
	    if matchres != []
		let s = matchres[1]
	    endif
	endif
    endif

    call s:get_user_info(s)
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

" Set syntax highlighting in timeline window.
function! s:twitter_win_syntax(wintype)
    " Beautify the Twitter window with syntax highlighting.
    if has("syntax") && exists("g:syntax_on")
	" Reset syntax items in case there are any predefined in the new buffer.
	syntax clear

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

	if a:wintype != "userinfo"
	    " Use the extra star at the end to recognize the title but hide the
	    " star.
	    syntax match twitterTitle /^.\+\*$/ contains=twitterTitleStar
	    syntax match twitterTitleStar /\*$/ contained
	endif

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
" Returns 1 if new window created, 0 otherwise.
function! s:twitter_win(wintype)
    let winname = a:wintype == "userinfo" ? s:user_winname : s:twit_winname
    let newwin = 0

    let twit_bufnr = bufwinnr('^'.winname.'$')
    if twit_bufnr > 0
	execute twit_bufnr . "wincmd w"
    else
	let newwin = 1
	execute "new " . winname
	setlocal noswapfile
	setlocal buftype=nofile
	setlocal bufhidden=delete 
	setlocal foldcolumn=0
	setlocal nobuflisted
	setlocal nospell

	" Launch browser with URL in visual selection or at cursor position.
	nnoremap <buffer> <silent> <A-g> :call <SID>launch_url_cword()<cr>
	nnoremap <buffer> <silent> <Leader>g :call <SID>launch_url_cword()<cr>
	vnoremap <buffer> <silent> <A-g> y:call <SID>launch_browser(@")<cr>
	vnoremap <buffer> <silent> <Leader>g y:call <SID>launch_browser(@")<cr>

	" Get user info for current word or selection.
	nnoremap <buffer> <silent> <Leader>p :call <SID>do_user_info("")<cr>
	vnoremap <buffer> <silent> <Leader>p y:call <SID>do_user_info(@")<cr>

	" Call LongURL API on current word or selection.
	nnoremap <buffer> <silent> <Leader>e :call <SID>do_longurl("")<cr>
	vnoremap <buffer> <silent> <Leader>e y:call <SID>do_longurl(@")<cr>

	if a:wintype != "userinfo"

	    " Quick reply feature for replying from the timeline.
	    nnoremap <buffer> <silent> <A-r> :call <SID>Quick_Reply()<cr>
	    nnoremap <buffer> <silent> <Leader>r :call <SID>Quick_Reply()<cr>

	    " Quick DM feature for direct messaging from the timeline.
	    nnoremap <buffer> <silent> <A-d> :call <SID>Quick_DM()<cr>
	    nnoremap <buffer> <silent> <Leader>d :call <SID>Quick_DM()<cr>

	    " Retweet feature for replicating another user's tweet.
	    nnoremap <buffer> <silent> <Leader>R :call <SID>Retweet_2()<cr>

	    " Reply to all feature.
	    nnoremap <buffer> <silent> <Leader><C-r> :call <SID>Reply_All()<cr>

	    " Show in-reply-to for current tweet.
	    nnoremap <buffer> <silent> <Leader>@ :call <SID>show_inreplyto()<cr>

	    " Delete tweet or message on current line.
	    nnoremap <buffer> <silent> <Leader>X :call <SID>delete_tweet()<cr>

	    " Refresh timeline.
	    nnoremap <buffer> <silent> <Leader><Leader> :call <SID>RefreshTimeline()<cr>

	    " Next page in timeline.
	    nnoremap <buffer> <silent> <C-PageDown> :call <SID>NextPageTimeline()<cr>

	    " Previous page in timeline.
	    nnoremap <buffer> <silent> <C-PageUp> :call <SID>PrevPageTimeline()<cr>

	endif

	" Go back and forth through buffer stack.
	nnoremap <buffer> <silent> <C-o> :call <SID>back_buffer()<cr>
	nnoremap <buffer> <silent> <C-i> :call <SID>fwd_buffer()<cr>
    endif

    call s:twitter_win_syntax(a:wintype)
    return newwin
endfunction

" Get a Twitter window and stuff text into it. If view is not an empty
" dictionary then restore the cursor position to the saved view.
function! s:twitter_wintext_view(text, wintype, view)
    let curwin = winnr()
    let newwin = s:twitter_win(a:wintype)

    setlocal modifiable

    " Overwrite the entire buffer.
    " Need to use 'silent' or a 'No lines in buffer' message will appear.
    " Delete to the blackhole register "_ so that we don't affect registers.
    silent %delete _
    call setline('.', a:text)
    normal! 1G

    setlocal nomodifiable

    " Restore the saved view if provided.
    if a:view != {}
	call winrestview(a:view)
    endif

    " Go back to original window after updating buffer. If a new window is
    " created then our saved curwin number is wrong so the best we can do is to
    " take the user back to the last-accessed window using 'wincmd p'.
    if newwin
	wincmd p
    else
	execute curwin .  "wincmd w"
    endif
endfunction

" Get a Twitter window and stuff text into it.
function! s:twitter_wintext(text, wintype)
    call s:twitter_wintext_view(a:text, a:wintype, {})
endfunction

" Format XML status as a display line.
function! s:format_status_xml(item)
    let item = a:item

    " Quick hack. Even though we're getting new-style retweets in the timeline
    " XML, we'll still use the old-style retweet text from it.
    let item = s:xml_remove_elements(item, 'retweeted_status')

    let user = s:xml_get_element(item, 'screen_name')
    let text = s:convert_entity(s:xml_get_element(item, 'text'))
    let pubdate = s:time_filter(s:xml_get_element(item, 'created_at'))

    return user.': '.text.' |'.pubdate.'|'
endfunction

" Show a timeline from XML stream data.
function! s:show_timeline_xml(timeline, tline_name, username, page)
    let matchcount = 1
    let text = []

    let s:curbuffer.dmids = []

    " Construct page title.

    let title = substitute(a:tline_name, '^.', '\u&', '')." timeline"
    if a:username != ''
	let title .= " for ".a:username
    endif

    " Special case titles for Retweets and Mentions.
    if a:tline_name == "retweeted_to_me"
	let title = "Retweets by others"
    elseif a:tline_name == "retweeted_by_me"
	let title = "Retweets by you"
    elseif a:tline_name == "replies"
	let title = "Mentions timeline"
    endif

    if a:page > 1
	let title .= ' (page '.a:page.')'
    endif

    let s:curbuffer.showheader = s:get_show_header()
    if s:curbuffer.showheader
	" Index of first status will be 3 to match line numbers in timeline
	" display.
	let s:curbuffer.statuses = [0, 0, 0]
	let s:curbuffer.inreplyto = [0, 0, 0]

	" The extra stars at the end are for the syntax highlighter to
	" recognize the title. Then the syntax highlighter hides the stars by
	" coloring them the same as the background. It is a bad hack.
	call add(text, title.'*')
	call add(text, repeat('=', s:mbstrlen(title)).'*')
    else
	" Index of first status will be 1 to match line numbers in timeline
	" display.
	let s:curbuffer.statuses = [0]
	let s:curbuffer.inreplyto = [0]
    endif

    while 1
	let item = s:xml_get_nth(a:timeline, 'status', matchcount)
	if item == ""
	    break
	endif

	call add(s:curbuffer.statuses, s:xml_get_element(item, 'id'))
	call add(s:curbuffer.inreplyto, s:xml_get_element(item, 'in_reply_to_status_id'))

	let line = s:format_status_xml(item)
	call add(text, line)

	let matchcount += 1
    endwhile
    call s:twitter_wintext(text, "timeline")
    let s:curbuffer.buffer = text
endfunction

" Add a parameter to a URL.
function! s:add_to_url(url, parm)
    return a:url . (a:url =~ '?' ? '&' : '?') . a:parm
endfunction

" Generic timeline retrieval function.
function! s:get_timeline(tline_name, username, page)
    if a:tline_name == "public"
	" No authentication is needed for public timeline.
	let login = ''
    else
	let login = s:ologin
    endif

    let url_fname = (a:tline_name == "retweeted_to_me" || a:tline_name == "retweeted_by_me") ? a:tline_name.".xml" : a:tline_name == "friends" ? "home_timeline.xml" : a:tline_name == "replies" ? "mentions.xml" : a:tline_name."_timeline.xml"

    " Support pagination.
    if a:page > 1
	let url_fname = s:add_to_url(url_fname, 'page='.a:page)
    endif

    " Include retweets.
    let url_fname = s:add_to_url(url_fname, 'include_rts=true')

    " Twitter API allows you to specify a username for user_timeline to
    " retrieve another user's timeline.
    if a:username != ''
	let url_fname = s:add_to_url(url_fname, 'screen_name='.a:username)
    endif

    " Support count parameter in friends, user, mentions, and retweet timelines.
    if a:tline_name == 'friends' || a:tline_name == 'user' || a:tline_name == 'replies' || a:tline_name == 'retweeted_to_me' || a:tline_name == 'retweeted_by_me'
	let tcount = s:get_count()
	if tcount > 0
	    let url_fname = s:add_to_url(url_fname, 'count='.tcount)
	endif
    endif

    let tl_name = a:tline_name == "replies" ? "mentions" : a:tline_name

    redraw
    echo "Sending" tl_name "timeline request to Twitter..."

    let url = s:get_api_root()."/statuses/".url_fname

    let [error, output] = s:run_curl_oauth(url, login, s:get_proxy(), s:get_proxy_login(), {})

    if error != ''
	let errormsg = s:xml_get_element(output, 'error')
	call s:errormsg("Error getting Twitter ".tl_name." timeline: ".(errormsg != '' ? errormsg : error))
	return
    endif

    call s:save_buffer()
    let s:curbuffer = {}
    call s:show_timeline_xml(output, a:tline_name, a:username, a:page)
    let s:curbuffer.buftype = a:tline_name
    let s:curbuffer.user = a:username
    let s:curbuffer.list = ''
    let s:curbuffer.page = a:page
    redraw

    let foruser = a:username == '' ? '' : ' for user '.a:username

    " Uppercase the first letter in the timeline name.
    echo substitute(tl_name, '^.', '\u&', '') "timeline updated".foruser."."
endfunction

" Retrieve a Twitter list timeline.
function! s:get_list_timeline(username, listname, page)

    let user = a:username
    if user == ''
	let user = s:get_twitvim_username()
	if user == ''
	    call s:errormsg('Twitter login not set. Please specify a username.')
	    return -1
	endif
    endif

    let url = "/".user."/lists/".a:listname."/statuses.xml"

    " Support pagination.
    if a:page > 1
	let url = s:add_to_url(url, 'page='.a:page)
    endif

    " Support count parameter.
    let tcount = s:get_count()
    if tcount > 0
	let url = s:add_to_url(url, 'per_page='.tcount)
    endif

    redraw
    echo "Sending list timeline request to Twitter..."

    let url = s:get_api_root().url

    let [error, output] = s:run_curl_oauth(url, s:ologin, s:get_proxy(), s:get_proxy_login(), {})

    if error != ''
	let errormsg = s:xml_get_element(output, 'error')
	call s:errormsg("Error getting Twitter list timeline: ".(errormsg != '' ? errormsg : error))
	return
    endif

    call s:save_buffer()
    let s:curbuffer = {}
    call s:show_timeline_xml(output, "list", user."/".a:listname, a:page)
    let s:curbuffer.buftype = "list"
    let s:curbuffer.user = user
    let s:curbuffer.list = a:listname
    let s:curbuffer.page = a:page
    redraw

    " Uppercase the first letter in the timeline name.
    echo "List timeline updated for ".user."/".a:listname
endfunction

" Show direct message sent or received by user. First argument should be 'sent'
" or 'received' depending on which timeline we are displaying.
function! s:show_dm_xml(sent_or_recv, timeline, page)
    let matchcount = 1
    let text = []

    "No status IDs in direct messages.
    let s:curbuffer.statuses = []
    let s:curbuffer.inreplyto = []

    let title = 'Direct messages '.a:sent_or_recv

    if a:page > 1
	let title .= ' (page '.a:page.')'
    endif

    let s:curbuffer.showheader = s:get_show_header()
    if s:curbuffer.showheader
	" Index of first dmid will be 3 to match line numbers in timeline
	" display.
	let s:curbuffer.dmids = [0, 0, 0]

	" The extra stars at the end are for the syntax highlighter to
	" recognize the title. Then the syntax highlighter hides the stars by
	" coloring them the same as the background. It is a bad hack.
	call add(text, title.'*')
	call add(text, repeat('=', s:mbstrlen(title)).'*')
    else
	" Index of first dmid will be 1 to match line numbers in timeline
	" display.
	let s:curbuffer.dmids = [0]
    endif

    while 1
	let item = s:xml_get_nth(a:timeline, 'direct_message', matchcount)
	if item == ""
	    break
	endif

	call add(s:curbuffer.dmids, s:xml_get_element(item, 'id'))

	let user = s:xml_get_element(item, a:sent_or_recv == 'sent' ? 'recipient_screen_name' : 'sender_screen_name')
	let mesg = s:xml_get_element(item, 'text')
	let date = s:time_filter(s:xml_get_element(item, 'created_at'))

	call add(text, user.": ".s:convert_entity(mesg).' |'.date.'|')

	let matchcount += 1
    endwhile
    call s:twitter_wintext(text, "timeline")
    let s:curbuffer.buffer = text
endfunction

" Get direct messages sent to or received by user.
function! s:Direct_Messages(mode, page)
    let sent = (a:mode == "dmsent")
    let s_or_r = (sent ? "sent" : "received")

    " Support pagination.
    let pagearg = ''
    if a:page > 1
	let pagearg = '?page='.a:page
    endif

    redraw
    echo "Sending direct messages ".s_or_r." timeline request to Twitter..."

    let url = s:get_api_root()."/direct_messages".(sent ? "/sent" : "").".xml".pagearg

    let [error, output] = s:run_curl_oauth(url, s:ologin, s:get_proxy(), s:get_proxy_login(), {})

    if error != ''
	let errormsg = s:xml_get_element(output, 'error')
	call s:errormsg("Error getting Twitter direct messages ".s_or_r." timeline: ".(errormsg != '' ? errormsg : error))
	return
    endif

    call s:save_buffer()
    let s:curbuffer = {}
    call s:show_dm_xml(s_or_r, output, a:page)
    let s:curbuffer.buftype = a:mode
    let s:curbuffer.user = ''
    let s:curbuffer.list = ''
    let s:curbuffer.page = a:page
    redraw
    echo "Direct messages ".s_or_r." timeline updated."
endfunction

" Function to load a timeline from the given parameters. For use by refresh and
" next/prev pagination commands.
function! s:load_timeline(buftype, user, list, page)
    if a:buftype == "public" || a:buftype == "friends" || a:buftype == "user" || a:buftype == "replies" || a:buftype == "retweeted_by_me" || a:buftype == "retweeted_to_me"
	call s:get_timeline(a:buftype, a:user, a:page)
    elseif a:buftype == "list"
	call s:get_list_timeline(a:user, a:list, a:page)
    elseif a:buftype == "dmsent" || a:buftype == "dmrecv"
	call s:Direct_Messages(a:buftype, a:page)
    elseif a:buftype == "search"
	call s:get_summize(a:user, a:page)
    endif
endfunction

" Refresh the timeline buffer.
function! s:RefreshTimeline()
    if s:curbuffer != {}
	call s:load_timeline(s:curbuffer.buftype, s:curbuffer.user, s:curbuffer.list, s:curbuffer.page)
    else
	call s:warnmsg("No timeline buffer to refresh.")
    endif
endfunction

" Go to next page in timeline.
function! s:NextPageTimeline()
    if s:curbuffer != {}
	call s:load_timeline(s:curbuffer.buftype, s:curbuffer.user, s:curbuffer.list, s:curbuffer.page + 1)
    else
	call s:warnmsg("No timeline buffer.")
    endif
endfunction

" Go to previous page in timeline.
function! s:PrevPageTimeline()
    if s:curbuffer != {}
	if s:curbuffer.page <= 1
	    call s:warnmsg("Timeline is already on first page.")
	else
	    call s:load_timeline(s:curbuffer.buftype, s:curbuffer.user, s:curbuffer.list, s:curbuffer.page - 1)
	endif
    else
	call s:warnmsg("No timeline buffer.")
    endif
endfunction

" Get a Twitter list. Need to do a little fiddling because the 
" username argument is optional.
function! s:DoList(page, arg1, ...)
    let user = ''
    let list = a:arg1
    if a:0 > 0
	let user = a:arg1
	let list = a:1
    endif
    call s:get_list_timeline(user, list, a:page)
endfunction

if !exists(":PublicTwitter")
    command PublicTwitter :call <SID>get_timeline("public", '', 1)
endif
if !exists(":FriendsTwitter")
    command -count=1 FriendsTwitter :call <SID>get_timeline("friends", '', <count>)
endif
if !exists(":UserTwitter")
    command -range=1 -nargs=? UserTwitter :call <SID>get_timeline("user", <q-args>, <count>)
endif
if !exists(":MentionsTwitter")
    command -count=1 MentionsTwitter :call <SID>get_timeline("replies", '', <count>)
endif
if !exists(":RepliesTwitter")
    command -count=1 RepliesTwitter :call <SID>get_timeline("replies", '', <count>)
endif
if !exists(":DMTwitter")
    command -count=1 DMTwitter :call <SID>Direct_Messages("dmrecv", <count>)
endif
if !exists(":DMSentTwitter")
    command -count=1 DMSentTwitter :call <SID>Direct_Messages("dmsent", <count>)
endif
if !exists(":ListTwitter")
    command -range=1 -nargs=+ ListTwitter :call <SID>DoList(<count>, <f-args>)
endif
if !exists(":RetweetedByMeTwitter")
    command -count=1 RetweetedByMeTwitter :call <SID>get_timeline("retweeted_by_me", '', <count>)
endif
if !exists(":RetweetedToMeTwitter")
    command -count=1 RetweetedToMeTwitter :call <SID>get_timeline("retweeted_to_me", '', <count>)
endif

nnoremenu Plugin.TwitVim.-Sep1- :
nnoremenu Plugin.TwitVim.&Friends\ Timeline :call <SID>get_timeline("friends", '', 1)<cr>
nnoremenu Plugin.TwitVim.&User\ Timeline :call <SID>get_timeline("user", '', 1)<cr>
nnoremenu Plugin.TwitVim.&Mentions\ Timeline :call <SID>get_timeline("replies", '', 1)<cr>
nnoremenu Plugin.TwitVim.&Direct\ Messages :call <SID>Direct_Messages("dmrecv", 1)<cr>
nnoremenu Plugin.TwitVim.Direct\ Messages\ &Sent :call <SID>Direct_Messages("dmsent", 1)<cr>
nnoremenu Plugin.TwitVim.&Public\ Timeline :call <SID>get_timeline("public", '', 1)<cr>

nnoremenu Plugin.TwitVim.Retweeted\ &By\ Me :call <SID>get_timeline("retweeted_by_me", '', 1)<cr>
nnoremenu Plugin.TwitVim.Retweeted\ &To\ Me :call <SID>get_timeline("retweeted_to_me", '', 1)<cr>

if !exists(":RefreshTwitter")
    command RefreshTwitter :call <SID>RefreshTimeline()
endif
if !exists(":NextTwitter")
    command NextTwitter :call <SID>NextPageTimeline()
endif
if !exists(":PreviousTwitter")
    command PreviousTwitter :call <SID>PrevPageTimeline()
endif

if !exists(":SetLoginTwitter")
    command SetLoginTwitter :call <SID>prompt_twitvim_login()
endif
if !exists(":ResetLoginTwitter")
    command ResetLoginTwitter :call <SID>reset_twitvim_login()
endif

nnoremenu Plugin.TwitVim.-Sep2- :
nnoremenu Plugin.TwitVim.Set\ Twitter\ Login :call <SID>prompt_twitvim_login()<cr>
nnoremenu Plugin.TwitVim.Reset\ Twitter\ Login :call <SID>reset_twitvim_login()<cr>


" Send a direct message.
function! s:do_send_dm(user, mesg)
    let mesg = a:mesg

    " Remove trailing newline. You see that when you visual-select an entire
    " line. Don't let it count towards the message length.
    let mesg = substitute(mesg, '\n$', '', "")

    " Convert internal newlines to spaces.
    let mesg = substitute(mesg, '\n', ' ', "g")

    let mesglen = s:mbstrlen(mesg)

    " Check message length. Note that the message length should be checked
    " before URL-encoding the special characters because URL-encoding increases
    " the string length.
    if mesglen > s:char_limit
	call s:warnmsg("Your message has ".(mesglen - s:char_limit)." too many characters. It was not sent.")
    elseif mesglen < 1
	call s:warnmsg("Your message was empty. It was not sent.")
    else
	redraw
	echo "Sending message to ".a:user."..."

	let url = s:get_api_root()."/direct_messages/new.xml"
	let parms = { "source" : "twitvim", "user" : a:user, "text" : mesg }

	let [error, output] = s:run_curl_oauth(url, s:ologin, s:get_proxy(), s:get_proxy_login(), parms)

	if error != ''
	    let errormsg = s:xml_get_element(output, 'error')
	    call s:errormsg("Error sending your message: ".(errormsg != '' ? errormsg : error))
	else
	    redraw
	    echo "Your message was sent to ".a:user.". You used ".mesglen." characters."
	endif
    endif
endfunction

" Send a direct message. Prompt user for message if not given.
function! s:send_dm(user, mesg)
    if a:user == ""
	call s:warnmsg("No recipient specified for direct message.")
	return
    endif

    let mesg = a:mesg
    if mesg == ""
	call inputsave()
	let mesg = input("DM ".a:user.": ")
	call inputrestore()
    endif

    if mesg == ""
	call s:warnmsg("Your message was empty. It was not sent.")
	return
    endif

    call s:do_send_dm(a:user, mesg)
endfunction

if !exists(":SendDMTwitter")
    command -nargs=1 SendDMTwitter :call <SID>send_dm(<q-args>, '')
endif

" Call Twitter API to get rate limit information.
function! s:get_rate_limit()
    redraw
    echo "Querying Twitter for rate limit information..."

    let url = s:get_api_root()."/account/rate_limit_status.xml"
    let [error, output] = s:run_curl_oauth(url, s:ologin, s:get_proxy(), s:get_proxy_login(), {})
    if error != ''
	let errormsg = s:xml_get_element(output, 'error')
	call s:errormsg("Error getting rate limit info: ".(errormsg != '' ? errormsg : error))
	return
    endif

    let remaining = s:xml_get_element(output, 'remaining-hits')
    let resettime = s:time_filter(s:xml_get_element(output, 'reset-time'))
    let limit = s:xml_get_element(output, 'hourly-limit')

    redraw
    echo "Rate limit: ".limit." Remaining: ".remaining." Reset at: ".resettime
endfunction

if !exists(":RateLimitTwitter")
    command RateLimitTwitter :call <SID>get_rate_limit()
endif

" Set location field on Twitter profile.
function! s:set_location(loc)
    redraw
    echo "Setting location on Twitter profile..."

    let url = s:get_api_root()."/account/update_location.xml"
    let parms = { 'location' : a:loc }

    let [error, output] = s:run_curl_oauth(url, s:ologin, s:get_proxy(), s:get_proxy_login(), parms)
    if error != ''
	let errormsg = s:xml_get_element(output, 'error')
	call s:errormsg("Error setting location: ".(errormsg != '' ? errormsg : error))
	return
    endif

    redraw
    echo "Location: ".s:xml_get_element(output, 'location')
endfunction

if !exists(":LocationTwitter")
    command -nargs=+ LocationTwitter :call <SID>set_location(<q-args>)
endif


" Start following a user.
function! s:follow_user(user)
    redraw
    echo "Following user ".a:user."..."

    let parms = {}
    let parms["screen_name"] = a:user

    let url = s:get_api_root()."/friendships/create/".a:user.".xml"

    let [error, output] = s:run_curl_oauth(url, s:ologin, s:get_proxy(), s:get_proxy_login(), parms)
    if error != ''
	let errormsg = s:xml_get_element(output, 'error')
	call s:errormsg("Error following user: ".(errormsg != '' ? errormsg : error))
    else
	let protected = s:xml_get_element(output, 'protected')
	redraw
	if protected == "true"
	    echo "Made request to follow ".a:user."'s protected timeline."
	else
	    echo "Now following ".a:user."'s timeline."
	endif
    endif
endfunction

if !exists(":FollowTwitter")
    command -nargs=1 FollowTwitter :call <SID>follow_user(<q-args>)
endif


" Stop following a user.
function! s:unfollow_user(user)
    redraw
    echo "Unfollowing user ".a:user."..."

    let parms = {}
    let parms["screen_name"] = a:user

    let url = s:get_api_root()."/friendships/destroy.xml"

    let [error, output] = s:run_curl_oauth(url, s:ologin, s:get_proxy(), s:get_proxy_login(), parms)
    if error != ''
	let errormsg = s:xml_get_element(output, 'error')
	call s:errormsg("Error unfollowing user: ".(errormsg != '' ? errormsg : error))
    else
	redraw
	echo "Stopped following ".a:user."'s timeline."
    endif
endfunction

if !exists(":UnfollowTwitter")
    command -nargs=1 UnfollowTwitter :call <SID>unfollow_user(<q-args>)
endif


" Block a user.
function! s:block_user(user, unblock)
    redraw
    echo (a:unblock ? "Unblocking" : "Blocking")." user ".a:user."..."

    let parms = {}
    let parms["screen_name"] = a:user

    let url = s:get_api_root()."/blocks/".(a:unblock ? "destroy" : "create").".xml"

    let [error, output] = s:run_curl_oauth(url, s:ologin, s:get_proxy(), s:get_proxy_login(), parms)
    if error != ''
	let errormsg = s:xml_get_element(output, 'error')
	call s:errormsg("Error ".(a:unblock ? "unblocking" : "blocking")." user: ".(errormsg != '' ? errormsg : error))
    else
	redraw
	echo "User ".a:user." is now ".(a:unblock ? "unblocked" : "blocked")."."
    endif
endfunction

if !exists(":BlockTwitter")
    command -nargs=1 BlockTwitter :call <SID>block_user(<q-args>, 0)
endif
if !exists(":UnblockTwitter")
    command -nargs=1 UnblockTwitter :call <SID>block_user(<q-args>, 1)
endif


" Report user for spam.
function! s:report_spam(user)
    redraw
    echo "Reporting ".a:user." for spam..."

    let parms = {}
    let parms["screen_name"] = a:user

    let url = s:get_api_root()."/report_spam.xml"

    let [error, output] = s:run_curl_oauth(url, s:ologin, s:get_proxy(), s:get_proxy_login(), parms)
    if error != ''
	let errormsg = s:xml_get_element(output, 'error')
	call s:errormsg("Error reporting user for spam: ".(errormsg != '' ? errormsg : error))
    else
	redraw
	echo "Reported user ".a:user." for spam."
    endif
endfunction

if !exists(":ReportSpamTwitter")
    command -nargs=1 ReportSpamTwitter :call <SID>report_spam(<q-args>)
endif


" Add user to a list or remove user from a list.
function! s:add_to_list(remove, listname, username)
    let user = s:get_twitvim_username()
    if user == ''
	call s:errormsg('Twitter login not set. Please specify a username.')
	return -1
    endif

    redraw
    if a:remove
	echo "Removing ".a:username." from list ".a:listname."..."
    else
	echo "Adding ".a:username." to list ".a:listname."..."
    endif

    let parms = {}
    let parms["list_id"] = a:listname
    let parms["id"] = a:username
    if a:remove
	let parms["_method"] = "DELETE"
    endif

    let url = s:get_api_root()."/".user."/".a:listname."/members.xml"

    let [error, output] = s:run_curl_oauth(url, s:ologin, s:get_proxy(), s:get_proxy_login(), parms)
    if error != ''
	let errormsg = s:xml_get_element(output, 'error')
	call s:errormsg("Error ".(a:remove ? "removing user from" : "adding user to")." list: ".(errormsg != '' ? errormsg : error))
    else
	redraw
	if a:remove
	    echo "Removed ".a:username." from list ".a:listname."."
	else
	    echo "Added ".a:username." to list ".a:listname."."
	endif
    endif
endfunction

function! s:do_add_to_list(arg1, ...)
    if a:0 == 0
	call s:errormsg("Syntax: :AddToListTwitter listname username")
    else
	call s:add_to_list(0, a:arg1, a:1)
    endif
endfunction

if !exists(":AddToListTwitter")
    command -nargs=+ AddToListTwitter :call <SID>do_add_to_list(<f-args>)
endif


function! s:do_remove_from_list(arg1, ...)
    if a:0 == 0
	call s:errormsg("Syntax: :RemoveFromListTwitter listname username")
    else
	call s:add_to_list(1, a:arg1, a:1)
    endif
endfunction

if !exists(":RemoveFromListTwitter")
    command -nargs=+ RemoveFromListTwitter :call <SID>do_remove_from_list(<f-args>)
endif


let s:user_winname = "TwitterUserInfo_".localtime()

" Process/format the user information.
function! s:format_user_info(output)
    let text = []
    let output = a:output

    let name = s:xml_get_element(output, 'name')
    let screen = s:xml_get_element(output, 'screen_name')
    call add(text, 'Name: '.screen.' ('.name.')')

    call add(text, 'Location: '.s:xml_get_element(output, 'location'))
    call add(text, 'Website: '.s:xml_get_element(output, 'url'))
    call add(text, 'Bio: '.s:xml_get_element(output, 'description'))
    call add(text, '')
    call add(text, 'Following: '.s:xml_get_element(output, 'friends_count'))
    call add(text, 'Followers: '.s:xml_get_element(output, 'followers_count'))
    call add(text, 'Updates: '.s:xml_get_element(output, 'statuses_count'))
    call add(text, 'Favorites: '.s:xml_get_element(output, 'favourites_count'))
    call add(text, '')

    call add(text, 'Protected: '.s:xml_get_element(output, 'protected'))
    call add(text, 'Following: '.s:xml_get_element(output, 'following'))
    call add(text, '')

    let usernode = s:xml_remove_elements(output, 'status')
    let startdate = s:time_filter(s:xml_get_element(usernode, 'created_at'))
    call add(text, 'Started on: |'.startdate.'|')
    let timezone = s:convert_entity(s:xml_get_element(usernode, 'time_zone'))
    call add(text, 'Time zone: '.timezone)
    call add(text, '')

    let statusnode = s:xml_get_element(output, 'status')
    if statusnode != ""
	let status = s:xml_get_element(statusnode, 'text')
	let pubdate = s:time_filter(s:xml_get_element(statusnode, 'created_at'))
	call add(text, 'Status: '.s:convert_entity(status).' |'.pubdate.'|')
    endif

    return text
endfunction

" Call Twitter API to get user's info.
function! s:get_user_info(username)
    if a:username == ''
	call s:errormsg("Please specify a user name to retrieve info on.")
	return
    endif

    redraw
    echo "Querying Twitter for user information..."

    let url = s:get_api_root()."/users/show.xml?screen_name=".a:username
    let [error, output] = s:run_curl_oauth(url, s:ologin, s:get_proxy(), s:get_proxy_login(), {})
    if error != ''
	let errormsg = s:xml_get_element(output, 'error')
	call s:errormsg("Error getting user info: ".(errormsg != '' ? errormsg : error))
	return
    endif

    call s:twitter_wintext(s:format_user_info(output), "userinfo")

    redraw
    echo "User information retrieved."
endfunction

if !exists(":ProfileTwitter")
    command -nargs=1 ProfileTwitter :call <SID>get_user_info(<q-args>)
endif


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

" Get bit.ly username and api key if configured by the user. Otherwise, use a
" default username and api key.
function! s:get_bitly_key()
    if exists('g:twitvim_bitly_user') && exists('g:twitvim_bitly_key')
	return [ g:twitvim_bitly_user, g:twitvim_bitly_key ]
    endif
    return [ 'twitvim', 'R_a53414d2f36a90c3e189299c967e6efc' ]
endfunction

" Call bit.ly API to shorten a URL.
function! s:call_bitly(url)
    let [ user, key ] = s:get_bitly_key()

    redraw
    echo "Sending request to bit.ly..."

    let url = 'http://api.bit.ly/shorten?version=2.0.1'
    let url .= '&longUrl='.s:url_encode(a:url)
    let url .= '&login='.user
    let url .= '&apiKey='.key.'&format=xml&history=1'
    let [error, output] = s:run_curl(url, '', s:get_proxy(), s:get_proxy_login(), {})

    if error != ''
	call s:errormsg("Error calling bit.ly API: ".error)
	return ""
    endif

    let status = s:xml_get_element(output, 'statusCode')
    if status != 'OK'
	let errorcode = s:xml_get_element(output, 'errorCode')
	let errormsg = s:xml_get_element(output, 'errorMessage')
	if errorcode == 0
	    " For reasons unknown, bit.ly sometimes return two error codes and
	    " the first one is 0.
	    let errorcode = s:xml_get_nth(output, 'errorCode', 2)
	    let errormsg = s:xml_get_nth(output, 'errorMessage', 2)
	endif
	call s:errormsg("Error from bit.ly: ".errorcode." ".errormsg)
	return ""
    endif

    let shorturl = s:xml_get_element(output, 'shortUrl')
    redraw
    echo "Received response from bit.ly."
    return shorturl
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
	if output !~ '\c^http'
	    call s:errormsg("urlBorg error: ".output)
	    return ""
	endif

	redraw
	echo "Received response from urlBorg."
	return output
    endif
endfunction


" Get tr.im login info if configured by the user.
function! s:get_trim_login()
    return exists('g:twitvim_trim_login') ? g:twitvim_trim_login : ''
endfunction

" Call tr.im API to shorten a URL.
function! s:call_trim(url)
    let login = s:get_trim_login()

    redraw
    echo "Sending request to tr.im..."

    let url = 'http://tr.im/api/trim_url.xml?url='.s:url_encode(a:url)

    let [error, output] = s:run_curl(url, login, s:get_proxy(), s:get_proxy_login(), {})

    if error != ''
	call s:errormsg("Error calling tr.im API: ".error)
	return ""
    endif

    let statusattr = s:xml_get_attr(output, 'status')

    let trimmsg = statusattr['code'].' '.statusattr['message']

    if statusattr['result'] == "OK"
	return s:xml_get_element(output, 'url')
    elseif statusattr['result'] == "ERROR"
	call s:errormsg("tr.im error: ".trimmsg)
	return ""
    else
	call s:errormsg("Unknown result from tr.im: ".trimmsg)
	return ""
    endif
endfunction

" Get Cligs API key if configured by the user.
function! s:get_cligs_key()
    return exists('g:twitvim_cligs_key') ? g:twitvim_cligs_key : ''
endfunction

" Call Cligs API to shorten a URL.
function! s:call_cligs(url)
    let url = 'http://cli.gs/api/v1/cligs/create?appid=twitvim&url='.s:url_encode(a:url)

    let key = s:get_cligs_key()
    if key != ''
	let url .= '&key='.key
    endif

    redraw
    echo "Sending request to Cligs..."

    let [error, output] = s:run_curl(url, '', s:get_proxy(), s:get_proxy_login(), {})
    if error != ''
	call s:errormsg("Error calling Cligs API: ".error)
	return ""
    endif

    redraw
    echo "Received response from Cligs."
    return output
endfunction

" Call Zi.ma API to shorten a URL.
function! s:call_zima(url)
    let url = "http://zi.ma/?module=ShortURL&file=Add&mode=API&url=".s:url_encode(a:url)

    redraw
    echo "Sending request to Zi.ma..."

    let [error, output] = s:run_curl(url, '', s:get_proxy(), s:get_proxy_login(), {})
    if error != ''
	call s:errormsg("Error calling Zi.ma API: ".error)
	return ""
    endif

    let error = s:xml_get_element(output, 'h3')
    if error != ''
	call s:errormsg("Error from Zi.ma: ".error)
	return ""
    endif

    redraw
    echo "Received response from Zi.ma."
    return output
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
	    execute "normal! a".shorturl."\<esc>"
	else
	    execute "normal! i".shorturl." \<esc>"
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

if !exists(":Trim")
    command -nargs=? Trim :call <SID>GetShortURL("insert", <q-args>, "call_trim")
endif
if !exists(":ATrim")
    command -nargs=? ATrim :call <SID>GetShortURL("append", <q-args>, "call_trim")
endif
if !exists(":PTrim")
    command -nargs=? PTrim :call <SID>GetShortURL("cmdline", <q-args>, "call_trim")
endif

if !exists(":Cligs")
    command -nargs=? Cligs :call <SID>GetShortURL("insert", <q-args>, "call_cligs")
endif
if !exists(":ACligs")
    command -nargs=? ACligs :call <SID>GetShortURL("append", <q-args>, "call_cligs")
endif
if !exists(":PCligs")
    command -nargs=? PCligs :call <SID>GetShortURL("cmdline", <q-args>, "call_cligs")
endif

if !exists(":Zima")
    command -nargs=? Zima :call <SID>GetShortURL("insert", <q-args>, "call_zima")
endif
if !exists(":AZima")
    command -nargs=? AZima :call <SID>GetShortURL("append", <q-args>, "call_zima")
endif
if !exists(":PZima")
    command -nargs=? PZima :call <SID>GetShortURL("cmdline", <q-args>, "call_zima")
endif

" Parse and format search results from Twitter Search API.
function! s:show_summize(searchres, page)
    let text = []
    let matchcount = 1

    let s:curbuffer.dmids = []

    let channel = s:xml_remove_elements(a:searchres, 'entry')
    let title = s:xml_get_element(channel, 'title')

    if a:page > 1
	let title .= ' (page '.a:page.')'
    endif

    let s:curbuffer.showheader = s:get_show_header()
    if s:curbuffer.showheader
	" Index of first status will be 3 to match line numbers in timeline
	" display.
	let s:curbuffer.statuses = [0, 0, 0]
	let s:curbuffer.inreplyto = [0, 0, 0]

	" The extra stars at the end are for the syntax highlighter to
	" recognize the title. Then the syntax highlighter hides the stars by
	" coloring them the same as the background. It is a bad hack.
	call add(text, title.'*')
	call add(text, repeat('=', strlen(title)).'*')
    else
	" Index of first status will be 1 to match line numbers in timeline
	" display.
	let s:curbuffer.statuses = [0]
	let s:curbuffer.inreplyto = [0]
    endif

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
	call add(s:curbuffer.statuses, status)

	call add(text, sender.": ".s:convert_entity(title).' |'.pubdate.'|')

	let matchcount += 1
    endwhile
    call s:twitter_wintext(text, "timeline")
    let s:curbuffer.buffer = text
endfunction

" Query Twitter Search API and retrieve results
function! s:get_summize(query, page)
    redraw
    echo "Sending search request to Twitter Search..."

    let param = ''

    " Support pagination.
    if a:page > 1
	let param .= 'page='.a:page.'&'
    endif

    " Support count parameter in search results.
    let tcount = s:get_count()
    if tcount > 0
	let param .= 'rpp='.tcount.'&'
    endif

    let url = 'http://search.twitter.com/search.atom?'.param.'q='.s:url_encode(a:query)
    let [error, output] = s:run_curl(url, '', s:get_proxy(), s:get_proxy_login(), {})

    if error != ''
	call s:errormsg("Error querying Twitter Search: ".error)
	return
    endif

    call s:save_buffer()
    let s:curbuffer = {}
    call s:show_summize(output, a:page)
    let s:curbuffer.buftype = "search"

    " Stick the query in here to differentiate between sets of search results.
    let s:curbuffer.user = a:query

    let s:curbuffer.list = ''
    let s:curbuffer.page = a:page
    redraw
    echo "Received search results from Twitter Search."
endfunction

" Prompt user for Twitter Search query string if not entered on command line.
function! s:Summize(query, page)
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

    call s:get_summize(query, a:page)
endfunction

if !exists(":Summize")
    command -range=1 -nargs=? Summize :call <SID>Summize(<q-args>, <count>)
endif
if !exists(":SearchTwitter")
    command -range=1 -nargs=? SearchTwitter :call <SID>Summize(<q-args>, <count>)
endif

let &cpo = s:save_cpo
finish

" vim:set tw=0:
doc/twitvim.txt	[[[1
1785
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
	   OpenSSL................................: |TwitVim-OpenSSL|
	   cURL...................................: |TwitVim-cURL|
	   twitvim.vim............................: |TwitVim-add|
	   twitvim_proxy..........................: |twitvim_proxy|
	   twitvim_proxy_login....................: |twitvim_proxy_login|
	3.1. TwitVim and OAuth....................: |TwitVim-OAuth|
	     twitvim_token_file...................: |twitvim_token_file|
	     twitvim_disable_token_file...........: |twitvim_disable_token_file|
	3.2. identi.ca............................: |TwitVim-identica|
	     twitvim_login........................: |twitvim_login|
	     twitvim_api_root.....................: |twitvim_api_root|
	3.3. Base64-Encoded Login.................: |TwitVim-login-base64|
	     twitvim_login_b64....................: |twitvim_login_b64|
	     twitvim_proxy_login_b64..............: |twitvim_proxy_login_b64|
	3.4. Alternatives to cURL.................: |TwitVim-non-cURL|
	     twitvim_enable_perl..................: |twitvim_enable_perl|
	     twitvim_enable_python................: |twitvim_enable_python|
	     twitvim_enable_ruby..................: |twitvim_enable_ruby|
	     twitvim_enable_tcl...................: |twitvim_enable_tcl|
	3.5. Using Twitter SSL API................: |TwitVim-ssl|
	     Twitter SSL via cURL.................: |TwitVim-ssl-curl|
	     twitvim_cert_insecure................: |twitvim_cert_insecure|
	     Twitter SSL via Perl interface.......: |TwitVim-ssl-perl|
	     Twitter SSL via Ruby interface.......: |TwitVim-ssl-ruby|
	     Twitter SSL via Python interface.....: |TwitVim-ssl-python|
	     Twitter SSL via Tcl interface........: |TwitVim-ssl-tcl|
	3.6. Hide the header in timeline buffer...: |TwitVim-hide-header|
	     twitvim_show_header..................: |twitvim_show_header|
	4. Manual.................................: |TwitVim-manual|
	4.1. Update Commands......................: |TwitVim-update-commands|
	     :PosttoTwitter.......................: |:PosttoTwitter|
	     :CPosttoTwitter......................: |:CPosttoTwitter|
	     :BPosttoTwitter......................: |:BPosttoTwitter|
	     :SendDMTwitter.......................: |:SendDMTwitter|
	4.2. Timeline Commands....................: |TwitVim-timeline-commands|
	     :UserTwitter.........................: |:UserTwitter|
	     twitvim_count........................: |twitvim_count|
	     :FriendsTwitter......................: |:FriendsTwitter|
	     :MentionsTwitter.....................: |:MentionsTwitter|
	     :RepliesTwitter......................: |:RepliesTwitter|
	     :PublicTwitter.......................: |:PublicTwitter|
	     :DMTwitter...........................: |:DMTwitter|
	     :DMSentTwitter.......................: |:DMSentTwitter|
	     :ListTwitter.........................: |:ListTwitter|
	     :RetweetedToMeTwitter................: |:RetweetedToMeTwitter|
	     :RetweetedByMeTwitter................: |:RetweetedByMeTwitter|
	     :BackTwitter.........................: |:BackTwitter|
	     :ForwardTwitter......................: |:ForwardTwitter|
	     :RefreshTwitter......................: |:RefreshTwitter|
	     :NextTwitter.........................: |:NextTwitter|
	     :PreviousTwitter.....................: |:PreviousTwitter|
	     :SetLoginTwitter.....................: |:SetLoginTwitter|
	     :ResetLoginTwitter...................: |:ResetLoginTwitter|
	     :FollowTwitter.......................: |:FollowTwitter|
	     :UnfollowTwitter.....................: |:UnfollowTwitter|
	     :BlockTwitter........................: |:BlockTwitter|
	     :UnblockTwitter......................: |:UnblockTwitter|
	     :ReportSpamTwitter...................: |:ReportSpamTwitter|
	     :AddToListTwitter....................: |:AddToListTwitter|
	     :RemoveFromListTwitter...............: |:RemoveFromListTwitter|
	4.3. Mappings.............................: |TwitVim-mappings|
	     Alt-T................................: |TwitVim-A-t|
	     Ctrl-T...............................: |TwitVim-C-t|
	     Reply Feature........................: |TwitVim-reply|
	     Alt-R................................: |TwitVim-A-r|
	     <Leader>r............................: |TwitVim-Leader-r|
	     Reply to all Feature.................: |TwitVim-reply-all|
	     <Leader>Ctrl-R.......................: |TwitVim-Leader-C-r|
	     Retweet Feature......................: |TwitVim-retweet|
	     <Leader>R............................: |TwitVim-Leader-S-r|
	     Old-style retweets...................: |twitvim_old_retweet|
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
	     User Profiles........................: |TwitVim-profile|
	     <Leader>p............................: |TwitVim-Leader-p|
	     In-reply-to..........................: |TwitVim-inreplyto|
	     <Leader>@............................: |TwitVim-Leader-@|
	     Delete...............................: |TwitVim-delete|
	     <Leader>X............................: |TwitVim-Leader-X|
	     Ctrl-O...............................: |TwitVim-C-o|
	     Ctrl-I...............................: |TwitVim-C-i|
	     Refresh..............................: |TwitVim-refresh|
	     <Leader><Leader>.....................: |TwitVim-Leader-Leader|
	     Next page............................: |TwitVim-next|
	     Ctrl-PageDown........................: |TwitVim-C-PageDown|
	     Previous page........................: |TwitVim-previous|
	     Ctrl-PageUp..........................: |TwitVim-C-PageUp|
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
	     twitvim_bitly_user...................: |twitvim_bitly_user|
	     twitvim_bitly_key....................: |twitvim_bitly_key|
	     :ABitLy..............................: |:ABitLy|
	     :PBitLy..............................: |:PBitLy|
	     :IsGd................................: |:IsGd|
	     :AIsGd...............................: |:AIsGd|
	     :PIsGd...............................: |:PIsGd|
	     :UrlBorg.............................: |:UrlBorg|
	     twitvim_urlborg_key..................: |twitvim_urlborg_key|
	     :AUrlBorg............................: |:AUrlBorg|
	     :PUrlBorg............................: |:PUrlBorg|
	     :Trim................................: |:Trim|
	     twitvim_trim_login...................: |twitvim_trim_login|
	     :ATrim...............................: |:ATrim|
	     :PTrim...............................: |:PTrim|
	     :Cligs...............................: |:Cligs|
	     twitvim_cligs_key....................: |twitvim_cligs_key|
	     :ACligs..............................: |:ACligs|
	     :PCligs..............................: |:PCligs|
	     :Zima................................: |:Zima|
	     :AZima...............................: |:AZima|
	     :PZima...............................: |:PZima|
	     :SearchTwitter.......................: |:SearchTwitter|
	     :RateLimitTwitter....................: |:RateLimitTwitter|
	     :ProfileTwitter......................: |:ProfileTwitter|
	     :LocationTwitter.....................: |:LocationTwitter|
	5. Timeline Highlighting..................: |TwitVim-highlight|
	   twitterUser............................: |hl-twitterUser|
	   twitterTime............................: |hl-twitterTime|
	   twitterTitle...........................: |hl-twitterTitle|
	   twitterLink............................: |hl-twitterLink|
	   twitterReply...........................: |hl-twitterReply|
	6. Tips and Tricks........................: |TwitVim-tips|
	6.1. Timeline Hotkeys.....................: |TwitVim-hotkeys|
	6.2. Switching between identi.ca users....: |TwitVim-switch|
	6.3. Line length in status line...........: |TwitVim-line-length|
	7. History................................: |TwitVim-history|
	8. Credits................................: |TwitVim-credits|


==============================================================================
2. Introduction						*TwitVim-intro*

	TwitVim is a plugin that allows you to post to Twitter, a
	microblogging service at http://www.twitter.com.

	Since version 0.2.19, TwitVim also supports other microblogging
	services, such as identi.ca, that offer Twitter-compatible APIs. See
	|TwitVim-identica| for information on configuring TwitVim for those
	services.


==============================================================================
3. Installation						*TwitVim-install*

	
	Note: These instructions are for configuring TwitVim for use with
	Twitter. If you intend to use TwitVim only with identi.ca, see
	|TwitVim-identica|.


	1. Install OpenSSL or compile Vim with |Python|, |Perl|, |Ruby|, or |Tcl|.

	In order to compute HMAC-SHA1 digests and sign Twitter OAuth requests,
	TwitVim needs to either run the openssl command line tool from the
	OpenSSL toolkit or call a HMAC-SHA1 digest function from one of the
	above scripting interfaces.

	
							*TwitVim-OpenSSL*
	If you are using a precompiled Vim executable and do not wish to
	recompile Vim to add a scripting interface, then the OpenSSL approach
	is the simplest.

	If OpenSSL is not already on your system, you can download it from
	http://openssl.org/  If you are using Windows, check the OpenSSL FAQ
	for a link to a precompiled OpenSSL for Windows.

	After installing OpenSSL, make sure that the directory where the
	openssl executable resides is listed in your PATH environment
	variable so that TwitVim can find it.

	Note: TwitVim uses the openssl -hmac option, which is not available in
	old versions of OpenSSL. I recommend updating to OpenSSL 0.9.8o,
	1.0.0a, or later to get the -hmac option and the latest security
	fixes.


	Instead of using the openssl command line tool, you can also have
	TwitVim compute HMAC-SHA1 digests via a Vim scripting interface. This
	approach is significantly faster because it does not need to run an
	external program. You can use Perl, Python, Ruby, or Tcl.


	If you compiled Vim with Perl, add the following to your vimrc:
>
		let twitvim_enable_perl = 1
<
	Also, verify that your Perl installation has the Digest::HMAC_SHA1
	module. This module comes standard in some Perl distributions, e.g.
	ActivePerl. In other Perl setups, you'll need to download and install
	Digest::HMAC_SHA1 from CPAN. The Perl Package Manager PPM may be
	helpful here.


	If you compiled Vim with Python, add the following to your vimrc:
>
		let twitvim_enable_python = 1
<
	Also, verify that your Python installation has the base64, hashlib,
	and hmac modules. All of these are in the Python standard library as
	of Python 2.5.


	If you compiled Vim with Ruby, add the following to your vimrc:
>
		let twitvim_enable_ruby = 1
<
	TwitVim requires the openssl and base64 modules, both of which are
	in the Ruby standard library. However, you may need to install the
	OpenSSL library from http://www.openssl.org if it is not already on
	your system.


	If you compiled Vim with Tcl, add the following to your vimrc:
>
		let twitvim_enable_tcl = 1
<
	Also, verify that your Tcl installation has the base64 and sha1
	packages. These packages are in the Tcllib library. See
	|twitvim_enable_tcl| for help on obtaining and installing this
	library.


	2. Install cURL.				*TwitVim-cURL*

	If you don't already have cURL on your system, download it from
	http://curl.haxx.se/. Make sure that the curl executable is in a
	directory listed in your PATH environment variable, or the equivalent
	for your system.

	If you have already compiled Vim with Perl, Python, Ruby, or Tcl for
	Step 1, I recommend that you use the scripting interface instead of
	installing cURL. See |TwitVim-non-cURL| for setup details. Using a
	scripting interface for network I/O is faster because it avoids the
	overhead of running an external program.


	3. twitvim.vim					*TwitVim-add*

	Add twitvim.vim to your plugins directory. The location depends on
	your operating system. See |add-global-plugin| for details.

	If you installed from the Vimball (.vba) file, twitvim.vim should
	already be in its correct place.


	4. twitvim_proxy				*twitvim_proxy*

	This step is only needed if you access the web through a HTTP proxy.
	If you use a HTTP proxy, add the following to your vimrc:
>
		let twitvim_proxy = "proxyserver:proxyport"
<
	Replace proxyserver with the address of the HTTP proxy and proxyport
	with the port number of the HTTP proxy.


	5. twitvim_proxy_login				*twitvim_proxy_login*

	If the HTTP proxy requires authentication, add the following to your
	vimrc:
>
		let twitvim_proxy_login = "proxyuser:proxypassword"
<
	Where proxyuser is your proxy user and proxypassword is your proxy
	password.

	It is possible to avoid having your proxy password in plaintext in
	your vimrc. See |TwitVim-login-base64| for details.


	6. Set twitvim_browser_cmd.

	In order to log in with Twitter OAuth, TwitVim needs to launch your
	web browser and bring up the Twitter authentication web page.

	See |twitvim_browser_cmd| for details. For example, if you use Firefox
	under Windows, add the following to your vimrc:
>
		let twitvim_browser_cmd = 'firefox.exe'
<
	Note: If you do not set up twitvim_browser_cmd, TwitVim will display
	the authentication URL and wait for you to visit it in your browser
	manually and approve the application. If possible, this auth URL
	will be shortened with is.gd or Bit.ly for ease of entry.


	7. Sign into Twitter with OAuth.

	Use any TwitVim command that requires authentication. For example,
	run |:FriendsTwitter|. |:SetLoginTwitter| is the normal way to
	initiate authentication without running a timeline command.

	Since TwitVim does not yet have an OAuth access token, it will
	initiate the Twitter OAuth handshake. Then it'll launch your web
	browser to a Twitter web page that says "An application would like to
	connect to your account". On this page, sign in, if necessary, and
	then click on "Allow" to allow TwitVim access to your Twitter account.

	Twitter will then report that you have granted access to TwitVim and
	display a 7-digit PIN. Copy the PIN and paste it to the TwitVim input
	prompt "Enter Twitter OAuth PIN:".

	
	And now you are ready to use TwitVim.


------------------------------------------------------------------------------
3.1. TwitVim and OAuth					*TwitVim-OAuth*

	After you log into Twitter with OAuth, TwitVim stores the OAuth access
	token in a file so that you won't have to log in again when you
	restart TwitVim. By default, this file is $HOME/.twitvim.token

						*twitvim_token_file*
	You can change the name and location of this token file by setting
	twitvim_token_file in your vimrc. For example:
>
		let twitvim_token_file = "/etc/.twitvim.token"
<
	Since the access token grants full access to your Twitter account, it
	is recommended that you place the token file in a directory that is
	not readable or accessible by other users.


						*twitvim_disable_token_file*
	If you are using TwitVim on an insecure system, you may prefer to 
	not save access tokens at all. To turn off the token file, add
	the following to your vimrc:
>
		let twitvim_disable_token_file = 1
<
	If the token file is disabled, TwitVim will initiate an OAuth
	handshake every time you restart it.


	If TwitVim is logged into Twitter and you need to log in as a
	different Twitter user, use either the |:SetLoginTwitter| or
	|:ResetLoginTwitter| commands to discard the access token. When the
	Twitter authentication web page comes up, use the "Sign Out" link on
	that page to switch Twitter users.


------------------------------------------------------------------------------
3.2. identi.ca						*TwitVim-identica*

	identi.ca offers a Twitter-compatible API so you can use TwitVim with
	identi.ca. Setting up TwitVim for identi.ca is a bit different from
	setting up TwitVim for Twitter because identi.ca uses Basic
	authentication instead of OAuth.

	1. Install cURL

	See |TwitVim-cURL|.


	2. twitvim_login				*twitvim_login*

	Add the following to your vimrc:
>
		let twitvim_login = "USER:PASS"
<
	Replace USER with your Twitter user name and PASS with your Twitter
	password.

	It is possible to avoid having your Twitter password in plaintext in
	your vimrc. See |TwitVim-login-base64| for details.


	3. Set up proxy info, if necessary.

	See |twitvim_proxy| and |twitvim_proxy_login|.


	4. Set twitvim_api_root				*twitvim_api_root*

	This setting allows you to configure TwitVim to communicate with
	servers other than twitter.com that implement a Twitter-compatible
	API.

	For identi.ca, add the following to your vimrc:
>
		let twitvim_api_root = "http://identi.ca/api"
<

	And now you are ready to use TwitVim with identi.ca.


------------------------------------------------------------------------------
3.3. Base64-Encoded Login				*TwitVim-login-base64*

	For safety purposes, TwitVim allows you to configure your login and
	proxy login information preencoded in base64. This is not truly secure
	as it is not encryption but it can stop casual onlookers from reading
	off your password when you edit your vimrc.

						*twitvim_login_b64*
	To do that, set the following in your vimrc:
>
		let twitvim_login_b64 = "base64string"
<
	Note: This is only for identi.ca and other services that use Basic
	authentication. Twitter uses OAuth, so you do not need to add Twitter
	login info to your vimrc.

	
						*twitvim_proxy_login_b64*
	If your HTTP proxy needs authentication, set the following:
>
		let twitvim_proxy_login_b64 = "base64string"
<
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
>
		let twitvim_login_b64 = "am9ldXNlcjpqb2VwYXNzd29yZA=="
<
	And his setup is ready.


------------------------------------------------------------------------------
3.4. Alternatives to cURL				*TwitVim-non-cURL*

	TwitVim supports http networking through Vim's |Perl|, |Python|,
	|Ruby|, and |Tcl| interfaces, so if you have any of those interfaces
	compiled into your Vim program, you can use that instead of cURL.
	
	Generally, it is slightly faster to use one of those scripting
	interfaces for networking because it avoids running an external
	program. On Windows, it also avoids a brief taskbar flash when cURL
	runs.

	To find out if you have those interfaces, use the |:version| command
	and check the |+feature-list|. Then to enable this special http
	networking code in TwitVim, add one of the following lines to your
	vimrc:
>
		let twitvim_enable_perl = 1
		let twitvim_enable_python = 1
		let twitvim_enable_ruby = 1
		let twitvim_enable_tcl = 1
<
	You can enable more than one scripting language but TwitVim will only
	use the first one it finds.


	1. Perl interface				*twitvim_enable_perl*

	To enable TwitVim's Perl networking code, add the following to your
	vimrc:
>
		let twitvim_enable_perl = 1
<
	TwitVim requires the MIME::Base64 and LWP::UserAgent modules. If you
	have ActivePerl, these modules are included in the default
	installation.


	2. Python interface				*twitvim_enable_python*

	To enable TwitVim's Python networking code, add the following to your
	vimrc:
>
		let twitvim_enable_python = 1
<
	TwitVim requires the urllib, urllib2, and base64 modules. These
	modules are in the Python standard library.


	3. Ruby interface				*twitvim_enable_ruby*

	To enable TwitVim's Ruby networking code, add the following to your
	vimrc:
>
		let twitvim_enable_ruby = 1
<
	TwitVim requires the net/http, uri, and Base64 modules. These modules
	are in the Ruby standard library.

	In addition, if using the Ruby interface, TwitVim requires Vim
	7.2.360 or later to fix an if_ruby problem with Windows sockets.

	Alternatively, you can add the following patch to the Vim sources:

	http://www.mail-archive.com/vim_dev@googlegroups.com/msg03693.html

	See also Bram's correction to the patch:

	http://www.mail-archive.com/vim_dev@googlegroups.com/msg03713.html


	3. Tcl interface				*twitvim_enable_tcl*

	To enable TwitVim's Tcl networking code, add the following to your
	vimrc:
>
		let twitvim_enable_tcl = 1
<
	TwitVim requires the http, uri, and base64 packages. uri and base64
	are in the Tcllib library so you may need to install that. See
	http://tcllib.sourceforge.net/

	If you have ActiveTcl 8.5, the default installation does not include
	Tcllib. Run the following command from the shell to add Tcllib:
>
		teacup install tcllib85
<

------------------------------------------------------------------------------
3.5. Using Twitter SSL API				*TwitVim-ssl*

	For added security, TwitVim can use the Twitter SSL API instead of the
	regular Twitter API. You configure this by setting |twitvim_api_root|
	to the https version of the URL:
>
		let twitvim_api_root = "https://api.twitter.com/1"
<
	For identi.ca:
>
		let twitvim_api_root = "https://identi.ca/api"
<
	There are certain pre-requisites, as explained below.


	1. Twitter SSL via cURL				*TwitVim-ssl-curl*

	To use SSL via cURL, you need to install the SSL libraries and an
	SSL-enabled build of cURL.

							*twitvim_cert_insecure*
	Even after you've done that, cURL may complain about certificates that
	failed verification. If you need to override certificate checking, set
	twitvim_cert_insecure:
>
		let twitvim_cert_insecure = 1
<

	2. Twitter SSL via Perl interface		*TwitVim-ssl-perl*

	To use SSL via the TwitVim Perl interface (See |twitvim_enable_perl|),
	you need to install the SSL libraries and the Crypt::SSLeay Perl
	module.

	If you are using Twitter SSL over a proxy, do not set twitvim_proxy
	and twitvim_proxy_login. Crypt::SSLeay gets proxy information from
	the environment, so do this instead:
>
		let $HTTPS_PROXY="http://proxyserver:proxyport"
		let $HTTPS_PROXY_USERNAME="user"
		let $HTTPS_PROXY_PASSWORD="password"
<
	Alternatively, you can set those environment variables before starting
	Vim.


	3. Twitter SSL via Ruby interface		*TwitVim-ssl-ruby*

	To use SSL via Ruby, you need to install the SSL libraries and an
	SSL-enabled build of Ruby.

	If Ruby produces the error "`write': Bad file descriptor" in http.rb,
	then you need to check your certificates or override certificate
	checking. See |twitvim_cert_insecure|.

	Set twitvim_proxy and twitvim_proxy_login as usual if using Twitter
	SSL over a proxy.


	4. Twitter SSL via Python interface		*TwitVim-ssl-python*

	To use SSL via Python, you need to install the SSL libraries and an
	SSL-enabled build of Python.

	The Python interface does not yet support Twitter SSL over a proxy.
	This is due to a missing feature in urllib2.


	5. Twitter SSL via Tcl interface		*TwitVim-ssl-tcl*

	To use SSL via Tcl, you need to install the SSL libraries and Tcllib.
	To be more specific, TwitVim needs the tls package from Tcllib.

	All known versions of Vim (up to 7.3f beta, as of this writing) have a
	bug that prevents the tls package from being loaded if you compile Vim
	with Tcl 8.5. This discussion thread explains the problem:
>
	http://objectmix.com/tcl/15892-tcl-interp-inside-vim-throws-error-w-clock-format.html
<
	If you need to use Twitter SSL with the Tcl interface, you can try one
	of the following workarounds:

	a. Downgrade to Tcl 8.4.
	b. Edit if_tcl.c in the Vim source code to remove the redefinition of
	catch. Then rebuild Vim.


------------------------------------------------------------------------------
3.6. Hide the header in timeline buffer			*TwitVim-hide-header*

	In a timeline buffer, the first two lines are header lines. The first
	line tells you the type of buffer it is (e.g. friends, user, replies,
	direct messages, search) and other relevant buffer information. (e.g.
	user name, search terms, page number) The second line is a separator
	line.

	If you wish to suppress the header display, set twitvim_show_header
	to 0:

							*twitvim_show_header*
>
		let twitvim_show_header = 0
<
	If twitvim_show_header is unset, it defaults to 1, i.e. show the
	header.

	Note: Setting twitvim_show_header does not change the timeline buffer
	immediately. Use |:RefreshTwitter| to refresh the timeline to see the
	effect. Also, twitvim_show_header does not retroactively alter
	previous timelines in the timeline stack.


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

	:SendDMTwitter {username}			*:SendDMTwitter*

	This command will prompt you for a direct message to send to user
	{username}.

	Note: If you get a "403 Forbidden" error when you try to send a direct
	message, check if the user you're messaging is following you. That is
	the most common reason for this error when sending a direct message.

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

							*twitvim_count*
	You can configure the number of tweets returned by :UserTwitter by
	setting twitvim_count. For example,
>
		let twitvim_count = 50
<
	will make :UserTwitter return 50 tweets instead of the default of 20.
	You can set twitvim_count to any integer from 1 to 200.


	:[count]FriendsTwitter				*:FriendsTwitter*

	This command displays your Twitter timeline with updates from friends
	merged in.

	If you specify [count], that number is used as the page number. For
	example, :2FriendsTwitter displays the second page from your friends
	timeline.

	You can configure the number of tweets returned by :FriendsTwitter by
	setting |twitvim_count|.


	:[count]MentionsTwitter				*:MentionsTwitter*
	:[count]RepliesTwitter				*:RepliesTwitter*

	This command displays a timeline of mentions (updates containing
	@username) that you've received from other Twitter users.

	If you specify [count], that number is used as the page number. For
	example, :2MentionsTwitter displays the second page from your mentions
	timeline.

	:RepliesTwitter is the old name for :MentionsTwitter.

	You can configure the number of tweets returned by :MentionsTwitter by
	setting |twitvim_count|.


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


	:[count]ListTwitter {list}			*:ListTwitter*
	:[count]ListTwitter {user} {list}

	This command displays a Twitter list timeline.

	In the first form, {user} is assumed to be you so the command will
	display a list of yours named {list}.

	In the second form, the command displays list {list} from user
	{user}.

	If you specify [count], that number is used as the page number. For
	example, :2ListTwitter list1 displays the second page from the list1
	list timeline.


	:[count]RetweetedToMeTwitter			*:RetweetedToMeTwitter*

	This command displays a timeline of retweets by others to you.

	If you specify [count], that number is used as the page number. For
	example, :2RetweetedToMeTwitter displays the second page from the
	retweets timeline.


	:[count]RetweetedByMeTwitter			*:RetweetedByMeTwitter*

	This command displays a timeline of retweets by you.

	If you specify [count], that number is used as the page number. For
	example, :2RetweetedByMeTwitter displays the second page from the
	retweets timeline.


	:BackTwitter					*:BackTwitter*

	This command takes you back to the previous timeline in the timeline
	stack. TwitVim saves a limited number of timelines. This command
	will display a warning if you attempt to go beyond the oldest saved
	timeline. See |TwitVim-C-o|.


	:ForwardTwitter					*:ForwardTwitter*

	This command takes you to the next timeline in the timeline stack.
	It will display a warning if you attempt to go past the newest saved
	timeline so this command can only be used after :BackTwitter.
	See |TwitVim-C-i|.


	:RefreshTwitter					*:RefreshTwitter*

	This command refreshes the timeline. See |TwitVim-Leader-Leader|.


	:NextTwitter					*:NextTwitter*

	This command loads the next (older) page in the timeline.
	See |TwitVim-C-PageDown|.


	:PreviousTwitter				*:PreviousTwitter*

	This command loads the previous (newer) page in the timeline. If the
	timeline is on the first page, it issues a warning and doesn't do
	anything. See |TwitVim-C-PageUp|.


	:SetLoginTwitter				*:SetLoginTwitter*

	This command discards the OAuth access token and initiates an OAuth
	handshake. Use this command if you need to log out and log in as
	another Twitter user. When the Twitter authentication web page comes
	up, use the "Sign Out" link to log in as a different Twitter user and
	grant TwitVim access to that user.

	This command has no effect in identi.ca and other Twitter-compatible
	services that use Basic authentication.


	:ResetLoginTwitter				*:ResetLoginTwitter*

	This command discards the OAuth access token. The next TwitVim command
	that needs Twitter authentication will initiate an OAuth handshake.

	This command has no effect in identi.ca and other Twitter-compatible
	services that use Basic authentication.


	:FollowTwitter {username}			*:FollowTwitter*

	Start following user {username}'s timeline. If the user's timeline is
	protected, this command makes a request to follow that user.

	Note: This command does not enable notifications for the target user.
	If you need that, you'll have to do that separately through the web
	interface.


	:UnfollowTwitter {username}			*:UnfollowTwitter*

	Stop following user {username}'s timeline.


	:BlockTwitter {username}			*:BlockTwitter*

	Block user {username}.


	:UnblockTwitter {username}			*:UnblockTwitter*

	Unblock user {username}.


	:ReportSpamTwitter {username}			*:ReportSpamTwitter*

	Reports user {username} for spam. This command will also block the
	user.


	:AddToListTwitter {listname} {username}		*:AddToListTwitter*

	Adds user {username} to list {listname}.


	:RemoveFromListTwitter {listname} {username}	*:RemoveFromListTwitter*

	Removes user {username} from list {listname}.


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


							*TwitVim-reply-all*
	<Leader>Ctrl-R					*TwitVim-Leader-C-r*

	This mapping is local to the timeline buffer. It starts composing a
	reply to all, i.e. a reply to the tweet author and also to everyone
	mentioned in @-replies on the current line.


							*TwitVim-retweet*
	<Leader>R					*TwitVim-Leader-S-r*

	This mapping (Note: uppercase 'R' instead of 'r'.) is local to the
	timeline buffer. It is similar to the retweet feature in popular
	Twitter clients. In the timeline buffer, it retweets the current line.


							*twitvim_old_retweet*
	If you prefer old-style retweets, add this to your vimrc:
>
		let twitvim_old_retweet = 1
<	
	The difference is an old-style retweet does not use the retweet API.
	Instead, it copies the current line to the command line so that you
	can repost it as a new tweet and optionally edit it or add your own
	comments. Note that an old-style retweet may end up longer than 140
	characters. If you have problems posting a retweet, try editing it to
	make it shorter.

						    *twitvim_retweet_format*
	If you use old-style retweets, you can configure the retweet format.
	By default, TwitVim retweets tweets in the following format:

		RT @user: text of the tweet

	You can customize the retweet format by adding the following to your
	vimrc, for example:
>
		let twitvim_retweet_format = 'Retweet from %s: %t'

		let twitvim_retweet_format = '%t (retweeted from %s)'
<
	When you retweet a tweet, TwitVim will replace "%s" in
	twitvim_retweet_format with the user name of the original poster and
	"%t" with the text of the tweet.

	The default setting of twitvim_retweet_format is "RT %s: %t"


							*TwitVim-direct-message*
	Alt-D						*TwitVim-A-d*
	<Leader>d					*TwitVim-Leader-d*

	This mapping is local to the timeline buffer. In the timeline buffer,
	it starts composing a direct message on the command line to the author
	of the tweet on the current line.

	Under Cygwin, Alt-D is not recognized so you can use <Leader>d as an
	alternative. The <Leader> character defaults to \ (backslash) but see
	|mapleader| for information on customizing that.

	Note: If you get a "403 Forbidden" error when you try to send a direct
	message, check if the user you're messaging is following you. That is
	the most common reason for this error when sending a direct message.


							*TwitVim-goto*
	Alt-G						*TwitVim-A-g*
	<Leader>g					*TwitVim-Leader-g*

	This mapping is local to the timeline and user profile buffers. It
	launches the web browser with the URL at the cursor position. If you
	visually select text before invoking this mapping, it launches the web
	browser with the selected text as is.

	As a special case, if the cursor is on a word of the form @user or
	user:, TwitVim will display that user's timeline in the timeline
	buffer. This will not launch the web browser.

	In addition, if the cursor is on a word of the form #hashtag, TwitVim
	will do a Twitter Search for that #hashtag. This too will not launch
	the web browser.

							*twitvim_browser_cmd*
	Before using this command, you need to tell TwitVim how to launch your
	browser. For example, you can add the following to your vimrc:
>
		let twitvim_browser_cmd = 'firefox.exe'
<
	Of course, replace firefox.exe with the browser of your choice.


							*TwitVim-LongURL*
	<Leader>e					*TwitVim-Leader-e*

	This mapping is local to the timeline and user profile buffers. It
	calls the LongURL API (see http://longurl.org/) to expand the short
	URL at the cursor position. A short URL is a URL from a URL shortening
	service such as TinyURL, SnipURL, etc. Use this feature if you wish to
	preview a URL before browsing to it with |TwitVim-goto|.

	If you visually select text before invoking this mapping, it calls the
	LongURL API with the selected text as is.

	If successful, TwitVim will display the result from LongURL in the
	message area.


							*TwitVim-profile*
	<Leader>p					*TwitVim-Leader-p*

	This mapping is local to the timeline and user profile buffers. It
	calls the Twitter API to retrieve user profile information (e.g. name,
	location, bio, update count) for the user name at the cursor position.
	It displays the information in a user profile buffer.

	If you visually select text before invoking this mapping, it uses the
	selected text as is for the user name.

	See also |:ProfileTwitter|.


							*TwitVim-inreplyto*
	<Leader>@					*TwitVim-Leader-@*

	This mapping is local to the timeline buffer. If the current line is
	an @-reply tweet, it calls the Twitter API to retrieve the tweet to
	which this one is replying. Then it will display that predecessor
	tweet below the current one.
	
	If there is no in-reply-to information, it will show a warning and do
	nothing.

	This mapping is useful in the replies timeline. See |:RepliesTwitter|.


							*TwitVim-delete*
	<Leader>X					*TwitVim-Leader-X*

	This mapping is local to the timeline buffer. The 'X' in the mapping
	is uppercase. It calls the Twitter API to delete the tweet or message
	on the current line.

	Note: You have to be the author of the tweet in order to delete it.
	You can delete direct messages that you sent or received.


	Ctrl-O						*TwitVim-C-o*

	This mapping takes you to the previous timeline in the timeline stack.
	See |:BackTwitter|.

	Ctrl-I						*TwitVim-C-i*

	This mapping takes you to the next timeline in the timeline stack.
	See |:ForwardTwitter|.


							*TwitVim-refresh*
	<Leader><Leader> 				*TwitVim-Leader-Leader*

	This mapping refreshes the timeline. See |:RefreshTwitter|.


							*TwitVim-next*
	Ctrl-PageDown					*TwitVim-C-PageDown*

	This mapping loads the next (older) page in the timeline.
	See |:NextTwitter|.

	
							*TwitVim-previous*
	Ctrl-PageUp					*TwitVim-C-PageUp*

	This command loads the previous (newer) page in the timeline. If the
	timeline is on the first page, it issues a warning and doesn't do
	anything. See |:PreviousTwitter|.


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
	http://bit.ly/

	This command calls the bit.ly API to get a short URL in place of
	<url>. If {url} is not provided on the command line, the command will
	prompt you to enter a URL. The short URL is then inserted into the
	current buffer at the current position.

	The bit.ly API requires a bit.ly login and a bit.ly API key. A default
	login and key pair is provided with TwitVim and no configuration is
	needed. However, if you wish to supply your own login and key to track
	your bit.ly history and stats, visit
	http://bit.ly/account/your_api_key to retrieve your API info and add
	the following to your vimrc:

							*twitvim_bitly_user*
							*twitvim_bitly_key*
>
		let twitvim_bitly_user = "username"
		let twitvim_bitly_key = "R_123456789"
<

	Replace username with your bit.ly login and R_123456789 with your
	bit.ly API key.

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
	with TwitVim and no configuration is needed. However, if you wish to
	supply your own key in order to track your urlBorg history and stats,
	visit http://urlborg.com/a/account/ to retrieve your API key and then
	add the following to your vimrc:

							*twitvim_urlborg_key*
>
		let twitvim_urlborg_key = "12345-6789"
<
	Replace 12345-6789 with your API key.

	:AUrlBorg					*:AUrlBorg*
	:AUrlBorg {url}

	Same as :UrlBorg but appends, i.e. inserts after the current position
	instead of at the current position, the short URL instead.

	:PUrlBorg					*:PUrlBorg*
	:PUrlBorg {url}
	
	Same as :UrlBorg but prompts for a tweet on the command line with the
	short URL already inserted.


	:Trim						*:Trim*
	:Trim {url}

	tr.im is a URL forwarding and shortening service. See http://tr.im/

	This command calls the tr.im API to get a short URL in place of
	<url>. If {url} is not provided on the command line, the command will
	prompt you to enter a URL. The short URL is then inserted into the
	current buffer at the current position.

	If you login to the tr.im API, tr.im will keep track
	of URLs that you have shortened. In order to do that, add the
	following to your vimrc:

							*twitvim_trim_login*
>
		let twitvim_trim_login = "trimuser:trimpassword"
<
	Where trimuser and trimpassword are your tr.im account user name and
	password.

	You may also specify trimuser:trimpassword as a base64 encoded string:
>
		let twitvim_trim_login = "base64string"
<
	See |TwitVim-login-base64| for information on generating base64
	strings.

	:ATrim						*:ATrim*
	:ATrim {url}

	Same as :Trim but appends, i.e. inserts after the current position
	instead of at the current position, the short URL instead.

	:PTrim						*:PTrim*
	:PTrim {url}
	
	Same as :Trim but prompts for a tweet on the command line with the
	short URL already inserted.


	:Cligs						*:Cligs*
	:Cligs {url}

	Cligs is a URL forwarding and shortening service. See http://cli.gs/

	This command calls the Cligs API to get a short URL in place of
	<url>. If {url} is not provided on the command line, the command will
	prompt you to enter a URL. The short URL is then inserted into the
	current buffer at the current position.

	If you supply a Cligs API key, Cligs will keep track of URLs that you
	have shortened. In order to do that, add the following to your vimrc:

							*twitvim_cligs_key*
>
		let twitvim_cligs_key = "hexstring"
<
	where hexstring is the API key. You can get an API key by registering
	for a user account at Cligs and then visiting http://cli.gs/user/api

	:ACligs						*:ACligs*
	:ACligs {url}

	Same as :Cligs but appends, i.e. inserts after the current position
	instead of at the current position, the short URL instead.

	:PCligs						*:PCligs*
	:PCligs {url}
	
	Same as :Cligs but prompts for a tweet on the command line with the
	short URL already inserted.


	:Zima						*:Zima*
	:Zima {url}

	Zima is a URL forwarding and shortening service. See http://zi.ma/

	This command calls the Zi.ma API to get a short URL in place of
	<url>. If {url} is not provided on the command line, the command will
	prompt you to enter a URL. The short URL is then inserted into the
	current buffer at the current position.

	:AZima						*:AZima*
	:AZima {url}

	Same as :Zima but appends, i.e. inserts after the current position
	instead of at the current position, the short URL instead.

	:PZima						*:PZima*
	:PZima {url}
	
	Same as :Zima but prompts for a tweet on the command line with the
	short URL already inserted.


	:[count]SearchTwitter					*:SearchTwitter*
	:[count]SearchTwitter {query}
	
	This command calls the Twitter Search API to search for {query}. If
	{query} is not provided on the command line, the command will prompt
	you for it. Search results are then displayed in the timeline buffer.

	All of the Twitter Search operators are supported implicitly. See
	http://search.twitter.com/operators for a list of search operators.

	If you specify [count], that number is used as the page number. For
	example, :2SearchTwitter hello displays the second page of search
	results for the word hello.

	You can configure the number of tweets returned by :SearchTwitter by
	setting |twitvim_count|.


	:RateLimitTwitter				*:RateLimitTwitter*

	This command calls the Twitter API to retrieve rate limit information.
	It shows the current hourly limit, how many API calls you have
	remaining, and when your quota will be reset. You can use it to check
	if you have been temporarily locked out of Twitter for hitting the
	rate limit. This command does not work on identi.ca.


	:ProfileTwitter {username}			*:ProfileTwitter*

	This command calls the Twitter API to retrieve user profile
	information (e.g. name, location, bio, update count) for the specified
	user. It displays the information in a user profile buffer.

	See also |TwitVim-Leader-p|.


	:LocationTwitter {location}			*:LocationTwitter*

	This command calls the Twitter API to set the location field in your
	profile. There is no mandatory format for the location. It could be a
	zip code, a town, coordinates, or pretty much anything.

	For example:
>
	:LocationTwitter 10027
	:LocationTwitter New York, NY, USA
	:LocationTwitter 40.811583, -73.954486
<

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
>
		nnoremap <F8> :FriendsTwitter<cr>
		nnoremap <S-F8> :UserTwitter<cr>
		nnoremap <A-F8> :RepliesTwitter<cr>
		nnoremap <C-F8> :DMTwitter<cr>
<

------------------------------------------------------------------------------
6.2. Switching between identi.ca users			*TwitVim-switch*

	If you have multiple user accounts on identi.ca, you can add something
	like to following to your vimrc to make it easy to switch between
	those accounts:
>
		function! Switch_to_identica_user1()
		    let g:twitvim_api_root = "http://identi.ca/api"

		    let g:twitvim_login = "logininfo1"

		    FriendsTwitter
		endfunction

		function! Switch_to_identica_user2()
		    let g:twitvim_api_root = "http://identi.ca/api"

		    let g:twitvim_login = "logininfo2"

		    FriendsTwitter
		endfunction

		command! ToIdentica1 :call Switch_to_identica_user1()
		command! ToIdentica2 :call Switch_to_identica_user2()
<	
	With that in place, you can use :ToIdentica1 and :ToIdentica2 to
	switch between user accounts. There is a call to |:FriendsTwitter| at
	the end of each function to refresh the timeline view after switching.

	Note: This won't work on Twitter because logging in is handled via
	OAuth. Use |:SetLoginTwitter| or |:ResetLoginTwitter| instead to
	switch to a different Twitter user account.


------------------------------------------------------------------------------
6.3. Line length in status line				*TwitVim-line-length*

	Add the following to your |'statusline'| to display the length of the
	current line:
>
		%{strlen(getline('.'))}
<	
	This is useful if you compose tweets in a separate buffer and post
	them with |:CPosttoTwitter|. With the line length in your status line,
	you will know when you've reached the 140-character boundary.


==============================================================================
7. TwitVim History					*TwitVim-history*

	0.5.5 : 2010-08-16 * Added support for computing HMAC-SHA1 digests
			     using the openssl command line tool from the
			     OpenSSL toolkit. |TwitVim-OpenSSL|
	0.5.4 : 2010-08-11 * Added Ruby and Tcl versions of HMAC-SHA1 digest
			     code.
			   * Improved error messages for cURL users.
			   * Fix to keep |'nomodifiable'| setting from leaking
			     out into other buffers.
			   * Support Twitter SSL via Tcl interface.
			     |TwitVim-ssl-tcl|
	0.5.3 : 2010-06-23 * Improved error messages for most commands if 
			     using Perl, Python, Ruby, or Tcl interfaces.
			   * Added |:FollowTwitter|, |:UnfollowTwitter|,
			     |:BlockTwitter|, |:UnblockTwitter|,
			     |:ReportSpamTwitter|, |:AddToListTwitter|,
			     |:RemoveFromListTwitter|.
	0.5.2 : 2010-06-22 * More fixes for Twitter OAuth.
	0.5.1 : 2010-06-19 * Shorten auth URL with is.gd/Bitly if we need
			     to ask the user to visit it manually.
			   * Fixed the |:PublicTwitter| invalid request error.
			   * Include new-style retweets in user timeline.
	0.5.0 : 2010-06-16 * Switched to OAuth for user authentication on 
			     Twitter. |TwitVim-OAuth|
			   * Improved |:ProfileTwitter|.
	0.4.7 : 2010-03-13 * Added |:MentionsTwitter| as an alias for
			     |:RepliesTwitter|.
			   * Support |twitvim_count| in |:MentionsTwitter|.
			   * Fixed |twitvim_count| bug in |:ListTwitter|.
			   * Fixed Ruby interface problem with
			     Vim patch 7.2.374.
			   * Fixed |:BackTwitter| behavior when timeline
			     window is hidden by user.
			   * Handle SocketError exception in Ruby code.
	0.4.6 : 2010-02-05 * Added option to hide header in timeline buffer.
			     |TwitVim-hide-header|
	0.4.5 : 2009-12-20 * Prompt for login info if not configured.
			     |:SetLoginTwitter| |:ResetLoginTwitter|
			   * Reintroduced old-style retweet via
			     |twitvim_old_retweet|.
	0.4.4 : 2009-12-13 * Upgraded bit.ly API support to version 2.0.1
			     with configurable user login and key.
			   * Added support for Zima. |:Zima|
			   * Fixed :BackTwitter behavior when browsing
			     multiple lists.
			   * Added support for displaying retweets in
			     friends timeline.
			   * Use Twitter Retweet API to retweet.
			   * Added commands to display retweets to you or
			     by you. |:RetweetedToMeTwitter|
			     |:RetweetedByMeTwitter|
	0.4.3 : 2009-11-27 * Fixed some minor breakage in LongURL support.
			   * Added |:ListTwitter|
			   * Omit author's name from the list when doing a
			     reply to all. |TwitVim-reply-all|
	0.4.2 : 2009-06-22 * Bugfix: Reset syntax items in Twitter window.
			   * Bugfix: Show progress message before querying
			     for in-reply-to tweet.
			   * Added reply to all feature. |TwitVim-reply-all|
	0.4.1 : 2009-03-30 * Fixed a problem with usernames and search terms
			     that begin with digits.
	0.4.0 : 2009-03-09 * Added |:SendDMTwitter| to send direct messages
			     through API without relying on the "d user ..."
			     syntax.
			   * Modified Alt-D mapping in timeline to use
			     the :SendDMTwitter code.
			   * Added |:BackTwitter| and |:ForwardTwitter|
			     commands, Ctrl-O and Ctrl-I mappings to move back
			     and forth in the timeline stack.
			   * Improvements in window handling. TwitVim commands
			     will restore the cursor to the original window
			     when possible.
			   * Wrote some notes on using TwitVim with Twitter
			     SSL API.
			   * Added mapping to show predecessor tweet for an
			     @-reply. |TwitVim-inreplyto|
			   * Added mapping to delete a tweet or message.
			     |TwitVim-delete|
			   * Added commands and mappings to refresh the
			     timeline and load the next or previous page.
			     |TwitVim-refresh|, |TwitVim-next|,
			     |TwitVim-previous|.
	0.3.5 : 2009-01-30 * Added support for pagination and page length to
			     :SearchTwitter.
			   * Shortened default retweet prefix to "RT".
	0.3.4 : 2008-11-11 * Added |twitvim_count| option to allow user to
			     configure the number of tweets returned by
			     :FriendsTwitter and :UserTwitter.
	0.3.3 : 2008-10-06 * Added support for Cligs. |:Cligs|
	                   * Fixed a problem with not being able to unset
			     the proxy if using Tcl http.
	0.3.2 : 2008-09-30 * Added command to display rate limit info.
			     |:RateLimitTwitter|
			   * Improved error reporting for :UserTwitter.
			   * Added command and mapping to display user
			     profile information. |:ProfileTwitter|
			     |TwitVim-Leader-p|
			   * Added command for updating location.
			     |:LocationTwitter|
			   * Added support for tr.im. |:Trim|
			   * Fixed error reporting in Tcl http code.
	0.3.1 : 2008-09-18 * Added support for LongURL. |TwitVim-LongURL|
			   * Added support for posting multibyte/Unicode
			     tweets in cURL mode.
			   * Remove newlines from text before retweeting.
	0.3.0 : 2008-09-12 * Added support for http networking through Vim's
			     Perl, Python, Ruby, and Tcl interfaces, as
			     alternatives to cURL. |TwitVim-non-cURL|
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
			      |:SearchTwitter|
	0.2.19 : 2008-07-23 * Added support for non-Twitter servers
			      implementing the Twitter API. This is for
			      identi.ca support. See |twitvim-identi.ca|.
	0.2.18 : 2008-07-14 * Added support for urlBorg API. |:UrlBorg|
	0.2.17 : 2008-07-11 * Added command to show DM Sent Timeline.
	                      |:DMSentTwitter|
			    * Added support for pagination in Friends, User,
			      Replies, DM, and DM Sent timelines.
			    * Added support for bit.ly API and is.gd API.
			      |:BitLy| |:IsGd|
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
			      |twitvim_login_b64| |twitvim_proxy_login_b64|
	0.2.11 : 2008-05-02 * Scroll to top in timeline window after adding
			      an update line.
			    * Add <Leader>r and <Leader>d mappings as
			      alternative to Alt-R and Alt-D because the
			      latter are not valid key combos under Cygwin.
	0.2.10 : 2008-04-25 * Shortened snipurl.com to snipr.com
			    * Added support for proxy authentication.
			      |twitvim_proxy_login|
			    * Handle Perl module load failure. Not that I
			      expect those modules to ever be missing.
	0.2.9 : 2008-04-23 * Added some status messages.
			   * Added menu items under Plugin menu.
			   * Allow Ctrl-T as an alternative to Alt-T to avoid
			     conflict with the menu bar.
			   * Added support for UrlTea API.
			   * Generalize URL encoding to all non-alpha chars.
	0.2.8 : 2008-04-22 * Encode URLs sent to URL-shortening services.
	0.2.7 : 2008-04-21 * Add support for TinyURL API. |:TinyURL|
			   * Add quick direct message feature.
			     |TwitVim-direct-message|
	0.2.6 : 2008-04-15 * Delete Twitter buffer to the blackhole register
			     to avoid stepping on registers unnecessarily.
			   * Quote login and proxy arguments before sending to
			     cURL.
			   * Added support for SnipURL API and Metamark API.
			     |:Snipurl| |:Metamark|
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
			   * Added Tweetburner support. |:Tweetburner|
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
