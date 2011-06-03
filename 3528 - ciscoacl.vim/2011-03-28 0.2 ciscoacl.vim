" ciscoacl.vim: Vim syntax file
" Language: cisco access-list files
" Maintainer: Vladimir Kotal <vlada@devnull.cz>
" URL: http://www.vim.org/scripts/script.php?script_id=1141
"
" Modified by: Marcos Manoni
" 
" copy this file to ~/.vim/syntax/ directory (create dirs if they are not
" present) and add the following to your ~/.vimrc file:
"
" augroup filetype
"  au! BufRead,BufNewFile *.acl        set filetype=ciscoacl
" augroup END
" 
syntax clear

if version < 600
    syntax clear
elseif exists("b:current_syntax")
    finish
endif

setlocal iskeyword+=/
setlocal iskeyword+=:
setlocal iskeyword+=.
setlocal iskeyword+=-

setlocal foldmethod=syntax
syn sync fromstart

" keywords
syntax keyword aclCmd        permit 
syntax keyword aclDeny       deny

" protocols
syntax keyword aclProto      ahp eigrp esp gre icmp igmp ip ipinip ipv6 nos ospf
syntax keyword aclProto      pcp sctp pim tcp udp

" ports TCP only
syntax keyword aclPort       bgp chargen cmd daytime drip exec finger ftp
syntax keyword aclPort       ftp-data gopher hostname ident irc klogin kshell
syntax keyword aclPort       login lpd nntp pop2 pop3 smtp telnet uucp whois
syntax keyword aclPort       www
" ports UDP only
syntax keyword aclPort       biff bootpc bootps dnsix isakmp mobile-ip
syntax keyword aclPort       nameserver netbios-dgm netbios-ns netbios-ss
syntax keyword aclPort       non500-isakmp ntp rip snmp snmptrap syslog tftp
syntax keyword aclPort       who xdmcp
" ports TCP or UDP
syntax keyword aclPort       discard domain echo pim-auto-rp sunrpc tacacs
syntax keyword aclPort       talk time

syntax keyword aclCmd        any host

syntax keyword aclLog         log logging log-input
"syntax keyword aclOther       fragments option precedence reflect tos
"syntax keyword aclOther       dscp flow-label routing sequence time-range
"syntax keyword aclOtherTCP    ack established fin psh rst syn urg
"syntax keyword aclOtherICMP   beyond-scope destination-unreachable echo-reply
"syntax keyword aclOtherICMP   echo-request header hop-limit mld-query
"syntax keyword aclOtherICMP   mld-reduction mld-report nd-na nd-ns next-header
"syntax keyword aclOtherICMP   no-admin no-route packet-too-big parameter-option
"syntax keyword aclOtherICMP   parameter-problem port-unreachable
"syntax keyword aclOtherICMP   reassembly-timeout redirect renum-command
"syntax keyword aclOtherICMP   renum-result renum-seq-number
"syntax keyword aclOtherICMP   router-advertisement router-renumbering
"syntax keyword aclOtherICMP   router-solicitation time-exceeded unreachable

" regexps
syntax match   aclACL        /^access-list\s+/

" IPv4: simple version
"syntax match   ciscoipv4     /\d\{1,3}\.\d\{1,3}\.\d\{1,3}\.\d\{1,3}/
" IPv6: simple version
"syntax match   ciscoipv6     /\x\{0,4}\:\x\{0,4}/
"syntax match   ciscoipv6     /\x\{0,4}\:\(25[0-5]\|2[0-4]\d\|[01]\?\d\{1,2}\)\(\.\(25[0-5]\|2[0-4]\d\|[01]\?\d\{1,2}\)\)\{3}/

" IPv4: complex version
syntax match   ciscoipv4     /\ \(25[0-5]\|2[0-4]\d\|[01]\?\d\{1,2}\)\(\.\(25[0-5]\|2[0-4]\d\|[01]\?\d\{1,2}\)\)\{3}/

" IPv6: complex version (almost correct, see below)
syntax match   ciscoipv6     /\ \(\x\{1,4}\:\)\{7}\(\:\|\(\x\{1,4}\)\)/
syntax match   ciscoipv6     /\ \(\x\{1,4}\:\)\{6}\(\:\|\(\(25[0-5]\|2[0-4]\d\|[01]\?\d\{1,2}\)\(\.\(25[0-5]\|2[0-4]\d\|[01]\?\d\{1,2}\)\)\{3}\)\|\(\:\x\{1,4}\)\)/
syntax match   ciscoipv6     /\ \(\x\{1,4}\:\)\{5}\(\(\:\(\(25[0-5]\|2[0-4]\d\|[01]\?\d\{1,2}\)\(\.\(25[0-5]\|2[0-4]\d\|[01]\?\d\{1,2}\)\)\{3}\)\?\)\|\(\(\:\x\{1,4}\)\{1,2}\)\)/
syntax match   ciscoipv6     /\ \(\x\{1,4}\:\)\{4}\(\:\x\{1,4}\)\{0,1}\(\:\(\(25[0-5]\|2[0-4]\d\|[01]\?\d\{1,2}\)\(\.\(25[0-5]\|2[0-4]\d\|[01]\?\d\{1,2}\)\)\{3}\)\?\)/
syntax match   ciscoipv6     /\ \(\x\{1,4}\:\)\{4}\(\:\x\{1,4}\)\{0,1}\(\(\:\x\{1,4}\)\{1,2}\)/
syntax match   ciscoipv6     /\ \(\x\{1,4}\:\)\{3}\(\:\x\{1,4}\)\{0,2}\(\:\(\(25[0-5]\|2[0-4]\d\|[01]\?\d\{1,2}\)\(\.\(25[0-5]\|2[0-4]\d\|[01]\?\d\{1,2}\)\)\{3}\)\?\)/
syntax match   ciscoipv6     /\ \(\x\{1,4}\:\)\{3}\(\:\x\{1,4}\)\{0,2}\(\(\:\x\{1,4}\)\{1,2}\)/
syntax match   ciscoipv6     /\ \(\x\{1,4}\:\)\{2}\(\:\x\{1,4}\)\{0,3}\(\:\(\(25[0-5]\|2[0-4]\d\|[01]\?\d\{1,2}\)\(\.\(25[0-5]\|2[0-4]\d\|[01]\?\d\{1,2}\)\)\{3}\)\?\)/
syntax match   ciscoipv6     /\ \(\x\{1,4}\:\)\{2}\(\:\x\{1,4}\)\{0,3}\(\(\:\x\{1,4}\)\{1,2}\)/
syntax match   ciscoipv6     /\ \(\x\{1,4}\:\)\(\:\x\{1,4}\)\{0,4}\(\:\(\(25[0-5]\|2[0-4]\d\|[01]\?\d\{1,2}\)\(\.\(25[0-5]\|2[0-4]\d\|[01]\?\d\{1,2}\)\)\{3}\)\?\)/
syntax match   ciscoipv6     /\ \(\x\{1,4}\:\)\(\:\x\{1,4}\)\{0,4}\(\(\:\x\{1,4}\)\{1,2}\)/
syntax match   ciscoipv6     /\ \:\(\:\x\{1,4}\)\{0,5}\(\(\:\(\(25[0-5]\|2[0-4]\d\|[01]\?\d\{1,2}\)\(\.\(25[0-5]\|2[0-4]\d\|[01]\?\d\{1,2}\)\)\{3}\)\?\)\|\(\(\:\x\{1,4}\)\{1,2}\)\)/

" valid ipv6 not fully highlighted " 
" ::1
" 1::5:1.2.3.4
" 1::1.2.3.4
" 1:2::1.2.3.4
" 1:2:3::1.2.3.4
" 1:2:3:4::1.2.3.4
" 1:2:3:4::5:1.2.3.4
" 1:2:3:4:5:6::8
" 1:2:3:4:5::7:8
" 1:2:3:4:5::8

syntax keyword aclTodo       TODO XXX contained
syntax region  ciscocomment  start="!" matchgroup=NONE end="$" contains=@cCommentGroup,aclTodo
syntax region  ciscocomment  start=/^C/ end=/^C/

"syntax match   aclDefine     /[A-Z][A-Z\_0-9]*/ contained
syntax match   aclDefine     /[A-Z][A-Z\_0-9]*/
"syntax region  remark        start="^access-list.*remark" end="$" contains=aclACL,aclDefine
syntax region  remark        start="remark " end="$"

"syntax region  portspec      start="eq" end="\s*[a-z0-9]*"
"syntax region  portspec      start="neq" end="\s*[a-z0-9]*"
"syntax region  portspec      start="lt" end="\s*[a-z0-9]*"
"syntax region  portspec      start="gt" end="\s*[a-z0-9]*"
"syntax region  portspec      start="range" end="\s*[a-z0-9]*\s*[a-z0-9]*"

syntax match   aclGenericNum /\<\d\+\>/


" Define the default hightlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet

" There's a sintax error in this line
"if version >= 508 || !exists("did_cisco_syn_inits")
if version >= 508
    if version < 508
        let did_cisco_syn_inits = 1
        command -nargs=+ HiLink hi link <args>
    else
        command -nargs=+ HiLink hi def link <args>
    endif

    " special highlighting for deny keyword
    hi Deny gui=bold guifg=LightRed ctermfg=White ctermbg=LightRed term=underline
    
    HiLink aclDeny           Deny
    HiLink aclCmd            Statement
    HiLink ciscoipv4         Type
    HiLink ciscoipv6         Type
    HiLink aclProto          Constant
    HiLink remark            String
    HiLink ciscocomment      Comment
    HiLink portspec          Type
    HiLink aclDefine         Identifier
    HiLink aclGenericNum     Constant
    HiLink aclPort           Constant
    HiLink aclSrcDst         Type
    HiLink aclTodo           Todo

    delcommand HiLink
endif

let b:current_syntax = "cisco"

" vim:ts=4
