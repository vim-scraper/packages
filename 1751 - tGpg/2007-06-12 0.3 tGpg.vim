" tGpg.vim -- Yet another plugin for encrypting files with gpg
" @Author:      Thomas Link (mailto:samul AT web de?subject=vim-tGpg)
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2006-12-31.
" @Last Change: 2007-06-13.
" @Revision:    0.3.755
"
" TODO:
" - Remove gpg messages from the top of the file & display them with 
"   echom
" - :read doesn't work ('<,'>:write?)
" - test special characters (new template syntax)
" - test multiple recipients
" - passphrase vs multiple recipients?
" - signing & verification (embedded vs detached)
" - randomized g:tgpgRotTable[AB]
" - save cached values between sessions in a gpg encoded file?

if &cp || exists("loaded_tgpg") "{{{2
    finish
endif
let loaded_tgpg = 3

if !exists(':TAssert') "{{{2
    command! -nargs=* -bang TAssert :
    command! -nargs=* -bang TAssertBegin :
    command! -nargs=* -bang TAssertEnd :
endif

if !exists('g:tgpgMode') | let g:tgpgMode = 'symmetric' | endif "{{{2

" 'sign'
if !exists('g:tgpgModes') | let g:tgpgModes = ['symmetric', 'encrypt', 'clearsign'] | endif "{{{2

if !exists('g:tgpgPattern_symmetric') "{{{2
    let g:tgpgPattern_symmetric = g:tgpgMode == 'symmetric' ? '*.\(gpg\|asc\|pgp\)' : ''
endif
if !exists('g:tgpgWrite_symmetric') "{{{2
    let g:tgpgWrite_symmetric = '!gpg %{G_OPTIONS} %{B_OPTIONS} %{PASSPHRASE} -o %{FILE} -c'
    " let g:tgpgWrite_symmetric = '!gpg %{G_OPTIONS} %{B_OPTIONS} %{PASSPHRASE} -c'
endif
if !exists('g:tgpgRead_symmetric') "{{{2
    let g:tgpgRead_symmetric = '!gpg %{G_OPTIONS} %{B_OPTIONS} %{PASSPHRASE} -d %{FILE}'
    " let g:tgpgRead_symmetric = '!gpg %{G_OPTIONS} %{B_OPTIONS} %{PASSPHRASE} -d'
endif

if !exists('g:tgpgPattern_encrypt') "{{{2
    let g:tgpgPattern_encrypt = g:tgpgMode == 'encrypt' ? '*.\(gpg\|asc\|pgp\)' : ''
endif
if !exists('g:tgpgWrite_encrypt') "{{{2
    let g:tgpgWrite_encrypt = '!gpg %{G_OPTIONS} %{RECIPIENTS} %{B_OPTIONS} -e -o %{FILE}'
endif
if !exists('g:tgpgRead_encrypt') "{{{2
    let g:tgpgRead_encrypt = '!gpg %{G_OPTIONS} %{B_OPTIONS} %{PASSPHRASE} -d %{FILE}'
endif

if !exists('g:tgpgPattern_clearsign') | let g:tgpgPattern_clearsign = '' | endif "{{{2
if !exists('g:tgpgWrite_clearsign') "{{{2
    " let g:tgpgWrite_clearsign = '!gpg %{G_OPTIONS} %{B_OPTIONS} %{RECIPIENTS} %{PASSPHRASE} -o %{FILE} --clearsign'
    let g:tgpgWrite_clearsign = '!gpg %{G_OPTIONS} %{B_OPTIONS} %{PASSPHRASE} -o %{FILE} --clearsign'
endif
" if !exists('g:tgpgRead_clearsign') | let g:tgpgRead_clearsign = '!gpg %{G_OPTIONS} --verify %s' | endif

" if !exists('g:tgpgPattern_sign') | let g:tgpgPattern_sign = '*.\(sig\)' | endif
" if !exists('g:tgpgWrite_sign') | let g:tgpgWrite_sign = '!gpg %{G_OPTIONS} -r %s -s -o %s' | endif
" " if !exists('g:tgpgRead_sign') | let g:tgpgDecrypt = '' | endif

if !exists('g:tgpgOptions') "{{{2
    " --no-mdc-warning
    let g:tgpgOptions = '-q --no-secmem-warning'
endif
if !exists('g:tgpgCachePW')       | let g:tgpgCachePW = 2                         | endif "{{{2
if !exists('g:tgpgBackup')        | let g:tgpgBackup = 1                          | endif "{{{2
if !exists('g:tgpgCmdRecipient')  | let g:tgpgCmdRecipient = '-r "%s"'            | endif "{{{2
if !exists('g:tgpgCmdPassphrase') | let g:tgpgCmdPassphrase = '--passphrase "%s"' | endif "{{{2
if !exists('g:tgpgSepRecipient')  | let g:tgpgSepRecipient = ';|/&'               | endif "{{{2
if !exists('g:tgpgShellQuote')    | let g:tgpgShellQuote = '&'.&shellxquote       | endif "{{{2
if !exists('g:tgpgTempSuffix')    | let g:tgpgTempSuffix = '.~tGpg~'              | endif "{{{2
if !exists('g:tgpgInputsecret')   | let g:tgpgInputsecret = 'inputsecret'         | endif "{{{2
" if !exists('g:tgpgInputsecret')  | let g:tgpgInputsecret = 'input'         | endif

" ROT13
if !exists('g:tgpgRotTableA') "{{{2
    let g:tgpgRotTableA = 'ABCEFGHIJKLMNOPQRSTUVWXYZabcefghijklmnopqrstuvwxyz'
endif
if !exists('g:tgpgRotTableB') "{{{2
    let g:tgpgRotTableB = 'NOPQRSTUVWXYZABCEFGHIJKLMnopqrstuvwxyzabcefghijklm'
endif

command! TGpgResetCache let s:tgpgSecretCache = {}
" command! TGpgShowCache echo string(s:tgpgSecretCache)
TGpgResetCache

fun! s:EscapeShellCmdChars(text) "{{{3
    return escape(a:text, '%#'. g:tgpgShellQuote)
endf

fun! s:EscapeFilename(file) "{{{3
    return escape(a:file, ' ')
endf

fun! s:GetMode(mode) "{{{3
    if empty(a:mode)
        return exists('b:tgpgMode') ? b:tgpgMode : g:tgpgMode
    else
        return a:mode
    endif
endf

fun! s:GetRecipients(iomode, default) "{{{3
    " TAssert IsList(a:default)
    call inputsave()
    let user = input('Recipients (seperated by ['.g:tgpgSepRecipient .']): ', join(a:default, g:tgpgSepRecipient[0].' '))
    call inputrestore()
    return split(user, '['. g:tgpgSepRecipient .']\s*')
endf

fun! s:FormatRecipients(recipients) "{{{3
    " TAssert IsList(a:recipients)
    let luser = map(copy(a:recipients), 'printf(g:tgpgCmdRecipient, v:val)')
    return join(luser, ' ')
endf

fun! s:GetPassphrase(iomode, default) "{{{3
    " TAssert IsString(a:default)
    " TAssert IsExistent('*'.g:tgpgInputsecret)
    call inputsave()
    " call TLog('GetPassphrase default='. a:default)
    echo
    while 1
        let secret = {g:tgpgInputsecret}('Passphrase: ', a:default)
        if secret != '' && secret != a:default && a:iomode ==? 'w' && g:tgpgInputsecret =~? 'inputsecret'
            let secret0 = {g:tgpgInputsecret}('Please retype your passphrase: ', a:default)
            if secret0 != secret
                echo "Passphrases didn't match!"
                continue
            endif
        endif
        break
    endwh
    call inputrestore()
    echo
    return secret
endf

fun! s:CacheKey(id, file) "{{{3
    " TAssert IsString(a:id)
    " TAssert IsString(a:file)
    if has('fname_case')
        let file = a:file
    else
        " let file = substitute(a:file, '^\w\+\ze:', '\U&', '')
        let file = tolower(a:file)
    endif
    let rv = a:id .'*'. file
    return rv
endf

fun! s:EncodeValue(value) "{{{3
    return tr(string(a:value), g:tgpgRotTableA, g:tgpgRotTableB)
endf

fun! s:DecodeValue(text) "{{{3
    " TAssert IsString(a:text)
    return eval(tr(a:text, g:tgpgRotTableB, g:tgpgRotTableA))
endf

fun! s:GetCacheVar(id, file, default) "{{{3
    let id = s:EncodeValue(s:CacheKey(a:id, a:file))
    if has_key(s:tgpgSecretCache, id)
        let rv = s:DecodeValue(s:tgpgSecretCache[id])
        " call TLog('GetCacheVar '. id .'='. rv)
        return rv
    else
        " return s:PutCacheVar(a:id, a:file, a:default)
        return a:default
    endif
endf

fun! s:PutCacheVar(id, file, secret) "{{{3
    let id = s:CacheKey(a:id, a:file)
    let s:tgpgSecretCache[s:EncodeValue(id)] = s:EncodeValue(a:secret)
    return a:secret
endf

fun! s:GetCache(id, file, default) "{{{3
    " TAssert IsString(a:id)
    let tgpgCachePW = exists('b:tgpgCachePW') ? b:tgpgCachePW : g:tgpgCachePW
    if tgpgCachePW
        if tgpgCachePW >= 2
            return s:GetCacheVar(a:id, a:file, a:default)
        endif
        if exists('b:tgpgSecret_'. a:id) && !empty(b:tgpgSecret_{a:id})
            return b:tgpgSecret_{a:id}
        endif
    endif
    return a:default
endf

fun! s:PutCache(id, file, secret) "{{{3
    " TAssert IsString(a:id)
    let tgpgCachePW = exists('b:tgpgCachePW') ? b:tgpgCachePW : g:tgpgCachePW
    if tgpgCachePW && !empty(a:secret)
        if tgpgCachePW >= 2
            call s:PutCacheVar(a:id, a:file, a:secret)
        elseif tgpgCachePW >= 1
            let b:tgpgSecret_{a:id} = a:secret
        endif
    endif
endf

fun! s:CallInDestDir(autocommand, file, mode, FunRef, args) "{{{3
    if expand('%:p') != a:file
        let buf = bufnr('%')
        exec 'silent! buffer! '. bufnr(a:file)
    else
        let buf = -1
    endif
    let bin = &bin
    let pos = getpos('.')
    let t   = @t
    let parms = {'autocommand': a:autocommand, 'file': a:file, 'mode': s:GetMode(a:mode), 'pwd': getcwd()}
    try
        if empty(parms['file'])
            let parms['file'] = expand('%:p')
        endif
        " call TLog('file='. parms['file'])
        let parms['hfile'] = fnamemodify(parms['file'], ':p:h')
        let parms['tfile'] = fnamemodify(parms['file'], ':t')
        let parms['gfile'] = parms['tfile'] . g:tgpgTempSuffix
        silent exec 'cd '. s:EscapeShellCmdChars(s:EscapeFilename(parms['hfile']))
        set bin
        set noswapfile
        if !exists('b:tgpgMode')
            let b:tgpgMode = a:mode
        endif
        " set buftype=acwrite
        call call(a:FunRef, [parms] + a:args)
    finally
        let &bin = bin
        let @t   = t
        call setpos('.', pos)
        silent exec 'cd '. s:EscapeShellCmdChars(s:EscapeFilename(parms['pwd']))
        if buf != -1
            exec 'silent! buffer! '. buf
        endif
    endtry
endf

fun! s:TemplateValue(label) "{{{3
    " call TLog('TemplateValue: success='. s:templateSuccess)
    " TLog 'TemplateValue: '. a:label
    if s:templateSuccess
        if has_key(s:templateValues, a:label)
            " call TLog('TemplateValue => '. s:templateValues[a:label])
            return s:templateValues[a:label]
        endif
        if exists('*TGpgUserInput_'. a:label)
            let [s:templateSuccess, rv] = TGpgUserInput_{a:label}(s:templateValues)
            " TLog 'TemplateValue* => '. rv
            return rv
        endif
        let s:templateSuccess = 0
    endif
    return ''
endf

fun! s:ProcessTemplate(parms, iomode, template, vars) "{{{3
    " TAssert IsDictionary(a:parms)
    " TAssert IsString(a:iomode) && IsNotEmpty(a:iomode)
    " TAssert IsString(a:template) && IsNotEmpty(a:template)
    " TAssert IsDictionary(a:vars)
    let rv = a:template
    let s:templateValues = {'iomode': a:iomode}
    call extend(s:templateValues, a:vars)
    call extend(s:templateValues, a:parms)
    let s:templateSuccess = 1
    " TLog 'Template pre: '. rv
    let rv = substitute(rv, '\C\(^\|[^%]\)\zs%{\([A-Z_]\+\)}', '\=escape(s:TemplateValue(submatch(2)), ''\&'')', 'g')
    unlet s:templateValues
    " TLog 'Template after: '. rv
    if s:templateSuccess
        let rv = substitute(rv, '%%', "%", "g")
    else
        let rv = ''
    endif
    unlet s:templateSuccess
    return rv
endf

fun! TGpgUserInput_G_OPTIONS(parms) "{{{3
    return [1, g:tgpgOptions]
endf

fun! TGpgUserInput_B_OPTIONS(parms) "{{{3
    let id = a:parms['mode'] .'_'. a:parms['iomode']
    let rv = exists('b:tgpg_'. id .'_options') ? b:{a:id}_options : ''
    return [1, rv]
endf

fun! TGpgUserInput_PASSPHRASE(parms) "{{{3
    let id  = 'PW_'. a:parms['mode']
    " call TLog('TGpgUserInput_PASSPHRASE id='. id)
    " call TLog('TGpgUserInput_PASSPHRASE file='. a:parms['file'])
    let default = s:GetCache(id, a:parms['file'], '')
    " call TLog('TGpgUserInput_PASSPHRASE default='. default)
    let val = s:GetPassphrase(a:parms['iomode'], default)
    if !empty(val)
        call s:PutCache(id, a:parms['file'], val)
        return [1, printf(g:tgpgCmdPassphrase, val)]
    endif
    return [0, '']
endf

fun! TGpgUserInput_RECIPIENTS(parms) "{{{3
    let default = s:GetCache('recipients', a:parms['file'], [])
    let recipients = s:GetRecipients(a:parms['iomode'], default)
    if !empty(recipients)
        call s:PutCache('recipients', a:parms['file'], recipients)
        return [1, s:FormatRecipients(recipients)]
    endif
    return [0, '']
endf

fun! TGpgRead(parms, range) "{{{3
    if !filereadable(a:parms['tfile'])
        return
    endif
    let read = 0
    if exists('g:tgpgRead_'. a:parms['mode'])
        let args = {'FILE': s:EscapeFilename(a:parms['tfile'])}
        " TLogVAR a:parms['tfile']
        let cmd  = s:ProcessTemplate(a:parms, 'r', g:tgpgRead_{a:parms['mode']}, args)
        if !empty(cmd)
            " TLogVAR cmd
            exec a:range . s:EscapeShellCmdChars(cmd)
            let read = 1
        endif
    endif
    if !read
        exec a:range .'read '. s:EscapeShellCmdChars(s:EscapeFilename(a:parms['tfile']))
    endif
    if a:parms['autocommand'] =~ '^Buf'
        " exec 'doautocmd BufRead '. s:EscapeFilename(expand("%"))
        exec 'doautocmd BufRead '. s:EscapeFilename(expand("%:r"))
    endif
endf

fun! TGpgWrite(parms) "{{{3
    if exists('g:tgpgWrite_'. a:parms['mode'])
        " TLogVAR a:parms['tfile']
        " TLogVAR a:parms['gfile']
        let ftime = getftime(a:parms['tfile'])
        let args = {'FILE': s:EscapeFilename(a:parms['tfile'])}
        let cmd = s:ProcessTemplate(a:parms, 'w', g:tgpgWrite_{a:parms['mode']}, args)
        if !empty(cmd)
            " TLogVAR cmd
            if filereadable(a:parms['tfile'])
                call rename(a:parms['tfile'], a:parms['gfile'])
            endif
            let foldlevel = &foldlevel
            try
                setlocal foldlevel=99
                silent %yank t
                " TLog "'[,']". cmd
                exec "'[,']". s:EscapeShellCmdChars(cmd)
                silent norm! ggdG"tPGdd
                if filereadable(a:parms['tfile'])
                    if getfsize(a:parms['tfile']) == 0
                        echom 'tGpg: File size is zero -- writing has failed.'
                        if filereadable(a:parms['gfile'])
                            echom 'tGpg: Reverting to old file.'
                            call rename(a:parms['gfile'], a:parms['tfile'])
                        endif
                    else
                        set nomodified
                        if !g:tgpgBackup
                            call delete(a:parms['gfile'])
                        endif
                    endif
                else
                    echom 'tGpg: Reverting to old file.'
                    call rename(a:parms['gfile'], a:parms['tfile'])
                endif
            finally
                let &foldlevel = foldlevel
            endtry
        else
            echom 'tGpg: Aborted!'
        endif
    else
        exec "'[,']write ". s:EscapeShellCmdChars(s:EscapeFilename(a:parms['tfile']))
    endif
endf

fun! TGpgWrite_clearsign(parms) "{{{3
    let iomode = empty(a:parms['autocommand']) ? 'W' : 'w'
    if exists('g:tgpgWrite_'. a:parms['mode'])
        let args = {'FILE': s:EscapeFilename(a:parms['gfile'])}
        let cmd = s:ProcessTemplate(a:parms, iomode, g:tgpgWrite_{a:parms['mode']}, args)
        if !empty(cmd)
            if filereadable(a:parms['gfile'])
                call delete(a:parms['gfile'])
            endif
            " TLog '%'. cmd
            silent exec '%'. s:EscapeShellCmdChars(cmd)
            " silent exec '0read '. s:EscapeShellCmdChars(s:EscapeFilename(a:parms['gfile']))
            silent exec '%read '. s:EscapeShellCmdChars(s:EscapeFilename(a:parms['gfile']))
            norm! ggdd
            call delete(a:parms['gfile'])
            exec 'write! '. s:EscapeShellCmdChars(s:EscapeFilename(a:parms['tfile']))
        else
            echom 'tGpg: Aborted!'
        endif
    endif
endf


augroup tGpg
    au!
    for g in g:tgpgModes
        if !exists('g:tgpgPattern_'. g)
            continue
        endif

        let rcmd = exists('*TGpgRead_'. g) ? 'TGpgRead_'. g : 'TGpgRead'
        let wcmd = exists('*TGpgWrite_'. g) ? 'TGpgWrite_'. g : 'TGpgWrite'
        let gcap = toupper(g[0]).g[1:-1]
        exec 'command! -range=% -nargs=? TGpg'. gcap .' call s:CallInDestDir("", <q-args>, "'. g .'", function("'. wcmd .'"), [])'

        if empty(g:tgpgPattern_{g})
            continue
        endif
        if exists('g:tgpgRead_'. g)
            " I'm not sure. I never fully understood the difference between BufRead and FileRead.
            " exec 'autocmd BufReadCmd,FileReadCmd '. g:tgpgPattern_{g} .' echom "DBG <afile>"'
            exec 'autocmd BufReadCmd  '. g:tgpgPattern_{g} .' call s:CallInDestDir("BufReadCmd", expand("<afile>:p"), "'. g .'", function("'. rcmd .'"), ["%"])'
            exec 'autocmd FileReadCmd '. g:tgpgPattern_{g} .' call s:CallInDestDir("FileReadCmd", expand("<afile>:p"), "'. g .'", function("'. rcmd .'"), ["''[,'']"])'
            for m in ['BufReadPre', 'FileReadPre', 'BufReadPost', 'FileReadPost']
                " exec 'autocmd '. m .' '. g:tgpgPattern_{g} .' echom "DBG ". m ." ". escape(expand("<afile>:r"), "%")'
                exec 'autocmd '. m .' '. g:tgpgPattern_{g} .' exec ":doautocmd '. m .'" . expand("<afile>:r")'
            endfor
        endif
        if exists('g:tgpgWrite_'. g)
            exec 'autocmd BufWriteCmd '. g:tgpgPattern_{g} .' call s:CallInDestDir("BufWriteCmd", expand("<afile>:p"), "'. g .'", function("'. wcmd .'"), [])'
            exec 'autocmd FileWriteCmd '. g:tgpgPattern_{g} .' call s:CallInDestDir("FileWriteCmd", expand("<afile>:p"), "'. g .'", function("'. wcmd .'"), [])'
            for m in ['BufWritePre', 'FileWritePre', 'BufWritePost', 'FileWritePost']
                exec 'autocmd '. m .' '. g:tgpgPattern_{g} .' exec ":doautocmd '. m .'" . expand("<afile>:r")'
            endfor
            " exec 'autocmd FileAppendPre '. g:tgpgPattern_{g} .' call TGpgWrite(expand("<afile>", ""))'
            " exec 'autocmd FileAppendPost '. g:tgpgPattern_{g} .' call TGpgOutPost(expand("<afile>"))'
        endif
    endfor
augroup END

" untested <+TBD+>
" command! -range=% -nargs=1 TGpgWrite call TGpgWrite(<q-args>, '')


finish

This plugin currently can do the following:

    - encrypt symmetrically
    - encrypt asymmetrically
    - clearsign buffer contents

I couldn't get any of the existing gpg plugins to work properly (windows 
Gvim & cygwin gpg) and do all the things I wanted it to do, so I wrote 
this one. The main purpose is to perform symmetric encryption (the 
default) but it's flexible enough to do also clearsign and asymmetric 
encryption.

You can set g:tgpgMode or b:tgpgMode to 'encrypt' for switching to 
asymmetric encryption as default. You can also control the use of 
symmetric and asymmetric encryption by setting set 
g:tgpgPattern_symmetric and g:tgpgPattern_encrypt.

This plugin passes the passphrase on the command line to the gpg 
programm. So, it could be possible that somebody makes the passphrase 
show up in some command log. Under some circumstances it could also be 
possible that some info (eg the recipients) is logged in your viminfo 
file. If you clearsign a message, the plain text will be written to 
disk. Ie, if you clearsign a gpg encrypted message, the text will 
temporarily be written to disk as plain text -- please keep in mind the 
vast range of possible consequences. If you don't like to pass 
passphrases on the command line, you'd have to change the command 
templates.

This plugin uses the (Buf|File)(Read|Write)Cmd autocommand events to 
write/read the file. I'm not sure how this works out with other plugins 
using these events.

As I don't like typing passphrases, this plugin caches all the 
passphrases entered in rot13 encoded form in a script local variable. 
This means that passphrases could be written to the swapfile, from where 
somebody somehow could possibly do something ... Set g:tgpgCachePW to 1 
(buffer-wise caching only) or 0 (no caching) to change this.

This plugin was tested with Windows GVim & cygwin gpg 1.4.5 (using bash 
as shell) as well as linux vim & gpg 1.4.5. It's possible (albeit  
unlikely) that the use of a pure Windows version of gpg or cmd.exe as 
shell doesn't work.  (Please report problems.)

If you get a message telling you about gpg command line options instead 
of the decrypted file, please check the value of g:tgpgShellQuote.

If writing fails, it's possible that you end up with a corrupted or 
empty file. That's why we make backups by default. Set g:tgpgBackup 
to 0 to change this.


CHANGE LOG:
0.1
- Initial release

0.2
- Made the cache a script local variable.
- Let user retype passwords when writing a file with a new or changed 
passphrase.
- Display a warning if the size of the output file is 0 & revert to old 
file.
- Keep the original when writing.
- Run BufRead autocommands on filename root after reading the buffer.
- Slightly obscure cached values.

0.3
- Changed command template syntax
- The user is now queried for information only as required by the 
command template
- Changed default value of g:tgpgTempSuffix
- Removed recipients from the clearsign template
- Make sure we're in the right buffer
- Enable buffer local command line options (eg 
b:tgpgWrite_symmetric_*_options)

