" tGpg.vim -- Yet another plugin for encrypting files with gpg
" @Author:      Thomas Link (mailto:samul AT web de?subject=vim-tGpg)
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2006-12-31.
" @Last Change: 2007-01-03.
" @Revision:    0.2.564
"
" TODO:
" - signing & verification (embedded vs detached)
" - randomized g:tgpgRotTable[AB]
" - save cached values between sessions in a gpg encoded file?

if &cp || exists("loaded_tgpg")
    finish
endif
let loaded_tgpg = 2

if !exists(':TAssert')
    command! -nargs=* -bang TAssert :
    command! -nargs=* -bang TAssertBegin :
    command! -nargs=* -bang TAssertEnd :
endif

if !exists('g:tgpgMode') | let g:tgpgMode = 'symmetric' | endif

" 'sign'
if !exists('g:tgpgModes') | let g:tgpgModes = ['symmetric', 'encrypt', 'clearsign'] | endif

if !exists('g:tgpgPattern_symmetric')
    let g:tgpgPattern_symmetric = g:tgpgMode == 'symmetric' ? '*.\(gpg\|asc\|pgp\)' : ''
endif
if !exists('g:tgpgWrite_symmetric')   | let g:tgpgWrite_symmetric = '!gpg -q --no-secmem-warning --passphrase "%s" -c -o %s' | endif
if !exists('g:tgpgRead_symmetric')    | let g:tgpgRead_symmetric = '!gpg -q --no-mdc-warning --no-secmem-warning --passphrase "%s" -d %s' | endif

if !exists('g:tgpgPattern_encrypt')
    let g:tgpgPattern_encrypt = g:tgpgMode == 'encrypt' ? '*.\(gpg\|asc\|pgp\)' : ''
endif
if !exists('g:tgpgWrite_encrypt')   | let g:tgpgWrite_encrypt = '!gpg -q --no-secmem-warning %s -e -o %s' | endif
if !exists('g:tgpgRead_encrypt')    | let g:tgpgRead_encrypt = '!gpg -q --no-mdc-warning --no-secmem-warning --passphrase "%s" -d %s' | endif

if !exists('g:tgpgPattern_clearsign') | let g:tgpgPattern_clearsign = '' | endif
if !exists('g:tgpgWrite_clearsign')   | let g:tgpgWrite_clearsign = '!gpg -q --no-secmem-warning %s --passphrase "%s" -o %s --clearsign' | endif
" if !exists('g:tgpgRead_clearsign') | let g:tgpgRead_clearsign = '!gpg -q --no-secmem-warning --verify %s' | endif

" if !exists('g:tgpgPattern_sign') | let g:tgpgPattern_sign = '*.\(sig\)' | endif
" if !exists('g:tgpgWrite_sign') | let g:tgpgWrite_sign = '!gpg -q --no-secmem-warning -r %s -s -o %s' | endif
" " if !exists('g:tgpgRead_sign') | let g:tgpgDecrypt = '' | endif

if !exists('g:tgpgCachePW')      | let g:tgpgCachePW = 2                   | endif
if !exists('g:tgpgBackup')       | let g:tgpgBackup = 1                    | endif
if !exists('g:tgpgCmdRecipient') | let g:tgpgCmdRecipient = '-r "%s"'      | endif
if !exists('g:tgpgSepRecipient') | let g:tgpgSepRecipient = ';|/&'         | endif
if !exists('g:tgpgShellQuote')   | let g:tgpgShellQuote = '&'.&shellxquote | endif
if !exists('g:tgpgTempSuffix')   | let g:tgpgTempSuffix = '.~tGpgtmp~'     | endif
if !exists('g:tgpgInputsecret')  | let g:tgpgInputsecret = 'inputsecret'   | endif
" if !exists('g:tgpgInputsecret')  | let g:tgpgInputsecret = 'input'         | endif

" ROT13
if !exists('g:tgpgRotTableA')
    let g:tgpgRotTableA = 'ABCEFGHIJKLMNOPQRSTUVWXYZabcefghijklmnopqrstuvwxyz'
endif
if !exists('g:tgpgRotTableB')
    let g:tgpgRotTableB = 'NOPQRSTUVWXYZABCEFGHIJKLMnopqrstuvwxyzabcefghijklm'
endif

command! TGpgResetCache let s:tgpgSecretCache = {}
" command! TGpgShowCache echo string(s:tgpgSecretCache)
TGpgResetCache

fun! s:EscapeShellCmdChars(text)
    return escape(a:text, '%#'. g:tgpgShellQuote)
endf

fun! s:EscapeFilename(file)
    return escape(a:file, ' ')
endf

fun! s:GetMode(mode)
    if empty(a:mode)
        return exists('b:tgpgMode') ? b:tgpgMode : g:tgpgMode
    else
        return a:mode
    endif
endf

fun! s:GetRecipients(mode, default)
    TAssert IsList(a:default)
    call inputsave()
    let user = input('Recipients (seperated by ['.g:tgpgSepRecipient .']): ', join(a:default, g:tgpgSepRecipient[0].' '))
    call inputrestore()
    return split(user, '['. g:tgpgSepRecipient .']\s*')
endf

fun! s:FormatRecipients(recipients)
    TAssert IsList(a:recipients)
    let luser = map(copy(a:recipients), 'printf(g:tgpgCmdRecipient, v:val)')
    return join(luser, ' ')
endf

fun! s:GetPassphrase(mode, default)
    TAssert IsString(a:default)
    TAssert IsExistent('*'.g:tgpgInputsecret)
    call inputsave()
    echo
    while 1
        let secret = {g:tgpgInputsecret}('Passphrase: ', a:default)
        if secret != a:default && a:mode ==? 'w' && g:tgpgInputsecret =~? 'inputsecret'
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

fun! s:CacheKey(id, file)
    return a:id.'*'.a:file
endf

fun! s:EncodeValue(value)
    return tr(string(a:value), g:tgpgRotTableA, g:tgpgRotTableB)
endf

fun! s:DecodeValue(text)
    TAssert IsString(a:text)
    return eval(tr(a:text, g:tgpgRotTableB, g:tgpgRotTableA))
endf

fun! s:GetCacheVar(id, file, default)
    let id = s:CacheKey(a:id, a:file)
    if has_key(s:tgpgSecretCache, id)
        return s:DecodeValue(s:tgpgSecretCache[id])
    else
        " return s:PutCacheVar(a:id, a:file, a:default)
        return a:default
    endif
endf

fun! s:PutCacheVar(id, file, secret)
    let id = s:CacheKey(a:id, a:file)
    let s:tgpgSecretCache[id] = s:EncodeValue(a:secret)
    return a:secret
endf

fun! s:GetCache(id, file, default)
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

fun! s:PutCache(id, file, secret)
    let tgpgCachePW = exists('b:tgpgCachePW') ? b:tgpgCachePW : g:tgpgCachePW
    if tgpgCachePW && !empty(a:secret)
        if tgpgCachePW >= 2
            call s:PutCacheVar(a:id, a:file, a:secret)
        elseif tgpgCachePW >= 1
            let b:tgpgSecret_{a:id} = a:secret
        endif
    endif
endf

fun! s:GetCachedSecret(mode, id, file)
    if a:id == 'recipients'
        let default = s:GetCache(a:id, a:file, [])
        let recipients = s:GetRecipients(a:mode, default)
        call s:PutCache(a:id, a:file, recipients)
        let secret = s:FormatRecipients(recipients)
    else
        let default = s:GetCache(a:id, a:file, '')
        let secret = s:GetPassphrase(a:mode, default)
        call s:PutCache(a:id, a:file, secret)
    endif
    return secret
endf

fun! s:CallInDestDir(autocommand, file, mode, FunRef, args)
    let bin = &bin
    let pos = getpos('.')
    let t   = @t
    let parms = {'autocommand': a:autocommand, 'file': a:file, 'mode': s:GetMode(a:mode), 'pwd': getcwd()}
    try
        if empty(parms['file'])
            let parms['file'] = expand('%:p')
        endif
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
    endtry
endf

fun! TGpgUserInput_sign(mode, file)
    return s:GetCachedSecret(a:mode, 'recipients', a:file)
endf

fun! TGpgUserInput_encrypt(mode, file)
    if a:mode ==? 'w'
        return s:GetCachedSecret(a:mode, 'recipients', a:file)
    else
        return s:GetCachedSecret(a:mode, 'pw_encrypt', a:file)
    endif
endf

fun! TGpgUserInput_symmetric(mode, file)
    return s:GetCachedSecret(a:mode, 'pw_symmetric', a:file)
endf

fun! TGpgRead(parms, range)
    if !filereadable(a:parms['file'])
        return
    endif
    let secret = TGpgUserInput_{a:parms['mode']}('r', a:parms['file'])
    if !empty(secret) && exists('g:tgpgRead_'. a:parms['mode'])
        let cmd = printf(g:tgpgRead_{a:parms['mode']}, secret, s:EscapeFilename(a:parms['tfile']))
        " TLog a:range . cmd
        exec a:range . s:EscapeShellCmdChars(cmd)
    else
        exec a:range .'read '. s:EscapeShellCmdChars(s:EscapeFilename(a:parms['tfile']))
    endif
    if a:parms['autocommand'] =~ '^Buf'
        exec 'doautocmd BufRead '. s:EscapeFilename(expand("%:r"))
    endif
endf

fun!  TGpgWrite(parms)
    let secret = TGpgUserInput_{a:parms['mode']}('w', a:parms['file'])
    if !empty(secret) && exists('g:tgpgWrite_'. a:parms['mode'])
        let ftime = getftime(a:parms['tfile'])
        if filereadable(a:parms['tfile'])
            call rename(a:parms['tfile'], a:parms['gfile'])
        endif
        silent %yank t
        let cmd = printf(g:tgpgWrite_{a:parms['mode']}, secret, s:EscapeFilename(a:parms['tfile']))
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
            call rename(a:parms['gfile'], a:parms['tfile'])
        endif
    else
        exec "'[,']write ". s:EscapeShellCmdChars(s:EscapeFilename(a:parms['tfile']))
    endif
endf

fun! TGpgWrite_clearsign(parms)
    let imode = empty(a:parms['autocommand']) ? 'W' : 'w'
    let recipients = s:GetCachedSecret(imode, 'recipients', a:parms['file'])
    let secret = s:GetCachedSecret(imode, 'pw_clearsign', a:parms['file'])
    if !empty(secret) && !empty(recipients) && exists('g:tgpgWrite_'. a:parms['mode'])
        if filereadable(a:parms['gfile'])
            call delete(a:parms['gfile'])
        endif
        let cmd = printf(g:tgpgWrite_{a:parms['mode']}, recipients, secret, s:EscapeFilename(a:parms['gfile']))
        " TLog '%'. cmd
        silent exec '%'. s:EscapeShellCmdChars(cmd)
        silent exec '%read '. s:EscapeShellCmdChars(s:EscapeFilename(a:parms['gfile']))
        norm! ggdd
        call delete(a:parms['gfile'])
        exec 'write! '. s:EscapeShellCmdChars(s:EscapeFilename(a:parms['tfile']))
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
vast range of possible consequences.

This plugin uses the (Buf|File)(Read|Write)Cmd autocommand events to 
write/read the file. I'm not sure how this works out with other plugins 
using these events.

As I don't like typing passphrases, this plugin caches all the 
passphrases entered in a script local variable. This means that 
passphrases are likely to be written to the swapfile, from where 
somebody somehow could possibly do something ... Set g:tgpgCachePW to 1 
(buffer-wise caching only) or 0 (no caching) to change this.

This plugin was tested with Windows GVim & cygwin gpg 1.4.5 (using bash 
as shell) as well as linux vim & gpg 1.4.5. It's possible that the use 
of a pure Windows version of gpg or cmd.exe as shell doesn't work. 
(Please report problems.)

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

