" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
doc/vimform.txt	[[[1
204
*vimform.txt*       Simple text-based forms for vim
                    Author: Tom Link, micathom at gmail com

Example: >

    let form = vimform#New()
    let form.name = "Test Form"
    let form.fields = [
                \ ['Name'],
                \ ['Address',  {'join': "\n"}],
                \ ['Planet',   {'value': 'Earth'}],
                \ ['Phone',    {'validate': '%s =~ ''^[0-9()+-]*$''',
                \               'message': 'Must be a phone number'}],
                \ ['E-Mail',   {'validate': '%s =~ ''^\([a-zA-Z.]\+@[a-zA-Z]\+\.[a-zA-Z.]\+\)\?$''',
                \               'message': 'Must be an e-mail'}],
                \ ['Private',  {'value': 0, 'type': 'checkbox'}],
                \ ['Business', {'value': 1, 'type': 'checkbox'}],
                \ ]
    function! form.Do_Submit() dict "{{{3
        echom "Test: ". self.name
        for [field, value] in items(self.values)
            echom "Field" field value
        endfor
    endf

    call form.Split()

This will create a form, which looks like this: >

    <<&Submit>> <<&Cancel>>
         Name: 
      Address: 
       Planet: Earth
        Phone: 
       E-Mail: 
      Private: [ ]
     Business: [X]

Check out further examples on:
http://github.com/tomtom/vimform_vim/tree/master/test/

The following field types are currently supported:

    - text fields
    - check boxes
    - single choice/drop-down lists

                                                    *vimform-keys*
The following key maps can be used to navigate through forms:

<c-cr>          "Press" a button. If the cursor, is not over a button, 
                the form will be submitted, i.e. the Do_Submit() method 
                is called.

<c-w>c          Abandon/close/cancel the form.

<f5>            Redraw the form.

<tab>           Jump to the next field (this will also save the current 
                field's value so that it doesn't get lost when redrawing 
                a form)

<s-tab>         Jump to the previous field

<c-x><c-o>
<c-space>       Invoke the completion menu if a completion function is 
                defined for the current field.

`[LETTER]       Accelerator keys are prefixed with an ampersand (&). 
                Those letters are defined as |mark|s. Jump to a button 
                with |`|.

<LeftMouse>
<space>
<cr>            If the cursor is located over a checkbox, toggle that 
                check box. If the cursor is over a drop-down list, open the 
                list.

a
i               Switch to insert mode.

<c-n> <c-p> <c-y> <c-e>
                Navigate drop-down lists. See also |ins-completion|.


-----------------------------------------------------------------------
Install~

In order to install the vba, open the vba file in VIM and type: >

    :so %

See :help vimball for details.

Also available via git: http://github.com/tomtom/vimform_vim/


========================================================================
Contents~

        :Vimform ....................... |:Vimform|
        vimform#New .................... |vimform#New()|
        vimform#SimpleForm ............. |vimform#SimpleForm()|
        :VimformReset .................. |:VimformReset|
        g:vimform#forms ................ |g:vimform#forms|
        g:vimform#view ................. |g:vimform#view|
        g:vimform#prototype ............ |g:vimform#prototype|
        g:vimform#widgets .............. |g:vimform#widgets|
        vimform#AppendOrInsert ......... |vimform#AppendOrInsert()|
        vimform#Insertmode ............. |vimform#Insertmode()|
        vimform#Feedkeys ............... |vimform#Feedkeys()|
        vimform#PumKey ................. |vimform#PumKey()|
        vimform#SpecialInsertKey ....... |vimform#SpecialInsertKey()|
        vimform#Balloon ................ |vimform#Balloon()|
        vimform#Complete1 .............. |vimform#Complete1()|
        vimform#Complete ............... |vimform#Complete()|
        vimform#CompleteSingleChoice ... |vimform#CompleteSingleChoice()|
        vimform#CommandComplete ........ |vimform#CommandComplete()|
        vimform#Command ................ |vimform#Command()|


========================================================================
plugin/vimform.vim~

                                                    *:Vimform*
:Vimform


========================================================================
autoload/vimform.vim~

                                                    *vimform#New()*
vimform#New()
    Return the default form template.

                                                    *vimform#SimpleForm()*
vimform#SimpleForm()
    Return a form template that has no default buttons.

                                                    *:VimformReset*
:VimformReset
    Reset the current form.
    If called with [!], use the original values. Otherwise try to reuse 
    the current values.

                                                    *g:vimform#forms*
g:vimform#forms                (default: {})

                                                    *g:vimform#view*
g:vimform#view                 (default: "split")

                                                    *g:vimform#prototype*
g:vimform#prototype            (default: {...})
    The default form tepmlate.

                                                    *g:vimform#widgets*
g:vimform#widgets              (default: {})



    Show the form in a split window.


g:vimform#prototype#Show(?cmd = "split")
    Show the form.
    cmd should create a new buffer. By default, the new buffer will be 
    shown in a split view.

                                                    *vimform#AppendOrInsert()*
vimform#AppendOrInsert()

                                                    *vimform#Insertmode()*
vimform#Insertmode()

                                                    *vimform#Feedkeys()*
g:vimform#prototype.GetCurrentFieldName(?pos = '.') dict "{{{3

                                                    *vimform#PumKey()*
vimform#PumKey(key)

                                                    *vimform#SpecialInsertKey()*
vimform#SpecialInsertKey(key, pumkey, prepend)

                                                    *vimform#Balloon()*
vimform#Balloon()

                                                    *vimform#Complete1()*
vimform#Complete1()

                                                    *vimform#Complete()*
vimform#Complete(findstart, base)

                                                    *vimform#CompleteSingleChoice()*
vimform#CompleteSingleChoice(findstart, base)

                                                    *vimform#CommandComplete()*
vimform#CommandComplete(ArgLead, CmdLine, CursorPos)

                                                    *vimform#Command()*
vimform#Command(cmd)



vim:tw=78:fo=tcq2:isk=!-~,^*,^|,^":ts=8:ft=help:norl:
autoload/vimform.vim	[[[1
773
" vimform.vim -- Simple forms for vim scripts
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2008-07-16.
" @Last Change: 2010-04-14.
" @Revision:    0.0.1493

let s:save_cpo = &cpo
set cpo&vim


augroup Vimform
    autocmd!
augroup END


" Return the default form template.
function! vimform#New() "{{{3
    let form = deepcopy(g:vimform#prototype)
    return form
endf


" Return a form template that has no default buttons.
function! vimform#SimpleForm() "{{{3
    let form = vimform#New()
    let form.buttons = []
    return form
endf


" Reset the current form.
" If called with [!], use the original values. Otherwise try to reuse 
" the current values.
command! -bang VimformReset if !exists('b:vimform')
            \ |     echoerr 'Not a VimForm buffer'
            \ | else 
            \ |     call b:vimform.Reset(!empty('<bang>'))
            \ | endif


if !exists('g:vimform#forms')
    let g:vimform#forms = {}   "{{{2
endif


if !exists('g:vimform#view')
    let g:vimform#view = "split"   "{{{2
endif


if !exists('g:vimform#prototype')
    " The default form tepmlate.
    " :read: let g:vimform#prototype = {...}   "{{{2
    let g:vimform#prototype = {
                \ 'name': '__Form__',
                \ 'indent': 0,
                \ 'options': '',
                \ 'buttons': [
                \   {'name': 'Submit', 'label': '&Submit'},
                \   {'name': 'Cancel', 'label': '&Cancel'},
                \ ],
                \ 'values': {},
                \ 'fields': [],
                \ '_fields': {},
                \ '_formattedlabels': {},
                \ 'mapargs': {},
                \ 'header': [
                \   '<F1>:help; <F5>:redraw; <TAB>:next field; <C-CR>:submit'
                \ ],
                \ 'footer': [
                \ ]}
endif


if !exists('g:vimform#widgets')
    let g:vimform#widgets = {}   "{{{2
    runtime! autoload/vimform/widgets/*.vim
endif



let s:vimform_modification = 0
let s:indent_plus = 3
let s:skip_line_rx = '\V\^\(" \.\+\|_\+ \.\{-} _\+\)\$'
let s:special_line_rx = s:skip_line_rx .'\V\|\^\(| \.\{-} |\)\$'
let s:done_commands = 0


function! g:vimform#prototype.Delegate(fieldname, method, ...) dict "{{{3
    " TLogVAR "Delegate", a:fieldname, a:method, a:000
    if !empty(a:fieldname)
        let field = self._fields[a:fieldname]
        return call(field[a:method], [self] + a:000, field)
    endif
endf


function! g:vimform#prototype.DelegateCurrentField(method, ...) dict "{{{3
    " TLogVAR "DelegateCurrentField", a:method, a:000
    " TLogVAR keys(self)
    let fieldname = self.GetCurrentFieldName()
    return call(self.Delegate, [fieldname, a:method] + a:000, self)
endf


" Show the form in a split window.
function! g:vimform#prototype.Split() dict "{{{3
    call self.Show('split')
endf


" :display: g:vimform#prototype#Show(?cmd = "split")
" Show the form.
" cmd should create a new buffer. By default, the new buffer will be 
" shown in a split view.
function! g:vimform#prototype.Show(...) dict "{{{3
    let cmd = a:0 >= 1 ? a:1 : g:vimform#view
    silent exec cmd fnameescape(self.name)
    let self.bufnr = bufnr('%')
    let b:vimform = self.Setup()
    setlocal filetype=vimform
    if !empty(self.options)
        exec 'setlocal '. self.options
    endif
    call self.SetIndent()
    call self.Display()
    autocmd! Vimform * <buffer>
    autocmd Vimform CursorMoved,CursorMovedI <buffer> call b:vimform.CursorMoved()
endf


function! g:vimform#prototype.Setup() dict "{{{3
    let self._fields = {}
    for def in self.fields
        let fieldname = get(def, 0)
        if fieldname !~ '^-'
            let def1 = get(def, 1, {})
            let type = get(def1, 'type', 'text')
            " TLogVAR fieldname, type
            call extend(def1, g:vimform#widgets[type], 'keep')
            let def1.name = fieldname
            let def1.formattedlabel = def1.FormatLabel(self)
            let self._formattedlabels[def1.formattedlabel] = fieldname
            let self._fields[fieldname] = def1
        endif
    endfor

    let self._buttons = {}
    for button in self.buttons
        let buttonname = self.GetButtonLabel(button)
        let self._buttons[buttonname] = button
    endfor

    return self
endf


function! g:vimform#prototype.Reset(vanilla) dict "{{{3
    if a:vanilla
        let self.values = {}
    " else
    "     call self.CollectFields()
    endif
    call self.Show('edit')
endf


function! g:vimform#prototype.Display() dict "{{{3
    setlocal modifiable
    " TLogVAR line('$')
    %delete

    let width = winwidth(0) - &foldcolumn - 4

    if !empty(self.header)
        call append('$', map(copy(self.header), 'printf(''" %''. (width - 2) .''s "'', v:val)'))
    endif

    let fmt = ' %'. (self.indent - s:indent_plus) .'s: %s'
    " TLogVAR fmt
    for def0 in self.fields
        let fieldname = get(def0, 0)
        if fieldname =~ '^-'
            let text = matchstr(fieldname, '^-\+\s\+\zs.*$')
            let npre = self.indent - 1
            let npost = width - npre - len(text)
            let line = repeat('_', npre) .' '. text .' '. repeat('_', npost)
        else
            let def = self._fields[fieldname]
            let type = get(def, 'type', 'text')
            let value = get(self.values, fieldname, get(def, 'value', ''))
            let text = self.Delegate(fieldname, 'Format', value)
            let line = printf(fmt, def.formattedlabel, text)
        endif
        call append('$', line)
    endfor

    let formatted_buttons = []
    for button in self.buttons
        call add(formatted_buttons, s:FormatButton(self.GetButtonLabel(button)))
    endfor
    if !empty(formatted_buttons)
        let formatted_buttons_str = printf('| %'. (width - 2) .'s |', join(formatted_buttons))
        call append('$', formatted_buttons_str)
    endif

    if !empty(self.footer)
        call append('$', map(copy(self.footer), 'printf(''" %''. (width - 2) .''s "'', v:val)'))
    endif

    0delete
    call s:SetAccellerators()
    norm! ggzt
    call self.NextField('cw', 0, 1)
endf


function! s:EnsureBuffer() "{{{3
    if !exists('b:vimform') || bufnr('%') != b:vimform.bufnr
        " TLogVAR bufnr('%'), self.bufnr
        throw "Vimform: Wrong buffer"
    endif
endf


function! s:FormatButton(buttonname) "{{{3
    return printf('<<%s>>', a:buttonname)
endf


function! s:SetAccellerators() "{{{3
    norm! ggzt
    while search('&', 'W')
        let acc = tolower(getline('.')[col('.')])
        " TLogVAR acc
        if acc =~# '^[a-z]$'
            exec 'norm! m'. acc
        endif
    endwh
endf


function! g:vimform#prototype.Submit() dict "{{{3
    let m = matchlist(getline('.'), '<<\([^>]\{-}\%'. col('.') .'c[^>]\{-}\)>>')
    if empty(m)
        let buttonname = 'Submit'
    else
        let buttonname = substitute(m[1], '&', '', 'g')
        let buttonname = substitute(buttonname, '\W', '_', 'g')
        " TLogVAR buttonname
    endif
    call self.CollectFields()
    if self.Validate()
        let cb_name = 'Do_'. buttonname
        if buttonname == 'Cancel'
            call self.Do_Cancel()
        elseif buttonname == 'Submit'
            call self.Do_Cancel()
            call self.Do_Submit()
        elseif has_key(self, cb_name)
            call self.{cb_name}()
        else
            throw "VimForm: Unknown button: ". buttonname
        endif
    endif
endf


function! g:vimform#prototype.Do_Submit() dict "{{{3
endf


function! g:vimform#prototype.Do_Cancel() dict "{{{3
    wincmd c
endf


function! g:vimform#prototype.Validate() dict "{{{3
    let invalid_values = filter(copy(self.values), '!self.ValidateField(v:key, v:val)')
    if !empty(invalid_values)
        echohl WarningMsg
        let msgs = []
        let error_rx = map(keys(invalid_values), 'self.GetFieldRx(v:val)')
        exec '3match Error /'. escape(join(error_rx, '\|'), '/') .'/'
        redraw
        for [field, value] in items(invalid_values)
            let def = self._fields[field]
            let msg = 'Invalid value for '. field .': '. string(value)
            if len(def) > 1 && has_key(def, 'message')
                let msg .= ': '. def.message
            endif
            echom msg
        endfor
        echohl MoreMsg
        echo "Press any KEY to continue"
        echohl NONE
        call getchar()
        call search(error_rx[0], 'ew')
        call vimform#Feedkeys('a', 1)
        return 0
    else
        3match none
        return 1
    endif
endf


function! g:vimform#prototype.ValidateField(field, value) dict "{{{3
    let validate = self.Delegate(a:field, 'GetValidate')
    if empty(validate)
        return 1
    else
        return eval(printf(validate, string(a:value)))
    endif
endf


function! g:vimform#prototype.NextField(flags, in_insertmode, to_insertmode) dict "{{{3
    " TLogVAR a:flags, a:in_insertmode, a:to_insertmode
    call s:EnsureBuffer()
    exec 'resize '. line('$')
    let frx = self.GetFieldsRx()
    let brx = self.GetButtonsRx()
    let rx = frx .'\|'. brx
    " TLogVAR rx
    let fieldname = self.GetCurrentFieldName()
    if !empty(fieldname)
        let self.values[fieldname] = self.GetField(fieldname)
        if a:flags =~ 'b'
            norm! 0
        endif
    endif
    let lnum = search(rx, 'e'. a:flags)
    if lnum && getline(lnum) =~ frx
        " TLogVAR lnum
        call cursor(lnum, self.indent + 1)
        call self.DelegateCurrentField('SelectField', a:to_insertmode)
    endif
endf


function! vimform#AppendOrInsert() "{{{3
    let val = col('.') == col('$') - 1 ? 'a' : 'i'
    " TLogVAR col('.'), col('$'), val
    return val
endf


function! vimform#Insertmode() "{{{3
    call vimform#Feedkeys(vimform#AppendOrInsert(), 0)
endf


" :display: g:vimform#prototype.GetCurrentFieldName(?pos = '.') dict "{{{3
function! g:vimform#prototype.GetCurrentFieldName(...) dict "{{{3
    let frx = self.GetFieldsRx() .'\|'. s:special_line_rx
    " TLogVAR frx
    let view = winsaveview()
    try
        if a:0 >= 1
            call setpos('.', a:1)
        endif
        let fieldname = ''
        let lnum = search(frx, 'bcnW')
        if lnum
            let fieldname = matchstr(getline(lnum), self.GetFieldRx('\zs\.\{-}\ze'))
            " TLogVAR getline(lnum), fieldname
            if !empty(fieldname)
                let fieldname = self._formattedlabels[fieldname]
            endif
        endif
        " TLogVAR line('.'), lnum, fieldname
        return fieldname
    finally
        call winrestview(view)
    endtry
endf


function! g:vimform#prototype.GetFieldType(fieldname) dict "{{{3
    return get(self._fields[a:fieldname], 'type', 'text')
endf


function! vimform#Feedkeys(keys, level) "{{{3
    call b:vimform.SetModifiable(a:level)
    " TLogVAR a:keys, a:level, col('.'), col('$'), &modifiable
    call feedkeys(a:keys, 't')
endf


function! g:vimform#prototype.CursorMoved() dict "{{{3
    let lnum = line('.')
    let line = getline(lnum)
    " TLogVAR line
    if line =~ s:skip_line_rx
        call self.NextField('w', mode() == 'i', mode() != 'i')
    else
        " TLogVAR line, len(line)
        " TLogVAR col('$'), self.indent, mode()
        if col('$') - 1 < self.indent
            let diff = self.indent - len(line)
            let line .= repeat(' ', diff)
            " let vimform_modification = s:vimform_modification
            call self.SetModifiable(1)
            call setline(lnum, line)
            " let s:vimform_modification = vimform_modification
        endif
        call self.DelegateCurrentField('SetCursorMoved', mode() == 'i', lnum)
        " TLogVAR &modifiable, field, col('.'), self.indent
        call self.SetModifiable()
    endif
endf


function! g:vimform#prototype.SetModifiable(...) dict "{{{3
    if a:0 >= 1
        if a:1 >= 0
            let s:vimform_modification += a:1
        " elseif a:1 == 0
        "     let s:vimform_modification = 0
        else
            let s:vimform_modification = a:1
        endif
    endif
    " echom "DBG s:vimform_modification=". s:vimform_modification
    if s:vimform_modification < 0
        let modifiable = 1
    elseif s:vimform_modification > 0
        let s:vimform_modification -= 1
        let modifiable = 1
    else
        let line = getline('.')
        if line =~ s:special_line_rx
            let modifiable = 0
        else
            let fieldname = self.GetCurrentFieldName()
            " TLogVAR fieldname
            if empty(fieldname) || !self._fields[fieldname].modifiable
                let modifiable = 0
            else
                " TLogVAR col('.'), self.indent
                if col('.') <= self.indent
                    let modifiable = 0
                else
                    let modifiable = self._fields[fieldname].modifiable
                endif
            endif
        endif
    endif
    " TLogVAR modifiable
    " let &l:modifiable = modifiable
    if modifiable
        setlocal modifiable
    else
        setlocal nomodifiable
    endif
endf


function! g:vimform#prototype.SaveMapargs(...) dict "{{{3
    " TLogVAR a:000
    for map in a:000
        let arg = maparg(map)
        let arg = eval('"'. escape(substitute(arg, '<', '\\<', 'g'), '"') .'"')
        let self.mapargs[map] = arg
    endfor
endf


function! vimform#PumKey(key) "{{{3
    " TLogVAR a:key
    if pumvisible()
        let self = b:vimform
        call self.SetModifiable(1)
        let key = self.DelegateCurrentField('GetPumKey', a:key)
        return key
    else
        return a:key
    endif
endf


function! vimform#SpecialInsertKey(key, pumkey, prepend) "{{{3
    " TLogVAR a:key, a:prepend
    if pumvisible()
        return a:pumkey
    else
        let self = b:vimform
        let key = self.DelegateCurrentField('GetSpecialInsertKey', a:key)
        if a:prepend
            let key = a:key . key
        endif
        return key
    endif
endf


function! g:vimform#prototype.SpecialKey(key, insertmode) dict "{{{3
    let mode = 'n'
    let key = a:key
    " TLogVAR key, pumvisible()
    if !pumvisible()
        let fieldname = self.GetCurrentFieldName()
        let key = self.Delegate(fieldname, 'GetSpecialKey', fieldname, key)
    endif
    if !empty(key)
        " TLogVAR key, mode
        call feedkeys(key, mode)
    endif
endf


function! g:vimform#prototype.Key(key) dict "{{{3
    let key = self.DelegateCurrentField('GetKey', a:key)
    " TLogVAR a:key, key
    return key
endf


function! g:vimform#prototype.Key_dd() dict "{{{3
    let key = self.DelegateCurrentField('Key_dd')
    return key
endf


function! g:vimform#prototype.Key_BS() dict "{{{3
    let key = self.DelegateCurrentField('Key_BS')
    return key
endf


function! g:vimform#prototype.Key_DEL() dict "{{{3
    let key = self.DelegateCurrentField('Key_DEL')
    return key
endf


function! g:vimform#prototype.SetIndent() dict "{{{3
    let self.indent = max(map(copy(self._fields), 'len(v:val.formattedlabel) + s:indent_plus'))
endf


function! g:vimform#prototype.GetButtonLabel(def) dict "{{{3
    return get(a:def, 'label', a:def.name)
endf


function! g:vimform#prototype.GetButtonsRx() dict "{{{3
    let button_labels = map(copy(self.buttons), 'self.GetButtonLabel(v:val)')
    call map(button_labels, 'strpart(v:val, 1)')
    let rx = '\V<<\.\ze\('. join(button_labels, '\|') .'\)>>'
    " TLogVAR rx
    return rx
endf


function! g:vimform#prototype.CollectFields() dict "{{{3
    let self.values = self.GetAllFields()
endf


function! g:vimform#prototype.GetAllFields() dict "{{{3
    let dict = {}
    let names = self.GetOrderedFieldNames()
    " TLogVAR names
    for fieldname in names
        let dict[fieldname] = self.GetField(fieldname, names)
    endfor
    return dict
endf


function! g:vimform#prototype.GetOrderedFieldNames() dict "{{{3
    return filter(map(copy(self.fields), 'v:val[0]'), 'v:val !~ ''^-''')
endf


function! g:vimform#prototype.GetField(fieldname, ...) dict "{{{3
    call s:EnsureBuffer()
    let quiet = a:0 >= 1
    let names = a:0 >= 1 ? a:1 : self.GetOrderedFieldNames()
    let index = index(names, a:fieldname)
    if index == -1
        echoerr 'VimForm: No field of that name:' a:fieldname
    else
        let view = winsaveview()
        let def = self._fields[a:fieldname]
        try
            let crx = self.GetFieldRx(a:fieldname)
            let start = search(crx, 'w')
            " TLogVAR a:fieldname, crx, start
            if start
                if index < len(names) - 1
                    let nrx = self.GetFieldsRx() .'\|'. s:special_line_rx
                    let end = search(nrx, 'w') - 1
                else
                    let end = line('$')
                endif
                " TLogVAR end
                if end
                    let lines = getline(start, end)
                    call map(lines, 'strpart(v:val, self.indent)')
                    " TLogVAR lines
                    if has_key(def, 'join')
                        let ljoin = def.join
                        let pjoin = def.join
                    else
                        let ljoin = get(def, 'joinlines', ' ')
                        let pjoin = get(def, 'joinparas', "\n")
                    endif
                    let out = []
                    for line in lines
                        if line =~ '\S'
                            if len(out) > 0 && out[-1] != "\n"
                                call add(out, ljoin)
                            endif
                            call add(out, line)
                        elseif len(out) > 0
                            call add(out, pjoin)
                        endif
                    endfor
                    let value = self.Delegate(a:fieldname, 'GetFieldValue', join(out, ''))
                    " TLogVAR ljoin, pjoin, out, value
                    let return = get(def, 'return', {})
                    if !empty(return) && has_key(return, value)
                        return return[value]
                    else
                        return value
                    endif
                endif
            endif
            return def.default_value
        finally
            call winrestview(view)
        endtry
    endif
endf


function! g:vimform#prototype.GetIndentRx() dict "{{{3
    return '\V\s\{'. self.indent .'}'
endf


function! g:vimform#prototype.GetFieldRx(fieldname) dict "{{{3
    return '\V\^ \+'. a:fieldname .':\%'. self.indent .'c '
endf


function! g:vimform#prototype.GetFieldsRx() dict "{{{3
    let rxs = map(values(self._fields), 'escape(v:val.formattedlabel, ''\'')')
    return '\V\^ \+\('. join(rxs, '\|') .'\): '
endf


function! g:vimform#prototype.Indent() dict "{{{3
    let indent = self.indent
    let cline = getline(v:lnum)
    let rx = self.GetFieldsRx()
    " TLogVAR v:lnum, indent, cline
    " call tlog#Debug(cline =~# self.GetFieldsRx())
    if cline =~# self.GetFieldsRx()
        let indent = 0
    endif
    " TLogVAR indent
    return indent
endf


function! vimform#Balloon() "{{{3
    call s:EnsureBuffer()
    let pos = [v:beval_bufnr, v:beval_lnum, v:beval_col, 0]
    let field = b:vimform.GetCurrentFieldName(pos)
    if !empty(field)
        return get(b:vimform._fields[field], 'tooltip', '')
    endif
endf


function! vimform#Complete1() "{{{3
    return pumvisible() ? "\<c-n>" : "\<c-x>\<c-o>"
endf


function! vimform#Complete(findstart, base) "{{{3
    if exists('b:vimform')
        let self = b:vimform
        if a:findstart
            let field = self.GetCurrentFieldName()
            " TLogVAR a:findstart, a:base, field
            let def = self._fields[field]
            let type = self.GetFieldType(field)
            if type == 'singlechoice'
                let s:vimform_list = get(def, 'list', [])
            endif
            let b:vimform_complete = get(def, 'complete', '')
        endif
        if empty(b:vimform_complete)
            if a:findstart
                return col('.')
            else
                return ''
            endif
        else
            return call(b:vimform_complete, [a:findstart, a:base])
        endif
    endif
endf


function! vimform#CompleteSingleChoice(findstart, base) "{{{3
    " TLogVAR a:findstart, a:base
    let self = b:vimform
    " if a:findstart == -1
    "     let rx = '\V'. escape(a:base, '\')
    "     let list = filter(copy(s:vimform_list), 'v:val =~ rx')
    "     " TLogVAR list
    "     call complete(b:vimform.indent + 1, list)
    "     return ''
    " elseif a:findstart
    if a:findstart
        let self = b:vimform
        return self.indent
    else
        let rx = '\V'. escape(a:base, '\')
        call self.SetModifiable(1)
        " return filter(copy(s:vimform_list), 'v:val =~ rx')
        return s:vimform_list
    endif
endf


function! vimform#CommandComplete(ArgLead, CmdLine, CursorPos) "{{{3
    if !s:done_commands
        let s:done_commands = 1
        runtime! autoload/vimform/forms/*.vim
    endif
    let commands = copy(g:vimform#forms)
    " TLogVAR commands
    if !empty(a:ArgLead)
        call filter(commands, 'a:ArgLead =~ v:val.rx')
    endif
    " TLogVAR commands
    return keys(commands)
endf


function! vimform#Command(cmd) "{{{3
    let cmds = vimform#CommandComplete(a:cmd, '', 0)
    " TLogVAR cmds
    if len(cmds) == 1
        let form = g:vimform#forms[cmds[0]]
        call form.Show(g:vimform#view)
    else
        echoerr "Vimform: Unknown or ambivalent command: ". a:cmd
    endif
endf


let &cpo = s:save_cpo
unlet s:save_cpo

finish

0.1
Initial

0.2
- Changed syntax & everything

autoload/vimform/forms/substitute.vim	[[[1
61
" substitute.vim
" @Author:      Thomas Link (mailto:micathom AT gmail com?subject=[vim])
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2010-04-13.
" @Last Change: 2010-04-14.
" @Revision:    0.0.29


let s:substitute_form = vimform#SimpleForm()
let s:substitute_form.name = "Search & Replace"
let s:substitute_form.rx = '^s\%[ubstitute]'
let s:substitute_form.options = 'tw=0'
let s:substitute_form.fields = [
            \ ['Search', {'tooltip': 'A regular expression', 'join': '\n', 'validate': 'vimform#forms#substitute#ValidateRegexp(%s)'}],
            \ ['Replace', {'tooltip': 'The replacement expression', 'validate': 'vimform#forms#substitute#ValidateSubst(%s)'}],
            \ ['--- Options'],
            \ ['Range', {'value': '1,$'}],
            \ ['Replace all', {'value': 1, 'type': 'checkbox', 'return': {'1': 'g', '0': ''}}],
            \ ['Case-sensitive', {'type': 'checkbox', 'return': {'1': 'I', '0': ''}}],
            \ ['Confirm', {'value': 1, 'type': 'checkbox', 'return': {'1': 'c', '0': ''}}],
            \ ]

function! s:substitute_form.Do_Submit() dict "{{{3
    let search = self.values['Search']
    if !empty(search)
        let flags = map(["Replace all", "Case-sensitive", "Confirm"], 'self.values[v:val]')
        let replace = self.values['Replace']
        let cmd = printf(':%ss/%s/%s/%se',
                    \ self.values.Range,
                    \ escape(search, '/'),
                    \ escape(replace, '/'),
                    \ join(flags, ''))
        " TLogVAR cmd
        call feedkeys(cmd ."\<cr>", 'n')
    endif
endf


function! vimform#forms#substitute#ValidateRegexp(rx) "{{{3
    try
        call match("x", a:rx)
        return 1
    catch
        return 0
    endtry
endf


function! vimform#forms#substitute#ValidateSubst(str) "{{{3
    try
        call substitute('a', '.', a:str, '')
        return 1
    catch
        return 0
    endtry
endf


let g:vimform#forms['substitute'] = s:substitute_form


autoload/vimform/widget.vim	[[[1
145
" widget.vim
" @Author:      Thomas Link (mailto:micathom AT gmail com?subject=[vim])
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2010-04-14.
" @Last Change: 2010-04-14.
" @Revision:    0.0.10

let s:save_cpo = &cpo
set cpo&vim


let s:prototype = {
            \ 'modifiable': 0,
            \ 'default_value': 0,
            \ 'complete': '',
            \ }


function! s:prototype.FormatLabel(form) dict "{{{3
    return self.name
endf


function! s:prototype.Format(form, value) dict "{{{3
    return a:value
endf


function! s:prototype.GetFieldValue(form, value) dict "{{{3
    return a:value
endf


function! s:prototype.SelectField(form, to_insertmode) dict "{{{3
    " TLogVAR a:to_insertmode
    if self.modifiable && a:to_insertmode
        call vimform#Insertmode()
    endif
endf


function! s:prototype.GetValidate(form) dict "{{{3
    return get(self, 'validate', '')
endf


function! s:prototype.SetCursorMoved(form, insertmode, lnum) dict "{{{3
    " TLogVAR a:insertmode, a:lnum, col('.'), a:form.indent
    if col('.') <= a:form.indent
        call cursor(a:lnum, a:form.indent + 1)
    endif
endf


function! s:prototype.GetPumKey(form, key) dict "{{{3
    return a:key
endf


function! s:prototype.GetSpecialInsertKey(form, key) dict "{{{3
    return a:key
endf


function! s:prototype.GetSpecialKey(form, name, key) dict "{{{3
    return get(a:form.mapargs, a:key, a:key)
endf


function! s:prototype.GetKey(form, key) dict "{{{3
    let key = a:key
    if a:key =~ '^[ai]$'
        let ccol = col('.')
        " TLogVAR ccol, ecol, a:form.indent
        if a:key == 'a' && ccol < a:form.indent
            let key = ''
        elseif a:key == 'i' && ccol <= a:form.indent
            let key = ''
        elseif ccol >= a:form.indent && self.modifiable
            call a:form.SetModifiable(1)
        endif
    endif
    return key
endf


function! s:prototype.Key_dd(form) dict "{{{3
    let key  = 'dd'
    let lnum = line('.')
    let line = getline(lnum)
    let frx  = a:form.GetFieldsRx()
    let steps = 1
    if line =~ frx
        if lnum < line('$') && getline(lnum + 1) =~ a:form.GetIndentRx()
            let key = a:form.indent .'|d$J'
            let steps += 2
        elseif empty(strpart(line, a:form.indent))
            let key = ''
        else
            let key = a:form.indent .'|d$'
            let steps += 1
        endif
    elseif lnum < line('$') && getline(lnum + 1) =~ frx
        let key .= 'k'
    endif
    if !empty(key) && self.modifiable
        call a:form.SetModifiable(steps)
    endif
    return key
endf


function! s:prototype.Key_BS(form) dict "{{{3
    let min = a:form.indent + 1
    " (mode() == 'n')
    if col('.') <= min
        let key = ''
    elseif self.modifiable
        let key = mode() == 'n' ? 'X' : "\<bs>"
        call a:form.SetModifiable(1)
    endif
    return key
endf


function! s:prototype.Key_DEL(form) dict "{{{3
    let lnum = line('.')
    let frx  = a:form.GetFieldsRx()
    if col('.') >= col('$') && (lnum == line('$') || getline(lnum + 1) =~ frx)
        let key = ''
    elseif self.modifiable
        let key = "\<del>"
        call a:form.SetModifiable(1)
    endif
    return key
endf


function! vimform#widget#New() "{{{3
    return deepcopy(s:prototype)
endf


let &cpo = s:save_cpo
unlet s:save_cpo
autoload/vimform/widgets/checkbox.vim	[[[1
96
" checkbox.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2010-04-14.
" @Last Change: 2010-04-14.
" @Revision:    14

let s:save_cpo = &cpo
set cpo&vim


if has_key(g:vimform#widgets, 'checkbox')
    finish
endif


let s:prototype = vimform#widget#New()


function! s:prototype.Format(form, value) dict "{{{3
    return printf('[%s]', empty(a:value) ? ' ' : 'X')
endf


function! s:prototype.GetFieldValue(form, value) dict "{{{3
    return a:value =~ 'X'
endf


function! s:prototype.SelectField(form, to_insertmode) dict "{{{3
    call vimform#Feedkeys('l', 0)
endf


function! s:prototype.SetCursorMoved(form, insertmode, lnum) dict "{{{3
    if a:insertmode
        call feedkeys("\<esc>", 't')
    endif
    call cursor(a:lnum, a:form.indent + 2)
endf


function! s:prototype.GetPumKey(form, key) dict "{{{3
    return a:key ."\<esc>"
endf


function! s:prototype.GetSpecialInsertKey(form, key) dict "{{{3
    return "\<esc>"
endf


function! s:prototype.GetSpecialKey(form, name, key) dict "{{{3
    let line = getline('.')
    let notchecked = line =~ '\[ \]$'
    if notchecked
        let value = 'X'
    else
        let value = ' '
    endif
    let a:form.values[a:name] = !notchecked
    " TLogVAR value
    call a:form.SetModifiable(1)
    let line = substitute(line, '\[\zs.\ze\]$', value, '')
    call setline('.', line)
    return ''
endf


function! s:prototype.GetKey(form, key) dict "{{{3
    return "\<cr>"
endf


function! s:prototype.Key_dd(form) dict "{{{3
    return ''
endf


function! s:prototype.Key_BS(form) dict "{{{3
    return ''
endf


function! s:prototype.Key_DEL(form) dict "{{{3
    return ''
endf



let g:vimform#widgets['checkbox'] = s:prototype


let &cpo = s:save_cpo
unlet s:save_cpo
autoload/vimform/widgets/dir.vim	[[[1
72
" dir.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2010-04-14.
" @Last Change: 2010-04-14.
" @Revision:    79

let s:save_cpo = &cpo
set cpo&vim

if has_key(g:vimform#widgets, 'dir')
    finish
endif


let s:prototype = vimform#widget#New()
let s:prototype.modifiable = !has('browse')


" function! s:prototype.FormatLabel(form) dict "{{{3
"     return 'Dir| '. self.name
" endf


function! s:prototype.Format(form, value) dict "{{{3
    return empty(a:value) ? '<Browse>' : a:value
endf


function! s:prototype.GetFieldValue(form, value) dict "{{{3
    return a:value ==# '<Browse>' ? '' : a:value
endf


if has('browse')
    function! s:prototype.GetSpecialKey(form, name, key) dict "{{{3
        " TLogVAR a:name, a:key
        " let dir = get(self, 'cd', expand('%:h'))
        let dir = get(self, 'cd', '.')
        let dir = browsedir('Select dir', dir)
        if empty(dir)
            return ''
        else
            " TLOgVAR dir
            let line = strpart(getline('.'), 0, a:form.indent) . dir
            call a:form.SetModifiable(1)
            call setline('.', line)
            return ''
        endif
    endf
else
    function! s:prototype.GetSpecialKey(form, name, key) dict "{{{3
        call a:form.SetModifiable(2)
        let line = strpart(getline('.'), 0, a:form.indent)
        call setline('.', line)
        return "$a\<c-x>\<c-f>"
    endf
endif


function! s:prototype.Key_dd(form) dict "{{{3
    call a:form.SetModifiable(2)
    return (a:form.indent + 1) ."|d$<Browse>\<esc>"
endf


let g:vimform#widgets['dir'] = s:prototype


let &cpo = s:save_cpo
unlet s:save_cpo
autoload/vimform/widgets/file.vim	[[[1
72
" file.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2010-04-14.
" @Last Change: 2010-04-14.
" @Revision:    85

let s:save_cpo = &cpo
set cpo&vim

if has_key(g:vimform#widgets, 'file')
    finish
endif


let s:prototype = vimform#widget#New()
let s:prototype.modifiable = !has('browse')


" function! s:prototype.FormatLabel(form) dict "{{{3
"     return 'File| '. self.name
" endf


function! s:prototype.Format(form, value) dict "{{{3
    return empty(a:value) ? '<Browse>' : a:value
endf


function! s:prototype.GetFieldValue(form, value) dict "{{{3
    return a:value ==# '<Browse>' ? '' : a:value
endf


if has('browse')
    function! s:prototype.GetSpecialKey(form, name, key) dict "{{{3
        " TLogVAR a:name, a:key
        " let dir = get(self, 'cd', expand('%:h'))
        let dir = get(self, 'cd', '.')
        let file = browse(get(self, 'filesave', 1), 'Select file', dir, '')
        if empty(file)
            return ''
        else
            " TLOgVAR file
            let line = strpart(getline('.'), 0, a:form.indent) . file
            call a:form.SetModifiable(1)
            call setline('.', line)
            return ''
        endif
    endf
else
    function! s:prototype.GetSpecialKey(form, name, key) dict "{{{3
        call a:form.SetModifiable(2)
        let line = strpart(getline('.'), 0, a:form.indent)
        call setline('.', line)
        return "$a\<c-x>\<c-f>"
    endf
endif


function! s:prototype.Key_dd(form) dict "{{{3
    call a:form.SetModifiable(3)
    return (a:form.indent + 1) ."|d$a<Browse>\<esc>"
endf


let g:vimform#widgets['file'] = s:prototype


let &cpo = s:save_cpo
unlet s:save_cpo
autoload/vimform/widgets/singlechoice.vim	[[[1
83
" singlechoice.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2010-04-14.
" @Last Change: 2010-04-14.
" @Revision:    22

let s:save_cpo = &cpo
set cpo&vim


if has_key(g:vimform#widgets, 'singlechoice')
    finish
endif


let s:prototype = vimform#widget#New()
let s:prototype.complete = 'vimform#CompleteSingleChoice'


function! s:prototype.GetValidate(form) dict "{{{3
    " TLogVAR self
    if has_key(self, 'list')
        let list = get(self, 'list', [])
        return get(self, 'validate', 'index('. string(list) .', %s) != -1')
    else
        return get(self, 'validate', '')
    endif
endf


function! s:prototype.SetCursorMoved(form, insertmode, lnum) dict "{{{3
    if a:insertmode
        call feedkeys("\<esc>$", 't')
    endif
endf


function! s:prototype.GetPumKey(form, key) dict "{{{3
    return a:key ."\<esc>"
endf


function! s:prototype.GetSpecialInsertKey(form, key) dict "{{{3
    return "\<esc>"
endf


function! s:prototype.GetSpecialKey(form, name, key) dict "{{{3
    " TLogVAR a:name, a:key
    call a:form.SetModifiable(2)
    return "$a\<c-x>\<c-o>"
endf


function! s:prototype.GetKey(form, key) dict "{{{3
    " TLogVAR a:key
    return "\<cr>"
endf


function! s:prototype.Key_dd(form) dict "{{{3
    call a:form.SetModifiable(2)
    return (a:form.indent + 1) .'|d$'
endf


function! s:prototype.Key_BS(form) dict "{{{3
    return ''
endf


function! s:prototype.Key_DEL(form) dict "{{{3
    return ''
endf


let g:vimform#widgets['singlechoice'] = s:prototype


let &cpo = s:save_cpo
unlet s:save_cpo
autoload/vimform/widgets/text.vim	[[[1
25
" text.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2010-04-14.
" @Last Change: 2010-04-14.
" @Revision:    14

let s:save_cpo = &cpo
set cpo&vim

if has_key(g:vimform#widgets, 'text')
    finish
endif


let s:prototype = vimform#widget#New()
let s:prototype.modifiable = 1


let g:vimform#widgets['text'] = s:prototype


let &cpo = s:save_cpo
unlet s:save_cpo
ftplugin/vimform.vim	[[[1
72
" vimform.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2010-04-09.
" @Last Change: 2010-04-14.
" @Revision:    120

if exists("b:did_ftplugin")
    finish
endif
let b:did_ftplugin = 1
let s:save_cpo = &cpo
set cpo&vim


setlocal buftype=nofile
setlocal bufhidden=hide
setlocal noswapfile
setlocal modifiable
setlocal omnifunc=vimform#Complete
setlocal ballooneval
setlocal balloonexpr=vimform#Balloon()

let b:vimform.SaveMapargs('<cr>', ' ')
noremap <buffer> <silent> <cr> :call b:vimform.SpecialKey('<lt>cr>', 0)<cr>
noremap <buffer> <silent> <space> :call b:vimform.SpecialKey(' ', 0)<cr>
noremap <buffer> <silent> <LeftMouse> <LeftMouse>:call b:vimform.SpecialKey('', 0)<cr>

" inoremap <buffer> <silent> <LeftMouse> <LeftMouse><c-\><c-n>:call b:vimform.SpecialKey('', 1)<cr>
" inoremap <buffer> <silent> <space> <c-\><c-n>:call b:vimform.SpecialKey(' ', 1)<cr>
" inoremap <buffer> <silent> <cr> <c-\><c-n>:call b:vimform.SpecialKey('<lt>cr>', 1)<cr>
inoremap <expr> <buffer> <silent> <LeftMouse> vimform#SpecialInsertKey("\<lt>LeftMouse>", '', 1)
inoremap <expr> <buffer> <silent> <space> vimform#SpecialInsertKey(' ', "\<c-y>", 0)
inoremap <expr> <buffer> <silent> <cr> vimform#SpecialInsertKey("\<lt>cr>", "\<c-y>", 0)

imap <expr> <buffer> <c-y> vimform#PumKey("\<c-y>")
imap <expr> <buffer> <c-e> vimform#PumKey("\<c-e>")
" imap <expr> <buffer> <up> vimform#PumKey("\<up>")
" imap <expr> <buffer> <down> vimform#PumKey("\<down>")
" imap <expr> <buffer> <c-n> vimform#PumKey("\<c-n>")
" imap <expr> <buffer> <c-p> vimform#PumKey("\<c-p>")

noremap <buffer> <f5> :VimformReset<cr>
noremap <buffer> <f1> :help vimform-keys<cr>
inoremap <buffer> <f1> <c-\><c-n>:help vimform-keys<cr>

noremap <buffer> <c-cr> :call b:vimform.Submit()<cr>
inoremap <buffer> <c-cr> <c-\><c-n>:call b:vimform.Submit()<cr>
noremap <silent> <buffer> <tab> :call b:vimform.NextField('w', 0, 1)<cr>
inoremap <silent> <buffer> <tab> <c-\><c-n>:call b:vimform.NextField('w', 1, 1)<cr>
noremap <silent> <buffer> <s-tab> :call b:vimform.NextField('bw', 0, 1)<cr>
inoremap <silent> <buffer> <s-tab> <c-\><c-n>:call b:vimform.NextField('bw', 1, 1)<cr>

imap <expr> <buffer> <c-space> vimform#Complete1()

imap <expr> <buffer> <bs> b:vimform.Key_BS()
nmap <expr> <buffer> <bs> b:vimform.Key_BS()
nmap <expr> <buffer> X b:vimform.Key_BS()

imap <expr> <buffer> <del> b:vimform.Key_DEL()
nmap <expr> <buffer> <del> b:vimform.Key_DEL()
nmap <expr> <buffer> x b:vimform.Key_DEL()

nmap <expr> <buffer> dd b:vimform.Key_dd()

nmap <expr> <buffer> a b:vimform.Key('a')
nmap <expr> <buffer> i b:vimform.Key('i')


let &cpo = s:save_cpo
unlet s:save_cpo
indent/vimform.vim	[[[1
16
" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @GIT:         http://github.com/tomtom/vimform_vim/
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2010-04-09.
" @Last Change: 2010-04-09.
" @Revision:    4

if exists("b:did_indent") || exists("g:vikiNoIndent")
    finish
endif
let b:did_indent = 1

setlocal indentexpr=b:vimform.Indent()
" setlocal indentkeys=!^F,o,O

plugin/vimform.vim	[[[1
30
" vimform.vim -- Simple forms for vim
" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2008-07-16.
" @Last Change: 2010-04-14.
" @Revision:    8
" GetLatestVimScripts: 3051 0 vimform.vim

if &cp || exists("loaded_vimform")
    finish
endif
let loaded_vimform = 3

let s:save_cpo = &cpo
set cpo&vim


command! -nargs=1 -complete=customlist,vimform#CommandComplete Vimform call vimform#Command(<q-args>)


let &cpo = s:save_cpo
unlet s:save_cpo


finish
CHANGES:
0.1
- Initial release

syntax/vimform.vim	[[[1
37
" vimform.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2008-07-16.
" @Last Change: 2010-04-14.
" @Revision:    0.0.44

if version < 600
    syntax clear
elseif exists("b:current_syntax")
    finish
endif
if version < 508
    command! -nargs=+ HiLink hi link <args>
else
    command! -nargs=+ HiLink hi def link <args>
endif


syn match VimformSeparator /^_\+ .\{-} _\+$/
syn match VimformComment /^" .*$/
syn match VimformButton /<<.\{-}>>/ contained containedin=VimformControls
syn match VimformControls /^| .* |$/ contains=VimformButton
syn match VimformAttribute /^ \(File\|Dir\)| / contained containedin=VimformLabel
syn match VimformLabel /^ \+.\{-}\ze: / contains=VimformAttribute

HiLink VimformAttribute Type
HiLink VimformLabel Constant 
HiLink VimformButton Special
HiLink VimformControls Identifier
HiLink VimformSeparator PreProc
HiLink VimformComment Comment


delcommand HiLink
let b:current_syntax = 'vimform'
