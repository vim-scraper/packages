" -*- vim -*-
" FILE: mib_translator.vim
"   Vim plugin
"
" DESCRIPTION:
"   This plugin allows you to translate the SNMP OIDs from
"   within Vim.
"
" AUTHOR:
"   Caglar Toklu caglartoklu[aat]gmail.com
"   http://caglartoklu.blogspot.com/
"
" LICENSE:
"   Copyright (C) 2009  Caglar Toklu
"
"   This program is free software: you can redistribute it and/or modify
"   it under the terms of the GNU General Public License as published by
"   the Free Software Foundation, either version 3 of the License, or
"   (at your option) any later version.
"
"   This program is distributed in the hope that it will be useful,
"   but WITHOUT ANY WARRANTY; without even the implied warranty of
"   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
"   GNU General Public License for more details.
"
"   You should have received a copy of the GNU General Public License
"   along with this program.  If not, see <http://www.gnu.org/licenses/>.
"
" CHANGELOG:
"   0.0.4, 2010-04-12
"     - Fixed: If the opened buffer is deleted by hand, that raised an error
"       when the buffer is opened again.
"   0.0.3, 2010-04-12
"     - Fixed: Opened buffer remained unmodifiable for the second run.
"   0.0.2, 2010-04-12
"     - The opened buffer is colored with MIB file syntax,
"       making it more readable.
"     - The opened buffer is not modifiable from now on.
"     - Made it more compatible with other plugins,
"       the functions starts with 's:'.
"   0.0.1, 2009-12-04
"     - First version with forward and reverse OID translation.
"
" INSTALLATION:
"   - Drop the script into your plugin directory.
"   - The name of the plugin starts with the string 'mib', which
"     means it can be used with files with '.mib' extension if the
"     plugin is placed in the ftplugin directory.
"     But, most of the MIB files have the extension of '.txt',
"     and some have the extension '.my'.
"     Also, it can be used from anywhere since it has actually
"     nothing to do with the file extension, so it is better to
"     place it in the plugin directory instead of ftplugin.
"     For example, you can use it from a .py file too.
"
" COMMANDS:
"   :OidReverseTranslate <oidName>
"     Displays the OID information if the OID name is given.
"     This is the reverse translation.
"     :OidReverseTranslate ipv6MIB
"   :OidTranslate <oidNumber>
"     Displays the OID information if the OID number is given.
"     This is the forward translation.
"     :OidTranslate .1.3.6.1.2.1.55
"
" USAGE:
"   You can use the plugin as the COMMANDS section shows.
"   Note that you do not have to open a MIB file to use this plugin.
"   It will work anyway, but if you open a MIB file, you can use as:
"     1. Get the cursor to a OID name, for example ipv6MIB.
"     2. Type : to get to command mode.
"     3. Type OidReverseTranslate
"     4. Type <space>
"     5. Type <C-r> <C-w>
"     6. Press <enter>
"
"
" CONFIGURATION:
"   The options are:
"
"   g:OidTranslatorBufferName:
"     The name of the MIB translator buffer.
"     The tranlated values will be displayed here.
"     There is no need to change this if there are no clash
"     with another plugin.
"     Default is 'OIDTranslator'.
"
"   g:OidTranslatorBufferSize:
"     The size of the buffer. It is the line count. If your
"     screen is small, you can reduce the this size, or if
"     your screen is huge, you can raise this number.
"     Default is 10.
"
"   g:SnmpTranslatePath:
"     The executable path of the snmptranslate command of Net-SNMP.
"     If the bin folder is in your path, no need to change this setting.
"     If it is not, set this option from your VIMRC.
"     The value must the path to the executable, not the directory
"     including the executable.
"     For example, a valid value is:
"     let g:SnmpTranslatePath = 'C:\\usr\\bin\\snmptranslate.exe'
"     The default value is 'snmptranslate', so that it can
"     directly run without modification if snmptranslate is included
"     in a directory on path.
"     Note that if Net-SNMP is not on the path, even though you
"     define this option correct, snmptranslate itself can have
"     difficulties to find the MIB files, so it is recommended
"     to keep it on the path and not changing this option.
"
" REQUIREMENTS:
"   - Net-SNMP package must be installed on your system.
"     http://net-snmp.sourceforge.net/
"     It is also strongly recommended to add 'C:\usr\bin' to the
"     path in Windows.


if exists("g:loaded_mib_translator") || &cp
    " If it already loaded, do not load it again.
    finish
endif


let g:loaded_mib_translator = 1


function! s:SetDefaultSettings()
    " Reads the settings, if they are not defined,
    " it defines them with default settings.
    if exists('g:OidTranslatorBufferName') == 0
        " The name of the buffer to be used to display the
        " result of the translation process.
        " It is not recommended to change this value or
        " you will cause buffer name clashes, or the plugin itself
        " can not find the buffer.
        let g:OidTranslatorBufferName = 'OIDTranslator'
    endif

    if exists('g:OidTranslatorBufferSize') == 0
        " The line count of the translation buffer.
        " If it is not convenient for you, change it
        " from within VIMRC by copying the following line.
        let g:OidTranslatorBufferSize = 10
    endif

    if exists('g:SnmpTranslatePath') == 0
        " The path to the snmptranslate executable of Net-SNMP.
        let g:SnmpTranslatePath = 'snmptranslate'
    endif
endfunction


function! s:GetOidTranslatorBufferNumber()
    " Returns the buffer number of the OID Translator.
    " It can also be used to check if the buffer exists
    " or not. If this function returns -1, it means that
    " the buffer does not exist.
    return bufnr(g:OidTranslatorBufferName)
endfunction


function! s:CreateOidTranslatorBuffer()
    " Creates the OID Tranlator buffer.
    " The buffer must be deleted by calling
    " DeleteOidTranslatorBuffer() before this function.
    call s:CreateBuffer(g:OidTranslatorBufferName, g:OidTranslatorBufferSize)
endfunction


function! s:DeleteOidTranslatorBuffer()
    " If the OID Translator buffer exists, this function deletes it.
    " It has no effect otherwise.
    let OidTranslatorBufferNumber = s:GetOidTranslatorBufferNumber()
    if OidTranslatorBufferNumber != -1
        " If the buffer exists, delete it.
        " If the buffer is deleted by hand using ':bd' the following
        " exec used to raise an error. Now launched with 'silent!'.
        " exec 'bdelete ' . g:OidTranslatorBufferName
        silent! exec 'bdelete ' . g:OidTranslatorBufferName
    endif
endfunction


function! s:CreateBuffer(bufferName, splitSize)
    " Create the specified buffer name with the specified split size.
    " This is a more generic function and can be used for
    " other scripts too.

    let finalBufferName = a:bufferName
    exec a:splitSize . 'new "' . finalBufferName . '"'
    exec 'edit ' . finalBufferName

    " Make the buffer writable.
    set modifiable

    " Keep the window width when windows are opened or closed.
    " setlocal winfixwidth

    " Do not use a swapfile for the buffer.
    setlocal noswapfile

    " Buffer which is not related to a file and will not be
    " written.
    setlocal buftype=nofile

    " When off lines will not wrap and only part of long lines
    " will be displayed.
    setlocal nowrap

    " When non-zero, a column with the specified width is shown at the side
    " of the window which indicates open and closed folds.
    setlocal foldcolumn=0

    " When this option is set, the buffer shows up in the buffer list.
    " setlocal nobuflisted

    " When on spell checking will be done.
    setlocal nospell

    " Do not print the line number in front of each line.
    setlocal nonumber

    " Remove all abbreviations for Insert mode
    iabc <buffer>

    " Highlight the screen line of the cursor with CursorLine.
    setlocal cursorline
endfunction


function! s:ReverseTranslateSnmpOID(oidName)
    " Reverse translates the given SNMP OID.
    " This is the reverse translation, the OID information
    " is found from the OID name.
    " The oidName is a string like 'nlmConfigLogStorageType'.
    " This function uses snmptranslate command of Net-SNMP,
    " and returns extracts detailed information.
    call s:DeleteOidTranslatorBuffer()
    call s:CreateOidTranslatorBuffer()
    " At this point, we have a buffer for sure.

    " Focus the OID Translator buffer.
    exec 'edit ' . g:OidTranslatorBufferName

    " The following 2 lines are used to debug.
    " let temp = 'read !' . g:SnmpTranslatePath . parameters . a:oidName
    " let x = input(temp)

    " We do not want to see the process returned number,
    " so we are using silent!.
    " -m ALL : Uses all MIB files.
    " -IR    : Uses random access to OID lables.
    " -On    : Prints OIDs numerically.
    " -Td    : Prints full details of the given OID.
    let parameters = '-m ALL -IR -On -Td'
    silent! exec 'read !' . g:SnmpTranslatePath . ' ' . parameters . ' ' . a:oidName
    call s:DoPostOperations()
endfunction


function! s:TranslateSnmpOID(oidNumber)
    " Translates the given SNMP OID.
    " This is the forward translation, the OID information
    " is found from the OID number.
    " The oidName is a string like '.1.1.3'.
    " This function uses snmptranslate command of Net-SNMP,
    " and returns extracts detailed information.
    call s:DeleteOidTranslatorBuffer()
    call s:CreateOidTranslatorBuffer()
    " At this point, we have a buffer for sure.

    " Focus the OID Translator buffer.
    exec 'edit ' . g:OidTranslatorBufferName

    " The following 2 lines are used to debug.
    " let temp = 'read !' . g:SnmpTranslatePath . parameters . a:oidName
    " let x = input(temp)

    " We do not want to see the process returned number,
    " so we are using silent!.
    " -m ALL : Uses all MIB files.
    " -On    : Prints OIDs numerically.
    " -Td    : Prints full details of the given OID.
    let parameters = '-m ALL -On -Td'
    silent! exec 'read !' . g:SnmpTranslatePath . ' ' . parameters . ' ' . a:oidNumber
    call s:DoPostOperations()
endfunction


function! s:DoPostOperations()
    " This function is called at the end of ReverseTranslateSnmpOID() and
    " TranslateSnmpOID().

    " What is seen is a part of a MIB file, paint it with its syntax colors.
    exec "set filetype=mib"

    " When off the buffer contents cannot be changed.
    set nomodifiable
endfunction


" Set the settings once.
call s:SetDefaultSettings()


" Define commands to use.
command! -nargs=1 OidReverseTranslate : call s:ReverseTranslateSnmpOID(<q-args>)
command! -nargs=1 OidTranslate : call s:TranslateSnmpOID(<q-args>)
