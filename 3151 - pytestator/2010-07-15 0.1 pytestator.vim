" Description: This basic script intent to be facilitate the use of pyunit in
" combination with python scripts.
"
" Here are the mappings and commands:
"
" command! CreateTest         call CreateTest()
" command! CreateTestForce    call CreateTestForce()
" command! RunTest            call RunTest()
" command! SwapWithTest       call SwapWithTest()
"
" map ,tc     :CreateTest<cr>
" map ,tF     :CreateTestForce<cr>
" map ,tr     :RunTest<cr>
" map ,s      :SwapWithTest<cr>
"
" Author: Laurent Peuch <cortex@worlddomination.be>
" Maintainer: Laurent Peuch <cortex@worlddomination.be>
"
" Last Change: 10-Jan-2010
"
" Licence:
" This program is free software: you can redistribute it and/or modify
" it under the terms of the GNU General Public License as published by
" the Free Software Foundation, either version 3 of the License, or
" (at your option) any later version.
"
" This program is distributed in the hope that it will be useful,
" but WITHOUT ANY WARRANTY; without even the implied warranty of
" MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
" GNU General Public License for more details.
"
" You should have received a copy of the GNU General Public License
" along with this program.  If not, see <http://www.gnu.org/licenses/>.
"
" SpecialFeature: Lack of documentation.
" Inspired by cassidy.


if !executable("python")
    finish
endif

set statusline+=%#error#%{HasUnitTest()}%*

function! HasUnitTest()
    if !TestExist() && !IsTest() && &filetype == "python"
        let b:has_unit_test = "[" . "I NOT HAS UNIT TEST" . "]"
    else
        let b:has_unit_test = ""
    endif
    return b:has_unit_test
endfunction

fun! IsTest()
    if expand("%") =~ "^test_"
        return 1
    else
        return 0
    endif
endf

fun! TestExist()
    return filereadable('test_' . expand("%"))
endf

fun SwapWithTest()
    execute "w"
    if IsTest()
        execute "e " . substitute(expand("%"), "test_", "", "")
    elseif !TestExist()
        call WriteTest()
    else
        execute "e test_" . expand("%")
    endif
endf

fun! CreateTest()
    if TestExist()
        echo "Error: TestFile already exist, use CreateTestForce to force."
    else
        call WriteTest()
    endif
endf

fun! CreateTestForce()
    call WriteTest()
endf

fun WriteTest()
    call writefile(["\#!/usr/bin/python", "\# -*- coding:Utf-8 -*-", "", "", "import unittest", "import " . expand("%:t:r"), "", "class MaTest(unittest.TestCase):", "    def test_ma_function(self):", "        pass", "", 'if __name__ == "__main__":', "   unittest.main()", ""], 'test_' . expand("%"))
    execute "w"
    execute "e test_" . expand("%")
    call setpos(".", [0, 10, 9])
endf

fun RunTest()
    if &filetype != "python"
        echo "Hu ? Trying to run python unittest on an non python file ?"
    elseif !TestExist() && !IsTest()
        echo "Error: there isn't any test to run"
    elseif IsTest()
        execute "!python %"
    else
        execute "!python test_" . expand("%")
    endif
endf

command! CreateTest         call CreateTest()
command! CreateTestForce    call CreateTestForce()
command! RunTest            call RunTest()
command! SwapWithTest       call SwapWithTest()

map ,tc     :CreateTest<cr>
map ,tF     :CreateTestForce<cr>
map ,tr     :RunTest<cr>
map ,s      :SwapWithTest<cr>
