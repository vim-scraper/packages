" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
autoload/vimtap.vim	[[[1
197
"##### HEADER [ {{{ ]
" Plugin:       VimTAP
" Version:      0.2
" Author:       Meikel Brandmeyer <mb@kotka.de>
" Created:      Sat Apr 12 20:53:41 2008
" Last Change:  Mon Apr 14 2008
"
" License:
" Copyright (c) 2008 Meikel Brandmeyer, Frankfurt am Main
" 
" All rights reserved.
" 
" Permission is hereby granted, free of charge, to any person obtaining a copy
" of this software and associated documentation files (the "Software"), to deal
" in the Software without restriction, including without limitation the rights
" to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
" copies of the Software, and to permit persons to whom the Software is
" furnished to do so, subject to the following conditions:
" 
" The above copyright notice and this permission notice shall be included in
" all copies or substantial portions of the Software.
" 
" THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
" IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
" FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
" AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
" LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
" OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
" THE SOFTWARE.
"
" Description:
" VimTAP is an implementation of the Test Anything Protocol for vim. It is
" intended to assist the developer with testing his scripts. TAP makes it easy
" to test a project with different languages using a common harness to
" interpret the test results.
"
" See Also:
" http://search.cpan.org/~petdance/TAP-1.00/TAP.pm
" http://testanything.org
"##### [ }}} ]

"##### PROLOG [ {{{ ]
if exists("g:DidVimTAP") 
	finish
endif
let g:DidVimTAP = 1

let s:saved_cpo = &cpo
set cpo&vim
"##### [ }}} ]

"##### VARIABLES [ {{{ ]
"### VARIABLE s:test_number [ {{{ ]
" Description:
" The test_number keeps track of the number of tests run by the script.
"
let s:test_number = 0
"### [ }}} ]
"##### [ }}} ]

"##### FUNCTIONS [ {{{ ]
"### FUNCTION vimtap#Plan [ {{{ ]
" Description:
" Write the test plan to the output buffer.
"
" Example:
"   call vimtap#Plan(10)
"
" Source:
function vimtap#Plan(tests)
	execute printf("normal i1..%d\<CR>", a:tests)
	let s:test_number = 1
endfunction
"### [ }}} ]
"### FUNCTION vimtap#Ok [ {{{ ]
" Description:
" Ok is the simplest test function. The first argument is the result of an
" arbitrary test. In case the test succeeded, an ok line is printed into the
" test buffer. Otherwise a not ok line is printed. The description is appended
" to the test line.
"
" Example:
"   call vimtap#Ok(x == y, "x is equal to y")
"   call vimtap#Ok(IsFoo(x), "x is Foo")
"
" Source:
function vimtap#Ok(test_result, description)
	let result = a:test_result ? "ok" : "not ok"

	execute printf("normal i%s %d - %s\<CR>", result, s:test_number,
				\ a:description)

	let s:test_number = s:test_number + 1
endfunction
"### [ }}} ]
"### FUNCTION vimtap#Is [ {{{ ]
" Description:
" Is is a bit more complicated than Ok. It takes two entities and compares
" them using ==. Some diagnostic output gives more information about, why the
" test failed than it is possible for Ok.
"
" Example:
"   call vimtap#Is(x, y, "x is equal to y")
"
" Source:
function vimtap#Is(got, exp, description)
	let test_result = a:got == a:exp

	call vimtap#Ok(test_result, a:description)
	if !test_result
		call vimtap#Diag("Test '" . a:description . "' failed:\n"
					\ . "expected: '" . a:exp . "'\n"
					\ . "but got:  '" . a:got . "'")
	endif
endfunction
"### [ }}} ]
"### FUNCTION vimtap#Isnt [ {{{ ]
" Description:
" Isnt is similar to Is, but the generated value should be different from the
" supplied one.
"
" Example:
"   call vimtap#Isnt(x, y, "x is not equal to y")
"
" Source:
function vimtap#Isnt(got, unexp, description)
	let test_result = a:got != a:unexp

	call vimtap#Ok(test_result, a:description)
	if !test_result
		call vimtap#Diag("Test '" . a:description . "' failed:\n"
					\ . "got unexpected: '"
					\ . a:got
					\ . "'")
	endif
endfunction
"### [ }}} ]
"### FUNCTION vimtap#Like [ {{{ ]
" Description:
" Like is similar to Is, but the it uses a regular expression which is matched
" against the passed in value. If the value matches the regular expression,
" then the test succeeds.
"
" Example:
"   call vimtap#Like(x, '\d\d', "x has two-digit number")
"
" Source:
function vimtap#Like(got, re, description)
	let test_result = a:got =~ a:re

	call vimtap#Ok(test_result, a:description)
	if !test_result
		call vimtap#Diag("Test '" . a:description . "' failed:\n"
					\ . "got: '" . a:got . "'\n"
					\ . "does not match: '" . a:re . "'")
	endif
endfunction
"### [ }}} ]
"### FUNCTION vimtap#Unlike [ {{{ ]
" Description:
" Unlike is similar to Like, but the regular expression must not match.
"
" Example:
"   call vimtap#Unlike(x, '^\s*$', "x contains non-whitespace")
"
" Source:
function vimtap#Unlike(got, re, description)
	let test_result = a:got !~ a:re

	call vimtap#Ok(test_result, a:description)
	if !test_result
		call vimtap#Diag("Test '" . a:description . "' failed:\n"
					\ . "got: '" . a:got . "'\n"
					\ . "does match: '" . a:re . "'")
	endif
endfunction
"### [ }}} ]
"### FUNCTION vimtap#Diag [ {{{ ]
" Description:
" Print the given string into the output. Preface each line with a '#'.
"
" Example:
"   call vimtap#Diag("Some Diagnostic Message")
"
" Source:
function vimtap#Diag(str)
	for line in split(a:str, '\(\r\n\|\r\|\n\)', 1)
		execute "normal i# " . line . "\<CR>"
	endfor
endfunction
"### [ }}} ]
"##### [ }}} ]

"##### EPILOG [ {{{ ]
let &cpo = s:saved_cpo
unlet s:saved_cpo
"##### [ }}} ]
doc/VimTAP.txt	[[[1
145

			    VimTAP

					    *VimTAP*

VimTAP is an implementation of the Test Anything Protocol for Vim. It is
intended to provide an easy way to test your Vim scripts and functions.

TAP is widely used by the Perl community. To its main features count the
easy way of reading test protocols manually and the independence of
producers and consumers of the test protocol data.

==============================================================================
CONTENTS

1. The Plan				    |vimtap#Plan|
2. Ok: the simple way			    |vimtap#Ok|
3. Is: more information			    |vimtap#Is|
4. Isnt: turn the tides			    |vimtap#Isnt|
5. Like: more fuzzyness needed?		    |vimtap#Like|
6. Unlike: avoid the anti-patterns	    |vimtap#Unlike|
7. Diag: It hurts, Doctor!		    |vimtap#Diag|
8. License				    |VimTAPLicense|
==============================================================================

1. The Plan				    *vimtap#Plan*

vimtap#Plan(number_of_tests)

Never do something without a plan! The plan tells the harness how many
tests should be run. In this way it is easy to know, when your script
stop somewhere in the middle.

TAP allows the plan also to follow the actual test output, but this
should be rarely needed. Providing the plan beforehand is better. >

	" Plan to run 4 tests.
	call vimtap#Plan(4)
<
==============================================================================

2. Ok: the simple way			    *vimtap#Ok*

vimtap#Ok(test_result, description)

The simplest way to do a test, is to use Ok. It is most suitable for some
kind of predicate or some complicated calculation. However, since it only
sees the result, Ok cannot tell you, why the result is like it is.

The description (as it maybe given to any test function) is used to identify
the test in the output. >

	call vimtap#Ok(x == y, "x is equal to y")
	call vimtap#Ok(IsFoo(x), "x is Foo")
<
==============================================================================

3. Is: more information			    *vimtap#Is*

vimtap#Is(got, expected, description)

Is does not take the result, but does the comparison of the actual value
against the expected one. This gives Is the possibility to be more verbose
than Ok in case the test fails. Then it prints the expected value as well
as the actual one in order to give the developer more hints, why the test
actually failed. >

	call vimtap#Is(x, "good value", "x is a good value")
<
==============================================================================

4. Isnt: turn the tides			    *vimtap#Isnt*

vimtap#Isnt(got, unexpected, description)

Sometimes there are values, which you don't want to see as a result of a
computation. This can be tested with Isnt. The actual value has to be
different from the given reference value for the test to succeed. >

	call vimtap#Isnt(x, "bad value", "x is not a bad value")
<
==============================================================================

5. Like: more fuzziness needed?		    *vimtap#Like*

vimtap#Like(got, regular expression, description)

When you don't expect a certain value, but a result of a certain structure
you can also use Like to test the result against a given regular expression. >

	call vimtap#Like(x, '^\d$', "x is a digit")
<
==============================================================================

6. Unlike				    *vimtap#Unlike*

vimtap#Unlike(got, regular expression, description)

As with Is and Isnt, you might want to test, that a result doesn't contain
some unwanted anti-patterns. >

	call vimtap#Unlike(x, '^\s*$', "x does contain non-whitespace")
<
==============================================================================

7. Diag: It hurts, Doctor!		    *vimtap#Diag*

vimtap#Diag(diagnostic message)

From time to time one may want to print some diagnostic output. This has to
be prefixed by '#' to conform to TAP. But Diag to the rescue: it takes of
this for you. >

	call vimtap#Diag("Testing now this and that feature...")
<
Note: For now \n, \r and \r\n are all considered to delimit lines.

==============================================================================

8. License				    *VimTAPLicense*

Copyright (c) 2008 Meikel Brandmeyer, Frankfurt am Main

All rights reserved.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

==============================================================================
vim: ft=help:norl:ts=8:tw=78
vtruntest.sh	[[[1
44
#! /bin/sh
# Copyright (c) 2008 Meikel Brandmeyer, Frankfurt am Main
# 
# All rights reserved.
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
# 
# Description:
# This is a small script coming with VimTAP in order to run the
# tests from outside of vim.
#

if [ $# -ne 1 ]; then
	echo "Usage: vtruntest.sh <test>"
	exit 1
fi

test=$1

vim -e ${test}.output <<EOF
source ${test}
normal Gdd
w
q
EOF
cat ${test}.output
rm -f ${test}.output

LICENSE.VimTAP	[[[1
21
Copyright (c) 2008 Meikel Brandmeyer, Frankfurt am Main

All rights reserved.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
