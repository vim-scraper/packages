" Waimea config syntax file
" Author:		Michael Brailsford <brailsmt@yahoo.com>
" Copyright:	2002 Michael Brailsford
" Version:		0.1
" License:		This is released under the terms of the BSD license.  The text
"				of the license follows.
"
"	Copyright (c) 2002, Michael Brailsford.  All rights reserved.
"	
"	Redistribution and use in source and binary forms, with or without
"	modification, are permitted provided that the following conditions are met:
"	
"   * Redistributions of source code must retain the above copyright notice,
"     this list of conditions and the following disclaimer.  
"   * Redistributions in binary form must reproduce the above copyright notice,
"	  this list of conditions and the following disclaimer in the documentation
"	  and/or other materials provided with the distribution.
"   * Neither the name of the Author nor the names of contributors may be used 
"     to endorse or promote products derived from this software without 
"     specific prior written permission.
"	
"	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
"	AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
"	IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
"	DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
"	ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
"	(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
"	LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
"	ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
"	(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
"	THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
"
"	Note:  This is not complete. I rate it as follows:  menu file - 98%, style
"	file - 85%, action file - 10%.

" Quit when a syntax file was already loaded	{{{
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif
"}}}

syn match waimeaComment #!.*$#
syn match waimeaMenuTag #\[\w*\]#
syn match waimeaCheckbox #\[checkbox=\w*\]\c# contains=waimeaCheckboxOption
syn keyword waimeaCheckboxOption MAXIMIZED SHADED STICKY ALWAYSONTOP ALWAYSATBOTTOM DECORTITLE DECORHANDLE DECORBORDER DECORALL
syn match waimeaBoolean #@FALSE#
syn match waimeaBoolean #@TRUE#
syn match waimeaMenuTitle #(.\{-})#
syn match waimeaMenuAction #".\{-}"#
syn match waimeaMenuCommandLine #{.*}#
syn match waimeaSubMenu #<.*>#
syn match waimeaColor /#\x\x\x\x\x\x\>/ contains=waimeaColorRed
syn match waimeaColorRed /\x\x\x\x\x\x\>/ contains=waimeaColorGreen contained
syn match waimeaColorGreen /\x\x\x\x\>/ contains=waimeaColorBlue contained
syn match waimeaColorBlue /\x\x\>/ contained
syn region waimeaWindowStyleKeys start=#\s*window\.\c# end=#:#
syn region waimeaMenuStyleKeys start=#\s*menu\.\c# end=#:#
syn region waimeaDockStyleKeys start=#\s*dockappholder\.\c# end=#:#
syn match waimeaStyleKeys #\(rootCommand\|borderColor\|borderWidth\|handleWidth\):\c#
syn match waimeaTexture  #Flat\|Raised\|Sunken\|Gradient\|Solid\|Pixmap\|Horizontal\|Vertical\c#
syn match waimeaTexture  #Diagonal\|Crossdiagonal\|Pipecross\|Elliptic\|Rectangle\|Pyramid\c#
syn match waimeaTexture  #Tiled\|Scaled\|Stretched\|Interlaced\|ParentRelative\|Bevel1\|Bevel2\c#
syn match waimeaPixmapStretch #LEFT\|RIGHT\|TOP\|BOTTOM\c#
syn match waimeaOpacity #1\?\d\d#


hi link waimeaComment Comment
hi link waimeaMenuTag type
hi link waimeaCheckbox type
hi link waimeaCheckboxOption constant
hi link waimeaBoolean constant
hi link waimeaMenuTitle string
hi link waimeaMenuAction preproc
hi link waimeaMenuCommandLine waimeaMenuAction
hi link waimeaSubMenu waimeaMenuAction
hi link waimeaColor preproc
hi waimeaColorRed guifg=red
hi waimeaColorGreen guifg=green
hi waimeaColorBlue guifg=blue
hi link waimeaStyleKeys statement
hi link waimeaWindowStyleKeys waimeaStyleKeys
hi link waimeaMenuStyleKeys waimeaStyleKeys
hi link waimeaDockStyleKeys waimeaStyleKeys
hi link waimeaTexture type
hi link waimeaPixmapStretch waimeaCheckboxOption
hi link waimeaOpacity number
