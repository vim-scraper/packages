"======================================================================
" cream-ascii.vim
"
" Date:   16 Mar 2002
" Source: http://vim.sourceforge.net/scripts/script.php?script_id=XXXX
" Author: Steve Hall  [ digitect@mindspring.com ]
"
" Description:
" Insert an ASCII character from a dialog box. See the screenshot at http://cream.sourceforge.net/ss-ascii.png .
"
" One of the many custom utilities and functions for gVim from the Cream project ( http://cream.sourceforge.net ), a configuration of Vim in the vein of Apple and Windows software you already know.
"
" Installation:
" Just copy this file and paste it into your vimrc. Or you can drop the entire file into your plugins directory.
"
" Two cream ASCII menu items attempt to load into the bottom of the default Tools menu. (If you have customized your menus and Tools has been removed or altered, this could fail, obviously.)
"
" If you choose not to use the menus, you can also call the Cream_ascii() function directly from the command line. Options are as follows:
"
"   :call Cream_ascii()  -- No argument inserts a complete ASCII table into the current document
"
"   :call Cream_ascii("dialog")  -- The argument "dialog" calls the dialog version of the program
"
"   :call Cream_ascii([32-255])  -- You can also use a number 32-255 to insert that decimal character with Vim's nr2char()
"
"
" Example Insert Dialog:
"
" A "paging" dialog box, with a row of characters indicated on the
" screen with a single "hot" character in the middle, like this:
" +--------------------------------------------------------------------+
" |                                                                    |
" |    ...EFGHIJKLMNOPQRST   [ U ]   VWXYZ[\]^_`abcdefghijklm...       |
" |                                                                    |
" | [<<-40] [<<-10] [<-1] [insert 85] [+1>] [+10>>] [+40>>>]  [cancel] |
" |                                                                    |
" +--------------------------------------------------------------------+
" As the (bottom row) buttons are pressed, the characters scroll
" correspondingly.
"
"
" Example ASCII Table:
"
" +----------------------- ASCII TABLE --------------------------------+
" | Notes:                                                             |
" | * Characters 0-31 are non-printable (32 is a space)                |
" | * Character 127 is the non-printable DEL character                 |
" | * Characters above 255 are inconsistant or redundant               |
" | * Fonts represent most characters above 127 inconsistently         |
" |                                                                    |
" |   001     033 !   065 A   097 a   129 Å   161 °   193 ¡   225 ·    |
" |   002     034 "   066 B   098 b   130 Ç   162 ¢   194 ¬   226 ‚    |
" |   003     035 #   067 C   099 c   131 É   163 £   195 √   227 „    |
" |   004     036 $   068 D   100 d   132 Ñ   164 §   196 ƒ   228 ‰    |
" |   005     037 %   069 E   101 e   133 Ö   165 •   197 ≈   229 Â    |
" |   006     038 &   070 F   102 f   134 Ü   166 ¶   198 ∆   230 Ê    |
" |   007     039 '   071 G   103 g   135 á   167 ß   199 «   231 Á    |
" |   008     040 (   072 H   104 h   136 à   168 ®   200 »   232 Ë    |
" |   009     041 )   073 I   105 i   137 â   169 ©   201 …   233 È    |
" |   010     042 *   074 J   106 j   138 ä   170 ™   202     234 Í    |
" |   011     043 +   075 K   107 k   139 ã   171 ´   203 À   235 Î    |
" |   012     044 ,   076 L   108 l   140 å   172 ¨   204 Ã   236 Ï    |
" |   013     045 -   077 M   109 m   141 ç   173 ≠   205 Õ   237 Ì    |
" |   014     046 .   078 N   110 n   142 é   174 Æ   206 Œ   238 Ó    |
" |   015     047 /   079 O   111 o   143 è   175 Ø   207 œ   239 Ô    |
" |   016     048 0   080 P   112 p   144 ê   176 ∞   208 –   240     |
" |   017     049 1   081 Q   113 q   145 ë   177 ±   209 —   241 Ò    |
" |   018     050 2   082 R   114 r   146 í   178 ≤   210 “   242 Ú    |
" |   019     051 3   083 S   115 s   147 ì   179 ≥   211 ”   243 Û    |
" |   020     052 4   084 T   116 t   148 î   180 ¥   212 ‘   244 Ù    |
" |   021     053 5   085 U   117 u   149 ï   181 µ   213 ’   245 ı    |
" |   022     054 6   086 V   118 v   150 ñ   182 ∂   214 ÷   246 ˆ    |
" |   023     055 7   087 W   119 w   151 ó   183 ∑   215 ◊   247 ˜    |
" |   024     056 8   088 X   120 x   152 ò   184 ∏   216 ÿ   248 ¯    |
" |   025     057 9   089 Y   121 y   153 ô   185 π   217 Ÿ   249 ˘    |
" |   026     058 :   090 Z   122 z   154 ö   186 ∫   218 ⁄   250 ˙    |
" |   027     059 ;   091 [   123 {   155 õ   187 ª   219 €   251 ˚    |
" |   028     060 <   092 \   124 |   156 ú   188 º   220 ‹   252 ¸    |
" |   029     061 =   093 ]   125 }   157 ù   189 Ω   221 ›   253 ˝    |
" |   030     062 >   094 ^   126 ~   158 û   190 æ   222 ﬁ   254 ˛    |
" |   031     063 ?   095 _   127    159 ü   191 ø   223 ﬂ   255 ˇ    |
" |   032     064 @   096 `   128 Ä   160 †   192 ¿   224 ‡   256 ˇ    |
" |                                                                    |
" +--------------------------------------------------------------------+
"

" menu loading
if       has("gui_running")
	\ && has("menu")
	\ && stridx(&guioptions, "m") != -1
	if exists("$CREAM")
		" if using the Cream configuration (http://cream.sourceforge.net)
		amenu 40.200.1 &Insert.Insert\ ASCII\ Character\ (dialog)		:call Cream_ascii("dialog")<CR>
		amenu 40.200.2 &Insert.Insert\ ASCII\ Table						:call Cream_ascii()<CR>
	else
		" otherwise
		amenu 40.999 &Tools.-Sep99-		<Nul>
		amenu 40.999.1 &Tools.Cream\ Insert\ ASCII\ Character\ (dialog)	:call Cream_ascii("dialog")<CR>
		amenu 40.999.2 &Tools.Cream\ Insert\ ASCII\ Table				:call Cream_ascii()<CR>
	endif
endif


function! Cream_ascii(...)
" main function
" * If no argument, inserts entire ASCII table
" * If argument "dialog", use dialog insert
" * Otherwise, insert ASCII character of decimal value passed

	if exists("a:1")
		if a:1 == 'dialog'
			" call ASCII dialog
			call Cream_ascii_dialog()
		else
			" insert ASCII character
			call Cream_ascii_insert(a:1)
		endif
	else
		" insert ASCII table
		call Cream_ascii_table()
	endif

endfunction

function! Cream_ascii_dialog()

	if exists("g:Cream_ascii_char")
		let i = g:Cream_ascii_char
	else
		let i = 33
	endif
	
	while i != ''
		let i = Cream_ascii_dialog_prompt(i)
		" force wrap above 254
		if i > 254
			let i = 32
		" force wrap below 32
		elseif i < 32 && i > 0
			let i = 254
		elseif i == 0
			" quit
		endif
	endwhile

endfunction

function! Cream_ascii_dialog_prompt(mychar)

	let i = a:mychar

	" calculate dialog scroll text
	let scrollstrpre  = ''
	let scrollstr     = ''
	let scrollstrpost = ''

	" scrollstrpre
	let j = 1
	while j < 20
		let temp = i - j
		if temp < 32
			let temp = temp + 254 - 32 + 1
		endif
		let scrollstrpre = nr2char(temp) . scrollstrpre
		let j = j + 1
	endwhile
	" add some white space preceeding
	let scrollstrpre = "          " . scrollstrpre
	" append "character marker" and whitespace
	let scrollstrpre = scrollstrpre . '          [    '

	" scrollstr
	let scrollstr = nr2char(i)

	" scrollstrpost
	let j = 1
	while j < 20
		let temp = i + j
		if temp > 254
			let temp = temp - 254 + 32 - 1
		endif
		let scrollstrpost = scrollstrpost . nr2char(temp)
		let j = j + 1
	endwhile
	" prepend "character marker" and whitespace
	let scrollstrpost = '    ]          ' . scrollstrpost

	" escape ampersand with second ampersand (Windows only)
	if      has("win32")
		\|| has("win16")
		\|| has("win95")
		\|| has("dos16")
		\|| has("dos32")
		let scrollstrpre  = substitute(scrollstrpre , '&', '&&', 'g')
		let scrollstr     = substitute(scrollstr    , '&', '&&', 'g')
		let scrollstrpost = substitute(scrollstrpost, '&', '&&', 'g')
	endif

	" if character is a space
	if i == 32
		let scrollstr = "(SPACE)"
	endif
	" if character is a delete
	if i == 127
		let scrollstr = "(DEL)"
	endif

"*** DEBUG:
"call confirm(" i  = " . i . "\n temp  = " . temp, "&Ok", 1)
"***

	let myreturn = confirm(
		\ "  (Note: Some characters may not appear correctly below.)\n" . 
		\ "\n" . 
		\ scrollstrpre . scrollstr . scrollstrpost . "\n",
		\ "<<<\ \ \-40\n<<\ \ \-10\n<\ \ \-1\n&Insert\ ASCII\ " . i . "\n+1\ \ >\n+10\ \ >>\n+40\ \ >>>\n&Cancel",
		\ 3)
		
		" Windows button string
		"\ "<<<\ -40\n<<\ -10\n<\ -1\n&Insert\ ASCII\ " . i . "\n+1\ >\n+10\ >>\n+40\ >>>\n&Cancel",

	if myreturn == 0
		" user quit, pressed <Esc> or otherwise terminated
		" remember last char
		let g:Cream_ascii_char = i
		return
	elseif myreturn == 1
		let i = i - 40
		return i
	elseif myreturn == 2
		let i = i - 10
		return i
	elseif myreturn == 3
		let i = i - 1
		return i
	elseif myreturn == 4
		" insert character
		call Cream_ascii_insert(i)
		" remember last char
		let g:Cream_ascii_char = i
		return
	elseif myreturn == 5
		let i = i + 1
		return i
	elseif myreturn == 6
		let i = i + 10
		return i
	elseif myreturn == 7
		let i = i + 40
		return i
	elseif myreturn == 8
		" remember last char
		let g:Cream_ascii_char = i
		return
	endif

	" error if here!

endfunction

function! Cream_ascii_insert(mychar)
" insert ASCII character

    " puny attempt to foil non-legitimate values, fix later
	if strlen(a:mychar) > 3
		call confirm("String longer than 3 chars in Cream_ascii_insert(). (  a:mychar  = " . a:mychar, "&Ok", 1)
	else
		let temp = nr2char(a:mychar)
		execute "normal i" . temp
	endif

endfunction

function! Cream_ascii_table()
	
	execute "normal i+----------------------- ASCII TABLE --------------------------------+\<Return>"
	execute "normal i| Notes:                                                             |\<Return>"
	execute "normal i| * Characters 0-31 are non-printable (32 is a space)                |\<Return>"
	execute "normal i| * Character 127 is the non-printable DEL character                 |\<Return>"
	execute "normal i| * Characters above 255 are inconsistant or redundant               |\<Return>"
	execute "normal i| * Fonts represent most characters above 127 inconsistently         |\<Return>"
	execute "normal i|                                                                    |\<Return>"

	" 1-31
	let i = 1
	while i < 33

		" 0, 32, 64, 96, 128, 160, 192, 224, 256
		let j = 0
		while j < 256

			" prepend "0" if only two characters
			if (i + j) < 100
				" prepend "0" if only one character (theoretical)
				if (i + j) < 10
					let mychar = "00" . (i + j)
				else
					let mychar = "0" . (i + j)
				endif
			else
				let mychar = (i + j)
			endif

			let mycmd = ""

			" left spacing column
			if j == 0
				let mychar = "|   " . mychar
			endif

			" create string of decimal number and actual value
			if i < 32 && j == 0
				" don't do chars before 32
				let mycmd = mycmd . "normal i" . mychar . "  "
			else
				let mycmd = mycmd . "normal i" . mychar . " \<C-q>" . (i + j)
			endif

			" every 5 characters, new line, otherwise space between
			if (i + j) > 224
				let mycmd = mycmd . "    |\<Return>"
			else
				let mycmd = mycmd . "    "
			endif

			execute mycmd

			let j = j + 32

		endwhile

		let i = i + 1

	endwhile

	execute "normal i|                                                                    |\<Return>"
	execute "normal i+--------------------------------------------------------------------+\<Return>"

endfunction

