" ========================================================================== "
" Name:        showmultibase                                                 "
" Version:     1.0                                                           "
" Updated:     2010-09-26 16:07:34                                           "
" Description: Displays the number specified or under the cursor in binary,  "
"                octal, decimal and hexadecimal form.                        "
"                It also provides some conversion functions: {2,8,16}<->10   "
" Author:      Lajos Zaccomer <lajos AT zaccomer DOT org br>                 "
" Credits:     lilydjwg <lilydjwg AT gmail DOT com>                          "
"                Author of rfc.vim (vimscript #2930), source of              "
"                get_pattern_at_cursor().                                    "
"              Michael Geddes                                                "
"                Author of hex.vim (vimscript #238), source of Dec2Hex()     "
"              Mun Johl <Mun.Johl AT emulex DOT com>                         "
"                Bug report: 32<= bit numbers are displayed incorrectly      "
"                Requested configurable save in registers, prefixes and use  "
"                of delimiters. Special thanks for tests and feedbacks!      "
" ========================================================================== "


" The representation of a number is considered as follows:
"
"      +-+-------+-+-------+-+...+-------+
"      |P|D ... D|X|D ... D|X|   |D ... D|
"      +-+-------+-+-------+-+...+-------+
"         Segment   Segment       Segment   
"
"      P: Prefix
"      X: Delimiter
"      D: Digit
"      Subsequent digits form a segment
"

"
" Configuration
"
" The name of the configuration parameters are composed as follows, depending
" on the configuration section the parameter belongs to. There are the
" following configuration sections: General, Display, Register, Parse. Each
" section is represented with its name appended to the iparameter name. Then in
" each section, there are the following categories: Binary, Octal, Decimal,
" Hexadecimal and any other. Each is represented with its name appended to the
" the parameter name (except the any other category that has no prefix).
" Take ShowMultiBase_Parse_Binary_PrefixPattern as an example: this parameter
" defines the prefix pattern to parse binary numbers.

" Configuration: General section

" If not defined or set to 1, the default keymaps will be created.
" Otherwise, set this to 0 in your vimrc and define the mappings
" according to your taste!
if !exists('g:ShowMultiBase_General_UseDefaultMappings')
 \ || g:ShowMultiBase_General_UseDefaultMappings == 1
  noremap <silent> \= :ShowMultiBase<CR>
  noremap <silent> \b= :ShowMultiBase 2<CR>
  noremap <silent> \o= :ShowMultiBase 8<CR>
  noremap <silent> \d= :ShowMultiBase 10<CR>
  noremap <silent> \h= :ShowMultiBase 16<CR>
endif

" Configuration: Parse section
"
" The following patterns define the base prefixes used in the regexp to
" identify a number under the cursor and also in autosensing the base of
" the given or read number.
" These are VIM regexps used in the match() function, so please use
" :help regexp to find out more about how to fufil your own needs!
" - defaults
let s:p_BinPrefix = '[bB]'
let s:p_OctPrefix = '0\+'
let s:p_DecPrefix = ''
let s:p_HexPrefix = '0[xX]'
" - user definitions
if exists('g:ShowMultiBase_Parse_Binary_PrefixPattern')
  let s:p_BinPrefix = g:ShowMultiBase_Parse_Binary_PrefixPattern
endif
if exists('g:ShowMultiBase_Parse_Octal_PrefixPattern')
  let s:p_OctPrefix = g:ShowMultiBase_Parse_Octal_PrefixPattern
endif
if exists('g:ShowMultiBase_Parse_Decimal_PrefixPattern')
  let s:p_DecPrefix = g:ShowMultiBase_Parse_Decimal_PrefixPattern
endif
if exists('g:ShowMultiBase_Parse_Hexadecimal_PrefixPattern')
  let s:p_HexPrefix = g:ShowMultiBase_Parse_Hexadecimal_PrefixPattern
endif

" If set to 1, the autosense will set the base of numbers containing only 0
" and 1 digits to 2.
" - default
let s:p_Only01Bin = 0
" - user definition
if exists('g:ShowMultiBase_Parse_Binary_Only01')
  let s:p_Only01Bin = g:ShowMultiBase_Parse_Binary_Only01
endif

" The following parameter is a list of delimiters that may separate segments
" of digits. The delimiters are separated with the character '0' as a digit
" can obviously not delimit other digits.
" E.g. if the underscore and dot characters can delimit numbers, the
" parameters should be set to '_0.'! By default, no delimiter is configured.
" - default
let s:p_delimiters = ''
" - user definition
if exists('g:ShowMultiBase_Parse_Delimiters')
 let s:p_delimiters = g:ShowMultiBase_Parse_Delimiters
endif

" Configuration: Register section

function! s:check_register(name)
  let l:regname = strpart(a:name, 0, 1)
  " TODO: filter invalid register names
  return l:regname
endfunction

" The register names for the four bases can be set individually. Each register
" is set to some name: either the default name or the one defined in the
" user's vimrc file. If a rigister is not needed, use the black hole register
" "_ to eliminate it!
" - defaults
let s:r_regBin = 'b'
let s:r_regOct = 'o'
let s:r_regDec = 'd'
let s:r_regHex = 'h'
" - user definitions
if exists('g:ShowMultiBase_Register_Binary')
  let s:r_regBin = s:check_register(g:ShowMultiBase_Register_Binary)
endif
if exists('g:ShowMultiBase_Register_Octal')
  let s:r_regOct = s:check_register(g:ShowMultiBase_Register_Octal)
endif
if exists('g:ShowMultiBase_Register_Decimal')
  let s:r_regDec = s:check_register(g:ShowMultiBase_Register_Decimal)
endif
if exists('g:ShowMultiBase_Register_Hexadecimal')
  let s:r_regHex = s:check_register(g:ShowMultiBase_Register_Hexadecimal)
endif

" The number with this base is put in the unnamed register, so it can be
" pasted immediately after the conversion operation.
" Use e.g. 0 to save none in the unnamed register!
if !exists('g:ShowMultiBase_Register_UnnamedBase')
  let g:ShowMultiBase_Register_UnnamedBase = 10
endif
" The number with this base is put in the clipboard register, so it can be
" pasted immediately after the conversion operation, in other applications.
" Use e.g. 0 to save none in the clipboard register!
if !exists('g:ShowMultiBase_Register_ClipboardBase')
  let g:ShowMultiBase_Register_ClipboardBase = 10
endif

" Define the prefix for the numbers saved in registers
" - defaults
let s:r_prefixBin = ''
let s:r_prefixOct = ''
let s:r_prefixDec = ''
let s:r_prefixHex = ''
" - user definitions
if exists('g:ShowMultiBase_Register_Binary_Prefix')
  let s:r_prefixBin = g:ShowMultiBase_Register_Binary_Prefix
endif
if exists('g:ShowMultiBase_Register_Octal_Prefix')
  let s:r_prefixOct = g:ShowMultiBase_Register_Octal_Prefix
endif
if exists('g:ShowMultiBase_Register_Decimal_Prefix')
  let s:r_prefixDec = g:ShowMultiBase_Register_Decimal_Prefix
endif
if exists('g:ShowMultiBase_Register_Hexadecimal_Prefix')
  let s:r_prefixHex = g:ShowMultiBase_Register_Hexadecimal_Prefix
endif

" If set to 1, the numbers will be saved in register with upper case. This
" only affects the hexadecimal A-F digits, therefore there is no option for
" each base.
" - default
let s:r_cap = 0
" - user definition
if exists('g:ShowMultiBase_Register_UseCapital')
  let s:r_cap = g:ShowMultiBase_Register_UseCapital
endif

" With the following flags, leading zeros may be requested to prepend to the
" numbers saved in register to fill the specified segment size. E.g, if the
" binary segment size is 4, and the binary result is 10, the displayed number
" is 0010.
" - defaults
let s:r_fillSegBin = 1
let s:r_fillSegOct = 1
let s:r_fillSegDec = 0
let s:r_fillSegHex = 1
" - user definitions
if exists('g:ShowMultiBase_Register_Binary_FillSegment')
  let s:r_fillSegBin = g:ShowMultiBase_Register_Binary_FillSegment
endif
if exists('g:ShowMultiBase_Register_Octal_FillSegment')
  let s:r_fillSegBin = g:ShowMultiBase_Register_Octal_FillSegment
endif
if exists('g:ShowMultiBase_Register_Decimal_FillSegment')
  let s:r_fillSegBin = g:ShowMultiBase_Register_Decimal_FillSegment
endif
if exists('g:ShowMultiBase_Register_Hexadecimal_FillSegment')
  let s:r_fillSegBin = g:ShowMultiBase_Register_Hexadecimal_FillSegment
endif

" The following parameters define the delimiter of each base to use between
" the segments saved in register.
" - defaults
let s:r_xBin = ' '
let s:r_xOct = ' '
let s:r_xDec = ''
let s:r_xHex = ' '
" - user definitions
if exists('g:ShowMultiBase_Register_Binary_Delimiter')
  let s:r_xBin = g:ShowMultiBase_Register_Binary_Delimiter
endif
if exists('g:ShowMultiBase_Register_Octal_Delimiter')
  let s:r_xOct = g:ShowMultiBase_Register_Octal_Delimiter
endif
if exists('g:ShowMultiBase_Register_Decimal_Delimiter')
  let s:r_xDec = g:ShowMultiBase_Register_Decimal_Delimiter
endif
if exists('g:ShowMultiBase_Register_Hexadecimal_Delimiter')
  let s:r_xHex = g:ShowMultiBase_Register_Hexadecimal_Delimiter
endif

" The number of digits that shall be saved in register next to each other,
" without having a delimiter in between, can be set for each base individually
" with the parameters below. The segmentation can switched off by setting the
" value to 0.
" - defaults
let s:r_segSizeBin = 8
let s:r_segSizeOct = 3
let s:r_segSizeDec = 0
let s:r_segSizeHex = 4
" - user definitions
if exists('g:ShowMultiBase_Register_Binary_SegmentSize')
  let s:r_segSizeBin = g:ShowMultiBase_Register_Binary_SegmentSize
endif
if exists('g:ShowMultiBase_Register_Octal_SegmentSize')
  let s:r_segSizeBin = g:ShowMultiBase_Register_Octal_SegmentSize
endif
if exists('g:ShowMultiBase_Register_Decimal_SegmentSize')
  let s:r_segSizeBin = g:ShowMultiBase_Register_Decimal_SegmentSize
endif
if exists('g:ShowMultiBase_Register_Hexadecimal_SegmentSize')
  let s:r_segSizeBin = g:ShowMultiBase_Register_Hexadecimal_SegmentSize
endif

" Configuration: Display section

" The following flags are responsible to display the corresponding base in the
" command line. If not defined, the default values are used, otherwise 0 means
" no display, 1 means display.
" - defaults
let s:d_showBin = 1
let s:d_showOct = 1
let s:d_showDec = 0
let s:d_showHex = 1
" - user definitions
if exists('g:ShowMultiBase_Display_Binary_Show')
  let s:d_showBin = g:ShowMultiBase_Display_Binary_Show
endif
if exists('g:ShowMultiBase_Display_Octal_Show')
  let s:d_showOct = g:ShowMultiBase_Display_Octal_Show
endif
if exists('g:ShowMultiBase_Display_Decimal_Show')
  let s:d_showDec = g:ShowMultiBase_Display_Decimal_Show
endif
if exists('g:ShowMultiBase_Display_Hexadecimal_Show')
  let s:d_showHex = g:ShowMultiBase_Display_Hexadecimal_SHow
endif

" The displayed prefix of each base can be configured with the following
" parameters.
" - defaults
" - user definitions
let s:d_prefixBin = 'b'
let s:d_prefixOct = '0'
let s:d_prefixDec = ''
let s:d_prefixHex = '0x'
if exists('g:ShowMultiBase_Dispaly_Binary_Prefix')
  let s:d_prefixBin = g:ShowMultiBase_Dispaly_Binary_Prefix
endif
if exists('g:ShowMultiBase_Dispaly_Octal_Prefix')
  let s:d_prefixOct = g:ShowMultiBase_Dispaly_Octal_Prefix
endif
if exists('g:ShowMultiBase_Dispaly_Decimal_Prefix')
  let s:d_prefixDec = g:ShowMultiBase_Dispaly_Decimal_Prefix
endif
if exists('g:ShowMultiBase_Dispaly_Hexadecimal_Prefix')
  let s:d_prefixHex = g:ShowMultiBase_Dispaly_Hexadecimal_Prefix
endif

" The number of digits that shall be displayed next to each other, without
" having a delimiter in between, can be set for each base individually with
" the parameters below. The segmentation can switched off by setting the value
" to 0.
" - defaults
let s:d_segSizeBin = 8
let s:d_segSizeOct = 3
let s:d_segSizeDec = 0
let s:d_segSizeHex = 4
" - user definitions
if exists('g:ShowMultiBase_Display_Binary_SegmentSize')
  let s:d_segSizeBin = g:ShowMultiBase_Display_Binary_SegmentSize
endif
if exists('g:ShowMultiBase_Display_Octal_SegmentSize')
  let s:d_segSizeBin = g:ShowMultiBase_Display_Octal_SegmentSize
endif
if exists('g:ShowMultiBase_Display_Decimal_SegmentSize')
  let s:d_segSizeBin = g:ShowMultiBase_Display_Decimal_SegmentSize
endif
if exists('g:ShowMultiBase_Display_Hexadecimal_SegmentSize')
  let s:d_segSizeBin = g:ShowMultiBase_Display_Hexadecimal_SegmentSize
endif


" With the following flags, leading zeros may be requested to prepend to the
" displayed number to fill the specified segment size. E.g, if the binary
" segment size is 4, and the binary result is 10, the displayed number is 0010.
" - defaults
let s:d_fillSegBin = 1
let s:d_fillSegOct = 1
let s:d_fillSegDec = 0
let s:d_fillSegHex = 1
" - user definitions
if exists('g:ShowMultiBase_Display_Binary_FillSegment')
  let s:d_fillSegBin = g:ShowMultiBase_Display_Binary_FillSegment
endif
if exists('g:ShowMultiBase_Display_Octal_FillSegment')
  let s:d_fillSegBin = g:ShowMultiBase_Display_Octal_FillSegment
endif
if exists('g:ShowMultiBase_Display_Decimal_FillSegment')
  let s:d_fillSegBin = g:ShowMultiBase_Decimal_FillSegment
endif
if exists('g:ShowMultiBase_Display_Hexadecimal_FillSegment')
  let s:d_fillSegBin = g:ShowMultiBase_Display_Hexadecimal_FillSegment
endif

" The following parameters define the delimiter of each base to use between
" the displayed segments.
" - defaults
let s:d_xBin = ' '
let s:d_xOct = ' '
let s:d_xDec = ''
let s:d_xHex = ' '
" - user definitions
if exists('g:ShowMultiBase_Display_Binary_Delimiter')
  let s:d_xBin = g:ShowMultiBase_Display_Binary_Delimiter
endif
if exists('g:ShowMultiBase_Display_Octal_Delimiter')
  let s:d_xOct = g:ShowMultiBase_Display_Octal_Delimiter
endif
if exists('g:ShowMultiBase_Display_Decimal_Delimiter')
  let s:d_xDec = g:ShowMultiBase_Display_Decimal_Delimiter
endif
if exists('g:ShowMultiBase_Display_Hexadecimal_Delimiter')
  let s:d_xHex = g:ShowMultiBase_Display_Hexadecimal_Delimiter
endif

" If set to 1, the numbers will be displayed with upper case. This only
" affects the hexadecimal A-F digits, therefore there is no option for each
" base.
" - default
let s:d_cap = 0
" - user definition
if exists('g:ShowMultiBase_Display_UseCapital')
  let s:d_cap = g:ShowMultiBase_Display_UseCapital
endif

" The separator between the displayed numbers. It may contain the new line
" character to display the potentially long numbers in separate lines.
" - default
let s:d_sep = ' == '
" - user definition
if exists('g:ShowMultiBase_Display_BaseSeparator')
  let s:d_sep = g:ShowMultiBase_Display_BaseSeparator
endif



command -nargs=* ShowMultiBase :call ShowMultiBase(<f-args>)

let s:hexdig='0123456789abcdef'



"
" The main function
"

function! ShowMultiBase(...)
  let l:parse_delimiter = ''
  let l:nbin = ''
  let l:noct = ''
  let l:ndec = ''
  let l:nhex = ''
  let l:base = 0 " invalid: it must be autosensed
  if (a:0 > 0)
    let l:base = a:1
  endif

  if (a:0 < 2)
    let l:PrefixPattern = ''
    if s:p_BinPrefix != ''
      if l:PrefixPattern != ''
        let l:PrefixPattern = l:PrefixPattern . '\|'
      endif
      let l:PrefixPattern = l:PrefixPattern . s:p_BinPrefix
    endif
    if s:p_OctPrefix != ''
      if l:PrefixPattern != ''
        let l:PrefixPattern = l:PrefixPattern . '\|'
      endif
      let l:PrefixPattern = l:PrefixPattern . s:p_OctPrefix
    endif
    if s:p_DecPrefix != ''
      if l:PrefixPattern != ''
        let l:PrefixPattern = l:PrefixPattern . '\|'
      endif
      let l:PrefixPattern = l:PrefixPattern . s:p_DecPrefix
    endif
    if s:p_HexPrefix != ''
      if l:PrefixPattern != ''
        let l:PrefixPattern = l:PrefixPattern . '\|'
      endif
      let l:PrefixPattern = l:PrefixPattern . s:p_HexPrefix
    endif
    if l:PrefixPattern != ''
      let l:PrefixPattern = '\(' . l:PrefixPattern . '\)\?'
    endif
    let l:n = ''
    let l:delimiters = split(s:p_delimiters, '0', 1)
    for l:delimiter in l:delimiters
      let l:n1 = s:get_pattern_at_cursor(l:PrefixPattern . '\x\+\(' . l:delimiter . '\x\+\)*')
      if len(l:n1) > len(l:n)
        let l:n = l:n1
        let l:parse_delimiter = l:delimiter
      endif
    endfor
    if (l:n == '')
      echohl WarningMsg |
        echo "There is no number under the cursor!" |
      echohl None
      return
    endif
    if l:parse_delimiter != ''
      let l:n = substitute(l:n, l:parse_delimiter, '', 'g')
    endif
  else
    let l:n = a:2
  endif

  " all the conversions happen with lower case
  let l:n = substitute(l:n, "\\u", "\\l&", "g")

  " If the base is set by the caller, perform sanity check on the number!
  " If the number and the base do not match, print a message and continue
  " with autosensing the base.
  let l:base_error = 0
  if l:base == 2
    let l:PrefixPattern = '^'
    if s:p_BinPrefix != ''
      let l:PrefixPattern = l:PrefixPattern . '\(' . s:p_BinPrefix . '\)\?'
    endif
    if match(l:n, l:PrefixPattern . '[01]\+\(' . l:parse_delimiter . '[01]\+\)*$') == -1
      let l:base_error = 1
      echohl WarningMsg |
        echo "Not binary number, try autosense ..."
      echohl None
    else
      let l:nbin = l:n
    endif
  elseif l:base == 8
    let l:PrefixPattern = '^'
    if s:p_OctPrefix != ''
      let l:PrefixPattern = l:PrefixPattern . '\(' . s:p_OctPrefix . '\)\?'
    endif
    if match(l:n, l:PrefixPattern . '[0-7]\+\(' . l:parse_delimiter . '[0-7]\+\)*$') == -1
      let l:base_error = 1
      echohl WarningMsg |
        echoerr "Not octal number, try autosense ..."
      echohl None
    else
      let l:noct = l:n
    endif
  elseif l:base == 10
    let l:PrefixPattern = '^'
    if s:p_DecPrefix != ''
      let l:PrefixPattern = l:PrefixPattern . '\(' . s:p_DecPrefix . '\)\?'
    endif
    if match(l:n, l:PrefixPattern . '[0-9]\+\(' . l:parse_delimiter . '[0-9]\+\)*$') == -1
      let l:base_error = 1
      echohl WarningMsg |
        echoerr "Not decimal number, try autosense ..."
      echohl None
    else
      let l:ndec = l:n
    endif
  elseif l:base == 16
    let l:PrefixPattern = '^'
    if s:p_HexPrefix != ''
      let l:PrefixPattern = l:PrefixPattern . '\(' . s:p_HexPrefix . '\)\?'
    endif
    if match(l:n, l:PrefixPattern . '\x\+\(' . l:parse_delimiter . '\x\+\)*$') == -1
      let l:base_error = 1
      echohl WarningMsg |
        echoerr "Not hexadecimal number, try autosense ..."
      echohl None
    else
      let l:nhex = l:n
    endif
  elseif l:base != 0
    echohl WarningMsg |
      echo "This base is not supported. Use 2, 8, 10 or 16!" |
    echohl None
    let l:base_error = 1
  endif

  if l:base == 0 || l:base_error == 1
    " base not known, autosense
    let l:given_base = l:base
    let l:PrefixPattern = '^'
    if s:p_BinPrefix != ''
      let l:PrefixPattern = l:PrefixPattern . s:p_BinPrefix
    endif
    if match(l:n, l:PrefixPattern . '[01]\+\(' . l:parse_delimiter . '[01]\+\)*$') == 0
    \ || (s:p_Only01Bin == 1 && match(l:n, '^[01]\+\(' . l:parse_delimiter . '[01]\+\)*$') == 0)
      let l:base = 2
      let l:nbin = l:n
    else
      let l:PrefixPattern = '^'
      if s:p_DecPrefix != ''
        let l:PrefixPattern = l:PrefixPattern . s:p_DecPrefix
      endif
      if match(l:n, l:PrefixPattern . '[1-9][0-9]*\(' . l:parse_delimiter . '[0-9]\+\)*$') == 0
        let l:base = 10
        let l:ndec = l:n
      else
        let l:PrefixPattern = '^'
        if s:p_HexPrefix != ''
          let l:PrefixPattern = l:PrefixPattern . s:p_HexPrefix
        endif
        if match(l:n, l:PrefixPattern . '\x\+\(' . l:parse_delimiter . '\x\+\)*$') == 0
          let l:base = 16
          let l:nhex = l:n
        else
          let l:PrefixPattern = '^'
          if s:p_OctPrefix != ''
            let l:PrefixPattern = l:PrefixPattern . s:p_OctPrefix
          endif
          if match(l:n, l:PrefixPattern . '[0-7]\+\(' . l:parse_delimiter . '[0-7]\+\)*$') == 0
            let l:base = 8
            let l:noct = l:n
          else
            " TODO: should we make some last attempts based on the contained
            " characters, before we print error?
            echoerr "@DEV: This should not happen, "
                    \"improve the number regexp to avoid getting here!"
            return
          endif
        endif
      endif
    endif


    if l:base_error == 1
      echohl WarningMsg |
        echo "The selected number cannot have base " . given_base
        \ . "! The autoselected base is " . l:base . "." |
      echohl None
    endif
  endif

  " base is now known, remove possible prefix
  if l:base == 2
    let l:n = s:clean_number(l:n, s:p_BinPrefix)
    let l:nbin = s:clean_number(l:nbin, s:p_BinPrefix)
  elseif l:base == 8
    let l:n = s:clean_number(l:n, s:p_OctPrefix)
    let l:noct = s:clean_number(l:noct, s:p_OctPrefix)
  elseif l:base == 10
    let l:n = s:clean_number(l:n, s:p_DecPrefix)
    let l:ndec = s:clean_number(l:ndec, s:p_DecPrefix)
  else
    let l:n = s:clean_number(l:n, s:p_HexPrefix)
    let l:nhex = s:clean_number(l:nhex, s:p_HexPrefix)
  endif

  " Convert to decimal first
  if l:base != 10
    let l:ndec = printf("%.0f", Base2Dec(l:n, l:base))
  endif

  " Convert to octal
  if l:base != 8
    let l:noct = Dec2Oct(l:ndec)
  endif

  " Convert to binary
  if l:base != 2
    let l:nbin = Dec2Bin(l:ndec)
    let l:nbin = substitute(l:nbin, '^ ', '', '')
  endif

  " Convert to hex
  if l:base !=16
    let l:nhex = Dec2Hex(l:ndec)
    let l:nhex = substitute(l:nhex, '^ ', '', '')
  endif

  " Display the configured bases and save in registers
  let l:display = ''
  let l:separator = ''
  let l:prefix = ''

  " Handle binary
  if s:d_showBin == 1 || l:base == 2
    let l:tmpval = s:format_number(l:nbin, s:d_fillSegBin,
                   \ s:d_segSizeBin, s:d_cap, s:d_prefixBin, s:d_xBin)
    let l:display = l:display . l:separator . l:tmpval
    let l:tmpval = s:format_number(l:nbin, s:r_fillSegBin,
                   \ s:r_segSizeBin, s:r_cap, s:r_prefixBin, s:r_xBin)
    call s:save_in_register(l:tmpval, s:r_regBin, 2)
    let l:separator = s:d_sep
  endif

  " Handle octal
  if s:d_showOct == 1 || l:base == 8
    let l:tmpval = s:format_number(l:noct, s:d_fillSegOct,
                   \ s:d_segSizeOct, s:d_cap, s:d_prefixOct, s:d_xOct)
    let l:display = l:display . l:separator . l:tmpval
    let l:tmpval = s:format_number(l:noct, s:r_fillSegOct,
                   \ s:r_segSizeOct, s:r_cap, s:r_prefixOct, s:r_xOct)
    call s:save_in_register(l:tmpval, s:r_regOct, 8)
    let l:separator = s:d_sep
  endif

  " Handle decimal
  if s:d_showDec == 1 || l:base == 10
    let l:tmpval = s:format_number(l:ndec, s:d_fillSegDec,
                   \ s:d_segSizeDec, s:d_cap, s:d_prefixDec, s:d_xDec)
    let l:display = l:display . l:separator . l:tmpval
    let l:tmpval = s:format_number(l:ndec, s:r_fillSegDec,
                   \ s:r_segSizeDec, s:r_cap, s:r_prefixDec, s:r_xDec)
    call s:save_in_register(l:tmpval, s:r_regDec, 10)
    let l:separator = s:d_sep
  endif

  " Handle hexadecimal
  if s:d_showHex == 1 || l:base == 16
    let l:tmpval = s:format_number(l:nhex, s:d_fillSegHex,
                   \ s:d_segSizeHex, s:d_cap, s:d_prefixHex, s:d_xHex)
    let l:display = l:display . l:separator . l:tmpval
    let l:tmpval = s:format_number(l:nhex, s:r_fillSegHex,
                   \ s:r_segSizeHex, s:r_cap, s:r_prefixHex, s:r_xHex)
    call s:save_in_register(l:tmpval, s:r_regHex, 16)
  endif

  echo l:display

endfunction " ShowMultiBase



"
" Utility functions
"

function! s:get_pattern_at_cursor(pat)
  let col = col('.') - 1
  let line = getline('.')
  let ebeg = -1
  let cont = match(line, a:pat, 0)
  while (ebeg >= 0 || (0 <= cont) && (cont <= col))
    let contn = matchend(line, a:pat, cont)
    if (cont <= col) && (col < contn)
      let ebeg = match(line, a:pat, cont)
      let elen = contn - ebeg
      break
    else
      let cont = match(line, a:pat, contn)
    endif
  endwh
  if ebeg >= 0
    return strpart(line, ebeg, elen)
  else
    return ""
  endif
endfunction


function! Dec2Hex(number)
	let nr = str2float(a:number)
	let result = ''
	while nr >= 1
    let intresstr = printf("%f", nr / 16)
    let intresstr = substitute(intresstr, '\.\d\+$', '', '')
    let intres = str2float(intresstr)
    let modres = float2nr(nr - intres * 16)
    let result = printf("%x",modres) . result
		let nr = intres
	endwhile

  if result == ''
    let result = '0'
  endif

	return result
endfunction



function! Dec2Bin(number)
	let nr = str2float(a:number)
  let result = ''
  while nr >= 1
    let intresstr = printf("%f", nr / 2)
    let intresstr = substitute(intresstr, '\.\d\+$', '', '')
    let intres = str2float(intresstr)
    let modres = float2nr(nr - intres * 2)
    let result = printf("%d",modres) . result
		let nr = intres
  endwhile

  if result == ''
    let result = '0'
  endif

  return result
endfunction


" Convert any base = [2-16] to decimal
function! Base2Dec(number, base)
  let result = 0.0
  let pos = 0
  let len = strlen(a:number)
  while pos < len
    let x = strpart(a:number, pos, 1)
    let d = match(s:hexdig, x)
    let result = result * a:base + d
    let pos = pos + 1
  endwhile
  return result
endfunction


function! Dec2Oct(number)
	let nr = str2float(a:number)
  let result = ''
  while nr >= 1
    let intresstr = printf("%f", nr / 8)
    let intresstr = substitute(intresstr, '\.\d\+$', '', '')
    let intres = str2float(intresstr)
    let modres = float2nr(nr - intres * 8)
    let result = printf("%d",modres) . result
		let nr = intres
  endwhile
  if result == ''
    let result = '0'
  endif
  return result
endfunction



function! s:clean_number(num, prefix)
  let l:cnum = substitute(a:num, '^' . a:prefix, '', '')
  let l:cnum = substitute(l:cnum, '^0\+', '', '')
  return l:cnum
endfunction



function! s:save_in_register(value, regname, base)
  let l:regval = a:value
  call setreg(a:regname, l:regval, 'c')
  if g:ShowMultiBase_Register_UnnamedBase == a:base
    call setreg('"', l:regval, 'c')
  endif
  if g:ShowMultiBase_Register_ClipboardBase == a:base
    call setreg('+', l:regval, 'c')
  endif
endfunction



function! s:format_number(value, fill, segsize, usecap, prefix, delimiter)
  let len = len(a:value)
  let formatted = a:value
  if a:usecap == 1
    let formatted = substitute(formatted, "\\l", "\\u&", "g")
  endif
  if a:segsize > 0
    if a:fill == 1
      let newlen = ((len + a:segsize - 1) / a:segsize) * a:segsize
      while len < newlen
        let formatted = '0' . formatted
        let len = len + 1
      endwhile
    endif
    let delimited = ''
    let pos = 0
    while pos < len
      if pos > 0 " avoid leading delimiter
        let delimited = delimited . a:delimiter
      endif
      let delimited = delimited . strpart(formatted, pos, a:segsize)
      let pos = pos + a:segsize
    endwhile
    let formatted = delimited
  endif
  let formatted = a:prefix . formatted
  return formatted
endfunction

