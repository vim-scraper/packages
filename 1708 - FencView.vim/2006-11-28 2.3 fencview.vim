"==================================================
" File:         fencview.vim
" Brief:        View a file in different encodings
" Author:       Mingbai <mbbill AT gmail DOT com>
" Last Change:  2006-11-28 19:52:42
" Version:      2.3
"
" Usage:
"               :FencAutoDetect
"                   Auto detect the file encoding.
"                   Supported encodings:
"                       utf-8
"                       cp936(GBK)
"                       cp950(big5)
"                       cp932(sjis)
"               :FencView
"                   Open the encoding list window,
"               <up> and <down> to select an encoding,
"               enter to reload the file, or you can
"               select a encoding from the menu.
" Note:         Make sure there is no modeline at
"               the end of your file.
"
"==================================================

let s:FencWinName="FencView_8795684"
let s:Fenc8bit=[
            \"latin1    8-bit.characters (ISO 8859-1)",
            \"koi8-r    Russian",
            \"koi8-u    Ukrainian",
            \"macroman  MacRoman (Macintosh encoding)",
            \"cp437     similar to iso-8859-1",
            \"cp737     similar to iso-8859-7",
            \"cp775     Baltic",
            \"cp850     similar to iso-8859-4",
            \"cp852     similar to iso-8859-1",
            \"cp855     similar to iso-8859-2",
            \"cp857     similar to iso-8859-5",
            \"cp860     similar to iso-8859-9",
            \"cp861     similar to iso-8859-1",
            \"cp862     similar to iso-8859-1",
            \"cp863     similar to iso-8859-8",
            \"cp865     similar to iso-8859-1",
            \"cp866     similar to iso-8859-5",
            \"cp869     similar to iso-8859-7",
            \"cp874     Thai",
            \"cp1250    Czech, Polish, etc",
            \"cp1251    Cyrillic",
            \"cp1253    Greek",
            \"cp1254    Turkish",
            \"cp1255    Hebrew",
            \"cp1256    Arabic",
            \"cp1257    Baltic",
            \"cp1258    Vietnamese"]
let s:Fenc16bit=[
            \"cp936     simplified Chinese (Windows only)",
            \"gb18030   simplified Chinese (Windows only)",
            \"euc-cn    simplified Chinese (Unix only)",
            \"cp950     traditional Chinese (on Unix alias for big5)",
            \"big5      traditional Chinese (on Windows alias for cp950)",
            \"euc-tw    traditional Chinese (Unix only)",
            \"cp932     Japanese (Windows only)",
            \"euc-jp    Japanese (Unix only)",
            \"sjis      Japanese (Unix only)",
            \"cp949     Korean (Unix and Windows)",
            \"euc-kr    Korean (Unix only)"]
let s:FencUnicode=[
            \"utf-8     32 bit UTF-8 encoded Unicode (ISO/IEC 10646-1)",
            \"ucs-2     16 bit UCS-2 encoded Unicode (ISO/IEC 10646-1)",
            \"ucs-2le   like ucs-2, little endian",
            \"utf-16    ucs-2 extended with double-words for more characters",
            \"utf-16le  like utf-16, little endian",
            \"ucs-4     32 bit UCS-4 encoded Unicode (ISO/IEC 10646-1)",
            \"ucs-4le   like ucs-4, little endian"]
let g:FencCustom=
            \"Examples:\n".
            \"------------------\n".
            \"iso-8859-n    ISO_8859 variant (n=2 to 15)\n".
            \"8bit-{name}   any 8-bit encoding (Vim specific name)\n".
            \"cp{number}    MS-Windows: any installed single-byte codepage\n".
            \"cp{number}    MS-Windows: any installed double-byte codepage\n".
            \"2byte-{name}  Unix: any double-byte encoding (Vim specific name)"

let s:cp936TopChars=[
\0xb5.0xc4,0xd2.0xbb,0xb9.0xfa,0xd4.0xda,0xc8.0xcb,0xc1.0xcb,0xd3.0xd0,0xd6.0xd0,
\0xca.0xc7,0xc4.0xea,0xba.0xcd,0xb4.0xf3,0xd2.0xb5,0xb2.0xbb,0xce.0xaa,0xb7.0xa2,
\0xbb.0xe1,0xb9.0xa4,0xbe.0xad,0xc9.0xcf,0xb5.0xd8,0xca.0xd0,0xd2.0xaa,0xb8.0xf6,
\0xb2.0xfa,0xd5.0xe2,0xb3.0xf6,0xd0.0xd0,0xd7.0xf7,0xc9.0xfa,0xbc.0xd2,0xd2.0xd4,
\0xb3.0xc9,0xb5.0xbd,0xc8.0xd5,0xc3.0xf1,0xc0.0xb4,0xce.0xd2,0xb2.0xbf,0xb6.0xd4,
\0xbd.0xf8,0xb6.0xe0,0xc8.0xab,0xbd.0xa8,0xcb.0xfb,0xb9.0xab,0xbf.0xaa,0xc3.0xc7,
\0xb3.0xa1,0xd5.0xb9,0xca.0xb1,0xc0.0xed,0xd0.0xc2,0xb7.0xbd,0xd6.0xf7,0xc6.0xf3,
\0xd7.0xca,0xca.0xb5,0xd1.0xa7,0xb1.0xa8,0xd6.0xc6,0xd5.0xfe,0xbc.0xc3,0xd3.0xc3,
\0xcd.0xac,0xd3.0xda,0xb7.0xa8,0xb8.0xdf,0xb3.0xa4,0xcf.0xd6,0xb1.0xbe,0xd4.0xc2,
\0xb6.0xa8,0xbb.0xaf,0xbc.0xd3,0xb6.0xaf,0xba.0xcf,0xc6.0xb7,0xd6.0xd8,0xb9.0xd8,
\0xbb.0xfa,0xb7.0xd6,0xc1.0xa6,0xd7.0xd4,0xcd.0xe2,0xd5.0xdf,0xc7.0xf8,0xc4.0xdc,
\0xc9.0xe8,0xba.0xf3,0xbe.0xcd,0xb5.0xc8,0xcc.0xe5,0xcf.0xc2,0xcd.0xf2,0xd4.0xaa,
\0xc9.0xe7,0xb9.0xfd,0xc7.0xb0,0xc3.0xe6,0xc5.0xa9,0xd2.0xb2,0xb5.0xc3,0xd3.0xeb,
\0xcb.0xb5,0xd6.0xae,0xd4.0xb1,0xb6.0xf8,0xce.0xf1,0xc0.0xfb,0xb5.0xe7,0xce.0xc4,
\0xca.0xc2,0xbf.0xc9,0xd6.0xd6,0xd7.0xdc,0xb8.0xc4,0xc8.0xfd,0xb8.0xf7,0xba.0xc3,
\0xbd.0xf0,0xb5.0xda,0xcb.0xbe,0xc6.0xe4,0xb4.0xd3,0xc6.0xbd,0xb4.0xfa,0xb5.0xb1,
\0xcc.0xec,0xcb.0xae,0xca.0xa1,0xcc.0xe1,0xc9.0xcc,0xca.0xae,0xb9.0xdc,0xc4.0xda,
\0xd0.0xa1,0xbc.0xbc,0xce.0xbb,0xc4.0xbf,0xc6.0xf0,0xba.0xa3,0xcb.0xf9,0xc1.0xa2,
\0xd2.0xd1,0xcd.0xa8,0xc8.0xeb,0xc1.0xbf,0xd7.0xd3,0xce.0xca,0xb6.0xc8,0xb1.0xb1,
\0xb1.0xa3,0xd0.0xc4,0xbb.0xb9,0xbf.0xc6,0xce.0xaf,0xb6.0xbc,0xca.0xf5,0xca.0xb9,
\0xc3.0xf7,0xd7.0xc5,0xb4.0xce,0xbd.0xab,0xd4.0xf6,0xbb.0xf9,0xc3.0xfb,0xcf.0xf2,
\0xc3.0xc5,0xd3.0xa6,0xc0.0xef,0xc3.0xc0,0xd3.0xc9,0xb9.0xe6,0xbd.0xf1,0xcc.0xe2,
\0xbc.0xc7,0xb5.0xe3,0xbc.0xc6,0xc8.0xa5,0xc7.0xbf,0xc1.0xbd,0xd0.0xa9,0xb1.0xed,
\0xcf.0xb5,0xb0.0xec,0xbd.0xcc,0xd5.0xfd,0xcc.0xf5,0xd7.0xee,0xb4.0xef,0xcc.0xd8,
\0xb8.0xef,0xca.0xd5,0xb6.0xfe,0xc6.0xda,0xb2.0xa2,0xb3.0xcc,0xb3.0xa7,0xc8.0xe7,
\0xb5.0xc0,0xbc.0xca,0xbc.0xb0,0xce.0xf7,0xbf.0xda,0xbe.0xa9,0xbb.0xaa,0xc8.0xce,
\0xb5.0xf7,0xd0.0xd4,0xb5.0xbc,0xd7.0xe9,0xb6.0xab,0xc2.0xb7,0xbb.0xee,0xb9.0xe3,
\0xd2.0xe2,0xb1.0xc8,0xcd.0xb6,0xbe.0xf6,0xbd.0xbb,0xcd.0xb3,0xb5.0xb3,0xc4.0xcf,
\0xb0.0xb2,0xb4.0xcb,0xc1.0xec,0xbd.0xe1,0xd3.0xaa,0xcf.0xee,0xc7.0xe9,0xbd.0xe2,
\0xd2.0xe9,0xd2.0xe5,0xc9.0xbd,0xcf.0xc8,0xb3.0xb5,0xc8.0xbb,0xbc.0xdb,0xb7.0xc5,
\0xca.0xc0,0xbc.0xe4,0xd2.0xf2,0xb9.0xb2,0xd4.0xba,0xb2.0xbd,0xce.0xef,0xbd.0xe7,
\0xbc.0xaf,0xb0.0xd1,0xb3.0xd6,0xce.0xde,0xb5.0xab,0xb3.0xc7,0xcf.0xe0,0xca.0xe9,
\0xb4.0xe5,0xc7.0xf3,0xd6.0xce,0xc8.0xa1,0xd4.0xad,0xb4.0xa6,0xb8.0xae,0xd1.0xd0,
\0xd6.0xca,0xd0.0xc5,0xcb.0xc4,0xd4.0xcb,0xcf.0xd8,0xbe.0xfc,0xbc.0xfe,0xd3.0xfd,
\0xbe.0xd6,0xb8.0xc9,0xb6.0xd3,0xcd.0xc5,0xd3.0xd6,0xd4.0xec,0xd0.0xce,0xbc.0xb6,
\0xb1.0xea,0xc1.0xaa,0xd7.0xa8,0xc9.0xd9,0xb7.0xd1,0xd0.0xa7,0xbe.0xdd,0xca.0xd6,
\0xca.0xa9,0xc8.0xa8,0xbd.0xad,0xbd.0xfc,0xc9.0xee,0xb8.0xfc,0xc8.0xcf,0xb9.0xfb,
\0xb8.0xf1,0xbc.0xb8,0xbf.0xb4,0xc3.0xbb,0xd6.0xb0,0xb7.0xfe,0xcc.0xa8,0xca.0xbd,
\0xd2.0xe6,0xcf.0xeb,0xca.0xfd,0xb5.0xa5,0xd1.0xf9,0xd6.0xbb,0xb1.0xbb,0xd2.0xda,
\0xc0.0xcf,0xca.0xdc,0xd3.0xc5,0xb3.0xa3,0xcf.0xfa,0xd6.0xbe,0xd5.0xbd,0xc1.0xf7,
\0xba.0xdc,0xbd.0xd3,0xcf.0xe7,0xcd.0xb7,0xb8.0xf8,0xd6.0xc1,0xc4.0xd1,0xb9.0xdb,
\0xd6.0xb8,0xb4.0xb4,0xd6.0xa4,0xd6.0xaf,0xc2.0xdb,0xb1.0xf0,0xce.0xe5,0xd0.0xad,
\0xb1.0xe4,0xb7.0xe7,0xc5.0xfa,0xbc.0xfb,0xbe.0xbf,0xd6.0xa7,0xc4.0xc7,0xb2.0xe9,
\0xd5.0xc5,0xbe.0xab,0xc3.0xbf,0xc1.0xd6,0xd7.0xaa,0xbb.0xae,0xd7.0xbc,0xd7.0xf6,
\0xd0.0xe8,0xb4.0xab,0xd5.0xf9,0xcb.0xb0,0xb9.0xb9,0xbe.0xdf,0xb0.0xd9,0xbb.0xf2,
\0xb2.0xc5,0xbb.0xfd,0xca.0xc6,0xbe.0xd9,0xb1.0xd8,0xd0.0xcd,0xd2.0xd7,0xca.0xd3,
\0xbf.0xec,0xc0.0xee,0xb2.0xce,0xbb.0xd8,0xd2.0xfd,0xd5.0xf2,0xca.0xd7,0xcd.0xc6,
\0xcb.0xbc,0xcd.0xea,0xcf.0xfb,0xd6.0xb5,0xb8.0xc3,0xd7.0xdf,0xd7.0xb0,0xd6.0xda,
\0xd4.0xf0,0xb1.0xb8,0xd6.0xdd,0xb9.0xa9,0xb0.0xfc,0xb8.0xb1,0xbc.0xab,0xd5.0xfb,
\0xc8.0xb7,0xd6.0xaa,0xc3.0xb3,0xbc.0xba,0xbb.0xb7,0xbb.0xb0,0xb7.0xb4,0xc9.0xed,
\0xd1.0xa1,0xd1.0xc7,0xc3.0xb4,0xb4.0xf8,0xb2.0xc9,0xcd.0xf5,0xb2.0xdf,0xd5.0xe6,
\0xc5.0xae,0xcc.0xb8,0xd1.0xcf,0xcb.0xb9,0xbf.0xf6,0xc9.0xab,0xb4.0xf2,0xb5.0xc2,
\0xb8.0xe6,0xbd.0xf6,0xcb.0xfc,0xc6.0xf8,0xc1.0xcf,0xc9.0xf1,0xc2.0xca,0xca.0xb6,
\0xc0.0xcd,0xbe.0xb3,0xd4.0xb4,0xc7.0xe0,0xbb.0xa4,0xc1.0xd0,0xd0.0xcb,0xd0.0xed,
\0xbb.0xa7,0xc2.0xed,0xb8.0xdb,0xd4.0xf2,0xbd.0xda,0xbf.0xee,0xc0.0xad,0xd6.0xb1,
\0xb0.0xb8,0xb9.0xc9,0xb9.0xe2,0xbd.0xcf,0xba.0xd3,0xbb.0xa8,0xb8.0xf9,0xb2.0xbc,
\0xcf.0xdf,0xcd.0xc1,0xbf.0xcb,0xd4.0xd9,0xc8.0xba,0xd2.0xbd,0xc7.0xe5,0xcb.0xd9,
\0xc2.0xc9,0xcb.0xfd,0xd7.0xe5,0xc0.0xfa,0xb7.0xc7,0xb8.0xd0,0xd5.0xbc,0xd0.0xf8,
\0xca.0xa6,0xba.0xce,0xd3.0xb0,0xb9.0xa6,0xb8.0xba,0xd1.0xe9,0xcd.0xfb,0xb2.0xc6,
\0xc0.0xe0,0xbb.0xf5,0xd4.0xbc,0xd2.0xd5,0xca.0xdb,0xc1.0xac,0xbc.0xcd,0xb0.0xb4,
\0xd1.0xb6,0xca.0xb7,0xca.0xbe,0xcf.0xf3,0xd1.0xf8,0xbb.0xf1,0xca.0xaf,0xca.0xb3,
\0xd7.0xa5,0xb8.0xbb,0xc4.0xa3,0xca.0xbc,0xd7.0xa1,0xc8.0xfc,0xbf.0xcd,0xd4.0xbd,
\0xce.0xc5,0xd1.0xeb,0xcf.0xaf,0xbc.0xe1]

let s:cp950TopChars=[
\0xaa.0xba,0xa4.0x40,0xb0.0xea,0xa6.0x62,0xa4.0x48,0xa4.0x46,0xa6.0xb3,0xa4.0xa4,
\0xac.0x4f,0xa6.0x7e,0xa9.0x4d,0xa4.0x6a,0xb7.0x7e,0xa4.0xa3,0xac.0xb0,0xb5.0x6f,
\0xb7.0x7c,0xa4.0x75,0xb8.0x67,0xa4.0x57,0xa6.0x61,0xa5.0xab,0xad.0x6e,0xad.0xd3,
\0xb2.0xa3,0xb3.0x6f,0xa5.0x58,0xa6.0xe6,0xa7.0x40,0xa5.0xcd,0xae.0x61,0xa5.0x48,
\0xa6.0xa8,0xa8.0xec,0xa4.0xe9,0xa5.0xc1,0xa8.0xd3,0xa7.0xda,0xb3.0xa1,0xb9.0xef,
\0xb6.0x69,0xa6.0x68,0xa5.0xfe,0xab.0xd8,0xa5.0x4c,0xa4.0xbd,0xb6.0x7d,0xad.0xcc,
\0xb3.0xf5,0xae.0x69,0xae.0xc9,0xb2.0x7a,0xb7.0x73,0xa4.0xe8,0xa5.0x44,0xa5.0xf8,
\0xb8.0xea,0xb9.0xea,0xbe.0xc7,0xb3.0xf8,0xa8.0xee,0xac.0x46,0xc0.0xd9,0xa5.0xce,
\0xa6.0x50,0xa4.0x5f,0xaa.0x6b,0xb0.0xaa,0xaa.0xf8,0xb2.0x7b,0xa5.0xbb,0xa4.0xeb,
\0xa9.0x77,0xa4.0xc6,0xa5.0x5b,0xb0.0xca,0xa6.0x58,0xab.0x7e,0xad.0xab,0xc3.0xf6,
\0xbe.0xf7,0xa4.0xc0,0xa4.0x4f,0xa6.0xdb,0xa5.0x7e,0xaa.0xcc,0xb0.0xcf,0xaf.0xe0,
\0xb3.0x5d,0xab.0xe1,0xb4.0x4e,0xb5.0xa5,0xc5.0xe9,0xa4.0x55,0xb8.0x55,0xa4.0xb8,
\0xaa.0xc0,0xb9.0x4c,0xab.0x65,0xad.0xb1,0xb9.0x41,0xa4.0x5d,0xb1.0x6f,0xbb.0x50,
\0xbb.0xa1,0xa4.0xa7,0xad.0xfb,0xa6.0xd3,0xb0.0xc8,0xa7.0x51,0xb9.0x71,0xa4.0xe5,
\0xa8.0xc6,0xa5.0x69,0xba.0xd8,0xc1.0x60,0xa7.0xef,0xa4.0x54,0xa6.0x55,0xa6.0x6e,
\0xaa.0xf7,0xb2.0xc4,0xa5.0x71,0xa8.0xe4,0xb1.0x71,0xa5.0xad,0xa5.0x4e,0xb7.0xed,
\0xa4.0xd1,0xa4.0xf4,0xac.0xd9,0xb4.0xa3,0xb0.0xd3,0xa4.0x51,0xba.0xde,0xa4.0xba,
\0xa4.0x70,0xa7.0xde,0xa6.0xec,0xa5.0xd8,0xb0.0x5f,0xae.0xfc,0xa9.0xd2,0xa5.0xdf,
\0xa4.0x77,0xb3.0x71,0xa4.0x4a,0xb6.0x71,0xa4.0x6c,0xb0.0xdd,0xab.0xd7,0xa5.0x5f,
\0xab.0x4f,0xa4.0xdf,0xc1.0xd9,0xac.0xec,0xa9.0x65,0xb3.0xa3,0xb3.0x4e,0xa8.0xcf,
\0xa9.0xfa,0xb5.0xdb,0xa6.0xb8,0xb1.0x4e,0xbc.0x57,0xb0.0xf2,0xa6.0x57,0xa6.0x56,
\0xaa.0xf9,0xc0.0xb3,0xb8.0xcc,0xac.0xfc,0xa5.0xd1,0xb3.0x57,0xa4.0xb5,0xc3.0x44,
\0xb0.0x4f,0xc2.0x49,0xad.0x70,0xa5.0x68,0xb1.0x6a,0xa8.0xe2,0xa8.0xc7,0xaa.0xed,
\0xa8.0x74,0xbf.0xec,0xb1.0xd0,0xa5.0xbf,0xb1.0xf8,0xb3.0xcc,0xb9.0x46,0xaf.0x53,
\0xad.0xb2,0xa6.0xac,0xa4.0x47,0xb4.0xc1,0xa8.0xc3,0xb5.0x7b,0xbc.0x74,0xa6.0x70,
\0xb9.0x44,0xbb.0xda,0xa4.0xce,0xa6.0xe8,0xa4.0x66,0xa8.0xca,0xb5.0xd8,0xa5.0xf4,
\0xbd.0xd5,0xa9.0xca,0xbe.0xc9,0xb2.0xd5,0xaa.0x46,0xb8.0xf4,0xac.0xa1,0xbc.0x73,
\0xb7.0x4e,0xa4.0xf1,0xa7.0xeb,0xa8.0x4d,0xa5.0xe6,0xb2.0xce,0xc4.0xd2,0xab.0x6e,
\0xa6.0x77,0xa6.0xb9,0xbb.0xe2,0xb5.0xb2,0xc0.0xe7,0xb6.0xb5,0xb1.0xa1,0xb8.0xd1,
\0xc4.0xb3,0xb8.0x71,0xa4.0x73,0xa5.0xfd,0xa8.0xae,0xb5.0x4d,0xbb.0xf9,0xa9.0xf1,
\0xa5.0x40,0xb6.0xa1,0xa6.0x5d,0xa6.0x40,0xb0.0x7c,0xa8.0x42,0xaa.0xab,0xac.0xc9,
\0xb6.0xb0,0xa7.0xe2,0xab.0xf9,0xb5.0x4c,0xa6.0xfd,0xab.0xb0,0xac.0xdb,0xae.0xd1,
\0xa7.0xf8,0xa8.0x44,0xaa.0x76,0xa8.0xfa,0xad.0xec,0xb3.0x42,0xa9.0xb2,0xac.0xe3,
\0xbd.0xe8,0xab.0x48,0xa5.0x7c,0xb9.0x42,0xbf.0xa4,0xad.0x78,0xa5.0xf3,0xa8.0x7c,
\0xa7.0xbd,0xb7.0x46,0xb6.0xa4,0xb9.0xce,0xa4.0x53,0xb3.0x79,0xa7.0xce,0xaf.0xc5,
\0xbc.0xd0,0xc1.0x70,0xb1.0x4d,0xa4.0xd6,0xb6.0x4f,0xae.0xc4,0xbe.0xda,0xa4.0xe2,
\0xac.0x49,0xc5.0x76,0xa6.0xbf,0xaa.0xf1,0xb2.0x60,0xa7.0xf3,0xbb.0x7b,0xaa.0x47,
\0xae.0xe6,0xb4.0x58,0xac.0xdd,0xa8.0x53,0xc2.0xbe,0xaa.0x41,0xa5.0x78,0xa6.0xa1,
\0xaf.0x71,0xb7.0x51,0xbc.0xc6,0xb3.0xe6,0xbc.0xcb,0xa5.0x75,0xb3.0x51,0xbb.0xf5,
\0xa6.0xd1,0xa8.0xfc,0xc0.0x75,0xb1.0x60,0xbe.0x50,0xa7.0xd3,0xbe.0xd4,0xac.0x79,
\0xab.0xdc,0xb1.0xb5,0xb6.0x6d,0xc0.0x59,0xb5.0xb9,0xa6.0xdc,0xc3.0xf8,0xc6.0x5b,
\0xab.0xfc,0xb3.0xd0,0xb5.0xfd,0xc2.0xb4,0xbd.0xd7,0xa7.0x4f,0xa4.0xad,0xa8.0xf3,
\0xc5.0xdc,0xad.0xb7,0xa7.0xe5,0xa8.0xa3,0xa8.0x73,0xa4.0xe4,0xa8.0xba,0xac.0x64,
\0xb1.0x69,0xba.0xeb,0xa8.0x43,0xaa.0x4c,0xc2.0xe0,0xb9.0xba,0xb7.0xc7,0xb0.0xb5,
\0xbb.0xdd,0xb6.0xc7,0xaa.0xa7,0xb5.0x7c,0xba.0x63,0xa8.0xe3,0xa6.0xca,0xa9.0xce,
\0xa4.0x7e,0xbf.0x6e,0xb6.0xd5,0xc1.0x7c,0xa5.0xb2,0xab.0xac,0xa9.0xf6,0xb5.0xf8,
\0xa7.0xd6,0xa7.0xf5,0xb0.0xd1,0xa6.0x5e,0xa4.0xde,0xc2.0xed,0xad.0xba,0xb1.0xc0,
\0xab.0xe4,0xa7.0xb9,0xae.0xf8,0xad.0xc8,0xb8.0xd3,0xa8.0xab,0xb8.0xcb,0xb2.0xb3,
\0xb3.0x64,0xb3.0xc6,0xa6.0x7b,0xa8.0xd1,0xa5.0x5d,0xb0.0xc6,0xb7.0xa5,0xbe.0xe3,
\0xbd.0x54,0xaa.0xbe,0xb6.0x54,0xa4.0x76,0xc0.0xf4,0xb8.0xdc,0xa4.0xcf,0xa8.0xad,
\0xbf.0xef,0xa8.0xc8,0xbb.0xf2,0xb1.0x61,0xb1.0xc4,0xa4.0xfd,0xb5.0xa6,0xaf.0x75,
\0xa4.0x6b,0xbd.0xcd,0xc4.0x59,0xb4.0xb5,0xaa.0x70,0xa6.0xe2,0xa5.0xb4,0xbc.0x77,
\0xa7.0x69,0xb6.0xc8,0xa5.0xa6,0xae.0xf0,0xae.0xc6,0xaf.0xab,0xb2.0x76,0xc3.0xd1,
\0xb3.0xd2,0xb9.0xd2,0xb7.0xbd,0xab.0x43,0xc5.0x40,0xa6.0x43,0xbf.0xb3,0xb3.0x5c,
\0xa4.0xe1,0xb0.0xa8,0xb4.0xe4,0xab.0x68,0xb8.0x60,0xb4.0xda,0xa9.0xd4,0xaa.0xbd,
\0xae.0xd7,0xaa.0xd1,0xa5.0xfa,0xb8.0xfb,0xaa.0x65,0xaa.0xe1,0xae.0xda,0xa5.0xac,
\0xbd.0x75,0xa4.0x67,0xa7.0x4a,0xa6.0x41,0xb8.0x73,0xc2.0xe5,0xb2.0x4d,0xb3.0x74,
\0xab.0xdf,0xa6.0x6f,0xb1.0xda,0xbe.0xfa,0xab.0x44,0xb7.0x50,0xa6.0xfb,0xc4.0xf2,
\0xae.0x76,0xa6.0xf3,0xbc.0x76,0xa5.0x5c,0xad.0x74,0xc5.0xe7,0xb1.0xe6,0xb0.0x5d,
\0xc3.0xfe,0xb3.0x66,0xac.0xf9,0xc3.0xc0,0xb0.0xe2,0xb3.0x73,0xac.0xf6,0xab.0xf6,
\0xb0.0x54,0xa5.0x76,0xa5.0xdc,0xb6.0x48,0xbe.0x69,0xc0.0xf2,0xa5.0xdb,0xad.0xb9,
\0xa7.0xec,0xb4.0x49,0xbc.0xd2,0xa9.0x6c,0xa6.0xed,0xc1.0xc9,0xab.0xc8,0xb6.0x56,
\0xbb.0x44,0xa5.0xa1,0xae.0x75,0xb0.0xed]

let s:cp932TopChars=[
\0x82.0xb5,0x82.0xf0,0x82.0xcc,0x82.0xb7,0x82.0xdc,0x82.0xc9,0x82.0xe9,0x81.0x5b,
\0x82.0xc6,0x82.0xcd,0x83.0x93,0x83.0x8b,0x83.0x43,0x83.0x76,0x82.0xc5,0x82.0xa4,
\0x82.0xc4,0x83.0x5e,0x83.0x58,0x82.0xa2,0x8d.0x73,0x82.0xe5,0x82.0xb1,0x82.0xbd,
\0x82.0xea,0x83.0x68,0x82.0xaa,0x93.0xae,0x83.0x62,0x83.0x4a,0x82.0xc8,0x83.0x8c,
\0x83.0x7d,0x83.0x5c,0x82.0xe8,0x88.0xda,0x82.0xb3,0x83.0x52,0x82.0xe7,0x82.0xa9,
\0x83.0x67,0x83.0x74,0x95.0xb6,0x89.0xba,0x83.0x65,0x83.0x40,0x83.0x4c,0x82.0xe0,
\0x89.0x9f,0x83.0x82,0x82.0xab,0x92.0x75,0x8e.0x9a,0x8f.0x9c,0x8d.0xed,0x82.0xe6,
\0x82.0xc1,0x82.0xad,0x82.0xc2,0x8c.0xea,0x93.0xfc,0x82.0xdd,0x83.0x56,0x82.0xbb,
\0x8e.0xa6,0x83.0x49,0x88.0xc8,0x82.0xaf,0x82.0xa6,0x92.0x50,0x8e.0x67,0x8d.0x58,
\0x95.0xcf,0x83.0x87,0x82.0xd6,0x8d.0xf5,0x8e.0x9f,0x8c.0x9f,0x8d.0xc5,0x91.0x53,
\0x91.0x7d,0x88.0xca,0x82.0xa0,0x82.0xdf,0x8a.0xd4,0x8a.0xb7,0x96.0xbc,0x88.0xe1,
\0x8f.0x49,0x95.0xfb,0x8f.0x89,0x83.0x8a,0x83.0x79,0x8c.0xe3,0x95.0xd4,0x8c.0x4a,
\0x89.0xc1,0x91.0x4f,0x83.0x45,0x83.0x42,0x82.0xce,0x97.0x70,0x8e.0xc0,0x82.0xed,
\0x97.0x76,0x90.0xb3,0x90.0x94,0x88.0xea,0x83.0x4e,0x83.0x41,0x82.0xf1,0x82.0xbe,
\0x95.0x94,0x92.0xc7,0x97.0x6c,0x96.0x96,0x91.0xb6,0x8e.0xe6,0x83.0x66,0x8d.0x9e,
\0x8f.0xc1,0x97.0xcd,0x83.0x85,0x83.0x60,0x82.0xb9,0x95.0x5c,0x8a.0x6d,0x92.0xe8,
\0x83.0x77,0x83.0x6d,0x93.0xaa,0x94.0x46,0x96.0xda,0x93.0x78,0x91.0xe5,0x8d.0xec,
\0x8f.0xe3,0x82.0xe2,0x82.0xb6,0x93.0xc7,0x8a.0xae,0x8e.0x6e,0x91.0xbd,0x8f.0xea,
\0x8b.0xe5,0x90.0xe6,0x95.0xdb,0x91.0xcc,0x97.0xb9,0x8a.0x4a,0x8b.0x4e,0x8c.0xa9,
\0x96.0xf1,0x91.0xce,0x8d.0x87,0x92.0x6c,0x83.0x57,0x91.0x49,0x90.0x69,0x90.0xdd,
\0x8f.0x91,0x8e.0x9e,0x8f.0xac,0x8c.0xfc,0x93.0xaf,0x89.0xbd,0x82.0xc7,0x96.0x40,
\0x96.0xdf,0x89.0xf1,0x8f.0x6f,0x83.0x8d,0x82.0xb8,0x82.0xa8,0x8f.0x57,0x8d.0xdb,
\0x8e.0x8e,0x95.0xe2,0x95.0xd2,0x94.0xd4,0x8c.0xbb,0x96.0x7b,0x91.0xf0,0x94.0xf6,
\0x95.0xaa,0x8f.0x43,0x97.0xe1,0x83.0x89,0x96.0xca,0x97.0x97,0x8a.0x6f,0x91.0xb1,
\0x89.0xe6,0x8a.0xf9,0x8f.0x8a,0x8b.0xad,0x8d.0xc4,0x83.0x73,0x83.0x47,0x83.0x46,
\0x93.0xc1,0x96.0xb3,0x90.0xac,0x95.0x4b,0x8a.0x77,0x8a.0x4f,0x8d.0xdd,0x95.0x74,
\0x91.0xbc,0x83.0x6f,0x82.0xde,0x82.0x9f,0x92.0xb2,0x90.0xe0,0x94.0x5c,0x8f.0x4b,
\0x94.0xcd,0x92.0x5b,0x92.0xbc,0x8f.0xf3,0x94.0xc5,0x97.0x88,0x96.0xbe,0x8a.0x87,
\0x91.0xd4,0x8c.0xca,0x8a.0xdc,0x8d.0x86,0x95.0xca,0x93.0xe0,0x90.0x6c,0x92.0x86,
\0x97.0x5e,0x82.0xe4,0x82.0xcb,0x82.0xbf,0x8b.0x74,0x93.0x5c,0x89.0xf0,0x95.0xa1,
\0x8e.0xa9,0x8b.0xf3,0x92.0x6d,0x94.0x92,0x97.0x9d,0x88.0xd7,0x92.0x8d,0x8e.0x63,
\0x8b.0xc6,0x91.0x80,0x8e.0x77,0x94.0xb2,0x8a.0xb5,0x88.0xd3,0x8f.0xee,0x89.0x9e,
\0x92.0xa5,0x8c.0x60,0x8e.0xae,0x8a.0xf4,0x95.0xf1,0x88.0xcd,0x89.0x45,0x89.0xc2,
\0x8b.0xe6,0x95.0x73,0x83.0x80,0x83.0x72,0x83.0x69,0x82.0xeb,0x82.0xd9,0x82.0xd7,
\0x82.0xd1,0x82.0xc3,0x97.0xde,0x91.0xe8,0x90.0xc2,0x8d.0x7e,0x95.0xc2,0x98.0x41,
\0x92.0xca,0x91.0xab,0x90.0xd4,0x91.0x45,0x97.0xfb,0x91.0x67,0x94.0x5b,0x8a.0xc8,
\0x91.0xe6,0x8f.0xcd,0x97.0xa7,0x8e.0xd0,0x94.0x6a,0x96.0xee,0x88.0xd9,0x8f.0xc6,
\0x8c.0x88,0x8b.0x43,0x8a.0xfc,0x8a.0x69,0x90.0x84,0x8e.0x9d,0x96.0x59,0x90.0x53,
\0x93.0xbe,0x96.0xf0,0x88.0xf8,0x8d.0xb6,0x8d.0x44,0x8e.0x51,0x88.0xf3,0x8c.0xf8,
\0x8c.0xb3,0x93.0xad,0x90.0x4d,0x83.0x83,0x83.0x7e,0x83.0x75,0x83.0x5b,0x83.0x59,
\0x83.0x55,0x83.0x4f,0x82.0xd4,0x82.0xbc,0x82.0xb0,0x82.0xac,0x97.0xed,0x95.0x70,
\0x8f.0x87,0x8d.0x80,0x94.0xf1,0x8b.0xf7,0x8c.0xaf,0x92.0xb7,0x94.0x7a,0x98.0x59,
\0x93.0x4b,0x92.0x42,0x91.0xac,0x93.0x72,0x8f.0x71,0x8d.0xda,0x8c.0x79,0x8e.0xd4,
\0x90.0x67,0x93.0xa5,0x8b.0x4d,0x92.0x4e,0x8c.0xeb,0x8f.0xda,0x96.0xf3,0x8c.0x76,
\0x8c.0xbe,0x90.0x65,0x8e.0x8b,0x90.0xbb,0x97.0x8e,0x90.0x46,0x97.0xc7,0x92.0x76,
\0x8e.0xd2,0xe3.0x59,0x8d.0xd7,0x8e.0x86,0x8f.0x83,0x93.0x9c,0x89.0xd3,0x93.0x99,
\0x95.0x84,0x92.0xf6,0x8e.0x84,0x8d.0xbb,0x92.0x5a,0x8a.0xc4,0x93.0x49,0x94.0xad,
\0x97.0xaa,0x8a.0xc3,0x93.0x5f,0x91.0xd7,0x8b.0x81,0x92.0x69,0x8b.0x40,0x8d.0x5c,
\0x8a.0x54,0x8a.0x79,0x8f.0xbc,0x91.0xba,0x8a.0xfa,0x93.0xfa,0x8b.0xb3,0x8a.0xf6,
\0x8c.0x66,0x8b.0x93,0x91.0xc5,0x8e.0xe8,0x8a.0xb4,0x8c.0x62,0x8e.0x76,0x94.0x4f,
\0x89.0xf5,0x8d.0x4f,0x8d.0x4c,0x8f.0xed,0x89.0xaa,0x97.0x65,0x8f.0xa7,0x91.0xbe,
\0x8a.0xee,0x90.0x7d,0x92.0x51,0x96.0xe2,0x8c.0xc4,0x96.0xa1,0x8d.0x90,0x8c.0xc3,
\0x8b.0x79,0x8f.0x5c,0x95.0xd7,0x8d.0x8f,0x97.0x98,0x94.0xbb,0x96.0x60,0x8b.0xa4,
\0x8f.0x5b,0x94.0xf5,0x95.0xd6,0x88.0xcb,0x95.0xb9,0x8e.0x97,0x93.0x60,0x91.0xe3,
\0x8e.0x64,0x8d.0xa1,0x97.0xbc,0x96.0x9c,0x83.0x86,0x83.0x84,0x83.0x71,0x83.0x70,
\0x83.0x6a,0x83.0x54,0x82.0xda,0x82.0xb2,0x81.0x58]


function! s:ToggleFencView() "{{{1
    let FencWinNr=bufwinnr(s:FencWinName)
    if FencWinNr!=-1
        exec FencWinNr." wincmd w"
        exec "wincmd c"
        return
    endif
    let _tmpfenc=&fenc
    let bmod=&modified
    if  bmod==1
        echoerr "File is modified!"
        return
    endif
    let splitLocation="belowright "
    let splitMode="vertical "
    let splitSize=34
    let cmd=splitLocation.splitMode.splitSize.' new '.s:FencWinName
    echo cmd
    silent! execute cmd
    setlocal winfixwidth
    setlocal noswapfile
    setlocal buftype=nowrite
    setlocal bufhidden=delete
    setlocal nowrap
    setlocal foldcolumn=0
    setlocal nobuflisted
    setlocal nospell
    setlocal nonumber
    setlocal cursorline
    call append(0,s:Fenc8bit)
    call append(0,s:Fenc16bit)
    call append(0,s:FencUnicode)
    normal Gddgg
    setlocal readonly
    setlocal nomodifiable
    syn match Type "^.\{-}\s"
    syn match Comment "\s.*$"
    if _tmpfenc!=""
        let s=search(_tmpfenc)
        if s!=0
            let _line=getline(line("."))
            let _fenc=substitute(_line,'\s.*$','',"g")
            syn clear Search
            exec "syn match Search \""._line."\""
        endif
    else
        normal gg
    endif
    nnoremap <buffer> <CR> :call <SID>FencSelect()<CR>
    nnoremap <buffer> <2-leftmouse> :call <SID>FencSelect()<CR>
endfunction


function! s:FencSelect() "{{{1
    let _line=getline(line("."))
    let _fenc=substitute(_line,'\s.*$','',"g")
    if _fenc==''
        return
    endif
    syn clear Search
    exec "syn match Search \""._line."\""
    let MainWinNr=winnr("#")
    if MainWinNr==0
        echoerr "Main window not found!"
        return
    endif
    exec MainWinNr." wincmd w"
    let _bmod=&modified
    if  _bmod==1
        echoerr "File is modified!"
        return
    endif
    exec "edit ++enc="._fenc
    let FencWinNr=bufwinnr(s:FencWinName)
    if FencWinNr==-1
        echoerr "Encoding list window not found!"
        return
    endif
    exec FencWinNr." wincmd w"
endfunction


function! s:FencCreateMenu() "{{{1
    for i in s:Fenc8bit
        let fencla=substitute(i,'\s.*$','','g')
        let fenname=fencla.'<tab>('.substitute(i,'^.\{-}\s\+','','g').')'
        let fenname=substitute(fenname,' ','\\ ','g')
        let fenname=substitute(fenname,'\.','_','g')
        exec 'menu &Tools.Encoding.8bit\ encodings.'.fenname.' :call FencMenuSel("'.fencla.'")<CR>'
    endfor
    for i in s:Fenc16bit
        let fencla=substitute(i,'\s.*$','','g')
        let fenname=fencla.'<tab>('.substitute(i,'^.\{-}\s\+','','g').')'
        let fenname=substitute(fenname,' ','\\ ','g')
        let fenname=substitute(fenname,'\.','_','g')
        exec 'menu &Tools.Encoding.16bit\ encodings.'.fenname.' :call FencMenuSel("'.fencla.'")<CR>'
    endfor
    for i in s:FencUnicode
        let fencla=substitute(i,'\s.*$','','g')
        let fenname=fencla.'<tab>('.substitute(i,'^.\{-}\s\+','','g').')'
        let fenname=substitute(fenname,' ','\\ ','g')
        let fenname=substitute(fenname,'\.','_','g')
        exec 'menu &Tools.Encoding.Unicode.'.fenname.' :call FencMenuSel("'.fencla.'")<CR>'
    endfor
    menu &Tools.Encoding.-sep-                  :
    menu &Tools.Encoding.Auto\ Detect           :FencAutoDetect<CR>
    menu &Tools.Encoding.Toggle\ Encoding\ list :FencView<CR>
    menu &Tools.Encoding.Input\.\.\.            :call FencMenuSel(inputdialog(g:FencCustom))<CR>
endfunction


function! FencMenuSel(fen_name) "{{{1
    if a:fen_name==''
        return
    endif
    let WinNr=winnr()
    if bufname(winnr())==s:FencWinName
        return
    endif
    exec "edit ++enc=".a:fen_name
endfunction


function! s:FencProgressBar(percentage, string, char, barlen) "{{{1
"-----------------------------------------
"   a:percentage -- percent of bar complete
"   a:string     -- leading description string (empty acceptable)
"   a:char       -- character to use as bar (suggest "#", "|" or "*")
"   a:barlen     -- bar length in columns, use 0 to use window width
	if a:barlen == 0
		let barlen=winwidth(0)-strlen(a:string)-1-2-3-2
	else
		let barlen=a:barlen
	endif
	let chrs=barlen*a:percentage/50
    let chrx=barlen-chrs
	let bar=a:string."["
	while chrs
		let bar=bar.a:char
		let chrs=chrs-1
	endwhile
	while chrx
		let bar=bar." "
		let chrx=chrx-1
	endwhile
	let bar=bar."] "
	if a:percentage < 10
		let bar=bar." "
	endif
	let bar=bar.a:percentage."%"

	let cmdheight=&cmdheight
	if cmdheight < 2
	    let &cmdheight=2
	endif
	echo bar
	let &cmdheight=cmdheight
endfunction


function! s:FencProbeCp936(c) "{{{1
    if s:cp936_bchar<=0x80
        if a:c<0x80
            return
        else
            let s:cp936_bchar=a:c
            return
        endif
    else
        if a:c<0x40
            let s:cp936_error+=1
            let s:cp936_bchar=0x0
            return
        else
            let wc=s:cp936_bchar.a:c
            let s:cp936_bchar=0x0
            if index(s:cp936TopChars,wc)!=-1
                let s:cp936_count+=1
                if s:cp936_count>=50
                    let s:FencRes="cp936"
                    return
                endif
            endif
        endif
    endif
endfunction


function! s:FencProbeCp950(c) "{{{1
    if s:cp950_bchar<=0x80
        if a:c<0x80
            return
        else
            let s:cp950_bchar=a:c
            return
        endif
    else
        if a:c<0x40
            let s:cp950_error+=1
            let s:cp950_bchar=0x0
            return
        else
            let wc=s:cp950_bchar.a:c
            let s:cp950_bchar=0x0
            if index(s:cp950TopChars,wc)!=-1
                let s:cp950_count+=1
                if s:cp950_count>=50
                    let s:FencRes="cp950"
                    return
                endif
            endif
        endif
    endif
endfunction


function! s:FencProbeCp932(c) "{{{1
    if s:cp932_bchar<=0x80
        if a:c<0x80
            return
        else
            let s:cp932_bchar=a:c
            return
        endif
    else
        if a:c<0x40
            let s:cp932_error+=1
            let s:cp932_bchar=0x0
            return
        else
            let wc=s:cp932_bchar.a:c
            let s:cp932_bchar=0x0
            if index(s:cp932TopChars,wc)!=-1
                let s:cp932_count+=1
                if s:cp932_count>=50
                    let s:FencRes="cp932"
                    return
                endif
            endif
        endif
    endif
endfunction


function! s:FencProbeUTF8(c) "{{{1
    if s:UTF8_state=="start"
        if a:c<=0x7f "still start state
            return
        elseif (a:c>=0xc0) && (a:c<=0xdf)
            let s:UTF8_state="wait"
            let s:UTF8_waitNr=1
            return
        elseif (a:c>=0xe0) && (a:c<=0xef)
            let s:UTF8_state="wait"
            let s:UTF8_waitNr=2
            return
        elseif (a:c>=0xf0) && (a:c<=0xf7)
            let s:UTF8_state="wait"
            let s:UTF8_waitNr=3
            return
        elseif (a:c>=0xf8) && (a:c<=0xfb)
            let s:UTF8_state="wait"
            let s:UTF8_waitNr=4
            return
        elseif (a:c>=0xf0) && (a:c<=0xfd)
            let s:UTF8_state="wait"
            let s:UTF8_waitNr=5
            return
        else
            let s:UTF8_error+=1
            return
        endif
    else    "s:UTF8_state=="wait"
        if (a:c>=0x80) && (a:c<=0xbf)
            let s:UTF8_waitNr-=1
            if s:UTF8_waitNr==0
                let s:UTF8_state="start"
                let s:UTF8_count+=1
                if (s:UTF8_count>=20) && (s:UTF8_error==0)
                    let s:FencRes="utf-8"
                    return
                elseif s:UTF8_count>=50
                    let s:FencRes="utf-8"
                    return
                endif
            endif
            return
        else
            let s:UTF8_error+=1
            let s:UTF8_waitNr=0
            let s:UTF8_state="start"
            return
        endif
    endif
endfunction


function! s:FencHandleData() "{{{1
    let lnr=0
    let filename=bufname('%')
    if filename=='' || filename==s:FencWinName
        return
    endif
    let fbody=readfile(filename,'b')
    if fbody==[]
        return
    endif
    " really useful ?
    if s:FencProbeBOM(fbody[0])==1
        return
    endif
    let bodylen=len(fbody)
    for line in fbody
        let lnr+=1
        call s:FencProgressBar(50*lnr/bodylen,' Processing... ','=',35)
        let ci=0
        let ch="\x01"
        while ch!=''
            let ch=line[ci]
            let c=char2nr(ch)
            "=============
            call s:FencProbeUTF8(c)
            call s:FencProbeCp936(c)
            call s:FencProbeCp950(c)
            call s:FencProbeCp932(c)
            if s:FencRes!=''
                return
            endif
            "=============
            let ci=ci+1
        endwhile
    endfor
endfunction 

function! s:FencProbeBOM(Firstline) "{{{1
" Vim can probe the file encoding by BOM correctly.
" This function is used to prevent probing other
" encodings by mistake.
    let ah1=a:Firstline[0]
    let ah2=a:Firstline[1]
    let ah3=a:Firstline[2]
    let ah4=a:Firstline[3]
    let a1=char2nr(ah1)
    let a2=char2nr(ah2)
    let a3=char2nr(ah3)
    let a4=char2nr(ah4)
    if a1.a2.a3==0xef.0xbb.0xbf "utf-8
        let s:FencRes="BOM"
        return 1
    elseif a1.a2.a3.a4==0xfe.0xff.0x00.0x00 "ucs-4
        let s:FencRes="BOM"
        return 1
    elseif a1.a2==0xfe.0xff "utf-16
        let s:FencRes="BOM"
        return 1
    elseif a1.a2.a3.a4==0x00.0x00.0xfe.0xff "utf-32
        let s:FencRes="BOM"
        return 1
    elseif a1.a2.a3.a4==0x00.0x00.0xff.0xfe" "ucs-4
        let s:FencRes="BOM"
        return 1
    elseif a1.a2.a3.a4==0xff.0xfe.0x00.0x00 "utf-32le
        let s:FencRes="BOM"
        return 1
    elseif a1.a2==0xff.0xfe "utf-16le
        let s:FencRes="BOM"
        return 1
    endif
endfunction


function! s:FencInitVar() "{{{1
    " when to handle the error ?
    let s:FencResCount=0
    let s:FencRes=''
    let s:cp936_bchar=''
    let s:cp936_count=0
    let s:cp936_error=0
    let s:cp950_bchar=''
    let s:cp950_count=0
    let s:cp950_error=0
    let s:cp932_bchar=''
    let s:cp932_count=0
    let s:cp932_error=0
    let s:UTF8_state="start"
    let s:UTF8_waitNr=0
    let s:UTF8_count=0
    let s:UTF8_error=0
endfunction


function! s:FencDetectFileEncoding() "{{{1
    call s:FencInitVar()
    call s:FencHandleData()
    if s:FencRes!=''
        if s:FencRes=="BOM"
            exec "e"
        else
            exec "edit ++enc=".s:FencRes
        endif
        return
    else
        if s:FencResCount<(s:UTF8_count-s:UTF8_error)
            let s:FencResCount=s:UTF8_count
            let s:FencRes="utf-8"
        endif
        if s:FencResCount<(s:cp936_count-s:cp936_error)
            let s:FencResCount=s:cp936_count
            let s:FencRes="cp936"
        endif
        if s:FencResCount<(s:cp950_count-s:cp950_error)
            let s:FencResCount=s:cp950_count
            let s:FencRes="cp950"
        endif
        if s:FencResCount<(s:cp932_count-s:cp932_error)
            let s:FencResCount=s:cp932_count
            let s:FencRes="cp932"
        endif
    endif
    if s:FencRes!=''
        exec "edit ++enc=".s:FencRes
    endif
endfunction

call    s:FencCreateMenu()
command!    -nargs=0 FencView       call s:ToggleFencView()
command!    -nargs=0 FencAutoDetect call s:FencDetectFileEncoding()

" vim: set ft=vim ff=unix fdm=marker :