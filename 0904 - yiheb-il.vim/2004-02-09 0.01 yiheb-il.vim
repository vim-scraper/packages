" Yiddish/hebrew, on israeli keymap:
" Convenient for typing both Yiddish and Hebrew
" Created: 2004, Feb, by oyd11
" Based on Ron Aaron's hebrew_utf-8 file and Raphael Finkel's Yiddish keymap

" Mapped: Same as israeli 'hebrew' layout (as printed on keyboards)
" But: Final letters are mapped on 'shifted' keys, freeing puncuations
" and the letters o,i,l,q,w
"   o/O -> komec-aleph / Oj
"   i/I -> cvej-jodn
"   
" I find the final on shift mapping much more natual, + you don't have to have sep
" hebrew/yiddish maps this way

let b:keymap_name = "Yiddish"
loadkeymap

" Yiddish-ext:
o <char-0x05d0><char-0x05B8> " komec-aleph 'o'
O <char-0x5f1>    " 'oj'
i <char-0x05f2> " ײcvej-jodn 'ej'
I <char-0x05f2><char-0x05B7> " ײַ+ patax 'aj'
T <char-0x05d0><char-0x05B7> " ײַaleph-patax
U <char-0x05f0> " װcvej-vovn 'v'
q <char-0x5bf>	" rafe
D <char-0x5bc>	" dagesh (D)
S <char-0x5bc>	" dagesh (shift-dalet)
Q <char-0x5bc>	" dagesh (shift-rafe)
X <char-0x5ea>	" ת - tav (shift-tet)
Y <char-0x5ea>	" ת - tav (shift-samex)

" shin / sin dots:
A	<char-0x5e9><Char-0x5c2> " sin-dot (shift-shin)
Z	<char-0x5e9><Char-0x5c1> " shin-dot (shift-zajin)

" Final:
M	<char-0x5e5>	" ץ - final tsadi
P	<char-0x5e3>	" ף - final pe
B	<char-0x5df>	" ן - final nun
F	<char-0x5da>	" ך - final kaf
N	<char-0x5dd>	" ם - final mem

" Nikud:
w:	<Char-0x5b0>	" sheva
ww	<Char-0x5b0>	" sheva
JE	<Char-0x5b1>	" xataf segol (shift-xef)
JA	<Char-0x5b2>	" xataf patax
JO	<Char-0x5b3>	" xataf kamac
wi	<Char-0x5b4>	" xirik
H	<Char-0x5b4>	" xirik (shift-jod)
wj	<Char-0x5b5>	" tsere
we	<Char-0x5b6>	" segol
wa	<Char-0x5b7>	" patah
wo	<Char-0x5b8>	" qamats
ww	<Char-0x5b9>	" holam
wu	<Char-0x5bb>	" qubuts


" il-Hebrew layout:
a	<char-0x5e9>	" ש - shin
b	<char-0x5e0>	" נ - nun
c	<char-0x5d1>	" ב - bet
d	<char-0x5d2>	" ג - gimel
e	<char-0x5e7>	" ק - qof
f	<char-0x5db>	" כ - kaf
g	<char-0x5e2>	" ע - yajin
h	<char-0x5d9>	" י - jod
j	<char-0x5d7>	" ח - het
k	<char-0x5dc>	" ל - lamed
m	<char-0x5e6>	" צ - tsadi
n	<char-0x5de>	" מ - mem
p	<char-0x5e4>	" פ - pe
r	<char-0x5e8>	" ר - resh
s	<char-0x5d3>	" ד - dalet
t	<char-0x5d0>	" א - alef
u	<char-0x5d5>	" ו - vav
v	<char-0x5d4>	" ה - he
x	<char-0x5e1>	" ס - samekh
y	<char-0x5d8>	" ט - tet
z	<char-0x5d6>	" ז - zajin
