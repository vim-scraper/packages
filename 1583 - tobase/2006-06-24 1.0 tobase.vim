" Vim number conversion functions
" Convert from base(2,8,10,16) to base(2-32). Also character to html converter.

" Maintainer:      Walter Hutchins
" Last Change:     2006/06/24
" Version:         1.0
" Setup:           Copy to ~/.vim/plugin
" Help:            :Tobase -h
"
:let s:he1=         "Usage - :Tobase obase [0oxy&]inum"
:let s:he1=s:he1 . "\n"
:let s:he1=s:he1 . "Convert inum to obase and and return it."
:let s:he1=s:he1 . "\n"
:let s:he1=s:he1 . "The result will be placed in the unnamed register (@@)."
:let s:he1=s:he1 . "\n"
:let s:he1=s:he1 . "A subsequent 'p' command would put the result" 
:let s:he1=s:he1 . "\n"
:let s:he1=s:he1 . "[from register x] |P| before, |p| after the cursor. "
:let s:he1=s:he1 . "\n"
:let s:he1=s:he1 . "Optional modifier preceeding inum indicates "
:let s:he1=s:he1 . "\n"
:let s:he1=s:he1 . "(zero or o)-octal, x-hexadecimal, y-binary, &-htmlchar,"
:let s:he1=s:he1 . "\n"
:let s:he1=s:he1 . "and if not present, inum defaults to base 10 (decimal)."
:let s:he1=s:he1 . "\n"
:let s:he1=s:he1 . "obase may be 2-32, c, or h."
:let s:he1=s:he1 . "\n"
:let s:he1=s:he1 . "If obase='c', then it will convert to a character."
:let s:he1=s:he1 . "\n"
:let s:he1=s:he1 . "So, you can paste accented characters into document."
:let s:he1=s:he1 . "\n"
:let s:he1=s:he1 . "If obase='h', then it will convert to an html entity."
:let s:he1=s:he1 . "\n"
:let s:he1=s:he1 . "Also character to html converter. 'Tobase -hc for info'"
:let s:he1=s:he1 . "\n"
:let s:he1=s:he1 . "\n"
:let s:he1=s:he1 . "examples:"
:let s:he1=s:he1 . "\n"
:let s:he1=s:he1 . "    :Tobase 16 192             - prints C0"
:let s:he1=s:he1 . "\n"
:let s:he1=s:he1 . "    :Tobase 10 xc0             - prints 192"
:let s:he1=s:he1 . "\n"
:let s:he1=s:he1 . "    :Tobase 11 2662            - prints 2000"
:let s:he1=s:he1 . "\n"
:let s:he1=s:he1 . "    :Tobase h 192              - prints &Agrave;"
:let s:he1=s:he1 . "\n"
:let s:he1=s:he1 . "    :Tobase c xC0              - prints A with grave accent"
:let s:he1=s:he1 . "\n"
:let s:he1=s:he1 . "    :Tobase 2 7 5 5            - prints 111 101 101 (chmod)"
:let s:he1=s:he1 . "\n"
:let s:he1=s:he1 . "    :Tobase 10 y111 y101 y101  - prints 755"
:let s:he1=s:he1 . "\n"
:let s:he1=s:he1 . "    :Tobase 16 244 164 96      - prints F4 A4 60 (sandybrown"
:let s:he1=s:he1 . "\n"
:let s:he1=s:he1 . "    :Tobase 10 xF4 xA4 o140    - prints 244 164 96"
:let s:he1=s:he1 . "\n"
:let s:he1=s:he1 . "    :Tobase c &Agr &Aac &Aum   - prints accented A's"
:let s:he1=s:he1 . "\n"

:let s:he2=""
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "Converting to/from Characters/HtmlEntities..."
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "One method is to use execute with a statement like"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . ":execute Char3HtmlStr()"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "to convert from characters to html entities."
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "or"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . ":execute Html3CharStr()"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "to convert from html to characters."
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "Setting up commands might be easier to remember."
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "First, commands should be defined to do the conversions."
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "Here is how to setup the command named Foo to convert"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "from ISO-8859-1 and CP1252 characters to html entities:"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . ":command! Foo execute Char3HtmlStr()"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "Once that has been done, you may do the character to html"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "conversion with the command"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . ":Foo"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "Here is how to setup the command named Foof to convert"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "from html entities to ISO-8859-1 and CP1252 characters:"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . ":command! Foof execute Html3CharStr()"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "Once that has been done, you may do the html to character"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "conversion with the command"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . ":Foof"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "These command may also be placed in the .vimrc file"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "The default conversions being performed include all the"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "ISO-8859-1 and CP1252 characters, plus the ones"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "known to PHP as htmlspecialchars(): &, <, >, and \""
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "(&amp; &lt; &gt; &quot;)."
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "You may wish to complicate things a little, and either"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "j include only the 'htmlspecialchars' in the conversion"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "or"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "n exclude only the 'htmlspecialchars' in the conversion"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "If so, then make the commands like:"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . ":command! -nargs=* Foo execute Char3HtmlStr(<f-args>)"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "and"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . ":command! -nargs=* Foof execute Html3CharStr(<f-args>)"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "which would give you a combination of 6 conversions:"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . ":Foo      (convert all special characters to html)"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . ":Foo j    (just do  &, <, >, and \")"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . ":Foo n    (not do  &, <, >, and \")"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . ":Foof     (convert all html entities to characters)"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . ":Foof j   (just do  &, <, >, and \")"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . ":Foof n   (not do  &, <, >, and \")"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "You could"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . ":Foo j  (just do  &, <, >, and \")"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "to make html validate with tidy."
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "You may expect:"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "When you do this, you will get one message about some"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "substitutions right away."
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "In the terminal, there will be a length of time in which"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . " nothing seems to be happening. The time depends on system"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . " speed and the file size. On 800mz pentium III and 3mb file"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . " this took 20 seconds (this compares to a sed script that"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . " did the same thing in 6 seconds. See Char2HtmlStr inside.)"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . " Then you will get all the rest of the messages about"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . " substitutions."
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "In the gui, these messages appeared more regularly"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . " as each set of substitutions was made."
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "Regarding characters known to PHP as htmlspecialchars()"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "&, <, >, and \" :"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "If you were using PHP and you did htmlspecialchars() more"
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "than once, you could start getting &amp;amp; corruption."
:let s:he2=s:he2 . "\n"
:let s:he2=s:he2 . "This script doesn't seem to do that."
:let s:he2=s:he2 . "\n"
"
"If you have called Tobase with base="h" you may then :call s:SubOne() 
"to replace the first such character on the current line with the html entity. 
"Likewise, :call s:SubAll() will replace all occurances of that character 
"in the document with the html code.

" see :help normal-index, encoding, nr2char,  char2nr
" see :help variable if the only desired conversion is either from hexadecimal 
"     to decimal or from octal to decimal.
" see :help ga,g8 if you want the code for the character under the cursor.

command! -nargs=* Tobase echo s:Tobasem(<f-args>)

:function s:Tobasem(base, ...)
    :if tolower(a:base) == "-h" || match(tolower(a:base), "help\\|?\\--h") != -1
        :echo s:he1
    :endif
    :if tolower(a:base) == "-hc"
        :echo s:he2
    :endif
    :if a:0 == 1
        :return s:Tobase(a:base, a:1)
    :endif
    "call Tobase with multiple input numbers (<=20)
    :let remargs=a:0
    :let nxtarg=1
    :let s:mresnum=""
    :while remargs > 0
        :if nxtarg == 1
            :let res=s:Tobase(a:base, a:1)
        :elseif nxtarg == 2
            :let res=s:Tobase(a:base, a:2)
        :elseif nxtarg == 3
            :let res=s:Tobase(a:base, a:3)
        :elseif nxtarg == 4
            :let res=s:Tobase(a:base, a:4)
        :elseif nxtarg == 5
            :let res=s:Tobase(a:base, a:5)
        :elseif nxtarg == 6
            :let res=s:Tobase(a:base, a:6)
        :elseif nxtarg == 7
            :let res=s:Tobase(a:base, a:7)
        :elseif nxtarg == 8
            :let res=s:Tobase(a:base, a:8)
        :elseif nxtarg == 9
            :let res=s:Tobase(a:base, a:9)
        :elseif nxtarg == 10
            :let res=s:Tobase(a:base, a:10)
        :elseif nxtarg == 11
            :let res=s:Tobase(a:base, a:11)
        :elseif nxtarg == 12
            :let res=s:Tobase(a:base, a:12)
        :elseif nxtarg == 13
            :let res=s:Tobase(a:base, a:13)
        :elseif nxtarg == 14
            :let res=s:Tobase(a:base, a:14)
        :elseif nxtarg == 15
            :let res=s:Tobase(a:base, a:15)
        :elseif nxtarg == 16
            :let res=s:Tobase(a:base, a:16)
        :elseif nxtarg == 17
            :let res=s:Tobase(a:base, a:17)
        :elseif nxtarg == 18
            :let res=s:Tobase(a:base, a:18)
        :elseif nxtarg == 19
            :let res=s:Tobase(a:base, a:19)
        :elseif nxtarg == 20
            :let res=s:Tobase(a:base, a:20)
        :endif
        :let s:mresnum=s:mresnum . " " . res
        :let remargs=remargs - 1
        :let nxtarg=nxtarg + 1
    :endwhile
    :let arrrr = "let @@=s:mresnum"
    :execute arrrr
    :return s:mresnum
:endfunction

:function s:Tobase(base, inum, ...)
    :if a:0 > 1
        :echo "Too many arguments for function. try s:Tobasem"
        :return
    :endif
    :let digits="0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    :let prefix="0OXY" "zero, O, X, Y
    :let s:base=a:base
    :if  tolower(a:base) == "c"
         :let s:base=10
         :let s:showchar=1
    :endif
    :if  tolower(a:base) == "h"
         :let s:base=16
         :let s:showchar=2
    :endif
    :let s:resnum=""
    :if a:0 > 0
        :let s:resnum=a:1
    :endif
    :let num=a:inum
    :if type(a:inum) == 1                           "if arg is string
        :if strpart(num, 0, 1) == "&"
             :let num=s:FindChar(num)
        :endif
        :let num=toupper(num)
        :let bcode=strpart(num, 0, 1)
        :if match(prefix, strpart(num, 0, 1)) > 0
            :let num=s:Todec(num)                     "convert to decimal
        :else
            :let num=num + 0                        "vim can convert to decimal
        :endif
    :endif
    :let r=num % s:base
    :if (num - r) == 0
        :let digit=strpart(digits, num, 1)
        :let s:resnum=digit . s:resnum
    :else
        :let digit=strpart(digits, r, 1)
        :let nxt=(num - r) / s:base
        :let s:resnum=digit . s:resnum
        :call s:Tobase(s:base, nxt, s:resnum) 
    :endif
    :if a:0 == 0
        :if exists("s:showchar") && s:showchar != 0
            "This is the html character 'game'
            :if s:showchar == 1     "clean for next use
                :let s:showchar=0
                :let arr = nr2char(s:resnum)
                :let arrrr = "let @@=arr"
                :execute arrrr
                :return arr
            :endif
            :if s:showchar == 2
                :let s:showchar=0    "clean for next use
                :call Char2HtmlStr()
                :let srch="x" . s:resnum
                :let srch2="\\"
                :let srch3="/"
                :let mtch1=match(s:c2h, srch)
                :let mtch2=match(s:c2h, srch2, mtch1)
                :let mtch3=match(s:c2h, srch3, mtch2 + 1)
                :let mlen=mtch3 - mtch2
                :if mtch1 == -1 || mtch2 == -1 || mtch3 == -1
                    :let arr=srch . "- Out of range of supported html entities"
                    :echo arr
                    :return
                :endif
                :let htmlcode=strpart(s:c2h, mtch2 + 1, mlen - 1)
                :let arr = htmlcode
                :let arrrr = "let @@=arr"
                :execute arrrr
                :let arr2 = nr2char(s:Todec(s:resnum))
                :let arr = "<" . htmlcode . "> " . arr2 .  ", \\" . srch
                :let srchchar = nr2char(s:Todec(srch))
                :let s:subOneCmd='s/\' . srchchar . '/\' . htmlcode . '/'
                :let s:subAllCmd='%s/\' . srchchar . '/\' . htmlcode . '/g'
                :return arr
            :endif
        :else
            :let arrrr = "let @@=s:resnum"
            :execute arrrr
            :return s:resnum
        :endif
    :endif
:endfunction

:function s:Todec(inum)
    :let decnum=0
    :let digits="0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    :let prefix="0OXY" "zero, O, X, Y
    :let num=toupper(a:inum)
    :let bcode=strpart(num, 0, 1)
    :if match(prefix, bcode) == -1
        :let s:mult=16
    :else
        :let num=strpart(num, 1)
        :if bcode == "O" || bcode == "0"
            :let s:mult=8
        :elseif bcode == "X"
            :let s:mult=16
        :elseif bcode == "Y"
            :let s:mult=2
        :endif
    :endif
    :let l=strlen(num)
    :let c=0
    :while (l > 0)
        :let place=l - 1
        :let fac=1
        :while (place > 0)
            :let fac=fac * s:mult
            :let place=place - 1
        :endwhile
        :let hdigit=strpart(num, c, 1)
        :let ddigit=match(digits, hdigit)
        :let decnum=decnum + (ddigit * fac)
        :let c=c + 1
        :let l=l - 1
    :endwhile
    :return decnum
:endfunction

:function s:FindChar(...)
    :let ampcode=a:1
    :call Char2HtmlStr()
    :let srch='x[a-fA-F0-9]\{2,2}/\\' . ampcode
    :let mtch1=match(s:c2h, srch)
    :let htmlcode=strpart(s:c2h, mtch1, 3)
    :let arr = htmlcode
    :let arrrr = "let @@=arr"
    :execute arrrr
    :return arr
:endfunction

:function s:SubOne()
    :if exists("s:subOneCmd")
        :let doCmd=s:subOneCmd
        :execute doCmd
    :endif
:endfunction

:function s:SubAll()
    :if exists("s:subAllCmd")
        :let doCmd=s:subAllCmd
        :execute doCmd
    :endif
:endfunction

:function Char2HtmlStr()
    "This string is used by Tobase to lookup the html character reference 
    "associated with a particular number.
    "The resulting string is actually a viable bash sed script to convert
    "from iso-8859-1 and cp1252 characters to html entities.
    "One method to make the script:
    "     :new
    "     :put =Char2HtmlStr()
    "     :w! myIso2Html
    :let s:c2h=""
    :let s:c2h=s:c2h . "#! /bin/bash" . "\n"
    :let s:c2h=s:c2h . "sed $(cat<<EOF" . "\n"
    "this will miss &bar, but at least won't do &amp;amp; corruption
    "this is similar to  :help @! foo\(bar\)\@! any "foo" not followed by "bar"
    :let s:c2h=s:c2h . " -e s/\\&\\([^a-zA-Z0-9#]\\{1,6\\}\\)\\([^;]\\)/\\&amp;\\1\\2/g" . "\n"
    "to get &bar, use the following line, but only run once or &amp;amp; mess.
    ":let s:c2h=s:c2h . " -e s/\\&/\\&amp;\\1\\2/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\x22/\\&quot;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\x3C/\\&lt;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\x3E/\\&gt;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\x82/\\&sbquo;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\x83/\\&fnof;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\x84/\\&bdquo;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\x85/\\&hellip;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\x86/\\&dagger;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\x87/\\&Dagger;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\x88/\\&circ;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\x89/\\&permil;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\x8A/\\&Scaron;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\x8B/\\&lsaquo;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\x8C/\\&OElig;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\x8D/\\x8D/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\x8E/\\&#381;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\x8F/\\x8F/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\x90/\\x90/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\x91/\\&lsquo;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\x92/\\&rsquo;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\x93/\\&ldquo;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\x94/\\&rdquo;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\x95/\\&bull;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\x96/\\&ndash;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\x97/\\&mdash;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\x98/\\&tilde;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\x99/\\&trade;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\x9A/\\&scaron;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\x9B/\\&rsaquo;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\x9C/\\&oelig;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\x9D/\\x9D/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\x9E/\\&#382;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\x9F/\\&Yuml;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xA0/\\&nbsp;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xA1/\\&iexcl;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xA2/\\&cent;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xA3/\\&pound;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xA4/\\&curren;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xA5/\\&yen;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xA6/\\&brvbar;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xA7/\\&sect;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xA8/\\&uml;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xA9/\\&copy;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xAA/\\&ordf;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xAB/\\&laquo;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xAC/\\&not;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xAD/\\&shy;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xAE/\\&reg;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xAF/\\&macr;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xB0/\\&deg;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xB1/\\&plusmn;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xB2/\\&sup2;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xB3/\\&sup3;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xB4/\\&acute;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xB5/\\&micro;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xB6/\\&para;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xB7/\\&middot;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xB8/\\&cedil;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xB9/\\&sup1;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xBA/\\&ordm;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xBB/\\&raquo;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xBC/\\&frac14;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xBD/\\&frac12;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xBE/\\&frac34;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xBF/\\&iquest;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xC0/\\&Agrave;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xC1/\\&Aacute;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xC2/\\&Acirc;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xC3/\\&Atilde;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xC4/\\&Auml;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xC5/\\&Aring;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xC6/\\&AElig;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xC7/\\&Ccedil;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xC8/\\&Egrave;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xC9/\\&Eacute;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xCA/\\&Ecirc;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xCB/\\&Euml;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xCC/\\&Igrave;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xCD/\\&Iacute;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xCE/\\&Icirc;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xCF/\\&Iuml;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xD0/\\&ETH;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xD1/\\&Ntilde;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xD2/\\&Ograve;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xD3/\\&Oacute;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xD4/\\&Ocirc;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xD5/\\&Otilde;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xD6/\\&Ouml;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xD7/\\&times;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xD8/\\&Oslash;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xD9/\\&Ugrave;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xDA/\\&Uacute;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xDB/\\&Ucirc;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xDC/\\&Uuml;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xDD/\\&Yacute;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xDE/\\&THORN;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xDF/\\&szlig;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xE0/\\&agrave;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xE1/\\&aacute;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xE2/\\&acirc;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xE3/\\&atilde;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xE4/\\&auml;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xE5/\\&aring;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xE6/\\&aelig;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xE7/\\&ccedil;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xE8/\\&egrave;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xE9/\\&eacute;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xEA/\\&ecirc;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xEB/\\&euml;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xEC/\\&igrave;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xED/\\&iacute;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xEE/\\&icirc;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xEF/\\&iuml;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xF0/\\&eth;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xF1/\\&ntilde;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xF2/\\&ograve;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xF3/\\&oacute;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xF4/\\&ocirc;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xF5/\\&otilde;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xF6/\\&ouml;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xF7/\\&divide;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xF8/\\&oslash;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xF9/\\&ugrave;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xFA/\\&uacute;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xFB/\\&ucirc;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xFC/\\&uuml;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xFD/\\&yacute;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xFE/\\&thorn;/g" . "\n"
    :let s:c2h=s:c2h . " -e s/\\xFF/\\&yuml;/g" . "\n"
    :let s:c2h=s:c2h . "EOF)" . "\n"
    :return s:c2h
:endfunction

:function Char3HtmlStr(...)
"The resulting string convert
"from iso-8859-1 and cp1252 characters to html entities.
"To use:
"    :command! Foo execute Char3HtmlStr()
"    :Foo
":Foo      (convert all special characters to html)
":Foo j  (just do  &, <, >, and ")
":Foo n  (not do  &, <, >, and ")
:if a:0 == 0
    :let s:conv=""
:else 
    :let s:conv=a:1
:endif

:let s:c2h=""

:if s:conv != "n"
":let s:c2h=s:c2h . "%s/&/\\&amp;/ge" . "\n"
"from :help @! -- foo\(bar\)\@!         any "foo" not followed by "bar"
:let s:c2h=s:c2h . '%s/&\([a-zA-Z0-9#]\{2,6};\)\@!/\&amp;/ge' . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('x22')) . "/\\&quot;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('x3C')) . "/\\&lt;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('x3E')) . "/\\&gt;/ge" . "\n"
:endif

:if s:conv != "j"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('x82')) . "/\\&sbquo;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('x83')) . "/\\&fnof;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('x84')) . "/\\&bdquo;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('x85')) . "/\\&hellip;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('x86')) . "/\\&dagger;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('x87')) . "/\\&Dagger;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('x88')) . "/\\&circ;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('x89')) . "/\\&permil;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('x8A')) . "/\\&Scaron;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('x8B')) . "/\\&lsaquo;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('x8C')) . "/\\&OElig;/ge" . "\n"
":let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('x8D')) . "/\\x8D/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('x8E')) . "/\\&#381;/ge" . "\n"
":let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('x8F')) . "/\\x8F/ge" . "\n"
":let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('x90')) . "/\\x90/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('x91')) . "/\\&lsquo;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('x92')) . "/\\&rsquo;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('x93')) . "/\\&ldquo;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('x94')) . "/\\&rdquo;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('x95')) . "/\\&bull;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('x96')) . "/\\&ndash;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('x97')) . "/\\&mdash;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('x98')) . "/\\&tilde;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('x99')) . "/\\&trade;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('x9A')) . "/\\&scaron;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('x9B')) . "/\\&rsaquo;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('x9C')) . "/\\&oelig;/ge" . "\n"
":let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('x9D')) . "/\\x9D/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('x9E')) . "/\\&#382;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('x9F')) . "/\\&Yuml;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xA0')) . "/\\&nbsp;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xA1')) . "/\\&iexcl;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xA2')) . "/\\&cent;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xA3')) . "/\\&pound;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xA4')) . "/\\&curren;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xA5')) . "/\\&yen;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xA6')) . "/\\&brvbar;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xA7')) . "/\\&sect;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xA8')) . "/\\&uml;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xA9')) . "/\\&copy;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xAA')) . "/\\&ordf;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xAB')) . "/\\&laquo;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xAC')) . "/\\&not;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xAD')) . "/\\&shy;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xAE')) . "/\\&reg;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xAF')) . "/\\&macr;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xB0')) . "/\\&deg;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xB1')) . "/\\&plusmn;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xB2')) . "/\\&sup2;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xB3')) . "/\\&sup3;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xB4')) . "/\\&acute;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xB5')) . "/\\&micro;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xB6')) . "/\\&para;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xB7')) . "/\\&middot;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xB8')) . "/\\&cedil;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xB9')) . "/\\&sup1;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xBA')) . "/\\&ordm;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xBB')) . "/\\&raquo;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xBC')) . "/\\&frac14;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xBD')) . "/\\&frac12;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xBE')) . "/\\&frac34;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xBF')) . "/\\&iquest;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xC0')) . "/\\&Agrave;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xC1')) . "/\\&Aacute;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xC2')) . "/\\&Acirc;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xC3')) . "/\\&Atilde;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xC4')) . "/\\&Auml;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xC5')) . "/\\&Aring;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xC6')) . "/\\&AElig;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xC7')) . "/\\&Ccedil;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xC8')) . "/\\&Egrave;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xC9')) . "/\\&Eacute;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xCA')) . "/\\&Ecirc;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xCB')) . "/\\&Euml;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xCC')) . "/\\&Igrave;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xCD')) . "/\\&Iacute;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xCE')) . "/\\&Icirc;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xCF')) . "/\\&Iuml;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xD0')) . "/\\&ETH;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xD1')) . "/\\&Ntilde;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xD2')) . "/\\&Ograve;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xD3')) . "/\\&Oacute;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xD4')) . "/\\&Ocirc;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xD5')) . "/\\&Otilde;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xD6')) . "/\\&Ouml;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xD7')) . "/\\&times;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xD8')) . "/\\&Oslash;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xD9')) . "/\\&Ugrave;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xDA')) . "/\\&Uacute;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xDB')) . "/\\&Ucirc;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xDC')) . "/\\&Uuml;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xDD')) . "/\\&Yacute;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xDE')) . "/\\&THORN;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xDF')) . "/\\&szlig;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xE0')) . "/\\&agrave;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xE1')) . "/\\&aacute;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xE2')) . "/\\&acirc;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xE3')) . "/\\&atilde;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xE4')) . "/\\&auml;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xE5')) . "/\\&aring;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xE6')) . "/\\&aelig;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xE7')) . "/\\&ccedil;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xE8')) . "/\\&egrave;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xE9')) . "/\\&eacute;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xEA')) . "/\\&ecirc;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xEB')) . "/\\&euml;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xEC')) . "/\\&igrave;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xED')) . "/\\&iacute;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xEE')) . "/\\&icirc;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xEF')) . "/\\&iuml;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xF0')) . "/\\&eth;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xF1')) . "/\\&ntilde;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xF2')) . "/\\&ograve;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xF3')) . "/\\&oacute;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xF4')) . "/\\&ocirc;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xF5')) . "/\\&otilde;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xF6')) . "/\\&ouml;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xF7')) . "/\\&divide;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xF8')) . "/\\&oslash;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xF9')) . "/\\&ugrave;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xFA')) . "/\\&uacute;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xFB')) . "/\\&ucirc;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xFC')) . "/\\&uuml;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xFD')) . "/\\&yacute;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xFE')) . "/\\&thorn;/ge" . "\n"
:let s:c2h=s:c2h . "%s/" . nr2char(s:Todec('xFF')) . "/\\&yuml;/ge" . "\n"
:endif
:return s:c2h
:endfunction

:function Html3CharStr(...)
"Reverses Char3HtmlStr()
"The resulting string convert
"to iso-8859-1 and cp1252 characters from html entities.
"To use:
"    :command! Foof execute Html3CharStr()
"    :Foof
":Foof      (convert all special characters to html)
":Foof j  (just do  &, <, >, and ")
":Foof n  (not do  &, <, >, and ")
:if a:0 == 0
    :let s:conv=""
:else 
    :let s:conv=a:1
:endif

:let s:h2c=""

:if s:conv != "n"
:let s:h2c=s:h2c . "%s/&amp;/\\&/ge" . "\n"
:let s:h2c=s:h2c . "%s/&quot;/" . nr2char(s:Todec('x22')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&lt;/" . nr2char(s:Todec('x3C')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&gt;/" . nr2char(s:Todec('x3E')) . "/ge" . "\n"
:endif

:if s:conv != "j"
:let s:h2c=s:h2c . "%s/&sbquo;/" . nr2char(s:Todec('x82')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&fnof;/" . nr2char(s:Todec('x83')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&bdquo;/" . nr2char(s:Todec('x84')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&hellip;/" . nr2char(s:Todec('x85')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&dagger;/" . nr2char(s:Todec('x86')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&Dagger;/" . nr2char(s:Todec('x87')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&circ;/" . nr2char(s:Todec('x88')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&permil;/" . nr2char(s:Todec('x89')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&Scaron;/" . nr2char(s:Todec('x8A')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&lsaquo;/" . nr2char(s:Todec('x8B')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&OElig;/" . nr2char(s:Todec('x8C')) . "/ge" . "\n"
":let s:h2c=s:h2c . "%s/" . nr2char(s:Todec('x8D')) . "/\\x8D/ge" . "\n"
:let s:h2c=s:h2c . "%s/&#381;/" . nr2char(s:Todec('x8E')) . "/ge" . "\n"
":let s:h2c=s:h2c . "%s/" . nr2char(s:Todec('x8F')) . "/\\x8F/ge" . "\n"
":let s:h2c=s:h2c . "%s/" . nr2char(s:Todec('x90')) . "/\\x90/ge" . "\n"
:let s:h2c=s:h2c . "%s/&lsquo;/" . nr2char(s:Todec('x91')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&rsquo;/" . nr2char(s:Todec('x92')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&ldquo;/" . nr2char(s:Todec('x93')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&rdquo;/" . nr2char(s:Todec('x94')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&bull;/" . nr2char(s:Todec('x95')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&ndash;/" . nr2char(s:Todec('x96')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&mdash;/" . nr2char(s:Todec('x97')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&tilde;/" . nr2char(s:Todec('x98')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&trade;/" . nr2char(s:Todec('x99')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&scaron;/" . nr2char(s:Todec('x9A')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&rsaquo;/" . nr2char(s:Todec('x9B')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&oelig;/" . nr2char(s:Todec('x9C')) . "/ge" . "\n"
":let s:h2c=s:h2c . "%s/" . nr2char(s:Todec('x9D')) . "/\\x9D/ge" . "\n"
:let s:h2c=s:h2c . "%s/&#382;/" . nr2char(s:Todec('x9E')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&Yuml;/" . nr2char(s:Todec('x9F')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&nbsp;/" . nr2char(s:Todec('xA0')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&iexcl;/" . nr2char(s:Todec('xA1')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&cent;/" . nr2char(s:Todec('xA2')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&pound;/" . nr2char(s:Todec('xA3')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&curren;/" . nr2char(s:Todec('xA4')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&yen;/" . nr2char(s:Todec('xA5')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&brvbar;/" . nr2char(s:Todec('xA6')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&sect;/" . nr2char(s:Todec('xA7')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&uml;/" . nr2char(s:Todec('xA8')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&copy;/" . nr2char(s:Todec('xA9')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&ordf;/" . nr2char(s:Todec('xAA')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&laquo;/" . nr2char(s:Todec('xAB')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&not;/" . nr2char(s:Todec('xAC')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&shy;/" . nr2char(s:Todec('xAD')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&reg;/" . nr2char(s:Todec('xAE')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&macr;/" . nr2char(s:Todec('xAF')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&deg;/" . nr2char(s:Todec('xB0')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&plusmn;/" . nr2char(s:Todec('xB1')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&sup2;/" . nr2char(s:Todec('xB2')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&sup3;/" . nr2char(s:Todec('xB3')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&acute;/" . nr2char(s:Todec('xB4')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&micro;/" . nr2char(s:Todec('xB5')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&para;/" . nr2char(s:Todec('xB6')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&middot;/" . nr2char(s:Todec('xB7')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&cedil;/" . nr2char(s:Todec('xB8')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&sup1;/" . nr2char(s:Todec('xB9')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&ordm;/" . nr2char(s:Todec('xBA')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&raquo;/" . nr2char(s:Todec('xBB')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&frac14;/" . nr2char(s:Todec('xBC')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&frac12;/" . nr2char(s:Todec('xBD')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&frac34;/" . nr2char(s:Todec('xBE')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&iquest;/" . nr2char(s:Todec('xBF')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&Agrave;/" . nr2char(s:Todec('xC0')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&Aacute;/" . nr2char(s:Todec('xC1')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&Acirc;/" . nr2char(s:Todec('xC2')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&Atilde;/" . nr2char(s:Todec('xC3')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&Auml;/" . nr2char(s:Todec('xC4')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&Aring;/" . nr2char(s:Todec('xC5')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&AElig;/" . nr2char(s:Todec('xC6')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&Ccedil;/" . nr2char(s:Todec('xC7')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&Egrave;/" . nr2char(s:Todec('xC8')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&Eacute;/" . nr2char(s:Todec('xC9')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&Ecirc;/" . nr2char(s:Todec('xCA')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&Euml;/" . nr2char(s:Todec('xCB')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&Igrave;/" . nr2char(s:Todec('xCC')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&Iacute;/" . nr2char(s:Todec('xCD')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&Icirc;/" . nr2char(s:Todec('xCE')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&Iuml;/" . nr2char(s:Todec('xCF')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&ETH;/" . nr2char(s:Todec('xD0')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&Ntilde;/" . nr2char(s:Todec('xD1')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&Ograve;/" . nr2char(s:Todec('xD2')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&Oacute;/" . nr2char(s:Todec('xD3')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&Ocirc;/" . nr2char(s:Todec('xD4')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&Otilde;/" . nr2char(s:Todec('xD5')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&Ouml;/" . nr2char(s:Todec('xD6')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&times;/" . nr2char(s:Todec('xD7')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&Oslash;/" . nr2char(s:Todec('xD8')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&Ugrave;/" . nr2char(s:Todec('xD9')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&Uacute;/" . nr2char(s:Todec('xDA')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&Ucirc;/" . nr2char(s:Todec('xDB')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&Uuml;/" . nr2char(s:Todec('xDC')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&Yacute;/" . nr2char(s:Todec('xDD')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&THORN;/" . nr2char(s:Todec('xDE')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&szlig;/" . nr2char(s:Todec('xDF')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&agrave;/" . nr2char(s:Todec('xE0')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&aacute;/" . nr2char(s:Todec('xE1')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&acirc;/" . nr2char(s:Todec('xE2')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&atilde;/" . nr2char(s:Todec('xE3')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&auml;/" . nr2char(s:Todec('xE4')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&aring;/" . nr2char(s:Todec('xE5')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&aelig;/" . nr2char(s:Todec('xE6')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&ccedil;/" . nr2char(s:Todec('xE7')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&egrave;/" . nr2char(s:Todec('xE8')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&eacute;/" . nr2char(s:Todec('xE9')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&ecirc;/" . nr2char(s:Todec('xEA')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&euml;/" . nr2char(s:Todec('xEB')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&igrave;/" . nr2char(s:Todec('xEC')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&iacute;/" . nr2char(s:Todec('xED')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&icirc;/" . nr2char(s:Todec('xEE')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&iuml;/" . nr2char(s:Todec('xEF')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&eth;/" . nr2char(s:Todec('xF0')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&ntilde;/" . nr2char(s:Todec('xF1')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&ograve;/" . nr2char(s:Todec('xF2')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&oacute;/" . nr2char(s:Todec('xF3')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&ocirc;/" . nr2char(s:Todec('xF4')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&otilde;/" . nr2char(s:Todec('xF5')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&ouml;/" . nr2char(s:Todec('xF6')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&divide;/" . nr2char(s:Todec('xF7')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&oslash;/" . nr2char(s:Todec('xF8')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&ugrave;/" . nr2char(s:Todec('xF9')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&uacute;/" . nr2char(s:Todec('xFA')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&ucirc;/" . nr2char(s:Todec('xFB')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&uuml;/" . nr2char(s:Todec('xFC')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&yacute;/" . nr2char(s:Todec('xFD')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&thorn;/" . nr2char(s:Todec('xFE')) . "/ge" . "\n"
:let s:h2c=s:h2c . "%s/&yuml;/" . nr2char(s:Todec('xFF')) . "/ge" . "\n"
:endif
:return s:h2c
:endfunction
