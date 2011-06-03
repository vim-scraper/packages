"=============================================================================
"     FileName: gtrans.vim
"         Desc: Google Translate Plugin for Vim
"				Need python
"       Author: yicuan
"        Email: bolasblack@gmail.com
"     HomePage: http://plafer.tk
"      Version: 0.1
"   LastChange: 2011-04-18 20:30:45
"      Install: 把文件复制到 $VIM/plugin/ 目录下即可，插件需要 Python 的支持。
"        Usage: 使用时，可以在可视模式下高亮选择文本，或者在普通模式下将光标移
"               到目标单词上，使用 /gt 快捷键或者输入 :Gtrans 来获得翻译
"      History: 2011.4.18 完工～（应该差不多了……），目前的情况是，翻译一次可视
"				模式高亮的内容后，无法重复翻译相同内容……
"=============================================================================
if !has('python')
	finish
endif

command! -nargs=0 Gtrans call GetTrans()
map <leader>gt :call GetTrans()<CR>

let g:pmsg = ''

func! GetTrans() "{{{
	let msg = GetVisual()
	"let currMode = mode()
	"echo currMode.' '.msg
	if msg == '' || g:pmsg == msg
		call Translate(eval("expand('<cword>')"))
	else
		call Translate(msg)
		let g:pmsg = msg
	endif
endfunc
"}}}

func! GetVisual() "{{{
	let firstcol= col("'<")
	let lastcol= col("'>")
	let firstline = line("'<")
	let lastline = line("'>")
	let str = ''
	if firstline == lastline 
		let ll  = getline(firstline)
		let str = strpart(ll,firstcol-1,lastcol-firstcol)
	else
		let lcount = firstline+1
		let lines = []
		let ll  = strpart(getline(firstline),firstcol-1)
		call add(lines,ll)
		while lcount < lastline
			let ll = getline(lcount)
			call add(lines,ll)
			let lcount += 1
		endw
		let ll = strpart(getline(lcount),0,lastcol-1)
		call add(lines,ll)
		let str = join(lines,"\n")
	endif
	return str
endfunc
"}}}

func! Translate(text) "{{{
python << EOF
# -*- coding:utf-8 -*-
import urllib
import urllib2
import json
import vim
import sys 
reload(sys) 
sys.setdefaultencoding('utf8')

def translate(word):
    output = ''
    url    = 'http://translate.google.com.tw/translate_a/t'
    params = urllib.urlencode({'client':'json',
	                          #'sl':'en',
                               'tl':'zh-CN',
                               'text':word})
    req    = url + '?' + params
    opener = urllib2.build_opener()
    opener.addheaders = [('User-agent','Mozilla/5.0')]
    urllib2.install_opener(opener)
    data   = urllib2.urlopen(req).read()
    data   = json.loads(data)
    try:
        result = data['dict']
    except:
        sentences = data['sentences']
        for i in range(0,len(sentences)):
            output += sentences[i]['orig'] + ':\n'
            output += sentences[i]['trans']
    else:
        output = word + ':\n'
        for i in range(0,len(result)):
            output += result[i]['pos']
            output += ':'
            for j in range(0,len(result[i]['terms'])):
                if j != 0:
                    output += ','
                output += result[i]['terms'][j]
            if i != len(result)-1:
                output += '\n'
    vim.command('call ShowTransWindow("' + output.encode(vim.eval("&encoding")) + '")')

text = vim.eval('a:text')
#text = text.strip()
text.replace(' ','%20')
text.replace('\n','%0A')
translate(text)
EOF
endfunc
"}}}

func! ShowTransWindow(string) "{{{
	let @z = a:string
	if bufexists("TransWindow") > 0
		sil! bwipeout TransWindow
	endif
	silent botright new TransWindow
	if bufexists("TransWindow") > 0
		resize 5
		set buftype=nofile
		sil normal "zP
	endif
endfunc
"}}}
