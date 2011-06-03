"translate.vim
"author      : sowill
"email       : sowill@gmail.com
"description : translate the english word that under cursor to chinese
"usage       : put cursor on the english word , then type :Trans , 
			  "I map the :Trans to <Space>,just like 
			  "nmap <Space> :Trans<CR>
			  "in .vimrc files.
"Install     : just drop it to your plugin directory.

function! Translate_dic_cn()
python << EOF
import urllib2,re,vim

def Trans_word(word):
    if not word:
        print 'be sure there is word under cursor'
        return
    url='http://dict.cn/'+word
    req = urllib2.Request(url)
    response = urllib2.urlopen(req)
    res=response.read()
    ox_result=re.findall(r"exp_exp.*?\<\/div\>",res,re.S)
    ming_result=''
    for i in ox_result:
    	ming_result += i
    ming_result=re.sub('\<br \/\>','\n',ming_result);
    ming_result=re.sub('\t|exp_exp.*?\>|\<\/div\>|\r','',ming_result)
    if ming_result:
        print ming_result.decode('gb2312').encode(vim.eval('&encoding'))
    else:
        print 'no result was found for : \''+word+'\''
Trans_word(vim.eval("expand('<cword>')"))
EOF
endfunction
command! -nargs=0 Trans :call Translate_dic_cn()
