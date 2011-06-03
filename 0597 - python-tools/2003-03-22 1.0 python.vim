"
" HappyDoc : [CTRL]-h
"
"  Computes the happydoc diagram for
" the current class and open it
"
nmap <C-h> :silent !happydoc --dia % && dia --nosplash doc/dia.dia&

"
" getPythonDoc : [CTRL]-j
"
"  Regexp parser to determine in
" which class/method the cursor
" is located.
"
"  getPythonDoc does real-time
" parsing and does not require
" a tags file.
"
" nb: " mark 'p' used 
"
nmap <C-j> mo:py msg=getPythonDoc()'o:py print msg

"
" collapsePythonBlock : [CTRL]-k
"
"  Regexp parser to collapse the
" current class/method block
"
" nb: " mark 'p' used 
"
nmap <C-k> :py collapsePythonBlock()

"
" putEmptyFunc : [CTRL]-n
" 
" Writes a new empty python
" function under the current
" cursor line
" 
" (like typing [o] 
" [...func text...] [ESC])
" 
nmap <C-n> :py putEmptyFunc()

"
" ExploreUpper : [F9]
"
" Cool directory exploration ;)
"
nmap <F9> :vsp %:h

"
" Drastic terminal measures
" 
" Every character above 80th column
" just becomes ... unreadable and
" warning
"
" Comment to prevent
"
syntax match rightMargin /.\%>80v/
highlight rightMargin term=bold ctermfg=blue guifg=blue guibg=gray


"
" Python bindings
"
python << __end_of_python__
import vim
import re

emptyFunc = ("""def func(self, arg):
    """+'"""'+"""function func
    
    arg: string

    returns
    """+'"""'+"""
    return None # raise NotImplementedError()
""").split("\n")
emptyFunc.reverse()

def putEmptyFunc():
    """function putEmptyFunc
    """
    for line in emptyFunc:
        vim.current.range.append(line)

def getPythonDoc():
    """function getPythonDoc

    Computes the current path in class
    such as 'MyClass.myFunction' when
    cursor is in function myFunction,
    belonging to class MyClass.

    Mark 'p' for 'python' is set in Vi 
    to help retrieve the function/class
    headline declaration
    
    returns string
    """
    funcname = "Nowhere"
    classname = None

    # Look for the surrounding block definition
    if vim.eval("search('^\s*\(class\|def\) ','bW')"):
        vim.command("mark p")
        results = re.compile('(class|def) +([0-9a-zA-Z_]+) *').search(vim.current.line)
        if results:
            funcname = results.group(2)

            # Look for the upper class
            if vim.eval("search('^class','bW')"):
                results = (results, re.compile('class *([0-9a-zA-Z_]+)[ :(]').search(vim.current.line),)
                classname = results[1] and results[1].group(1)
    
    if classname:
        return classname+"."+funcname
    return funcname


def collapsePythonBlock():
    """function collapsePythonBlock

    Collapse the current python block,
    either a method or a class

    Mark 'p' for 'python' is set in Vi 
    to help retrieve the function/class
    headline declaration
    
    returns string
    """
    if vim.eval("search('^\s*\(class\|def\) ','bW')"):
        vim.command("mark p")
        start = vim.current.buffer.mark("p")
        # retain indentation level
        # and search for next non-blank
        # less-indented line
        if vim.eval("search('^\s*\(class\|def\) ','W')"):
            vim.command("mark p")
            end = vim.current.buffer.mark("p")

            # assume at least one blank
            # line is out there
            end = max(start[0],end[0]-2)

            # collapse block
            vim.command("%s,%sfold"%(start[0],end))

__end_of_python__


