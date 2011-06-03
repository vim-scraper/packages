"File:           python_showpydoc.vim
"Brief:          show document string of python3 or later in command line
"Authors:        VimPcDes <vimpcdes@163.com>
"Last Change:    8.17.2010 10:55
"Version:        1.0
"
"Usage:          commands:
"                 :Spydoc object
"                 the object can be an "imported" class, module or function(/method)
"                 the command return document string corresponding to the object.
"                 else return type of the object.
"                 if the object is "unimported" the command will fail except for
"                 modules re, vim and
"                 FunctionType, MethodType, ModuleType, BuiltinFunctionType,
"                 BuiltinMethodType imported from module types
"
"Install:        the plugin needs python3/dyn, make sure "filetype plugin on".
"                copy python_showpydoc.vim to your ftplugin directory
"
"Tip:            this plugin now is just designed for standart python3 library,
"                if try the command with a user-defined "object", 
"                be very likely to get an error "name 'object' is not defined".
"
"Examples:       lets say a simple python script:
"                import sys;
"                form sys import path;
"                class Test(object): pass;
"                t = Test();
"                and input following commands:
"
"                :Spydoc sys 
"                sys document(module)
"
"                :Spydoc sys.exit
"                sys.exit document(function)
"
"                :Spydoc sys.path
"                type of sys.path(built-in type list)
"
"                :Spydoc path
"                type of os.path(same as above)
"
"                :Spydoc Test:
"                error: name 'Test' is not define
"
"                :Spydoc t:
"                error: name 't' is not define
"
"                :Spydoc sys.ss
"                error: 'module' object has no attribute 'ss'
"
"                :Spydoc sys.exit()
"                error: 'sys.exit()' is a wrong?



if !has('python3')
    echo "Error: Required vim compiled with +python3/dyn"
    finish
endif

if exists("s:has_init")
    finish
endif
let s:has_init = 1

function! s:DefPython()
python3 << EOF

import re;
import vim;
from types import FunctionType;
from types import MethodType;
from types import ModuleType;
from types import BuiltinFunctionType;
from types import BuiltinMethodType;

class PythonDoc(object):

    class DocStringError(Exception):
        def __init__(self, value):
            self.value = value;
        def __str__(self):
            return repr(self.value);

    _re_find_import = r"\s+(from\s+[\w_\.]+[\s]*import\s+[\w_\.]+)|\s+(import\s+[\w_\.]+)";
    _AcpTypes = (FunctionType, MethodType, ModuleType, BuiltinFunctionType, BuiltinMethodType, type); 

    def __init__(self, code = ""):
        self._code = code;
        self._imports = [];
        self._docString = "";
        self._object = "";

    def SetCode(self, code):
        self._code = code;

    def ExtractImports(self):
        imports = re.findall(PythonDoc._re_find_import, self._code);
        for i in imports:
           self._imports += ["".join(i)];

    def GetDocString(self, objStr):
        self.ExtractImports();
        for i in self._imports:
            try: exec(i);
            except(ImportError): pass;

        try:
            obj = eval(objStr);
        except(NameError, AttributeError, SyntaxError, BaseException, Exception) as e:
            if(isinstance(e, (NameError, AttributeError, SyntaxError))):
                errMsg = str(e);
            else:
                errMsg = repr(objStr) + " its a wrong?";
            raise PythonDoc.DocStringError(errMsg);

        if(isinstance(obj, PythonDoc._AcpTypes)):
            self._docString += str(obj.__doc__);
        else:
            self._docString = objStr + " is an instance of " + str(type(obj));
        return self._docString;

def main(code, objStr):

    pd = PythonDoc(code);
    try:
        s = objStr + "->>>\n" + pd.GetDocString(objStr);
        print(s);
    except(PythonDoc.DocStringError) as e:
        print("error: " + e.value);

EOF
endfunction

function! s:CallPython(objStr)
py3 << EOF

objStr = vim.eval("a:objStr");
code = vim.current.buffer[:];
code = "\n".join(code);
main(code, objStr);

EOF
endfunction

call s:DefPython()
command! -nargs=1 Spydoc :call s:CallPython(<f-args>)
