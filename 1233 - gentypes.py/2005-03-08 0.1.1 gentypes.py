#!/usr/bin/env python

import os, sys, string

def OutputTypes(files, opts='cnstug'):
    types_vim = open('types.vim', 'w')
    types_vim.write('syntax keyword Type ')
    # Build command line
    cmd = 'ctags --c-types=' + opts + ' '
    cmd += '-o- ' + string.join([f for f in files if f[:1] <> '-'])
    pipe = os.popen(cmd, 'r')
    lines = pipe.readlines()
    pipe.close()
    # Collect type names into dictionary so that there are no duplicates
    types = {}
    for line in lines:
        fields = string.split(line)
        if len(fields) > 0 and fields[0][0] <> '!':
            types[fields[0]] = 1
    types_vim.write(string.join(types.keys(), ' '))
    types_vim.close()

def GetIncludes(files):
    'Return list of headers included in files.'
    result = {}
    cmd = 'cpp -H ' + ' '.join(files) + ' 2>&1 >/dev/null'
    pipe = os.popen(cmd, 'r')
    lines = pipe.readlines()
    pipe.close()
    for line in lines:
        line = line.strip(' .\t\n')
        if line.endswith('.h') and os.access(line, os.R_OK):
            result[line] = 1
    return result.keys()

def CreateTypesVim(files):
    OutputTypes(GetIncludes(files) + files)

if __name__ == '__main__': CreateTypesVim(sys.argv[1:])

#vim: set et:
