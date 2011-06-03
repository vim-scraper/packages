" Vim omni completion
" Language:     R
" Maintainer:   Tyberius Prime <tyberius_prime@coonabibba.de>
" Last change 2009/05/25
" Released to the public domain. No warranties.
"
" Straight forward omni completion for R.
" It first suggest words in the current file (closests on top, before before
" after), then everything in the libraries (which are a one time call to R to
" find out. 
" Requires +python and an installed R
python <<EOF
import vim
import re
import subprocess
class Rcompletion:

    def __init__(self):
        self.lastLineNo = -1
        #regeexps to grab for R names in the current file
        self.parsingRe = re.compile("[A-Za-z_][A-Za-z0-9_.]*")
        #regeps to find library(...) calls
        self.libraryRegexps = re.compile('(library\()([A-Za-z0-9]+)(\))')
        self.packages = {}

    def reparse(self):
        """Parse the current file for names and library calls"""
        self.thisFile = []
        self.currentPackages = []
        currentLineNo, col = vim.current.window.cursor
        lines = vim.current.buffer
        #names
        for i in xrange(0, len(lines)):
            groups = self.parsingRe.findall(lines[i])
            if i < currentLineNo:
                groups = groups[::-1]
            for g in groups:
                if len(g) > 3:
                    self.thisFile.append((g, i))
        #library callso
        for g in self.libraryRegexps.findall("\n".join(lines)):
            libName = g[1]
            self.currentPackages.append(libName)

    def getPackageInfo(self, packageName):
        """Call R and retrieve meta information about packages, and store
        name, and a description for the menu in self.packages[packageName]
        Cached function."""
        if not packageName in self.packages: # caching
            identifiers = []
            p = subprocess.Popen(["R","--no-save"], stdin = subprocess.PIPE, stdout=subprocess.PIPE, stderr = subprocess.PIPE)
            stdout, stderr = p.communicate("""y <- .readRDS(file = system.file("Meta", "Rd.rds", package = "%s"))
            writeLines(paste(sep="\t", y["Name"][y["Type"] == ""], y["Title"][y["Type"] == ""]))
            quit()
            """ % packageName)#the filtering on type should remove most of the additional documentation in the meta data.
            stdout = stdout[stdout.find("writeLines("):] #cut to output
            if stderr != '':
                raise ValueError("R call: %s" % stderqr)
            identifiers = stdout.split("\n")[1:-2] #remove quit()\n\n at the end
            annotated = []
            for name, title in (x.split("\t") for x in identifiers):
                annotated.append({'word': name, 'menu': packageName + "; " + title})
            self.packages[packageName] = annotated
        return self.packages[packageName]
      
        
    
    def findWordToComplete(self):
        """Look at the current line and decide what word we need to complete"""
        line = vim.current.line
        lineno, col = vim.current.window.cursor
        if lineno != self.lastLineNo:
            self.reparse()
        groups = re.finditer('[^A-Za-z0-9._]', line[:col]) #we could of course also find the last valid identifier instead...
        last = None
        for g in groups:
            last = g
        if last is None:
            return 0
        else:
            return last.start(0) + 1

    def distanceToCurrentLine(self, lineNo):
        """Generator for comparison function that considers the distance to
        current line"""
        def diff(a, b):
            diffA = abs(a[1] - lineNo)
            diffB = abs(b[1] - lineNo)
            if diffA == diffB:
                return a[1] - b[1] #text before before text after
            else:
                return diffA - diffB
        return diff

    def completeWord(self, word):
        """offer suggestions for the word in word"""
        words = []
        lineno, col = vim.current.window.cursor
        word = word.lower()
        wordsAndLines= []
        #first for the ones in this file
        for aword, line in self.thisFile:
            if aword.lower().startswith(word):
                wordsAndLines.append((aword,line))
        wordsAndLines.sort(self.distanceToCurrentLine(lineno - 1))
        for aword,  line in wordsAndLines:
            words.append(aword)
        #now for the libraries
        for aLib in self.currentPackages:
            for aword in self.getPackageInfo(aLib):
                if aword['word'].lower().startswith(word):
                    words.append(aword)
                if vim.eval("complete_check()") != "0":
                    return
        for w in words:
            if isinstance(w, dict):
                vim.command("call complete_add({'word': '%s', 'menu': '%s'})" % (w['word'],w['menu'].replace("'","`")))
            else:
                vim.command("call complete_add('%s')" % w)
"""
Quick tryout arena
library(affy)
library(limma)
c
a
lambda
lambda
lumbda
lisha
"""

rcomp = Rcompletion()
EOF


fun! CompleteR(findstart, base)
    python <<EOF
findstart = vim.eval("a:findstart")
base = vim.eval("a:base")
if findstart == "1":
    vim.command("return %i " % rcomp.findWordToComplete())
else:
    rcomp.completeWord(base)
EOF
endfun

setlocal omnifunc=CompleteR
