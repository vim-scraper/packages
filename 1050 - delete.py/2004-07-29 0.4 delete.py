# -*- encoding: utf-8 -*-
# A Python script for the delete key
# by Alexander Rødseth, 29.07.04
# Use for whatever you like

import vim

class VimLine(list):

    def __init__(self, softtabstop=int(vim.eval("&sts"))):
        list.__init__(self, vim.current.line)
        self.sts = softtabstop
        self.linenr, self.pos = vim.current.window.cursor
        self.buf = vim.current.buffer
        self.deleted = False

    def on_space_indent(self):
        return self[self.pos:self.pos + self.sts].count(" ") == self.sts

    def delete_spaces(self):
        del self[self.pos:self.pos + self.sts]

    def delete_once(self):
        del self[self.pos]
        length = len(self)
        if self.pos == length:
            self.pos = length - 1

    def delete(self):
        del self.buf[self.linenr - 1]
        self.deleted = True
        self.pos = 0

    def __del__(self):
        if not self.deleted:
            vim.current.line = "".join(self)
        vim.current.window.cursor = (self.linenr, self.pos)

def main():
    line = VimLine()
    if line:
        if line.on_space_indent():
            line.delete_spaces()
        else:
            line.delete_once()
    else:
        line.delete()

if __name__ == "__main__":
    main()
