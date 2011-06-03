
syn match sifHeader  +^\[.*\]$+
syn match sifComment +^;.*$+
hi link sifComment Comment
hi link sifHeader  Identifier
