syn clear

" 
" Motorola S-Record Syntax highlighting file
"
" Written By Brad Phelan kotku@rocketmail.com 2002
"
" Version 2.0
"

" Match the Record type field
syn match s0 /^S0/ nextgroup=count0

syn match s1 /^S1/ nextgroup=count1
syn match s2 /^S2/ nextgroup=count2
syn match s3 /^S3/ nextgroup=count3

syn match s7 /^S7/ nextgroup=count7
syn match s8 /^S7/ nextgroup=count8
syn match s9 /^S7/ nextgroup=count9

" -----------------s0 -------------------------
" Match the count field
syn match count0 /\x\x/ nextgroup=address0
" Match the address field
syn match address0 /\x\{4\}/ nextgroup=body1
" ---------------- s1 --------------------------
" Match the count field
syn match count1 /\x\x/ nextgroup=address1
" Match the address field
syn match address1 /\x\{4\}/ nextgroup=body1
" ---------------- s2 --------------------------
" Match the count field
syn match count2 /\x\x/ nextgroup=address2
" Match the address field
syn match address2 /\x\{6\}/ nextgroup=body1
" ---------------- s3 --------------------------
" Match the count field
syn match count3 /\x\x/ nextgroup=address3
" Match the address field
syn match address3 /\x\{8\}/ nextgroup=body1
" ---------------- s7 --------------------------
" Match the count field
syn match count7 /\x\x/ nextgroup=address7
" Match the address field
syn match address7 /\x\{8\}/ nextgroup=crc
" ---------------- s8 --------------------------
" Match the count field
syn match count8 /\x\x/ nextgroup=address8
" Match the address field
syn match address8 /\x\{8\}/ nextgroup=crc
" ---------------- s9 --------------------------
" Match the count field
syn match count9 /\x\x/ nextgroup=address9
" Match the address field
syn match address9 /\x\{8\}/ nextgroup=crc

" Color every byte an alternate color for easy
" skim reading
syn match body1 /\x\{2}/ nextgroup=crc,body2
syn match body2 /\x\{2}/ nextgroup=crc,body1

" Match the CRC
syn match crc /\x\x\s*$/

hi count0 guifg=Grey guibg=Black
hi address0 guifg=Yellow guibg=Black
hi count1 guifg=Grey guibg=Black
hi address1 guifg=Yellow guibg=Black
hi count2 guifg=Grey guibg=Black
hi address2 guifg=Yellow guibg=Black
hi count3 guifg=Grey guibg=Black
hi address3 guifg=Yellow guibg=Black

hi count7 guifg=Grey guibg=Black
hi address7 guifg=Yellow guibg=Black
hi count8 guifg=Grey guibg=Black
hi address8 guifg=Yellow guibg=Black
hi count9 guifg=Grey guibg=Black
hi address9 guifg=Yellow guibg=Black

hi body1 guibg=LightGrey guifg=Black 
hi body2 guibg=DarkBlue guifg=yellow 
hi crc guifg=Orange
