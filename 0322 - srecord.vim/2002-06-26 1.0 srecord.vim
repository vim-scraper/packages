syn clear

" 
" Motorola S-Record Syntax highlighting file
"

" Match the Record type field
syn match s3 /^S3/ nextgroup=count

" Match the count field
syn match count /\x\x/ nextgroup=address

" Match the address field
syn match address /\x\{8\}/ nextgroup=body1

" Color every byte an alternate color for easy
" skim reading
syn match body1 /\x\{2}/ nextgroup=crc,body2
syn match body2 /\x\{2}/ nextgroup=crc,body1

" Match the CRC
syn match crc /\x\x\s*$/

hi count guifg=Grey guibg=Black
hi address guifg=Yellow guibg=Black
hi body1 guibg=LightGrey guifg=Black 
hi body2 guibg=DarkBlue guifg=yellow 
hi crc guifg=Orange
