" Function : signal_dec_VHDL()
" Author : Sunil Shukla
" Email: sunilkshukla@gmail.com
" Date : 15 March 2006
" 
" Description: This function automatically defines
" a signal into the signal declaration region between architecture
" declaration and BEGIN keyword
"
" Assumptions: 
" 1) In a VHDL file there is only one architecture
" 2) Just declares STD_LOGIC and STD_LOGIC_VECTOR, anyway other data types are
" strongly discouraged in RTL coding, so perhaps a positive point :-) and a
" point for future improvement for me
" 3) Not available for variable declaration, again its use is not a good practise 
" for RTL coding


map ,, : call signal_dec_VHDL()<CR>

function! signal_dec_VHDL()
				normal yiw
				normal ma " mark the current line with a
				let curr_pos = line(".")  
				" let curr_word= getreg('"0') 
				let curr_word= @0


				" checks for the BEGIN keyword			
				normal gg
				set ignorecase 
				let flags = "W"
				let check_for_validity = search("^[ ]*begin[ \t]*$", flags)	
				normal mb
				if check_for_validity == 0
							normal `a
							echo "This is not a valid VHDL file"
							return
				endif
				if curr_pos < check_for_validity 
								normal `a
								echo "Not a signal, probably a port"
								return  
				endif
				

				" checks whether the signal has been already defined 
				normal gg
				let flags = "W"
				while search("^[^--]*".expand(@0), flags) > 0
								if line(".") < check_for_validity
												normal `a
												echo "Signal already defined"
												return
								endif
								let flags = "W"
				endwhile



				let bit_width = input("Enter the length of signal : ")
				let bit_width_minus1 = bit_width - 1
				"let sig_type = input("Convention (v - std_logic_vector, s - signed, u - unsigned) : ")
        sig_type = 'v' ;
				normal 'b
				if bit_width == 1
								normal O
								exe "s/^/SIGNAL ".expand(@0)." : STD_LOGIC ;/"
								normal `a
								
				elseif bit_width > 1
								normal O
                if sig_type == 'v'
                        exe "s/^/SIGNAL ".expand(@0).": STD_LOGIC_VECTOR(".bit_width_minus1." DOWNTO 0) ;/"
                elseif sig_type == 's'
                        exe "s/^/SIGNAL ".expand(@0).": signed(".bit_width_minus1." DOWNTO 0) ;/"
                elseif sig_type == 'u'
                        exe "s/^/SIGNAL ".expand(@0).": unsigned(".bit_width_minus1." DOWNTO 0) ;/"
                else 
                        echo "wrong selection"
                endif
								normal `a
				else
								normal `a							
								echo "Signal NOT DEFINED"	
								return

				endif
				return "Done"
endfunction				
