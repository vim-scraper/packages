" Vim script to put thousand separator in between numbers
"	to make them more readable. E.g. 125668899 => 125 668 899
" Maintainer:	Lubomir Husar
" Last Change:	2009-01-26 15:43

"Define separator - space
let thousand_separator = ' '

"Define patterns - 1 number followed by 3 numbers followed by
"	either non-digit (\D), whitespace (\s) or end of the line ($)
let regexp = '\(\d\)\(\d\{3\}\)\(\D\|\s\|$\)'

"Repeat the loop until there is a match
while search(regexp)
		"Put thousand_separator in between first number and three numbers
		silent exec('%s/'.regexp.'/\1'.thousand_separator.'\2\3/g')
endwhile

"Inform the user
echo "Script has finished."
