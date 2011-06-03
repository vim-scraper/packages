fun! LoadHeaderFile( arg, loadCPP )
	if match( a:arg, "^#include" ) >= 0
		let start = match( a:arg, "<" )

		if start < 0
			let start = match( a:arg, "\"" )
		endif

		if start < 0
			return
		endif

		let start = start + 1
		let end = match( a:arg, "\\V.h" )
		if end >= 0
			let end = end - start + 2

			let $filename = strpart( a:arg, start, end )

			if a:loadCPP == 1
				let $filename = substitute( $filename, "\\V.h", ".cpp", "" )
			endif

			sfind $filename
			return
		endif
	endif
endfun
