" LoadHeaderFile
" Last Change: 07-05-2001
" Maintainer: Garner Halloran (garner@havoc.gtf.org)

if exists( "loaded_LoadHeaderFile" )
	finish
endif
let loaded_LoadHeaderFile = 1

fun! LoadHeaderFile( arg, loadSource )
	if match( a:arg, "#include" ) >= 0
		" find either a starting < or "
		let start = match( a:arg, "<\\|\"" )

		if start < 0
			return
		endif

		let start = start + 1

		let $filename = strpart( a:arg, start )

		" find either an ending > or "
		let end = match( $filename, ">\\|\"" )

		if end > 0
			" get the final filename to open
			let $filename = strpart( $filename, 0, end )

			" if loadSource is 1, then replace .h with .cpp and load that file instead
			if a:loadSource == 1
				let $filename = substitute( $filename, "\\V.h", ".cpp", "" )
			" if loadSource is 2, then replace .h with .c and load that file instead
			elseif a:loadSource == 2
				let $filename = substitute( $filename, "\\V.h", ".c", "" )
			endif

			sfind $filename
			return
		endif
	endif
endfun
