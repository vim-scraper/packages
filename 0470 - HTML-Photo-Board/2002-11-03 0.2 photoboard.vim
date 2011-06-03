" Generate a HTML Thumbnail Photoboard using ImageMagic.
" Author: Michael Geddes <michaelrgeddes at optushome.com.au>
" Version: 0.2
" Feel free to copy and use this script as long as the original source is
" acknowledged.  I am always interested to see what people make of the
" scripts, and how they are improved, so please send me any
" changes/improvements you make, and I can make it part of the official
" script.

" The 'orig' parameter can either be a directory of jpg images, or specify a
" file.
" The file format is basicly a file-list, with special attributes (on a line
" by themselves).  In addition to the attributes, a file may have a rotation
" within exclamation marks (eg  Original/DCP00751.JPG!90! ).
" Blank lines ARE significant - and will leave a blank cell.
"
" .columns <n>          The number of columns in the table.
" .tableattrib          Attributes of the table (rules, border, bgcolor &c)
" .titlecolor <color>   The color of the title. (Any valid HTML color specifier)
" .imageheight <n>		Image max height
" .imagewidth  <n>      Image max width
" .thumbheight <n>		Thumbnail max height
" .thumbwidth  <n>      Thumbnail max width

" .title <text>         Insert a title row
" .alt <text>           Alternate text for the following image.

" Example:
" .columns 5
" .tableattrib  rules="groups" border=2 bgcolor="#c40000"
" .titlecolor White
" .title Here we are!
" .alt Michael giving Jared his first bath
" Original/DCP00709.JPG!90!

fun! s:Dlet(lhs,rhs)
  let lhs='g:'.a:lhs 
  if !exists(lhs) | exe 'let '.lhs.' = "'.escape(a:rhs,"\\\"").'"' | endif
endfun

" Set up defaults for maximum height-width (maintains aspect ration)
call s:Dlet('photoboard_thumb_height',120)
call s:Dlet('photoboard_thumb_width',120)
call s:Dlet('photoboard_image_height',860)
call s:Dlet('photoboard_image_width',860)

" Force Regenerate all image thumbnails.
" orig: original image directory, or txt configuration file
fun! GenerateDummyAlbum( orig, imagedir)
	call GeneratePhotoBoard( a:orig, a:imagedir, '_tn', 5,0,0,g:photoboard_thumb_height,g:photoboard_thumb_width)
endfun

" Generate/Regenerate the Album
" orig: original image directory, or txt configuration file
fun! GenerateAlbum( orig, imagedir)
	call GeneratePhotoBoard( a:orig, a:imagedir, '_tn', 5,g:photoboard_image_height,g:photoboard_image_width,g:photoboard_thumb_height,g:photoboard_thumb_width)
endfun

" Generate/Regenerate the Album with 
" orig: original image directory, or txt configuration file
fun! GeneratePhotoBoard( orig, imagedir, thumbnail, across, width,height, tnwidth, tnheight )

	" Try and find where to put it.
	let z=line('.')
	" By default, see if we are inside a definition already.
	let x=0
	if z < line('$')
		+
		let x=search( "<!-- Photoboard START -->", 'bW')
		let y=search( "<!-- Photoboard END -->", 'W')
		echo z.'-'.x.'-'.y
	endif
	if x==0 ||  z < x || z > y
		"Then check the cursor was within the body.
		1
		let x1=search( '\c<body', 'W')
		let y1=search( '\c</body', 'W')
		if z<=x1 || z >=y1
			if x==0 
				" If not, find a photoboard anywhere
				let x=search( "<!-- Photoboard START -->", 'W')
				let y=search( "<!-- Photoboard END -->", 'W')
			endif
			if x==0
				" put it at the top, or after the first heading.
				let z=x1
				exe x1
				let w=search('\c<h[1-9]')
				if w>0
					let z=w
				endif
			endif
		endif
	endif

	if x>0 && y>0
	  if x+1 < y
			" Delete the old photoboard.
		  exe (x+1).','.(y-1).' d'
		  exe x
	  endif
	else
	  exe z
	  put ='<!-- Photoboard START -->'
	  put ='<!-- Photoboard END -->'
	  exe (z+1)
	endif

	" Now construct the table
	put ='<table>'
	mark y
	put =''
	put ='</table>'
	-1
	mark z
	" Either read in the photoboard file, or get the directory.
	if filereadable( a:orig )
		if has('windows')
			exe "'y r! type ".a:orig
		else
			exe "'y r! cat ".a:orig
		endif
		let has_rc=1
	else
		if has('windows')
			exe "'y r!dir /b ".a:orig.'\*'
		else
			exe "'y r!ls ".a:orig.'/*'
		endif
		let has_rc=0
	endif
	" Set up defaults
	let tnwidth=a:tnwidth
	let tnheight=a:tnheight
	let width=a:width
	let height=a:height
	let across=a:across
	let tableattrib=''
	let titlecolor=''
	let alt=''

	" Iterate over the lines
	'y+1
	let files=(line("'z") - line("'y"))-1
	let h=0
	while line('.') < line("'z")
		" Check for special attribute lines
		let src=getline('.')
		if has_rc
		  if src=~'^\.tableattrib'
			  let tableattrib=matchstr(src,'^.tableattrib\s\+\zs.\{-}\ze\s*$')
			  del
			  continue
		  elseif src=~'^\.titlecolor '
			  let titlecolor=matchstr(src,'^.titlecolor\s\+\zs.\{-}\ze\s*$')
			  del
			  continue
		  elseif src=~'^\.imagewidth '
		      if width != 0 
				let width=matchstr(src,'^.imagewidth\s\+\zs.\{-}\ze\s*$')
			  endif
			  del
			  continue
		  elseif src=~'^\.imageheight '
			  let height=matchstr(src,'^.imageheight\s\+\zs.\{-}\ze\s*$')
			  del
			  continue
		  elseif src=~'^\.thumbheight '
			  let tnheight=matchstr(src,'^.thumbheight\s\+\zs.\{-}\ze\s*$')
			  del
			  continue
		  elseif src=~'^\.thumbwidth '
			  let tnwidth=matchstr(src,'^.thumbwidth\s\+\zs.\{-}\ze\s*$')
			  del
			  continue
		  elseif src=~'^\.columns '
			  let across=matchstr(src,'^.columns\s\+\zs.\{-}\ze\s*$')
			  del
			  continue
		  elseif src=~'^\.alt '
			  let alt=" alt='".matchstr(src,'^\.alt\s\+\zs.\{-}\ze\s*$')."' "
			  del
			  continue
		  elseif  src=~ '^\.title '
			  let src=substitute(src,'^.title \+','','')
			  let closers=''
			  if h != 0
				  while h % across > 0
					  let closers=closers.'<td>&nbsp;</td>'
					  let h=h+1
				  endwhile
				  let closers=closers.'</tr>'
				  let lastline=line('.')-1
				  call setline( lastline, getline(lastline).closers)
			  endif
			  if titlecolor != ''
				  let src='<font color="'.titlecolor.'">'.src.'</font>'
			  endif
			  call setline(line('.'), '<tr><td align="center" colspan='.across.'>'.src.'</td><tr>')
			  +
			  continue
		  endif
		endif

		" Check for a new table row.
		let prefix=''
		if (h % across)== 0
			if h != 0
				let lastline=line('.')-1
				call setline( lastline, getline(lastline).'</tr>')
			endif
			let prefix='<tr>'
		endif
		if src!~ '^\s*$'
			" Now do stuff to the image - get the image and thumbnail
			" filenames.
			let rotate=matchstr(src,'!\zs\d\+\ze!$')
			if rotate!= ''
			  let src=substitute(src, '!\d\+!$','','') 
			  let rotate=' -rotate '.rotate.' '
			endif
			let base=fnamemodify(src,':p:t')
			let imagefile=a:imagedir.'/'.base
			let tnimagefile=a:imagedir.'/'.fnamemodify(base,':r').a:thumbnail.'.'.fnamemodify(base,':e')
			" generate the images and thumbnails.
			if width != 0 && !filereadable(imagefile)
				" If the image doesn't exist, then create it
				let sizedesc=width.'x'.height
				echo 'resizing to '.sizedesc
				echo system('convert '.rotate.'-size '.sizedesc.' '.src.' -resize '.sizedesc.' '.imagefile)
			endif
			if (width ==0 || !filereadable(tnimagefile))
				" If the thumbnail doesn't exist, then create it.
				let sizedesc=tnwidth.'x'.tnheight
				echo 'resizing to '.sizedesc
				echo system('convert '.rotate.'-size '.sizedesc.' '.src.' -resize '.sizedesc.' '.tnimagefile)
			endif
			call setline( line('.'), prefix."<td align=\"center\"><a href='".imagefile."'><img ".alt."src=\"".tnimagefile."\" /></a></td>")
			let alt=''
		else
			call setline( line('.'), prefix."<td>&nbsp;</td>")
		endif
		+
		let h=h+1
	endwhile
	let closers=''
	if h != 0
	" Do the closers.
		while h % across > 0
			let closers=closers.'<td>&nbsp;</td>'
			let h=h+1
		endwhile
		let closers=closers.'</tr>'
		let lastline=line('.')-1
		call setline( lastline, getline(lastline).closers)
	endif
	" Reset the first table line with the table attributes.
	if tableattrib != '' 
		call setline(line("'y"), "<table ".tableattrib.">")
	endif

endfun


