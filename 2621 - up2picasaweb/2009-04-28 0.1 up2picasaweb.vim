" Filename: up2picasaweb.vim
" Author: tensecor <heibbai AT gmail.com>
" Date: April 28 , 2009
" Function: upload a image file form local to your picasaweb ablum 
" using the Picasa Web Albums Data API and return the url of your upoad
" file which can be embed in  html
" Usage: 
" you must have python and python-gdata installed
" put it into your ~/.vim/plugin dirctory "
" edit the setting section in line 33 to 35
" Command: 
" ":Up2Pic" use this command to upload you image
 
command! -nargs=* Up2Pic exec('py up2pciasa()')
 
function! GetInfo()
	let s:img_name = input("File: ")
	let s:img_title = input("Title: ")
	let s:img_summary = input("Summary: ")
endfunction
 
python << EOF
#!/usr/bin/env python
# -*- coding: UTF-8 -*-
 
import gdata.photos.service
import vim
import os
 
#####################
#      Settings     #
#####################
user_name = 'username'
password = 'password'
ablum_name = 'wordpress'
 
def up2pciasa():
	#get image file info
	vim.command("call GetInfo()")
	filename = vim.eval("s:img_name")
	title = vim.eval("s:img_title")
	summary = vim.eval("s:img_summary")
	if(os.path.exists(filename) == False):
		raise Exception,'file %s not found' %filename
	else:
		content_type = 'image/%s' %(filename.split('.')[-1])
		content_type = content_type.lower()
	#login in
	gd_client = gdata.photos.service.PhotosService()
	gd_client.email = '%s@gmail.com' %user_name
	gd_client.password = password
	gd_client.source = 'up2picasa.vim'
	gd_client.ProgrammaticLogin()
 
	#get ablum id with the specificed ablume name
	ablums = gd_client.GetUserFeed(kind='album',user=user_name)
	ablum_id = None
	for ablum in ablums.entry:
		if  ablum_name == ablum.title.text:
			ablum_id = ablum.gphoto_id.text
			break
	if ablum_id == None:
		raise Exception,"can't find ablum %s',ablumName"
 
	#upload photo
	album_url = '/data/feed/api/user/%s/albumid/%s' %(user_name,ablum_id)
	photo = gd_client.InsertPhotoSimple(album_url,title,summary,filename,content_type)
 
	#write the url of your upload photo
	vim.current.buffer.append(photo.content.src)
	#wirt the htlm to vim buffer
	#html ='<a href="%s"><img title="%s" style="border-width: 0px; display: inline;" alt="%s" src="%s" border="0"></a>' \
	#%(photo.content.src,title,summary,photo.content.src)
	#vim.current.buffer.append(html)
EOF
