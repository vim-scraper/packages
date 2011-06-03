" This program is free software; you can redistribute it and/or modify
" it under the terms of the GNU General Public License as published by
" the Free Software Foundation; either version 2, or (at your option)
" any later version.
"
" This program is distributed in the hope that it will be useful,
" but WITHOUT ANY WARRANTY; without even the implied warranty of
" MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
" GNU General Public License for more details.
"
" You should have received a copy of the GNU General Public License
" along with this program; if not, write to the Free Software Foundation,
" Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  
" 
" BlogDel, revised BlogList via http://wiki.yepn.net/vimpress
"
" Original Author: Adrien Friggeri <adrien@friggeri.net>
" Maintainer:	   Josh Kenzer <jkenzer@radicalbehavior.com>
" URL:		   http://www.radicalbehavior.net/projets/vimblog/
" Version:	   1.2
" Last Change:     2011 March 2
"
" Commands :
" ":BlogList [count]"
"   Lists the last x articles in the blog - if left blank 10 are returned
" ":BlogNew"
"   Opens page to write new article
" ":BlogOpen <id>"
"   Opens the article <id> for edition
" ":BlogDel <id>" 
"   Del the article <id>
" ":BlogSend"
"   Saves the article to the blog
" ":BlogUp <image name>"
"   Uploads an image to the blog and inserts the image url. It will use the
"   img_template var to fill in a whole img tag if wanted. Use %s where you'd
"   like the actual url to be.
"
" Configuration : 
"   Edit the "Settings" section (starts at line 51).
"
"   If you wish to use UTW tags, you should install the following plugin : 
"   http://blog.circlesixdesign.com/download/utw-rpc-autotag/
"   and set "enable_tags" to 1 on line 50
"
" Usage : 
"   Just fill in the blanks, do not modify the highlighted parts and everything
"   should be ok.

command! -nargs=* BlogList exec("py blog_list_posts(<args>)")
command! -nargs=0 BlogNew exec("py blog_new_post()")
command! -nargs=0 BlogSend exec("py blog_send_post()")
command! -nargs=1 BlogOpen exec('py blog_open_post(<f-args>)')
command! -nargs=1 BlogUp exec('py blog_upload_img(<f-args>)')
command! -nargs=1 BlogDel exec('py blog_del_post(<f-args>)')

python <<EOF
# -*- coding: utf-8 -*-
import urllib , urllib2 , vim , xml.dom.minidom , xmlrpclib , sys , string , re

#####################
#      Settings     #
#####################

enable_tags = 1
blog_username = 'username'
blog_password = 'password'
blog_url = 'http://blog.url.com/'  
blog_api = 'http://blog.url.com/xmlrpc.php'  
img_dir = '/path/to/images/'

#Remove this line if you wish to just have the url inserted
img_template = '<img src="%s" width="500" height="" border=0 style="margin-bottom:5px;" />'

#####################
# Do not edit below #
#####################

handler = xmlrpclib.ServerProxy(blog_api).metaWeblog
edit = 1

def blog_edit_off():
  global edit
  if edit:
    edit = 0
    for i in ["i","a","s","o","I","A","S","O"]:
      vim.command('map '+i+' <nop>')

def blog_edit_on():
  global edit
  if not edit:
    edit = 1
    for i in ["i","a","s","o","I","A","S","O"]:
      vim.command('unmap '+i)

def blog_send_post():
  def get_line(what):
    start = 0
    while not vim.current.buffer[start].startswith('"'+what):
      start +=1
    return start
  def get_meta(what): 
    start = get_line(what)
    end = start + 1
    while not vim.current.buffer[end][0] == '"':
      end +=1
    return " ".join(vim.current.buffer[start:end]).split(":")[1].strip()
      
  strid = get_meta("StrID")
  title = get_meta("Title")
  post_status = get_meta("Status")
  cats = [i.strip() for i in get_meta("Cats").split(",")]
  if enable_tags:
    tags = get_meta("Tags")
  
  text_start = 0
  while not vim.current.buffer[text_start] == "\"========== Content ==========":
    text_start +=1
  text_start +=1
  text = '\n'.join(vim.current.buffer[text_start:])

  content = text

  if enable_tags:
    post = {
      'title': title,
      'description': content,
      'categories': cats,
      'post_status': post_status,
      'mt_keywords': tags
    }
  else:
    post = {
      'title': title,
      'description': content,
      'categories': cats,
    }

  if strid == '':
    strid = handler.newPost('', blog_username,
      blog_password, post, 1)

    vim.current.buffer[get_line("StrID")] = "\"StrID : "+strid
    vim.current.buffer[get_line("Status")] = "\"Status : draft"
    vim.current.buffer[get_line("Preview")] = "\"Preview: "+blog_url+"?p="+strid+"&preview=true"
  else:
    handler.editPost(strid, blog_username,
      blog_password, post, 1)

  vim.command('set nomodified')


def blog_new_post():
  def blog_get_cats():
    l = handler.getCategories('', blog_username, blog_password)
    s = ""
    for i in l:
      s = s + (i["description"].encode("utf-8"))+", "
    if s != "": 
      return s[:-2]
    else:
      return s
  del vim.current.buffer[:]
  blog_edit_on()
  vim.command("set syntax=blogsyntax.html")
  vim.current.buffer[0] =   "\"=========== Meta ============\n"
  vim.current.buffer.append("\"StrID : ")
  vim.current.buffer.append("\"Title : ")
  vim.current.buffer.append("\"Cats  : "+blog_get_cats())
  vim.current.buffer.append("\"Status: draft")
  vim.current.buffer.append("\"Preview: ")
  if enable_tags:
    vim.current.buffer.append("\"Tags  : ")
  vim.current.buffer.append("\"========== Content ==========\n")
  vim.current.buffer.append("\n")
  vim.current.window.cursor = (len(vim.current.buffer), 0)
  vim.command('set nomodified')
  vim.command('set textwidth=0')

def blog_open_post(id):
  try:
    post = handler.getPost(id, blog_username, blog_password)
    blog_edit_on()
    vim.command("set syntax=blogsyntax")
    del vim.current.buffer[:]
    vim.current.buffer[0] =   "\"=========== Meta ============\n"
    vim.current.buffer.append("\"StrID : "+str(id))
    vim.current.buffer.append("\"Title : "+(post["title"]).encode("utf-8"))
    vim.current.buffer.append("\"Cats  : "+",".join(post["categories"]).encode("utf-8"))
    vim.current.buffer.append("\"Status: "+(post["post_status"]).encode("utf-8"))
    vim.current.buffer.append("\"Preview: "+blog_url+"?p="+str(id)+"&preview=true")
    if enable_tags:
      vim.current.buffer.append("\"Tags  : "+(post["mt_keywords"]).encode("utf-8"))
    vim.current.buffer.append("\"========== Content ==========\n")
    content = (post["description"]).encode("utf-8")
    for line in content.split('\n'):
      vim.current.buffer.append(line)
    text_start = 0
    while not vim.current.buffer[text_start] == "\"========== Content ==========":
      text_start +=1
    text_start +=1
    vim.current.window.cursor = (text_start+1, 0)
    vim.command('set nomodified')
    vim.command('set textwidth=0')
  except:
    sys.stderr.write("An error has occured")

def blog_list_edit():
  try:
    row,col = vim.current.window.cursor
    id = vim.current.buffer[row-1].split()[0]
    blog_open_post(int(id))
  except:
    pass

def blog_list_posts(count=10): 
  try: 
    allposts = handler.getRecentPosts('',blog_username, blog_password, count) 
    del vim.current.buffer[:] 
    vim.command("set syntax=blogsyntax") 
    vim.current.buffer[0] = "\"====== List of Posts =========" 
    for p in allposts: 
      vim.current.buffer.append(("%-7s\t" % p["postid"]) + (p["title"]).encode("utf-8")) 
      vim.command('set nomodified') 
    blog_edit_off() 
    vim.current.window.cursor = (2, 0) 
    vim.command('map <enter> :py blog_list_edit()<cr>') 
  except: 
    sys.stderr.write("An error has occured") 

def blog_del_post(id): 
  try: 
    handler.deletePost('',id, blog_username, blog_password) 
    allposts = handler.getRecentPosts('',blog_username, blog_password, 10) 
    del vim.current.buffer[:] 
    vim.command("set syntax=blogsyntax") 
    vim.current.buffer[0] = "\"====== New List of Posts =========" 
    for p in allposts: 
      vim.current.buffer.append(("%-7s\t" % p["postid"]) + (p["title"]).encode("utf-8")) 
      vim.command('set nomodified') 
    blog_edit_off() 
    vim.current.window.cursor = (2, 0) 
    vim.command('map <enter> :py blog_list_edit()<cr>') 
  except: 
    pass 
    
def blog_upload_img(filename):
  try:
    #filename = vim.eval(filename)
    if(os.path.exists(img_dir+filename) == False):
      raise FileError('file not found'+filename)
    else:
      content_type = 'image/%s' %(filename.split('.')[-1])
      if content_type == 'image/jpg':
        content_type = 'image/jpeg'
        content_type = content_type.lower()

    #upload
    newFile = handler.newMediaObject('', blog_username, blog_password,{'name': filename, 'type': content_type, 'bits': xmlrpclib.Binary(open(img_dir+filename).read())})

    #write the url of your upload photo
    #vim.command("normal i"+newFile['url'])
    if(img_template):
      vim.command("normal i"+re.sub(r'%s', newFile['url'], img_template))
    else:
      vim.command("normal i"+newFile['url'])
  except:
    sys.stderr.write("An error has occured")
