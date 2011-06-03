" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
doc/vimblogger_ft.txt	[[[1
229
*vimblogger_ft.txt*   reStructuredText to Blogger Interface
                    Author: Roman Dobosz, gryf73 at gmail com

Simple interface to create blog articles in rsST format. It provides
commands for preview in browser, post and delete articles.

-----------------------------------------------------------------------
Requirements~

Module for communication was written in Python. So, VIm has to be
compiled with +python.

Other requirements:

- Python (tested with version 2.6, should work also in others)
  - gdata http://code.google.com/p/gdata-python-client
  - docutils http://docutils.sourceforge.net
  - pygments http://pygments.org (optional)
- Blogger account

-----------------------------------------------------------------------
Install~

Edit the vba file and type: >

    :so %

========================================================================
Usage~

This plugin is targeting for people, who has blogger account, want to 
use VIm for creating blog articles and don't really likes to manually do 
this in html.

Before starting writing a post, at least |g:blogger_name| and
|g:blogger_login| has to be set up in |.vimrc|. Next, an article has to
be written using standard reST markup, |:Title:| added (not required,
but it's nice to have some title for a blog entry). Now,
|:PreviewBlogArticle| can be used for saving generated HTML page into
the file of the same name as reST file. Please note, that it'll silently
overwrite existing file, because it is treated as a temporary file.

When article is done, |:SendBlogArticle| will send it to the server.

Output provided by |:PreviewBlogArticle| without any
css stylesheet will look pretty raw, so it is generally good idea to
grab stylesheets from blog itself, and tweak it a little, and add to
list in |g:blogger_stylesheets|. They will be automatically linked to 
generated preview file.

Unfortunately, this script has several limitations, like it is
impossible to use multiple blogs or edit existing articles without reST
source files. It has to be somehow converted to reStructuredText, id of
an article added to |:Id:| docinfo item and then updated. Id of an
article is available through blogger account - every action for each
post listed on Posting->Edit Posts has URL with query string item
postID, for example:

>
    http://www.blogger.com/post-edit.g?blogID=9876&postID=12345
<

-----------------------------------------------------------------------
Options~
                                                     *g:blogger_browser*
g:blogger_browser       (default: 0)

    If set to 1 output file from :PreviewBlogArticle will be opened in 
    browser (used webbrowser Python module)

                                                        *g:blogger_name*
g:blogger_name          (default: "")
    
    This is blog name, which is part of the URL, and user was obligated 
    to enter it during blog creation. If in doubt, check first part od 
    the URL of the blog, just after 'http://'. Note, that blog name may
    differ from the blog title, but also could be the same. 

                                                       *g:blogger_login*
g:blogger_login         (default: "")

    Google login name, usually gmail address.
                                                        *g:blogger_pass*
g:blogger_pass          (default: "")

    Password. If set to empty string, user will be asked for it every 
    time if any blogger connectivity is performed.
                                                       *g:blogger_draft*
g:blogger_draft         (default: 1)

    By default, don't publish articles immediately, just save it on the 
    service. If set to 0, article will be published.

                                                 *g:blogger_maxarticles*
g:blogger_maxarticles   (default: 0)
    
    Number of displayed articles during deletion. 0 means all. Any 
    positive number will display only that numbers of articles on list.

                                                 *g:blogger_confirm_del*
g:blogger_confirm_del   (default: 1)
    
    Confirm every deletion. If set to 0, suppress the confirmation.

                                                 *g:blogger_stylesheets*
g:blogger_stylesheets   (default: [])

    List of relative paths (relative to generated HTML document) of CSS 
    stylesheets, used only for article preview in HTML document. Usually 
    one wanted to save stylesheets from his own blog, so that article 
    can be displayed almost in the same way as in blog.

========================================================================
Commands~

*:PreviewBlogArticle*

    Generate article in HTML format, save it to the file with te same 
    name as a reST source with .html extension in the same directory, 
    and optionally opens it in browser. No connection to the blogger is 
    performed.

*:SendBlogArticle*

    Generate partial HTML document, which holds article, from current
    reST buffer and send it to the blog.

    See reST document structure below for further description.

*:DeleteBlogArticle*

    Display list of articles, and lets user choose one (or none) of them 
    to perform deletions.

========================================================================
reST document structure~

It is assumed, that following template will be used:

-----8<-----
:Id:
:Title: Title for the blog
:Date:
:Modified:
:Tags: some, tags

Penatibus et magnis dis parturient montes, nascetur ridiculus mus. Nulla
facilisis massa ut massa. Sed nisi purus, malesuada eu, porta vulputate,
suscipit auctor, nunc. Vestibulum convallis, augue eu luctus malesuada,
mi ante mattis odio, ac venenatis neque sem vitae nisi.

.. more


heading
-------

**Congue** mi, quis posuere augue nulla a augue. Pellentesque sed est. 
Mauris cursus urna id lectus. Integer dignissim feugiat eros. Sed tempor 
volutpat dolor. Vestibulum vel lectus nec mauris semper adipiscing.

Aliquam tincidunt enim sit amet tellus. Sed mauris nulla, semper 
tincidunt, luctus a, sodales eget, leo. Sed ligula augue, cursus et. 
----->8-----

reST document (optionally) starts with *docinfo* section (first several 
lines, that are starting from ":" character) separaded from other 
content with one empty line.

Docinfo items holds article attributes, and are updated automatically 
every each of upload to blogger, which is triggered by 
":SendBlogArticle" command.

*:Id:*
    Holds article id on blogger side. If not defined, new article will 
    be created (even if there is already existing one with the very same 
    content). If wrong Id is entered (or an Id of deleted article), 
    exception will be raised, and no action on blogger side will be 
    performed.

*:Title:*
    Holds article title. Can be changed when |:Id:| is obtained.

*:Date:*
    This is published date in RFC 3339 
    http://www.ietf.org/rfc/rfc3339.txt format. If empty on first 
    upload, it will be set to current date. Can be set/changed to 
    desired date.

*:Modified:*
    This is read-only item, which store modification date which happens 
    on blogger side.

*:Tags:*
    Comma separated list of tags (Labels). Can be empty.

All other items are ignored.

After docinfo block, article body should be placed using markup for 
reStructuredText.

Note, that `.. more' will became HTML comment `<!-- more -->' which will 
prevent from displaying entire post on the bloggers front page, but will 
not have any visible effect during preview in browser.

Additionally, if pygments is installed, there is sourcecode directive,
simple syntax highlighter using Pygments module. Very simple usage could
be as follows:

-----8<-----
.. sourcecode:: python

    import vim
    print vim.current.buffer.name

----->8-----

Note: All headings for generated HTML by |:SendBlogArticle| will be 
shifted by 3, so the first heading will become <h3>, second <h4> and so 
on, to fit into blogger template (well, most of them). Remember, that 
HTML allow up to 6 level of headings, while reST doesn't have this 
limitation. 

========================================================================
Changelog~

0.1 First release

vim:tw=72:fo=tcq2:isk=!-~,^*,^|,^":ts=8:ft=help:norl:
ftplugin/rst/vimblogger_ft.vim	[[[1
72
" reST to blogger vim interface.
" Provide some convinient commands for creating preview from the reST file 
" and to send articles to blog.

if exists("b:did_rst_plugin")
    finish " load only once
else
    let b:did_blogger_plugin = 1
endif

if exists(':PreviewBlogArticle')
    finish
endif

if !exists("g:blogger_browser")
    let g:blogger_browser = 0
endif

if !exists("g:blogger_name")
    let g:blogger_name = ""
endif

if !exists("g:blogger_login")
    let g:blogger_login= ""
endif

if !exists("g:blogger_pass")
    let g:blogger_pass = ""
endif

if !exists("g:blogger_draft")
    let g:blogger_draft = 1
endif

if !exists("g:blogger_maxarticles")
    let g:blogger_maxarticles = 0
endif

if !exists("g:blogger_confirm_del")
    let g:blogger_confirm_del = 1
endif

if !exists("g:blogger_stylesheets")
    let g:blogger_stylesheets = []
endif

python << EOF
import os
import sys

import vim

scriptdir = os.path.dirname(vim.eval('expand("<sfile>")'))
sys.path.insert(0, scriptdir)

try:
    from rst2blogger.main import Rst2Blogger
except ImportError:
    print "Plugin vimblogger cannot be loaded, due to lack of required modules"
EOF

if !exists(":PreviewBlogArticle")
    command PreviewBlogArticle py print Rst2Blogger().preview()
endif

if !exists(":SendBlogArticle")
    command SendBlogArticle py print Rst2Blogger().post()
endif

if !exists(":DeleteBlogArticle")
    command DeleteBlogArticle py print Rst2Blogger().delete()
endif
ftplugin/rst/rst2blogger/blogger.py	[[[1
214
"""
File: blogger.py
Author: Roman 'gryf' Dobosz
Description: This is blogger activity connected module. It is using gdata[1]
             blogger module to provide add/modify/delete articles interface.

             [1] http://code.google.com/p/gdata-python-client
"""

import datetime
import re

import atom
from gdata.blogger.client import BloggerClient, BLOG_POST_URL
from gdata.blogger.data import BlogPost


class VimBlogger(object):
    """
    Communicate with blogger through gdata.blogger modules
    """
    DATE_PATTERN = re.compile(r"^(\d{4}-\d{2}-\d{2})"
                              "T(\d{2}:\d{2}:\d{2})(\.\d{3})?[+-]"
                              "(\d{2}:\d{2})$")
    DATE_FORMAT = "%Y-%m-%d"
    TIME_FORMAT = "%H:%M:%S"
    TZ_FORMAT = "%H:%M"

    def __init__(self, blogname, login, password):
        """
        Initialization.
        """
        self.draft = True
        self.blog_id = None
        self.blog = None
        self.client = BloggerClient()
        self._authorize(login, password)

        self.feed = self.client.get_blogs()
        self._set_blog(blogname)

    def get_articles(self, maxarticles=0):
        """
        Return list of articles
        """
        feed = self.client.get_posts(self.blog_id)
        posts = []

        for index, entry in enumerate(feed.entry):
            if maxarticles and index >= maxarticles:
                break
            posts.append((entry.get_post_id(),
                          entry.title.text,
                          self._extract_date(entry.published.text)))
        return posts

    def create_article(self, html_doc, attrs=None):
        """
        Create new article
        html_doc is content of the article in HTML format, without headers,
        preamble, doctype and body tags.
        attrs is a dictionary that should hold title, date and tags.
        return BlogPost object
        """
        if not attrs:
            attrs = {}

        title = 'title' in attrs and attrs['title'] or ""
        title = atom.data.Title(text=title, type="text")
        html_doc = atom.data.Content(text=html_doc, type="html")

        new_post = BlogPost(title=title, content=html_doc)

        if 'tags' in attrs and attrs['tags']:
            for tag in attrs['tags'].split(','):
                new_post.add_label(tag.strip())

        if 'date' in attrs and attrs['date'] and \
                self._check_date(attrs['date']):
            new_post.published = atom.data.Published(text=attrs['date'])

        if self.draft:
            new_post.control = atom.data.Control(\
                  draft=atom.data.Draft(text='yes'))

        return self.client.post(new_post, BLOG_POST_URL % self.blog_id)

    def update_article(self, html_doc, attrs):
        """
        Update article.
        html_doc is content of the article in HTML format, without headers,
        preamble, doctype and body tags.
        attrs is a dictionary that should hold title, date and tags.
        return BlogPost object
        """
        if "id" not in attrs:
            raise Exception("Post Id not found in attributes!")

        post = self._get_post(attrs['id'])
        post.content = atom.data.Content(text=html_doc, type="html")

        # update published date
        if 'date' in attrs and attrs['date'] and \
                self._check_date(attrs['date']):
            post.published = atom.data.Published(text=attrs['date'])

        if 'title' in attrs and attrs['title']:
            post.title = atom.data.Title(text=attrs['title'], type="text")
        #
        # update tag list
        if 'tags' in attrs:
            tags = [tag.strip() for tag in attrs['tags'].split(',')]
            for index, label in enumerate(post.category):
                if label.term not in tags:
                    del(post.category[index])

            for tag in tags:
                self._add_tag(post, tag.strip())

        return self.client.update(post)

    def delete_article(self, post_id):
        """
        Delete selected article
        """
        if not post_id:
            return "No article id provided"

        post = self._get_post(post_id)
        self.client.delete(post)
        return None

    def _get_post(self, post_id):
        """
        Return post with specified ID
        """
        post_href = self.blog.get_post_link().href
        return self.client.get_feed(post_href + "/%s" % post_id,
                                    desired_class=BlogPost)

    def _add_tag(self, post, tag):
        """
        post - BlogPost object
        tag - string with tag/label to add
        """
        for label in post.category:
            if label.term == tag:
                return

        post.add_label(tag)

    def _extract_date(self, date_string, time=False):
        """
        Extract date from the string and optionally time
        """

        if not self.DATE_PATTERN.match(date_string):
            return False

        if not time:
            return self.DATE_PATTERN.match(date_string).groups()[0]

        groups = self.DATE_PATTERN.match(date_string).groups()
        return groups[0] + " " + groups[1]

    def _check_date(self, date):
        """
        Parse date as RFC 3339 format, for example:
            2010-11-30T21:06:48.678+01:00
            or
            2010-11-30T21:06:48+01:00

        Returns true, if date is acceptable, false otherwise
        """
        if not self.DATE_PATTERN.match(date):
            return False

        groups = self.DATE_PATTERN.match(date).groups()
        _date = groups[0]
        _time = groups[1]
        _tz = len(groups) == 3 and groups[2] or groups[3]

        try:
            datetime.datetime.strptime(_date, self.DATE_FORMAT)
            datetime.datetime.strptime(_time, self.TIME_FORMAT)
            datetime.datetime.strptime(_tz, self.TZ_FORMAT)
        except ValueError:
            return False

        return True

    def _authorize(self, login, password):
        """
        Try to authorize in Google service.
        Authorization is kept in client object. In case of wrong credentials,
        exception is thrown.
        """
        source = 'Vim rst2blogger interface'
        service = 'blogger'

        self.client.client_login(login,
                                 password,
                                 source=source,
                                 service=service)

    def _set_blog(self, blogname):
        """
        Set correct blog, as defined in blogname
        """
        for blog in self.feed.entry:
            if blog.get_blog_name() == blogname:
                self.blog_id = blog.get_blog_id()
                self.blog = blog
                break
ftplugin/rst/rst2blogger/__init__.py	[[[1
1
# module rst2blogger
ftplugin/rst/rst2blogger/main.py	[[[1
204
# vim: fileencoding=utf8
"""
File: main.py
Author: Roman 'gryf' Dobosz
Description: main file to provide fuctionality between vim and moudles rest
             and blogger
"""

import webbrowser
from xml.dom import minidom
from xml.parsers.expat import ExpatError

import vim

from rst2blogger.rest import blogPreview, blogArticleString
from rst2blogger.blogger import VimBlogger


class Rst2Blogger(object):
    """
    Provide convenient way to communicate between vim and blogger through reST
    """
    def __init__(self):
        vim.command('call setqflist([])')

        self.buff = vim.current.buffer
        self.docinfo_len = 0
        self._set_docinfo_len()
        self.login = vim.eval("g:blogger_login")
        self.password = vim.eval("g:blogger_pass")
        self.blogname = vim.eval("g:blogger_name")
        self.buffer_encoding = vim.eval("&fileencoding")
        self.vim_encoding = vim.eval("&encoding")
        self.draft = int(vim.eval("g:blogger_draft"))
        self.maxarticles = int(vim.eval("g:blogger_maxarticles"))
        self.confirm_del = int(vim.eval("g:blogger_confirm_del"))
        self.stylesheets = vim.eval("g:blogger_stylesheets")

    def preview(self):
        """
        Generate HTML Blogger article preview and (optionally) display it in
        systems' web browser
        """
        bufcontent = "\n".join(self.buff)
        name = self.buff.name

        name = name[:-4] + ".html"
        html = blogPreview(bufcontent, self.stylesheets)
        self._open_qf(self._check_html(html))

        output_file = open(name, "w")
        output_file.write(html)
        output_file.close()
        if vim.eval("g:blogger_browser"):
            webbrowser.open(name)
            return "Generated HTML has been opened in browser"
        else:
            return "Generated HTML has been written to %s" % name

    def post(self):
        """
        Do post article
        """
        bufcontent = "\n".join(self.buff)
        html, attrs = blogArticleString(bufcontent)

        parse_msg = self._check_html(html, True)
        if parse_msg:
            self._open_qf(parse_msg)
            return "There are errors in generated document"

        if not self.password:
            self.password = \
                    vim.eval('inputsecret("Enter your gmail password: ")')

        blog = VimBlogger(self.blogname, self.login, self.password)
        blog.draft = self.draft > 0

        if 'id' in attrs and attrs['id']:
            post = blog.update_article(html, attrs=attrs)
            msg = unicode("Article '%s' has been updated" % post.title.text)
            msg = msg.encode(self.vim_encoding)
        else:
            post = blog.create_article(html, attrs=attrs)
            msg = "New article with id %s has been created" % \
                    post.get_post_id()

        for item, value in (('id', post.get_post_id()),
                            ('date', post.published.text),
                            ('title', post.title.text),
                            ('modified', post.updated.text),
                            ('tags',
                             ", ".join([cat.term for cat in post.category]))):
            self._update_docinfo(item, value)
        return msg

    def delete(self):
        """
        Get list of articles, display it to the user, make him choose one and
        delete
        """
        if not self.password:
            self.password = \
                    vim.eval('inputsecret("Enter your gmail password: ")')
        blog = VimBlogger(self.blogname, self.login, self.password)

        posts = blog.get_articles(self.maxarticles)

        msg = u"inputlist(["
        for index, entries in enumerate(posts):
            line = "%2d %s  %s" % (index + 1,
                                   entries[1],
                                   entries[2])
            msg += u'"' + line.replace('"', '\\"') + u'",'
        msg = msg[:-1]
        msg += u"])"
        msg = unicode(msg).encode(self.vim_encoding)

        choice = int(vim.eval(msg))
        if choice:
            art = posts[choice - 1]
            msg = 'confirm("You are about to delete article \'%s\'. '
            msg += 'Are you sure?"'
            msg = unicode(msg % art[1]).encode(self.vim_encoding)
            msg += ', "&No\n&Yes")'

            if self.confirm_del:
                choice = int(vim.eval(msg))
            else:
                choice = 2

            if choice == 2:
                blog.delete_article(art[0])
                return "Article deleted"
        return "No articles deleted"

    def _update_docinfo(self, attr, val):
        """
        Update current buffer with attributes value
        """

        val = unicode(":%s: %s" % (attr.capitalize(), val))
        val = val.encode(self.buffer_encoding)

        if not self.docinfo_len:
            self.buff.append(val, 0)
            return

        for num, line in enumerate(self.buff[:self.docinfo_len]):
            if ':%s:' % attr in line.lower() and line.startswith(":"):
                self.buff[num] = val
                return

        self.buff.append(val, 0)
        self.docinfo_len += 1

    def _set_docinfo_len(self):
        """
        Set docinfo_len, which means number of lines from the beginning of the
        buffer to the first empty line.
        """
        for num, line in enumerate(self.buff):
            if line and line.startswith(':'):
                continue
            elif not line:
                self.docinfo_len = num
                break
            else:
                self.docinfo_len = 0
                break

    def _open_qf(self, msg):
        """
        Open VIm QuickFix window with message, if argument msg is non empty
        string.
        """
        if msg:
            msg1 = "There are problems reported by XML parser:"
            msg2 = "Check generated html for errors."
            vim.command('call setqflist([{"text": "%s"}, {"text": "%s"}, '
                        '{"text": "%s"}])' % (msg1, msg, msg2))
            vim.command('copen')

    def _check_html(self, html, add_container=False):
        """
        Check HTML generated document, by simply use minidom parser
        If add_container is set to True, entire document is wrapped inside
        additional div
        returns empty string if parses succeed, else exception message.
        """

        # minidom doesn't understand html entities like '&nbsp;' For checking
        # purpose it is perfectly ok, to switch them with '&amp;'
        html = html.replace("&nbsp;", "&amp;")
        if add_container:
            html = "<div>" + html + "</div>"

        message = ""
        try:
            minidom.parseString(html)
        except ExpatError as ex:
            message = str(ex)

        return message
ftplugin/rst/rst2blogger/rest.py	[[[1
287
"""
File: rest.py
Author: Roman 'gryf' Dobosz
Description: This module is responsible for conversion between reST and HTML
             with some goods added.
"""

import re

from docutils import core
from docutils import nodes
from docutils.parsers.rst import directives, Directive
from docutils.writers.html4css1 import Writer, HTMLTranslator

try:
    from pygments import highlight
    from pygments.lexers import get_lexer_by_name, TextLexer
    from pygments.formatters import HtmlFormatter

    class Pygments(Directive):
        """
        Source code syntax highlighting.
        """
        required_arguments = 1
        optional_arguments = 0
        final_argument_whitespace = True
        has_content = True

        def run(self):
            self.assert_has_content()
            try:
                lexer = get_lexer_by_name(self.arguments[0])
            except ValueError:
                # no lexer found - use the text one instead of an exception
                lexer = TextLexer()
            # take an arbitrary option if more than one is given
            formatter = HtmlFormatter(noclasses=True)
            parsed = highlight(u'\n'.join(self.content), lexer, formatter)
            return [nodes.raw('', parsed, format='html')]

    directives.register_directive('sourcecode', Pygments)
except ImportError:
    pass


class Attrs(object):
    ATTRS = {}


class CustomHTMLTranslator(HTMLTranslator):
    """
    Base class for reST files translations.
    There are couple of customizations for docinfo fields behaviour and
    abbreviations and acronyms.
    """
    def __init__(self, document):
        """
        Set some nice defaults for articles translations
        """
        HTMLTranslator.__init__(self, document)
        self.initial_header_level = 4

    def visit_section(self, node):
        """
        Don't affect document, just keep track of the section levels
        """
        self.section_level += 1

    def depart_section(self, node):
        self.section_level -= 1

    def visit_meta(self, node):
        pass

    def depart_meta(self, node):
        pass

    def visit_document(self, node):
        pass

    def depart_document(self, node):
        pass

    def depart_docinfo(self, node):
        """
        Reset body, remove unnecessary content.
        """
        self.body = []

    def visit_date(self, node):
        pass

    def depart_date(self, node):
        pass

    def visit_literal(self, node):
        """
        This is almos the same as the original one from HTMLTranslator class.
        The only difference is in used HTML tag: it uses 'code' instead of
        'tt'
        """
        self.body.append(self.starttag(node, 'code', ''))
        text = node.astext()
        for token in self.words_and_spaces.findall(text):
            if token.strip():
                # Protect text like "--an-option" and the regular expression
                # ``[+]?(\d+(\.\d*)?|\.\d+)`` from bad line wrapping
                if self.sollbruchstelle.search(token):
                    self.body.append('<span class="pre">%s</span>'
                                     % self.encode(token))
                else:
                    self.body.append(self.encode(token))
            elif token in ('\n', ' '):
                # Allow breaks at whitespace:
                self.body.append(token)
            else:
                # Protect runs of multiple spaces; the last space can wrap:
                self.body.append('&nbsp;' * (len(token) - 1) + ' ')
        self.body.append('</code>')
        # Content already processed:
        raise nodes.SkipNode

    def visit_acronym(self, node):
        """
        Define missing acronym HTML tag
        """
        node_text = node.children[0].astext()
        node_text = node_text.replace('\n', ' ')
        patt = re.compile(r'^(.+)\s<(.+)>')

        if patt.match(node_text):
            node.children[0] = nodes.Text(patt.match(node_text).groups()[0])
            self.body.append(\
                self.starttag(node, 'acronym',
                              '', title=patt.match(node_text).groups()[1]))

        else:
            self.body.append(self.starttag(node, 'acronym', ''))

    def visit_abbreviation(self, node):
        """
        Define missing abbr HTML tag
        """
        node_text = node.children[0].astext()
        node_text = node_text.replace('\n', ' ')
        patt = re.compile(r'^(.+)\s<(.+)>')

        if patt.match(node_text):
            node.children[0] = nodes.Text(patt.match(node_text).groups()[0])
            self.body.append(\
                self.starttag(node, 'abbr',
                              '', title=patt.match(node_text).groups()[1]))

        else:
            self.body.append(self.starttag(node, 'abbr', ''))


class NoHeaderHTMLTranslator(CustomHTMLTranslator):
    """
    Special subclass for generating only body of an article
    """
    def __init__(self, document):
        """
        Remove all needless parts of HTML document.
        """
        CustomHTMLTranslator.__init__(self, document)
        self.head = []
        self.meta = []
        self.head_prefix = ['', '', '', '', '']
        self.body_prefix = []
        self.body_suffix = []
        self.stylesheet = []
        self.generator = ('')

    def visit_field(self, node):
        """
        Harvest docinfo fields and store it in global dictionary.
        """
        key, val = [n.astext() for n in node]
        Attrs.ATTRS[key.lower()] = val.strip()

    def visit_date(self, node):
        """
        Store published date in global dictionary.
        """
        Attrs.ATTRS['date'] = node.astext()


class PreviewHTMLTranslator(CustomHTMLTranslator):
    """
    Class for display article in the browser as a preview.
    """
    CSS = []

    def __init__(self, document):
        """
        Alter levels for the heading tags, define custom, blog specific
        stylesheets. Note, that style_custom is present only locally to adjust
        way of display the page
        """
        CustomHTMLTranslator.__init__(self, document)
        self.initial_header_level = 1
        self.section_level = 1
        # order of css files is important
        self.default_stylesheets = PreviewHTMLTranslator.CSS
        self.stylesheet = [self.stylesheet_link % self.encode(css) \
                for css in self.default_stylesheets]
        self.body_ = []

    def depart_docinfo(self, node):
        """
        Overwrite body with some custom one. body_ will hold the first heading
        with title of the document.
        """
        self.body = self.body_

    def visit_field(self, node):
        """
        Make title visible as a heading
        """
        key, node_ = [n.astext() for n in node]
        key = key.lower()
        if key == 'title':
            self.head.append('<title>%s</title>\n' % self.encode(node_))
            self.body_.append('<h1 class="post-title entry-title">'
                             '<a href="#">%s</a></h1>\n' % self.encode(node_))


class BlogBodyWriter(Writer):
    """
    Custom Writer class for generating HTML partial with the article
    """
    def __init__(self):
        Writer.__init__(self)
        self.translator_class = NoHeaderHTMLTranslator

    def translate(self):
        self.document.settings.output_encoding = "utf-8"
        Writer.translate(self)


class BlogPreviewWriter(Writer):
    """
    Custom Writer class for generating full HTML of the article
    """
    def __init__(self, stylesheets=None):
        Writer.__init__(self)
        if not stylesheets:
            stylesheets = []
        self.translator_class = PreviewHTMLTranslator
        self.translator_class.CSS = stylesheets

    def translate(self):
        self.document.settings.output_encoding = "utf-8"
        Writer.translate(self)


def blogPreview(string, stylesheets=None):
    """
    Returns full HTML of the article.
    string argument is an article in reST
    """
    if not stylesheets:
        stylesheets = []
    html_output = core.publish_string(string,
                                      writer=BlogPreviewWriter(stylesheets))
    html_output = html_output.strip()
    html_output = html_output.replace("<!-- more -->", "\n<!-- more -->\n")
    return html_output


def blogArticleString(string):
    """
    Returns partial HTML of the article, and attribute dictionary
    string argument is an article in reST
    """
    # reset ATTRS
    Attrs.ATTRS = {}
    html_output = core.publish_string(string, writer=BlogBodyWriter())
    html_output = html_output.strip()
    html_output = html_output.replace("<!-- more -->", "\n<!-- more -->\n")
    attrs = {}
    for key in Attrs.ATTRS:
        if Attrs.ATTRS[key]:
            attrs[key] = Attrs.ATTRS[key]

    return html_output, attrs
ftplugin/rst/rst2blogger/tests/__init__.py	[[[1
1
# module rst2blogger.tests
ftplugin/rst/rst2blogger/tests/shared.py	[[[1
267
# vim: set fileencoding=utf-8
import sys
import os
from datetime import datetime
from tempfile import mkstemp


LOGIN = "John"
PASS = "secret"
REST_ARTICLE = u""":Title: Title — This is a test
:Date: 2010-12-12T12:36:36+01:00
:Tags: this is a test, Blogger, rest

.. meta::
    :description: meta are completely ignored in blogger parsers

`Amet`, convallis sollicitudin, commodo a, purus. Nulla vitae eros a diam
blandit **mollis**. Proin luctus ``ls --color    ~/`` feugiat eros.

.. more

Pellentesque habitant morbi tristique senectus et *netus* et malesuada fames
ac turpis egestas. Duis ultricies urna: ``easy_install pygments``. Etiam enim
urna, pharetra suscipit, varius et, congue quis, odio. Donec `NES <Nintendo
Entertainment System>`:acronym: lobortis, elit bibendum euismod faucibus,
velit nibh egestas libero, vitae pellentesque elit augue ut massa.

test empty `acronym`:acronym: and `abbrev`:abbreviation:

Section 1
---------

Nulla consequat erat at massa. Vivamus id mi. Morbi purus enim, dapibus a,
facilisis non, tincidunt at, enim. Vestibulum ante ipsum primis in faucibus
orci luctus et ultrices posuere cubilia Curae; `WTF? <What the
fcuk?>`:abbreviation: Duis imperdiet eleifend arcu.  Cras magna ligula,
consequat at, tempor non, posuere.

Subsection 1.1
..............

.. sourcecode:: python

    import vim
    print vim.current.buffer.name

.. sourcecode:: unknown_lexer

    Cras dignissim vulputate metus.
    Phasellus eu quam. Quisque interdum cursus purus. In.

End.
"""


class Eval(object):
    """
    Communication class
    """
    value = ""
    blog = None
    gdata_delete = 0


class Dummy(sys.__class__):
    """
    Dummy class, for faking modules and other objects, not directly needed
    """
    def __getattr__(self, attrname):
        """ The dummy class should have no attribute """
        if attrname == 'util':
            return Dummy("util")
        return None

# fake vim module.
sys.modules["vim"] = Dummy("vim")


class MockBuffer(list):
    """
    Vim buffer-like class
    """
    def append(self, val, line=None):
        """
        Override append method to mimic vim.buffer append behaviour
        """
        if line is None:
            super(MockBuffer, self).append(val)
        else:
            super(MockBuffer, self).insert(line, val)


class Mock(object):
    """
    Generic all-purpose mock class
    """
    pass


import vim
vim.command = lambda x: None
vim.current = Mock()
vim.current.buffer = MockBuffer(REST_ARTICLE.split("\n"))
fdesc, vim.current.buffer.name = mkstemp()
vim.current.buffer.name += ".rst"
os.close(fdesc)  # close descriptor, only filename is needed


def mock_vim_eval(string):
    ints = ("g:blogger_draft", "g:blogger_maxarticles",
            "g:blogger_confirm_del")
    if string in ints:
        return "0"
    elif string == "g:blogger_stylesheets":
        return []
    else:
        return Eval.value
vim.eval = mock_vim_eval


class MockBlog(object):
    """
    Mock blog class
    """
    def __init__(self, name, id):
        self.name = name
        self.id = id

    def get_blog_name(self):
        return self.name

    def get_blog_id(self):
        return self.id

    def get_post_link(self):
        link = Mock()
        link.href = "http://www.mock.org"
        return link

    def get_post_id(self):
        return self.id


class MockPost(object):
    """
    Mock class imitating posts
    """
    def __init__(self):
        self.category = Mock()
        self.category = []
        self.id = None
        self.title = Mock()
        self.title.text = ""
        self.published = Mock()
        self.published.text = ""

    def add_label(self, label):
        item = Mock()
        item.term = label
        self.category.append(item)

    def get_post_id(self):
        return self.id


class MockBlogFeed(object):
    """
    Mock class for feed objects
    """
    def __init__(self, *args, **kwargs):
        self.entry = []
        if Eval.blog:
            for bid, bname in {1: 'one', 3: 'test', 7: 'blog_name'}.items():
                blog = MockBlog(bname, bid)
                self.entry.append(blog)


class MockPostFeed(object):
    """
    Mock class for feed objects
    """
    def __init__(self, *args, **kwargs):
        self.entry = []


from atom.data import Id, Updated
from gdata.blogger.client import BloggerClient

BloggerClient.get_blogs = lambda x: MockBlogFeed()

from gdata.client import BadAuthentication


def mock_client_login(self, login, password, source=None, service=None):
    """
    Mock method for client login.
    """
    if login != LOGIN or password != PASS:
        raise BadAuthentication("Incorrect username or password")
BloggerClient.client_login = mock_client_login


def mock_client_post(self, post, url=None):
    """
    Mimic post method
    """
    if Eval.value == 10:
        return None
    new_id = Id(text='1234567890')
    post.id = new_id
    date = datetime.utcnow()
    milli = str(date.microsecond)[:3]
    date = date.strftime("%Y-%m-%dT%H:%M:%S")
    date = date + ".%s+00:00" % milli
    post.updated = Updated(text=date)
    return post
BloggerClient.post = mock_client_post
BloggerClient.update = mock_client_post


def mock_client_delete(self, post):
    """
    Mock delete method
    """
    if not post:
        raise AttributeError("%s object has no attribute 'etag'" % type(post))
    if Eval.gdata_delete:
        return "404 Mock"
    return None
BloggerClient.delete = mock_client_delete


def mock_client_get_posts(self, blog_id):
    """
    Mock get_posts method
    """
    posts = (('title1', 1, "2000-01-01T00:04:00.001+01:00"),
             ('title2', 2, "2001-01-01T00:02:19.001+01:00"),
             ('title3', 3, "2002-01-01T00:01:00.001+01:00"),
             ('title4', 4, "2006-01-01T00:02:00.001+02:00"))
    feed = MockPostFeed()
    for p in posts:
        a = MockPost()
        a.id = p[1]
        a.title.text = p[0]
        a.published.text = p[2]
        feed.entry.append(a)
    return feed
BloggerClient.get_posts = mock_client_get_posts


def mock_client_get_feed(self, uri, desired_class=None):
    """
    Mock get_feed method
    """
    post = MockPost()
    post.add_label('test1')
    return post
BloggerClient.get_feed = mock_client_get_feed


from gdata.blogger.data import BlogPost


def mock_get_post_id(self):
    return self.id.text
BlogPost.get_post_id = mock_get_post_id
ftplugin/rst/rst2blogger/tests/test_blogger.py	[[[1
514
import os
import sys
import unittest

this_dir = os.path.dirname(os.path.abspath(__file__))
this_dir = os.path.abspath(os.path.join(this_dir, "../.."))
sys.path.insert(0, this_dir)

from rst2blogger.tests import shared
from rst2blogger.blogger import VimBlogger


class TestCheckDates(unittest.TestCase):
    """
    Tests for method VimBlogger._check_date
    """
    def setUp(self):
        """
        Create VimBlogger object
        """
        self.vimb = VimBlogger(None, shared.LOGIN, shared.PASS)

    def test_happy_case_CET(self):
        """
        Test on good date string on Central and East Europe
        """
        date = "2000-01-01T00:00:00.001+01:00"
        self.assertTrue(self.vimb._check_date(date))

    def test_happy_case_HST(self):
        """
        Test on good date string on Hawaii Time Zone
        """
        date = "2000-01-01T00:00:00.001-10:00"
        self.assertTrue(self.vimb._check_date(date))

    def test_happy_case_GMT(self):
        """
        Test UTC date string
        """
        date = "2000-01-01T00:00:00.001-00:00"
        self.assertTrue(self.vimb._check_date(date))

    def test_without_milliseconds(self):
        """
        Test on date string without milliseconds
        """
        date = "2000-01-01T00:00:00+01:00"
        self.assertTrue(self.vimb._check_date(date))

    def test_wrong_tz_format(self):
        """
        Test date with wrong timezone format (hour have no leading 0)
        """
        date = "2000-01-01T00:00:00.001+1:00"
        self.assertFalse(self.vimb._check_date(date))

        # Test date with wrong timezone format (minute have only one digit)
        date = "2000-01-01T00:00:00.001+01:0"
        self.assertFalse(self.vimb._check_date(date))

        # Test date with wrong timezone format (hours and minutes hasn't been
        # separated by colon)
        date = "2000-01-01T00:00:00.001+0100"
        self.assertFalse(self.vimb._check_date(date))

    def test_wrong_milliseconds(self):
        """
        Test date with wrong format of milliseconds (.01 instead of .010)
        """
        date = "2000-01-01T00:00:00.01+01:00"
        self.assertFalse(self.vimb._check_date(date))

        # Test date with wrong format of milliseconds (.1 instead of .100)
        date = "2000-01-01T00:00:00.1+01:00"
        self.assertFalse(self.vimb._check_date(date))

        # Test date with spolied format (dot for milliseconds, but no digits)
        date = "2000-01-01T00:00:00.+01:00"
        self.assertFalse(self.vimb._check_date(date))

    def test_good_milliseconds(self):
        """
        Test date with correct format of milliseconds
        """
        date = "2000-01-01T00:00:00.000+01:00"
        self.assertTrue(self.vimb._check_date(date), date + " is incorrect")

        date = "2000-01-01T00:00:00.999+01:00"
        self.assertTrue(self.vimb._check_date(date), date + " is incorrect")

    def test_wrong_hours(self):
        """
        Test date with wrong hours value
        """
        date = "2000-01-01T24:00:00.001+01:00"
        self.assertFalse(self.vimb._check_date(date))

    def test_good_hours(self):
        """
        Test date with correct hours values
        """
        date = "2000-01-01T00:00:00.001+01:00"
        self.assertTrue(self.vimb._check_date(date), date + " is incorrect")
        date = "2000-01-01T23:00:00.001+01:00"
        self.assertTrue(self.vimb._check_date(date), date + " is incorrect")

    def test_wrong_minutes(self):
        """
        Test date with wrong minutes value
        """
        date = "2000-01-01T00:60:00.001+01:00"
        self.assertFalse(self.vimb._check_date(date))

        date = "2000-01-01T00:000:00.001+01:00"
        self.assertFalse(self.vimb._check_date(date))

        date = "2000-01-01T00:1:00.001+01:00"
        self.assertFalse(self.vimb._check_date(date))

    def test_good_minutes(self):
        """
        Test date with correct minutes values
        """
        date = "2000-01-01T00:01:00.001+01:00"
        self.assertTrue(self.vimb._check_date(date))

        date = "2000-01-01T00:59:00.001+01:00"
        self.assertTrue(self.vimb._check_date(date))

    def test_wrong_seconds(self):
        """
        Test date with wrong seconds value
        """
        date = "2000-01-01T00:00:60.001+01:00"
        self.assertFalse(self.vimb._check_date(date))

    def test_good_seconds(self):
        """
        Test date with good seconds values
        """
        for second in range(60):
            date = "2000-01-01T00:00:%0.2d.001+01:00" % second
            self.assertTrue(self.vimb._check_date(date))

    def test_wrong_days(self):
        """
        Test date with incorrect days (january has always 31 days, no month
        has lower number than 1)
        """
        date = "2000-01-32T00:00:00.001+01:00"
        self.assertFalse(self.vimb._check_date(date))

        date = "2000-01-00T00:00:00.001+01:00"
        self.assertFalse(self.vimb._check_date(date))

    def test_good_days(self):
        """
        Test date with correct days (january has always 31 days)
        """
        date = "2000-01-01T00:00:00.001+01:00"
        self.assertTrue(self.vimb._check_date(date))

        date = "2000-01-31T00:00:00.001+01:00"
        self.assertTrue(self.vimb._check_date(date))

    def test_wrong_month(self):
        """
        Test date with wrong month
        """
        date = "2000-00-01T00:00:00.001+01:00"
        self.assertFalse(self.vimb._check_date(date))

        date = "2000-13-01T00:00:00.001+01:00"
        self.assertFalse(self.vimb._check_date(date))

        date = "2000-1-01T00:00:00.001+01:00"
        self.assertFalse(self.vimb._check_date(date))

        date = "2000-001-01T00:00:00.001+01:00"
        self.assertFalse(self.vimb._check_date(date))

    def test_good_month(self):
        """
        Test date with correct months
        """
        date = "2000-01-01T00:00:00.001+01:00"
        self.assertTrue(self.vimb._check_date(date))

        date = "2000-12-01T00:00:00.001+01:00"
        self.assertTrue(self.vimb._check_date(date))

    def test_wrong_year(self):
        """
        Test date with wrong year
        """
        date = "0000-01-01T00:00:00.001+01:00"
        self.assertFalse(self.vimb._check_date(date))

        date = "10000-01-01T00:00:00.001+01:00"
        self.assertFalse(self.vimb._check_date(date))

        date = "900-01-01T00:00:00.001+01:00"
        self.assertFalse(self.vimb._check_date(date))

    def test_good_year(self):
        """
        Test date with correct years
        """
        date = "0001-01-01T00:00:00.001+01:00"
        self.assertTrue(self.vimb._check_date(date))

        date = "9999-01-01T00:00:00.001+01:00"
        self.assertTrue(self.vimb._check_date(date))


class TestAuthorize(unittest.TestCase):
    """
    Test method VimBlogger._authorize
    """
    def setUp(self):
        """
        Create VimBlogger object (with good credentials, yes :>)
        """
        self.vimob = VimBlogger(None, shared.LOGIN, shared.PASS)

    def test_happy_case(self):
        """
        Try to login with good credentials
        """
        self.assertTrue(self.vimob._authorize(shared.LOGIN,
                                              shared.PASS) is None)

    def test_wrong_login(self):
        """
        Try to login with wrong login
        """
        self.assertRaises(shared.BadAuthentication, self.vimob._authorize,
                          'joe', shared.PASS)

    def test_wrong_pass(self):
        """
        Try to login with wrong password
        """
        self.assertRaises(shared.BadAuthentication, self.vimob._authorize,
                          'joe', shared.PASS)


class TestAddTag(unittest.TestCase):
    """
    Test method VimBlogger._add_tag
    """
    def setUp(self):
        """
        Create VimBlogger object
        """
        self.vimob = VimBlogger(None, shared.LOGIN, shared.PASS)
        self.post = shared.MockPost()

    def test_add_tag(self):
        """
        Add items to existing categories. List should be uniq.
        """
        self.vimob._add_tag(self.post, 'item')
        self.assertTrue(len(self.post.category) == 1)

        # Item number should not change on the same label
        self.vimob._add_tag(self.post, 'item')
        self.assertTrue(len(self.post.category) == 1)

        self.vimob._add_tag(self.post, 'item2')
        self.assertTrue(len(self.post.category) == 2)


class TestExtractDate(unittest.TestCase):
    """
    Test method VimBlogger._extract_date
    """
    def setUp(self):
        """
        Create VimBlogger object
        """
        self.vimob = VimBlogger(None, shared.LOGIN, shared.PASS)

    def test_extract_date(self):
        """
        Date should be already verified by _check_date method, so only
        extraction is tested
        """
        date = "2000-01-01T00:00:00.001-10:00"

        # wrong scenario
        self.assertFalse(self.vimob._extract_date('wrong_date_string'))

        # only date should be returned
        self.assertEqual(self.vimob._extract_date(date), "2000-01-01")

        # date and time should be returned
        self.assertEqual(self.vimob._extract_date(date, True),
                         "2000-01-01 00:00:00")


class TestGetPost(unittest.TestCase):
    """
    Test method VimBlogger._get_post
    """
    def setUp(self):
        """
        Create VimBlogger object
        """
        self.vimob = VimBlogger(None, shared.LOGIN, shared.PASS)
        self.vimob.blog = shared.Mock()

        link = shared.Mock()
        link.href = "mock.com"
        link.feed = shared.Mock()

        self.vimob.blog.get_post_link = lambda: link

    def test_get_post(self):
        """
        Nothing really to test here. Maybe in the future :)
        """
        result = self.vimob._get_post('1234')
        self.assertEqual(type(result), shared.MockPost)


class TestSetBlog(unittest.TestCase):
    """
    Test method VimBlogger._set_blog
    """
    def setUp(self):
        """
        Create VimBlogger object
        """
        self.vimob = VimBlogger(None, shared.LOGIN, shared.PASS)
        for bid, bname in {1: 'one', 3: 'test', 7: 'blog_name'}.items():
            blog = shared.MockBlog(bname, bid)
            self.vimob.feed.entry.append(blog)

    def test_set_blog(self):
        """
        Test setting a blog
        """
        self.vimob._set_blog("no_valid_blog_name")
        self.assertEqual(self.vimob.blog_id, None)
        self.assertEqual(self.vimob.blog, None)

        self.vimob._set_blog("blog_name")
        self.assertEqual(self.vimob.blog_id, 7)
        self.assertEqual(self.vimob.blog.get_blog_name(), 'blog_name')

        self.vimob._set_blog("test")
        self.assertEqual(self.vimob.blog_id, 3)
        self.assertEqual(self.vimob.blog.get_blog_name(), 'test')

        self.vimob._set_blog("one")
        self.assertEqual(self.vimob.blog_id, 1)
        self.assertEqual(self.vimob.blog.get_blog_name(), 'one')


class TestCreateArticle(unittest.TestCase):
    """
    Test method VimBlogger.create_article
    """
    def setUp(self):
        """
        Create VimBlogger object
        """
        self.vimob = VimBlogger(None, shared.LOGIN, shared.PASS)

    def test_create_simple_article(self):
        """
        Test creation of article with minimum requirements
        """
        html = "<p>article</p>"
        post = self.vimob.create_article(html)
        self.vimob.draft = True

        self.assertEqual(post.id.text, '1234567890')
        self.assertEqual(post.content.text, html)
        self.assertEqual(post.published, None)
        self.assertTrue(post.updated is not None)
        self.assertEqual(post.title.text, "")
        self.assertEqual(post.category, [])
        self.assertEqual(post.control.draft.text, "yes")

    def test_create_article(self):
        """
        Test creation of article with full attrs
        """
        html = u"<p>article \xe2\x80\x94 article</p>"
        labels = "tag with spaces|vim|python|blogger".split("|")
        attrs = {"title":  u'Title \xe2\x80\x94 title',
                 "tags": ", ".join(labels),
                 "date": "2010-12-10T14:18:32+00:00"}
        self.vimob.draft = False

        post = self.vimob.create_article(html, attrs)
        self.assertEqual(post.id.text, '1234567890')
        self.assertEqual(post.content.text, html)
        self.assertEqual(post.published.text, attrs['date'])
        self.assertTrue(post.updated is not None)
        self.assertEqual(post.title.text, attrs['title'])
        self.assertEqual(len(post.category), 4)

        for label in post.category:
            self.assertTrue(label.term in labels)
            del(labels[labels.index(label.term)])

        self.assertEqual(post.control, None)


class TestDeleteArticle(unittest.TestCase):
    """
    Test method VimBlogger.create_article
    """
    def setUp(self):
        """
        Create VimBlogger object
        """
        self.vimob = VimBlogger(None, shared.LOGIN, shared.PASS)
        for bid, bname in {1: 'one', 3: 'test', 7: 'blog_name'}.items():
            blog = shared.MockBlog(bname, bid)
            self.vimob.feed.entry.append(blog)
        self.vimob._set_blog('test')

    def test_delete_non_existing_article(self):
        """
        Test removing article without id
        """
        self.assertEqual(self.vimob.delete_article(None),
                         "No article id provided")

    def test_delete_article(self):
        """
        Test removing article
        """
        html = u"<p>article \xe2\x80\x94 article</p>"
        labels = "tag with spaces|vim|python|blogger".split("|")
        attrs = {"title":  u'Title \xe2\x80\x94 title',
                 "tags": ", ".join(labels),
                 "date": "2010-12-10T14:18:32+00:00"}
        self.vimob.draft = False

        post = self.vimob.create_article(html, attrs)
        self.assertEqual(self.vimob.delete_article(post.id.text), None)


class TestGetArticles(unittest.TestCase):
    """
    Test method VimBlogger.get_articles
    """
    def setUp(self):
        """
        Create VimBlogger object
        """
        self.vimob = VimBlogger(None, shared.LOGIN, shared.PASS)

    def test_get_articles(self):
        """
        Test removing article without id
        """
        articles = self.vimob.get_articles()
        self.assertEqual(len(articles), 4)

        articles = self.vimob.get_articles(maxarticles=2)
        self.assertEqual(len(articles), 2)


class TestUpdateArticle(unittest.TestCase):
    """
    Test method VimBlogger.update_article
    """
    def setUp(self):
        """
        Create VimBlogger object
        """
        self.vimob = VimBlogger(None, shared.LOGIN, shared.PASS)
        for bid, bname in {1: 'one', 3: 'test', 7: 'blog_name'}.items():
            blog = shared.MockBlog(bname, bid)
            self.vimob.feed.entry.append(blog)
        self.vimob._set_blog('test')

    def test_wrong_argument_types(self):
        """
        Test update_article method with wrong argument types
        """
        self.assertRaises(TypeError, self.vimob.update_article, None, None)

    def test_no_id_in_attrs(self):
        """
        Test update_article method with no id in attrs
        """
        self.assertRaises(Exception, self.vimob.update_article,
                          '<p>update</p>', [])

    def test_update(self):
        """
        Test update_article method with no id in attrs
        """
        attrs = {'id': 1234567890, 'title': 'update',
                 'date': '2001-01-01T00:02:19.001+01:00',
                 'tags': "tag1, tag2, tag3"}
        post = self.vimob.update_article('<p>update</p>', attrs)

        self.assertEqual(post.title.text, 'update')
        self.assertEqual(post.id.text, '1234567890')
        self.assertEqual(post.content.text, '<p>update</p>')
        self.assertTrue(post.updated.text is not None)


if __name__ == "__main__":
    unittest.main()
ftplugin/rst/rst2blogger/tests/test_main.py	[[[1
294
# vim: set fileencoding=utf-8
import os
import sys
import unittest
import webbrowser

webbrowser.open = lambda x: None

this_dir = os.path.dirname(os.path.abspath(__file__))
this_dir = os.path.abspath(os.path.join(this_dir, "../.."))
sys.path.insert(0, this_dir)

from rst2blogger.tests.shared import LOGIN, PASS, Eval, MockBuffer
from rst2blogger.main import Rst2Blogger
from gdata.client import BadAuthentication


class TestRst2Blogger(unittest.TestCase):
    """
    Tests for vim - rest - blogger interface
    """
    def setUp(self):
        """
        Create Rst2Blogger object
        """
        self.obj = Rst2Blogger()

    def test_object_creation(self):
        """
        Create Rst2Blogger object and test it.
        """
        self.assertTrue(self.obj is not None)
        self.assertEqual(self.obj.docinfo_len, 3)
        self.assertEqual(self.obj.login, "")
        self.assertEqual(self.obj.password, "")
        self.assertEqual(self.obj.blogname, "")
        self.assertEqual(self.obj.buffer_encoding, "")
        self.assertEqual(self.obj.vim_encoding, "")
        self.assertEqual(self.obj.maxarticles, 0)
        self.assertEqual(self.obj.draft, 0)
        self.assertEqual(self.obj.confirm_del, 0)
        self.assertEqual(self.obj.stylesheets, [])


class TestRst2BloggerSetDocinfoLen(unittest.TestCase):
    """
    Test _set_docinfo_len method on different docinfo configurations
    """
    def setUp(self):
        """
        Create Rst2Blogger object
        """
        self.obj = Rst2Blogger()

    def test_set_docinfo_len(self):
        """
        Test with no defined docinfo
        """
        self.obj.buff = self.obj.buff[4:]
        self.obj._set_docinfo_len()
        self.assertEqual(self.obj.docinfo_len, 0)

    def test_set_docinfo_len2(self):
        """
        Test with one docinfo entry
        """
        self.obj.buff = self.obj.buff[:1] + [''] + self.obj.buff[4:]
        self.obj._set_docinfo_len()
        self.assertEqual(self.obj.docinfo_len, 1)

    def test_set_docinfo_len3(self):
        """
        Test with wrong docinfo definition
        """
        self.obj.buff = self.obj.buff[:1] + self.obj.buff[4:]
        self.obj._set_docinfo_len()
        self.assertEqual(self.obj.docinfo_len, 0)


class TestCheckHtml(unittest.TestCase):
    """
    Check HTML parser
    """
    def setUp(self):
        """
        Create Rst2Blogger object
        """
        self.obj = Rst2Blogger()

    def test_check_html1(self):
        """
        Parse (generated) html string, should return empty string
        """
        html = "<html><head><title>test</title></head><body></body></html>"
        self.assertEqual(self.obj._check_html(html), "")
        self.assertEqual(self.obj._check_html(html, True), "")

    def test_check_html2(self):
        """
        Parse html fragment string
        """
        html = "<p>first paragraph</p><p>another paragraph</p>"
        self.assertEqual(self.obj._check_html(html),
                         "junk after document element: line 1, column 22")
        self.assertEqual(self.obj._check_html(html, True), "")

    def test_check_html3(self):
        """
        Parse wrong html string (crossed tags)
        """
        html = "<p>first paragraph<b></p>another paragraph</b>"
        self.assertEqual(self.obj._check_html(html),
                         "mismatched tag: line 1, column 23")
        self.assertEqual(self.obj._check_html(html, True),
                         "mismatched tag: line 1, column 28")


class TestRst2BloggerDelete(unittest.TestCase):
    """
    Test delete method
    """
    def setUp(self):
        """
        Create Rst2Blogger object
        """
        self.obj = Rst2Blogger()
        self.obj.login = LOGIN
        self.obj.password = PASS
        self.obj.blogname = "test"
        self.obj.vim_encoding = "utf-8"

    def test_delete_without_password(self):
        """
        Delete article, while password is incorrect/nonexistend
        """
        self.obj.password = ""
        self.assertRaises(BadAuthentication, self.obj.delete)

    def test_delete(self):
        """
        Delete article. Set confirmation attribute.
        """
        self.obj.confirm_del = 1
        Eval.value = 2  # set choice to answer "Y" for confirmation
        Eval.blog = "test"
        self.assertEqual(self.obj.delete(), "Article deleted")

    def test_delete2(self):
        """
        Delete article. Set confirmation attribute. Refuse to delete.
        """
        self.obj.confirm_del = 1
        Eval.value = 1  # set choice to answer "N" for confirmation
        Eval.blog = "test"
        self.assertEqual(self.obj.delete(), "No articles deleted")

    def test_delete3(self):
        """
        Delete article. Unset confirmation attribute. Delete returns something
        else then None.
        """
        Eval.value = 2
        Eval.blog = "test"
        Eval.gdata_delete = 1
        self.assertEqual(self.obj.delete(), "Article deleted")


class TestRst2BloggerPost(unittest.TestCase):
    """
    Test post method
    """
    def setUp(self):
        """
        Create Rst2Blogger object
        """
        self.obj = Rst2Blogger()
        self.obj.login = LOGIN
        self.obj.password = PASS
        self.obj.blogname = "test"
        self.obj.vim_encoding = "utf-8"
        self.obj.buffer_encoding = "utf-8"
        # create copy of the buffer list and assign copy to the buff attribute
        self._rest = MockBuffer(self.obj.buff[:])
        self.obj.buff = self._rest

    def test_without_password(self):
        """
        Post article, while password is incorrect/nonexistend
        """
        self.obj.password = ""
        self.assertRaises(BadAuthentication, self.obj.post)

    def test_with_wrong_data(self):
        """
        Try to post not well formed html
        """
        self.obj.buff.append('')
        self.obj.buff.append('.. raw:: html')
        self.obj.buff.append('')
        self.obj.buff.append('    <p>foo<b>bar</p>baz</b>')
        self.obj.buff.append('')
        self.obj.post()
        self.assertEqual(self.obj.post(),
                         'There are errors in generated document')

    def test_post_create(self):
        """
        Try to post well formed html, as a new article
        """
        self.assertEqual(self.obj.post(),
                         'New article with id 1234567890 has been created')

    def test_post_update(self):
        """
        Try to post well formed html, as a new article
        """
        self.obj.buff.append(':Id: 1234567890', 0)
        self.assertEqual(self.obj.post(),
                         "Article 'Title \xe2\x80\x94 This is a test' "
                         "has been updated")


class TestRst2BloggerUpdateDocinfo(unittest.TestCase):
    """
    Test _update_docinfo
    """
    def setUp(self):
        """
        Create Rst2Blogger object
        """
        self.obj = Rst2Blogger()
        self.obj.login = LOGIN
        self.obj.password = PASS
        self.obj.blogname = "test"
        self.obj.vim_encoding = "utf-8"
        self.obj.buffer_encoding = "utf-8"
        # create copy of the buffer list and assign copy to the buff attribute
        self._rest = MockBuffer(self.obj.buff[:])
        self.obj.buff = self._rest

    def test_with_empty_docinfo(self):
        """
        Try to post not well formed html
        """
        self.obj.buff = MockBuffer(self.obj.buff[4:])
        self.obj.docinfo_len = 0
        self.obj._update_docinfo('title', 'title2')


class TestRst2BloggerPreview(unittest.TestCase):
    """
    Test preview
    """
    def setUp(self):
        """
        Create Rst2Blogger object
        """
        self.obj = Rst2Blogger()
        self.obj.login = LOGIN
        self.obj.password = PASS
        self.obj.blogname = "test"

    def tearDown(self):
        """
        Remove leftovers in fs
        """
        try:
            os.unlink(self.obj.buff.name[:-4])
        except OSError:
            pass
        try:
            os.unlink(self.obj.buff.name[:-4] + ".html")
        except OSError:
            pass

    def test_preview_open_in_browser(self):
        """
        Try to post not well formed html
        """
        Eval.value = 1
        print self.obj.preview()

    def test_preview_save_to_file(self):
        """
        Try to post not well formed html
        """
        Eval.value = 0
        name = self.obj.buff.name[:-4] + ".html"
        self.assertEqual(self.obj.preview(),
                         "Generated HTML has been written to %s" % name)


if __name__ == "__main__":
    unittest.main()
ftplugin/rst/rst2blogger/tests/test_rest.py	[[[1
79
# vim: set fileencoding=utf-8
import os
import sys
import unittest
import re

this_dir = os.path.dirname(os.path.abspath(__file__))
this_dir = os.path.abspath(os.path.join(this_dir, "../.."))
sys.path.insert(0, this_dir)

from rst2blogger.rest import blogArticleString, blogPreview
from rst2blogger.tests.shared import REST_ARTICLE


class TestBlogPreview(unittest.TestCase):
    """
    Test generating HTML out of prepared reST text. It tests only for some
    aspects of the entire thing, because it is not intendend to test all of
    reST directives.
    """
    def test_content(self):
        """
        Simple case, check output
        """
        html_out = blogPreview(REST_ARTICLE)
        self.assertTrue(len(html_out) > 0)
        self.assertTrue("<html" in html_out)
        self.assertTrue("</html>" in html_out)
        self.assertTrue("<?xml version=\"1.0\" encoding=\"utf-8\"" in
                        html_out)
        self.assertTrue("\n\n<!-- more -->\n\n" in html_out)
        self.assertTrue("<title>Title — This is a test</title>" in html_out)
        self.assertTrue('type="text/css"' not in html_out)
        self.assertTrue(re.search(r"<h1.*><a href=\"#\">Title — This is a"
                                  " test</a></h1>", html_out))
        self.assertTrue(re.search(r"<h2>Section 1</h2>", html_out))
        self.assertTrue(re.search(r"<h3>Subsection 1.1</h3>", html_out))
        self.assertTrue("description" not in html_out)

    def test_stylesheets(self):
        """
        Test output for stylesheets
        """
        html_out = blogPreview(REST_ARTICLE, ["css/style1.css",
                                              "css/blogger1.css"])
        self.assertTrue('type="text/css"' in html_out)
        match = re.search(r'<link rel="stylesheet" '
                          'href=".*" type="text/css" />', html_out)
        self.assertTrue(match is not None)
        self.assertEqual(len(match.span()), 2)


class TestBlogArticleString(unittest.TestCase):
    """
    Test blogArticleString function, wich should return part of html and
    dictionary with attributes.
    """
    def test_blogArticleString(self):
        html_out, attrs = blogArticleString(REST_ARTICLE)
        self.assertEqual(len(attrs), 3)
        self.assertTrue(len(html_out) > 0)
        self.assertTrue("<html" not in html_out)
        self.assertTrue("</html>" not in html_out)
        self.assertTrue("<?xml version=\"1.0\" encoding=\"utf-8\"" not in
                        html_out)
        self.assertTrue("\n\n<!-- more -->\n\n" in html_out)
        self.assertTrue("<title>Title — This is a test</title>" not in
                        html_out)
        self.assertTrue('type="text/css"' not in html_out)
        self.assertTrue(re.search(r"<h4>Section 1</h4>", html_out))
        self.assertTrue(re.search(r"<h5>Subsection 1.1</h5>", html_out))
        self.assertTrue("description" not in html_out)

        self.assertEqual(attrs['title'], u"Title — This is a test")
        self.assertEqual(attrs['date'], "2010-12-12T12:36:36+01:00")
        self.assertEqual(attrs['tags'], "this is a test, Blogger, rest")

if __name__ == "__main__":
    unittest.main()
