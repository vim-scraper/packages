" Keystroke saving with Vim and Google Scribe
" Maintainer:   Enlil Dubenstein <https://github.com/dubenstein>
" Version:      0.1.0
" Last Change: October 6, 2011

if !has('python')
  echo "vim-google-scribe requires vim compiled with +python"
  finish
endif

if exists("g:loaded_googlescribe") || &cp || v:version < 700
    finish
endif
let g:loaded_googlescribe = 1

if !exists('g:googlescribe_cache_size')
    let g:googlescribe_cache_size = 150
endif

python << EOF
import re
import vim
import time
import json
import heapq
import string
import urllib2
import functools

def memorize(maxsize):
    """
    Least-recently-used cache decorator.

    Arguments to the cached function must be hashable.
    Cache performance statistics stored in f.hits and f.misses.
    Clear the cache with f.clear().

    http://en.wikipedia.org/wiki/Cache_algorithms#Least_Recently_Used
    """
    def decorating_function(f):
        cache = {}  # map from key to value
        heap = []   # list of keys, in LRU heap
        cursize = 0 # because len() is slow
        @functools.wraps(f)
        def wrapper(*args):
            key = repr(args)
            # performance crap
            _cache=cache
            _heap=heap
            _heappop = heapq.heappop
            _heappush = heapq.heappush
            _time = time.time
            _cursize = cursize
            _maxsize = maxsize
            if not _cache.has_key(key):
                if _cursize == _maxsize:
                    # pop oldest element
                    (_,oldkey) = _heappop(_heap)
                    _cache.pop(oldkey)
                else:
                    _cursize += 1
                # insert this element
                _cache[key] = f(*args)
                _heappush(_heap,(_time(),key))
                wrapper.misses += 1
            else:
                wrapper.hits += 1
            return cache[key]

        wrapper.hits = wrapper.misses = 0
        return wrapper
    return decorating_function

class GoogleScribeApi:
    """
    Object representing basic Google Scribe Api. The public interface consists of
    the suggest() method: Pass in text and receive suggestions.
    LRU cache and Unicode enabled.
    """
    _userAgent = 'vim-google-scribe'
    _googleScribeApi = 'https://www.googleapis.com/rpc?key=AIzaSyBStAcD4WoVx3l1Cmc8xKvJTm2c0G2eqH4'
    maxsize = vim.eval('g:googlescribe_cache_size')

    # Hardcode the recognized whitespace characters to the US-ASCII
    # whitespace characters.  The main reason for doing this is that in
    # ISO-8859-1, 0xa0 is non-breaking whitespace, so in certain locales
    # that character winds up in string.whitespace.  Respecting
    # string.whitespace in those cases would 1) make textwrap treat 0xa0 the
    # same as any other whitespace char, which is clearly wrong (it's a
    # *non-breaking* space), 2) possibly cause problems with Unicode,
    # since 0xa0 is not in range(128).
    _whitespace = '\t\n\x0b\x0c\r '
    whitespace_trans = string.maketrans(_whitespace, ' ' * len(_whitespace))
    unicode_whitespace_trans = {}
    uspace = ord(u' ')
    for x in map(ord, _whitespace):
        unicode_whitespace_trans[x] = uspace

    # This less funky little regex just split on recognized spaces. E.g.
    #   "Hello there -- you goof-ball, use the -b option!"
    # splits into
    #   Hello/ /there/ /--/ /you/ /goof-ball,/ /use/ /the/ /-b/ /option!/
    wordsep_simple_re = re.compile('(\\s+)')

    def __init__(self, width = 40, cacheMaxSize=150):
        self.width = width
        self.maxsize = cacheMaxSize
        # recompile the regex for Unicode mode
        self.wordsep_simple_re_uni = re.compile(self.wordsep_simple_re.pattern, re.U)

    def _mungle(self, text):
        """_mungle(text : string) -> string
        Munge whitespace in text: expand tabs and convert all other
        whitespace characters to spaces.  Eg. " foo\tbar\n\nbaz"
        becomes " foo    bar  baz". Then split the text, get the last
        words to fit self.width chars and wrap them for scribe api.
        """
        if isinstance(text, unicode):
            text = text.translate(self.unicode_whitespace_trans)
            pat = self.wordsep_simple_re_uni
        else:
            text = text.translate(self.whitespace_trans)
            pat = self.wordsep_simple_re
        words = []
        check = lambda w: len(w + ''.join(words)) <= self.width and words.insert(0, w)
        [ check(w) for w in filter(None, pat.split(text))[::-1] ]
        return ''.join(words)

    @memorize(maxsize)
    def suggest(self, text):
        """_suggest(text : string) -> tuple
        Thast's it` jsonize the request, receive the suggestions.
        Supose we've fed the word "tea", the method returned tuple`
        ([{u'completion': u'm', u'renderingCompletion': u'team'}], u'en')
            ^without "tea"      ^ suggestion prepended with "tea"    ^ language
        """
        data = json.dumps({'jsonrpc': '2.0',
                           'apiVersion': 'v1',
                           'id': 'scribe.textSuggestions.get',
                           'params': {'query': self._mungle(text),
                                      'cp': self.width},
                            'key': 'scribe.textSuggestions.get',
                            'method': 'scribe.textSuggestions.get'},
                         ensure_ascii=False)
        req = urllib2.Request(self._googleScribeApi,
                              headers={'Content-Type': 'application/json',
                                       'Accept': '*/*',
                                       'User-Agent': self._userAgent},
                             data=data)
        response = urllib2.urlopen(req).read()
        result = json.loads(response)['result']
        return result['suggestions'], result['lang']

api = GoogleScribeApi()

def complete(text):
    try:
        return api.suggest(text)
    except Exception:
        return False

EOF

fun! googlescribe#Complete(findstart, base)
  if a:findstart
    let line = getline('.')
    let start = col('.') - 1
    while start > 0 && line[start - 1] =~ '\a'
      let start -= 1
    endwhile
    return start
  else
    let res = []
python << EOF
query = vim.eval('strpart(getline("."), 0, col(".") - 1).a:base')
base = vim.eval('a:base')
response = complete(query)
if response:
    for stash in response[0]:
        if base == '':
            s = stash['completion']
        else:
            s = stash['renderingCompletion']
        vim.eval('add(res, "%s")' % s.encode(vim.eval("&encoding")))
EOF
    return res
  endif
endfun

