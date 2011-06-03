" This script adds a submenu under the Tools menu for vim tags commands
" Use it by copying it into your plugin directory, or source it
" from your .vimrc file.
amenu 40.325 Tools.Tags.Jump.To\ this\ tag<Tab>gCtrl-] g<C-]>
amenu Tools.Tags.Jump.Direct\ to\ first\ match<Tab>Ctrl-]  <C-]>
amenu Tools.Tags.Jump.Back<Tab>Ctrl-T <C-T>
amenu Tools.Tags.Jump.Prompt\ for\ tag<Tab>:tag :tag 
amenu Tools.Tags.Tagselect.on\ word\ at\ cursor<Tab>g] g]
amenu Tools.Tags.Tagselect.Next\ match<Tab>:tn :tn<CR>
amenu Tools.Tags.Tagselect.Previous\ match<Tab>:tp\ or\ :tN :tp <CR>
amenu Tools.Tags.Tagselect.Rewind\ to\ first\ match<Tab>:tr[ewind] :tr<CR>
amenu Tools.Tags.Tagselect.Last\ match<Tab>:tl[ast] :tl<CR>
amenu Tools.Tags.Tagselect.Prompt\ for\ tagselect<Tab>:ts :ts 
amenu Tools.Tags.Tagselect.Prompt\ for\ tagselect\ then\ split<Tab>:sts :sts 
amenu Tools.Tags.Tagselect.Search\ for\ regexp<Tab>:ts\ / :ts /
amenu Tools.Tags.Show.tag\ stack<Tab>:tags :tags<CR>
nmenu Tools.Tags.Show.tag\ file\ list<Tab>:set\ tags :se tags<CR>
