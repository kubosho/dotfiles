set encoding=utf-8
scriptencoding utf-8

""""""""""""""""""""""""""""""""""""""""""""""""""

set background=dark

""""""""""""""""""""""""""""""""""""""""""""""""""

filetype plugin indent on

""""""""""""""""""""""""""""""""""""""""""""""""""

set number
" color number
" ref: http://vim.wikia.com/wiki/Xterm256_color_names_for_console_Vim
hi LineNr ctermfg=148 ctermbg=24
set ruler
set wrap
set list
set listchars=tab:▸\ ,trail:-,eol:¬

""""""""""""""""""""""""""""""""""""""""""""""""""

set backspace=2
set laststatus=2
set hidden

""""""""""""""""""""""""""""""""""""""""""""""""""

set showmatch
set showcmd

""""""""""""""""""""""""""""""""""""""""""""""""""

set hlsearch
set ignorecase
set smartcase

""""""""""""""""""""""""""""""""""""""""""""""""""

set backupdir=$HOME/tmp/vim-backup
set clipboard=unnamed,autoselect
set noswapfile

""""""""""""""""""""""""""""""""""""""""""""""""""

set autoindent
set smartindent
set tabstop=2
set shiftwidth=2
set smarttab

""""""""""""""""""""""""""""""""""""""""""""""""""

" Show zenkaku space
" ref: http://inari.hatenablog.com/entry/2014/05/05/231307
function! ShowZenkakuSpace()
  highlight ShowZenkakuSpace cterm=underline ctermfg=lightblue guibg=darkgray
endfunction

if has('syntax')
  augroup ShowZenkakuSpace
    autocmd!
    autocmd ColorScheme * call ShowZenkakuSpace()
    autocmd VimEnter,WinEnter,BufRead * let w:m1=matchadd('ShowZenkakuSpace', '　')
  augroup END

  call ShowZenkakuSpace()
endif

""""""""""""""""""""""""""""""""""""""""""""""""""

" クックック……我の名は暗黒美夢王。Vimを愛し、精神までVimに支配されてしまったものだ。
" 我が作ったプラグインを使うか？良いだろう。
" Plugins listを'Shougo/*.vim'で埋め尽くされることを覚悟するんだな、クックック……。
if !1 | finish | endif

if has('vim_starting')
  set nocompatible
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

call neobundle#begin(expand('~/.vim/bundle/'))

NeoBundleFetch 'Shougo/neobundle.vim'

" Plugins
NeoBundle 'Shougo/neomru.vim'
NeoBundle 'Shougo/unite.vim'
NeoBundle has('lua') ? 'Shougo/neocomplete.vim' : ''
NeoBundle 'Shougo/neosnippet.vim'
NeoBundle 'Shougo/neosnippet-snippets'
NeoBundle 'Shougo/vimproc.vim', {
\ 'build' : {
\     'windows' : 'tools\\update-dll-mingw',
\     'mac'     : 'make -f make_mac.mak',
\     'linux'   : 'make',
\     'unix'    : 'gmake',
\   },
\ }
NeoBundle 'scrooloose/nerdtree'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'tpope/vim-surround'
NeoBundle 'nathanaelkane/vim-indent-guides'
NeoBundle 'bronson/vim-trailing-whitespace'
NeoBundle 'elzr/vim-json'
NeoBundle 'lilydjwg/colorizer'
NeoBundle 'scrooloose/syntastic'
NeoBundle 'thinca/vim-quickrun', { 'depends' : [ 'Shougo/vimproc.vim' ] }
NeoBundle 'Townk/vim-autoclose'
NeoBundle 'mattn/jscomplete-vim'
NeoBundle 'jelera/vim-javascript-syntax'
NeoBundle 'leafgarland/typescript-vim'
NeoBundle 'hail2u/vim-css3-syntax'
NeoBundle 'cakebaker/scss-syntax.vim'
NeoBundle 'groenewege/vim-less'

call neobundle#end()

NeoBundleCheck

""""""""""""""""""""""""""""""""""""""""""""""""""

" UNITE-CHAN!
let g:unite_enable_start_insert = 1

" Buffer list
noremap <C-P> :Unite buffer<CR>
" File list
noremap <C-N> :Unite -buffer-name=file file<CR>
" Recent file list
noremap <C-Z> :Unite file_mru<CR>

" Esc key two press to exit Unite.
au FileType unite nnoremap <silent> <buffer> <ESC><ESC> :q<CR>
au FileType unite inoremap <silent> <buffer> <ESC><ESC> <ESC>:q<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""

" Neocomplete
if neobundle#is_installed('neocomplete.vim')
  let g:neocomplete#enable_at_startup = 1
  let g:neocomplete#enable_smart_case = 1
  let g:neocomplete#sources#syntax#min_keyword_length = 1

  if !exists('g:neocomplete#keyword_patterns')
    let g:neocomplete#keyword_patterns = {}
  endif

  " Plugin key-mappings.
  inoremap <expr><C-g> neocomplete#undo_completion()
  inoremap <expr><C-l> neocomplete#complete_common_string()

  let g:neocomplete#keyword_patterns._ = '\h\w*'
endif

""""""""""""""""""""""""""""""""""""""""""""""""""

" Neosnippet
" Plugin key-mappings.
imap <C-k> <Plug>(neosnippet_expand_or_jump)
smap <C-k> <Plug>(neosnippet_expand_or_jump)
xmap <C-k> <Plug>(neosnippet_expand_target)

" SuperTab like snippets behavior.
imap <expr><TAB> neosnippet#expandable_or_jumpable() ?
\ "\<Plug>(neosnippet_expand_or_jump)"
\: pumvisible() ? "\<C-n>" : "\<TAB>"
smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
\ "\<Plug>(neosnippet_expand_or_jump)"
\: "\<TAB>"

" For snippet_complete marker.
if has('conceal')
  set conceallevel=2 concealcursor=i
endif

""""""""""""""""""""""""""""""""""""""""""""""""""

" fugitive.vim
set statusline+=%{fugitive#statusline()}

""""""""""""""""""""""""""""""""""""""""""""""""""

" Indent Guides
let g:indent_guides_enable_on_vim_startup = 1
let g:indent_guides_start_level = 2
let g:indent_guides_guide_size = 1

" color number
" ref: http://vim.wikia.com/wiki/Xterm256_color_names_for_console_Vim
hi IndentGuidesOdd  ctermbg=16
hi IndentGuidesEven ctermbg=232

""""""""""""""""""""""""""""""""""""""""""""""""""

" Typescript Syntax for Vim

autocmd QuickFixCmdPost [^l]* nested cwindow
autocmd QuickFixCmdPost    l* nested lwindow

""""""""""""""""""""""""""""""""""""""""""""""""""

" quickrun.vim
" ref: https://github.com/Layzie/dotfiles/blob/master/.vimrc#L692
let g:quickrun_config = {}
let g:quickrun_config['*'] =  {'runner': 'vimproc', 'runner/vimproc/updatetime' : 10}
let g:quickrun_config['coffee'] = {'command' : 'coffee', 'exec' : ['%c -cbp %s']}
let g:quickrun_config['mkd'] = {
\ 'outputter' : 'null',
\ 'command'   : 'open',
\ 'cmdopt'    : '-a',
\ 'args'      : 'Marked\ 2',
\ 'exec'      : '%c %o %a %s',
\ }

""""""""""""""""""""""""""""""""""""""""""""""""""

" ref: NeoBundleを有効にすると、シンタックスハイライトが無効になる - 混沌とした備忘録
" http://noboru.hatenablog.jp/entry/20130701/1372686224
syntax enable

