set encoding=utf-8
scriptencoding utf-8

""""""""""""""""""""""""""""""""""""""""""""""""""

colorscheme desert
set background=dark
syntax on

""""""""""""""""""""""""""""""""""""""""""""""""""

set cursorline
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

""""""""""""""""""""""""""""""""""""""""""""""""""

set showmatch
set showcmd
set noshowmode

""""""""""""""""""""""""""""""""""""""""""""""""""

set hlsearch
set ignorecase
set smartcase

""""""""""""""""""""""""""""""""""""""""""""""""""

set backupdir=~/.vim_backup/
set undodir=~/.vim_backup/
set clipboard=unnamed,autoselect

""""""""""""""""""""""""""""""""""""""""""""""""""

set swapfile
set directory=$HOME/.vim_swap

""""""""""""""""""""""""""""""""""""""""""""""""""

set confirm

""""""""""""""""""""""""""""""""""""""""""""""""""

set autoindent
set smartindent
set tabstop=4
set shiftwidth=2
set smarttab

""""""""""""""""""""""""""""""""""""""""""""""""""

if !has('gui_running')
  set t_Co=256
endif

""""""""""""""""""""""""""""""""""""""""""""""""""
" .vimrc

nnoremap <silent> <C-]> :<C-u>edit $MYVIMRC<CR>

augroup reload_vimrc
  autocmd!
  autocmd bufwritepost $MYVIMRC nested source $MYVIMRC
augroup END

""""""""""""""""""""""""""""""""""""""""""""""""""
" show zenkaku space
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
" auto cursorline
" http://thinca.hatenablog.com/entry/20090530/1243615055

augroup vimrc-auto-cursorline
  autocmd!
  autocmd CursorMoved,CursorMovedI * call s:auto_cursorline('CursorMoved')
  autocmd CursorHold,CursorHoldI * call s:auto_cursorline('CursorHold')
  autocmd WinEnter * call s:auto_cursorline('WinEnter')
  autocmd WinLeave * call s:auto_cursorline('WinLeave')

  let s:cursorline_lock = 0
  function! s:auto_cursorline(event)
    if a:event ==# 'WinEnter'
      setlocal cursorline
      let s:cursorline_lock = 2
    elseif a:event ==# 'WinLeave'
      setlocal nocursorline
    elseif a:event ==# 'CursorMoved'
      if s:cursorline_lock
        if 1 < s:cursorline_lock
          let s:cursorline_lock = 1
        else
          setlocal nocursorline
          let s:cursorline_lock = 0
        endif
      endif
    elseif a:event ==# 'CursorHold'
      setlocal cursorline
      let s:cursorline_lock = 1
    endif
  endfunction
augroup END

""""""""""""""""""""""""""""""""""""""""""""""""""
" autosave

autocmd CursorHold * wall
set updatetime=1000

""""""""""""""""""""""""""""""""""""""""""""""""""

set nocompatible " be iMproved, required
filetype off     " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" The following are examples of different formats supported.
" Keep Plugin commands between vundle#begin/end.
" plugin on GitHub repo Plugin 'tpope/vim-fugitive'
Plugin 'Shougo/neocomplete.vim'
Plugin 'Shougo/neomru.vim'
Plugin 'Shougo/neosnippet.vim'
Plugin 'Shougo/neosnippet-snippets'
Plugin 'Shougo/neoyank.vim'
Plugin 'Shougo/vimproc.vim'
Plugin 'Shougo/unite.vim'
Plugin 'sorah/unite-ghq'
Plugin 'nathanaelkane/vim-indent-guides'
Plugin 'pangloss/vim-javascript'
Plugin 'kchmck/vim-coffee-script'
Plugin 'mxw/vim-jsx'
Plugin 'Quramy/tsuquyomi'
Plugin 'digitaltoad/vim-pug'
Plugin 'editorconfig/editorconfig-vim'
Plugin 'tpope/vim-endwise'
Plugin 'szw/vim-tags'
Plugin 'majutsushi/tagbar'
Plugin 'soramugi/auto-ctags.vim'
Plugin 'itchyny/lightline.vim'
Plugin 'cocopon/iceberg.vim'
Plugin 'alvan/vim-closetag'
Plugin 'neomake/neomake'
Plugin 'benjie/neomake-local-eslint.vim'
Plugin 'kewah/vim-stylefmt'
Plugin 'tpope/vim-fugitive'
Plugin 'fuenor/qfixgrep'
Plugin 'mileszs/ack.vim'
Plugin 'kien/ctrlp.vim'
Plugin 'nixprime/cpsm'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

""""""""""""""""""""""""""""""""""""""""""""""""""
" neocomplete

" Disable AutoComplPop.
let g:acp_enableAtStartup = 0
" Use neocomplete.
let g:neocomplete#enable_at_startup = 1
" Use smartcase.
let g:neocomplete#enable_smart_case = 1
" Set minimum syntax keyword length.
let g:neocomplete#sources#syntax#min_keyword_length = 3

" Define dictionary.
let g:neocomplete#sources#dictionary#dictionaries = {
    \ 'default' : '',
    \ 'vimshell' : $HOME.'/.vimshell_hist',
    \ 'scheme' : $HOME.'/.gosh_completions'
        \ }

" Define keyword.
if !exists('g:neocomplete#keyword_patterns')
    let g:neocomplete#keyword_patterns = {}
endif
let g:neocomplete#keyword_patterns['default'] = '\h\w*'

" Plugin key-mappings.
inoremap <expr><C-g>     neocomplete#undo_completion()
inoremap <expr><C-l>     neocomplete#complete_common_string()

" Recommended key-mappings.
" <CR>: close popup and save indent.
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function()
  return (pumvisible() ? "\<C-y>" : "" ) . "\<CR>"
  " For no inserting <CR> key.
  "return pumvisible() ? "\<C-y>" : "\<CR>"
endfunction
" <TAB>: completion.
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
" <C-h>, <BS>: close popup and delete backword char.
inoremap <expr><C-h> neocomplete#smart_close_popup()."\<C-h>"
inoremap <expr><BS> neocomplete#smart_close_popup()."\<C-h>"

" Enable omni completion.
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

""""""""""""""""""""""""""""""""""""""""""""""""""
" auto-ctags.vim

let g:auto_ctags = 1
let g:auto_ctags_directory_list = ['.git', '.svn']
set tags+=.git/tags
set tags+=.svn/tags

""""""""""""""""""""""""""""""""""""""""""""""""""
" lightline

set laststatus=2

let g:lightline = {
        \ 'colorscheme': 'wombat',
        \ 'mode_map': {'c': 'NORMAL'},
        \ 'active': {
        \   'left': [ [ 'mode', 'paste' ], [ 'fugitive', 'filename' ] ]
        \ },
        \ 'component_function': {
        \   'modified': 'LightlineModified',
        \   'readonly': 'LightlineReadonly',
        \   'fugitive': 'LightlineFugitive',
        \   'filename': 'LightlineFilename',
        \   'fileformat': 'LightlineFileformat',
        \   'filetype': 'LightlineFiletype',
        \   'fileencoding': 'LightlineFileencoding',
        \   'mode': 'LightlineMode'
        \ },
        \ 'separator': { 'left': "\ue0b0", 'right': "\ue0b2" },
        \ 'subseparator': { 'left': "\ue0b1", 'right': "\ue0b3" }
        \ }

function! LightlineModified()
  return &ft =~ 'help\|vimfiler\|gundo' ? '' : &modified ? '+' : &modifiable ? '' : '-'
endfunction

function! LightlineReadonly()
  return &ft !~? 'help\|vimfiler\|gundo' && &readonly ? 'x' : ''
endfunction

function! LightlineFilename()
  return ('' != LightlineReadonly() ? LightlineReadonly() . ' ' : '') .
        \ (&ft == 'vimfiler' ? vimfiler#get_status_string() :
        \  &ft == 'unite' ? unite#get_status_string() :
        \  &ft == 'vimshell' ? vimshell#get_status_string() :
        \ '' != expand('%:t') ? expand('%:t') : '[No Name]') .
        \ ('' != LightlineModified() ? ' ' . LightlineModified() : '')
endfunction

function! LightlineFugitive()
  if &ft !~? 'vimfiler\|gundo' && exists('*fugitive#head')
    return fugitive#head()
  else
    return ''
  endif
endfunction

function! LightlineFileformat()
  return winwidth(0) > 70 ? &fileformat : ''
endfunction

function! LightlineFiletype()
  return winwidth(0) > 70 ? (&filetype !=# '' ? &filetype : 'no ft') : ''
endfunction

function! LightlineFileencoding()
  return winwidth(0) > 70 ? (&fenc !=# '' ? &fenc : &enc) : ''
endfunction

function! LightlineMode()
  return winwidth(0) > 60 ? lightline#mode() : ''
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""
" netrw

let g:netrw_liststyle = 3 " tree view
let g:netrw_list_hide = '\(^\|\s\s\)\zs\.\S\+'

""""""""""""""""""""""""""""""""""""""""""""""""""
" tagbar

nmap <F12> :TagbarToggle<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""
" vim indent guides

let g:indent_guides_enable_on_vim_startup = 1

hi IndentGuidesOdd ctermbg=black
hi IndentGuidesEven ctermbg=darkgrey

""""""""""""""""""""""""""""""""""""""""""""""""""
" unite

function! s:IgnoreUniteFileSources()
  let sources = []
  if filereadable('./.gitignore')
    for file in readfile('./.gitignore')
      " don't added comment and empty lines
      if file !~ "^#\\|^\s\*$"
        call add(sources, file)
      endif
    endfor
  endif

  if isdirectory('./.idea')
    call add(sources, '.idea')
  endif

  if isdirectory('./app')
    call add(sources, 'app/bower_components')
    call add(sources, 'app/fonts')
    call add(sources, 'app/images')
    call add(sources, 'app/video')
    call add(sources, '\(png\|gif\|jpe?g\)$')
  endif

  let pattern = escape(join(sources, '|'), './|')
  call unite#custom#source('file_rec/async', 'ignore_pattern', pattern)
  call unite#custom#source('file_rec/git', 'ignore_pattern', pattern)
endfunction

let g:unite_enable_start_insert=1
let g:unite_source_history_yank_enable =1
let g:unite_source_file_mru_limit = 200
let g:unite_source_rec_max_cache_files = 5000

if executable('ag')
  let g:unite_source_grep_command = 'ag'
  let g:unite_source_grep_default_opts = '--nogroup --nocolor --column'
  let g:unite_source_grep_recursive_opt = ''
endif

call s:IgnoreUniteFileSources()

nnoremap <silent> <Space>u]  :<C-u>Unite ghq<CR>
nnoremap <silent> <Space>ug  :<C-u>Unite grep:. -buffer-name=search-buffer<CR>
nnoremap <silent> <Space>ugc :<C-u>Unite grep:. -buffer-name=search-buffer<CR><C-R><C-W>
nnoremap <silent> <Space>ugr :<C-u>UniteResume search-buffer<CR>
nnoremap <silent> <Space>uy  :<C-u>Unite history/yank<CR>
nnoremap <silent> <Space>ub  :<C-u>Unite buffer<CR>
nnoremap <silent> <Space>uf  :<C-u>UniteWithBufferDir -buffer-name=files file<CR>
nnoremap <silent> <Space>ur  :<C-u>Unite -buffer-name=register register<CR>
nnoremap <silent> <Space>uu  :<C-u>Unite file_mru buffer<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""
" ctrlp

" let g:ctrlp_match_func = {'match': 'cpsm#CtrlPMatch'}
nnoremap <silent> <Space>up :<C-u>CtrlP<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""
" fugitive

nnoremap <silent> <Space>gb  :<C-u>Gblame<CR>
nnoremap <silent> <Space>gdf :<C-u>Gdiff<CR>
nnoremap <silent> <Space>gs  :<C-u>Gstatus<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""
" neomake

autocmd! BufWritePost * Neomake
let g:neomake_javascript_enabled_makers = ['eslint']

""""""""""""""""""""""""""""""""""""""""""""""""""

let g:javascript_plugin_jsdoc = 1
let g:javascript_plugin_flow = 1
let g:javascript_conceal_function       = "ƒ"
let g:javascript_conceal_null           = "ø"
let g:javascript_conceal_this           = "@"
let g:javascript_conceal_return         = "⇚"
let g:javascript_conceal_undefined      = "¿"
let g:javascript_conceal_NaN            = "ℕ"
let g:javascript_conceal_prototype      = "¶"
let g:javascript_conceal_static         = "•"
let g:javascript_conceal_super          = "Ω"
let g:javascript_conceal_arrow_function = "⇒"

au BufRead,BufNewFile *.jsx set filetype=javascript.jsx

