" UI
set background=light
set cursorline
set noshowmode
set number

" clipboard
set clipboard=unnamed

" IME
set ttimeout
set ttimeoutlen=50

" indent
set autoindent
set expandtab
set tabstop=4

" search
set incsearch
set smartcase

" plugin
call plug#begin('~/.vim/plugged')
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'Shougo/unite.vim'
Plug 'Shougo/vimfiler.vim'
Plug 'Yggdroot/indentLine'
Plug 'bronson/vim-trailing-whitespace'
Plug 'carlitux/deoplete-ternjs', { 'do': 'npm install -g tern' }
Plug 'cloudhead/neovim-fuzzy'
Plug 'cohama/agit.vim'
Plug 'cohama/lexima.vim'
Plug 'editorconfig/editorconfig-vim'
Plug 'flazz/vim-colorschemes'
Plug 'itchyny/lightline.vim'
Plug 'rhysd/committia.vim'
Plug 'ternjs/tern_for_vim'
Plug 'w0rp/ale'
Plug 'zchee/deoplete-go', { 'do': 'make'}
call plug#end()

" ale
let g:airline#extensions#ale#enabled = 1
let g:ale_echo_msg_error_str = '⨉'
let g:ale_echo_msg_warning_str = '⚠'
let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'

" keymap
let mapleader = "\<Space>"
nmap <leader>ev :e $MYVIMRC<CR>
nmap <leader>sv :so $MYVIMRC<CR>
inoremap jj <ESC>
:noremap <ESC><ESC> :noh<CR>
:nnoremap <C-p> :FuzzyOpen<CR>

" deoplete
let g:deoplete#enable_at_startup = 1

" deoplete-tern
let g:deoplete#sources#ternjs#tern_bin = '~/.anyenv/envs/ndenv/shims/tern'
let g:deoplete#sources#ternjs#timeout = 1
let g:deoplete#sources#ternjs#types = 1
let g:deoplete#sources#ternjs#depths = 1
let g:deoplete#sources#ternjs#docs = 1
let g:deoplete#sources#ternjs#filter = 0
let g:deoplete#sources#ternjs#case_insensitive = 1
let g:deoplete#sources#ternjs#guess = 0
let g:deoplete#sources#ternjs#sort = 0
let g:deoplete#sources#ternjs#expand_word_forward = 0
let g:deoplete#sources#ternjs#omit_object_prototype = 0
let g:deoplete#sources#ternjs#include_keywords = 1
let g:deoplete#sources#ternjs#in_literal = 0
let g:deoplete#sources#ternjs#filetypes = [
                \ 'jsx',
                \ 'javascript.jsx',
                \ 'vue'
                \ ]

" indentLine
let g:indentLine_setColors = 239
let g:indentLine_color_term = 0

" lightline
let g:lightline = {
      \ 'colorscheme': 'solarized',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'readonly', 'filename' ] ],
      \ },
      \ 'component_function': {
      \   'filename': 'LightlineFilename',
      \   'readonly': 'LightlineReadonly',
      \ },
      \ }

function! LightlineFilename()
  let filename = expand('%:t') !=# '' ? expand('%:t') : '[No Name]'
  let modified = &modified ? ' +' : ''
  return filename . modified
endfunction

function! LightlineReadonly()
  return &readonly && &filetype !=# 'help' ? 'RO' : ''
endfunction

" vimfiler
:let g:vimfiler_as_default_explorer = 1
