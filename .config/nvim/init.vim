set background=light
set clipboard=unnamed
set number

set autoindent
set expandtab

" Plugin
call plug#begin('~/.vim/plugged')
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'Shougo/unite.vim'
Plug 'Shougo/vimfiler.vim'
Plug 'carlitux/deoplete-ternjs', { 'do': 'npm install -g tern' }
Plug 'cloudhead/neovim-fuzzy'
Plug 'cohama/lexima.vim'
Plug 'editorconfig/editorconfig-vim'
Plug 'flazz/vim-colorschemes'
Plug 'itchyny/lightline.vim'
Plug 'neomake/neomake'
Plug 'ternjs/tern_for_vim'
Plug 'zchee/deoplete-go', { 'do': 'make'}
call plug#end()

" Keymap
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

" vimfiler
:let g:vimfiler_as_default_explorer = 1

" lightline
let g:lightline = {
      \ 'colorscheme': 'solarized',
      \ }
