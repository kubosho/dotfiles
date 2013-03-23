" Vi互換OFF
set nocompatible
filetype off

" 構文ごとに色分けする
syntax on

" 行番号表示
set nu

" -------------------------------------------
" NeoBundle設定
" -------------------------------------------
if has('vim_starting')
  set runtimepath+=~/.vim/bundle/neobundle.vim/
  call neobundle#rc(expand('~/.vim/bundle/'))
endif

" プラグイン管理
NeoBundle 'Shougo/neobundle.vim'

" ファイラー
NeoBundle 'Shougo/unite.vim'
NeoBundle 'scrooloose/nerdtree'

" 補完
NeoBundle 'Shougo/neocomplcache'
NeoBundle 'Shougo/neocomplcache-snippets-complete'
NeoBundle 'teramako/jscomplete-vim'

" check syntax
NeoBundle 'walm/jshint.vim'
NeoBundle "osyo-manga/vim-watchdogs"

" フロントエンド
NeoBundle 'othree/html5.vim'
NeoBundle 'hail2u/vim-css3-syntax'
NeoBundle 'cakebaker/scss-syntax.vim'
NeoBundle 'pangloss/vim-javascript'
NeoBundle 'kchmck/vim-coffee-script'
NeoBundle 'mattn/zencoding-vim'

" quickrun関連
NeoBundle 'thinca/vim-quickrun'
NeoBundle 'Shougo/vimproc'
NeoBundle 'osyo-manga/unite-quickfix'
NeoBundle 'osyo-manga/shabadou.vim'

" いろいろ便利なもの
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'plasticboy/vim-markdown'

" 外観
NeoBundle 'Lokaltog/vim-powerline'
NeoBundle 'nanotech/jellybeans.vim'

" ファイル形式別プラグインのロードを有効化
filetype plugin on
filetype indent on
 
" -------------------------------------------
" 設定ファイル読み込み
" -------------------------------------------
if isdirectory(expand('~/.vim/config/'))
  source ~/.vim/config/base.vim
  source ~/.vim/config/status.vim
  source ~/.vim/config/unite.vim
  source ~/.vim/config/quickrun.vim
  source ~/.vim/config/neocomp.vim
endif

" -------------------------------------------
" キーバインド
" -------------------------------------------
" おれは<ESC>をやめるぞ、jj------!
inoremap <expr> j getline('.')[col('.')-2] ==# 'j' ? "\<BS>\<ESC>" : 'j'

" key remap
imap { {}<LEFT>
imap [ []<LEFT>
imap ( ()<LEFT>

" 入力モードで削除
inoremap <C-d> <Del>
inoremap <C-h> <BS>
