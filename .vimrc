" 色の有効化
syntax on

" カラースキーム
colorscheme molokai

" insertモードを抜けるとIMEオフ
set noimdisable
set iminsert=0 imsearch=0
set noimcmdline
inoremap <silent> <ESC> <ESC>:set iminsert=0<CR>

" 行番号
set nu

" ツールバーを削除
set guioptions-=T

" ファイル名、文字エンコード、改行形式をステータスラインに表示
set statusline=%<%f\ %m%r%h%w%{'['.(&fenc!=''?&fenc:&enc).']['.&ff.']'}%=%l,%c%V%8P 

" 対応する括弧をハイライト表示
set showmatch

" 一行前のインデントにあわせてインデントする
set noautoindent

" 検索のとき、大文字小文字を無視する
set ignorecase

" 検索のとき、大文字がある場合は大文字小文字を区別する
set smartcase

" 検索のときに最後までいったらもう一度最初に戻る
set wrapscan

" 検索文字を打ち込むと即検索する
set incsearch

" 全角スペースを表示する
highlight ZenkakuSpace cterm=underline ctermfg=lightblue guibg=white

" カーソル行に下線を表示
set cursorline

" カーソルの点滅をやめる
set guicursor=a:blinkon0

" 前回終了時の位置を記憶
if has("autocmd")
    autocmd BufReadPost *
    \ if line("'\"") > 0 && line ("'\"") <= line("$") |
    \   exe "normal! g'\"" |
    \ endif
endif

" バックアップファイル位置変更
set backupdir=~/work/backup

" 透過
set transparency=3

" フォントサイズ
set guifont=Menlo:h12

" ウインドウサイズ
set lines=200 columns=400    

" key remap
imap <C-i> <C-[>

imap { {}<LEFT>
imap [ []<LEFT>
imap ( ()<LEFT>

" neobundle
set nocompatible
filetype off

if has('vim_starting')
  set rtp+=~/.vim/neobundle.vim
  call neobundle#rc(expand('~/.vim/neobundle.vim'))
endif

NeoBundle 'Shougo/neocomplcache'
NeoBundle 'Shougo/neobundle.vim'
NeoBundle 'Shougo/unite.vim'

filetype plugin on
filetype indent on
