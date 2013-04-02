" Vi互換OFF
set nocompatible
filetype off

" 構文ごとに色分けする
syntax on

" 行番号表示
set nu

" カレントディレクトリを変更するようにする
" http://kaworu.jpn.org/kaworu/2008-06-06-1.php
set autochdir

" コマンドライン補完を便利にする
set wildmenu

 " 対応する括弧をハイライト表示
set showmatch

" 一行前のインデントにあわせてインデントする
set noautoindent

" フルスクリーン
" http://d.hatena.ne.jp/acotie/20100123/1264271335
if has("gui_running")
  set fuoptions=maxvert,maxhorz
  au GUIEnter * set fullscreen
endif

" insertモードを抜けるとIMEオフ
set noimdisable
set iminsert=0 imsearch=0
set noimcmdline
inoremap <silent> <ESC> <ESC>:set iminsert=0<CR>
runtime macros/editexisting.vim

" 前回終了時の位置を記憶
if has("autocmd")
    autocmd BufReadPost *
    \ if line("'\"") > 0 && line ("'\"") <= line("$") |
    \   exe "normal! g'\"" |
    \ endif
endif

" --------------------------------------------------
" 表示・非表示
" --------------------------------------------------
" 全角スペースを表示
hi FullWidthSpace term=underline ctermbg=blue guibg=darkgray
autocmd BufNew,BufRead * match FullWidthSpace /　/

" ハードタブと行末のスペースを表示
hi SpecialKey ctermfg=blue guifg=#555555
set listchars=tab:>\ ,trail:_
set list

" --------------------------------------------------
" 保存時の設定
" http://qiita.com/items/bc9720826120f5f61fc1
" --------------------------------------------------
function! s:remove_dust()
  let cursor = getpos(".")
  " 保存時に行末の空白を除去する
  %s/\s\+$//ge
  " 保存時にtabを2スペースに変換する
  " %s/\t/  /ge
  call setpos(".", cursor)
  unlet cursor
endfunction
autocmd BufWritePre * call <SID>remove_dust()

" --------------------------------------------------
" バックアップ・スワップのディレクトリ
" --------------------------------------------------
" backup
set backupdir=~/work/vim/backup

" swap
set directory=~/work/vim/swap

" --------------------------------------------------
" インデント
" --------------------------------------------------
" タブを何文字分として表示するか
set tabstop=8

" インデントのスペース設定
set shiftwidth=2

" タブを使わない
set expandtab

" --------------------------------------------------
" カーソル
" --------------------------------------------------
" カーソル行に下線を表示
set cursorline

" カーソルの点滅をやめる
set guicursor=a:blinkon0

" --------------------------------------------------
" 検索
" --------------------------------------------------
" 検索のとき、大文字小文字を無視する
set ignorecase

" 検索のとき、大文字がある場合は大文字小文字を区別する
set smartcase

" 検索のときに最後までいったらもう一度最初に戻る
set wrapscan

" 検索文字を打ち込むと即検索する
set incsearch

" --------------------------------------------------
" 連番
" --------------------------------------------------
nnoremap <silent> co :ContinuousNumber <C-a><CR>
vnoremap <silent> co :ContinuousNumber <C-a><CR>
command! -count -nargs=1 ContinuousNumber let c = col('.')|for n in range(1, <count>?<count>-line('.'):1)|exec 'normal! j' . n . <q-args>|call cursor('.', c)|endfor

" --------------------------------------------------
" BackSpace
" --------------------------------------------------
" おまじない
" http://koexuka.blogspot.jp/2011/02/vim.html
noremap <BS> 
noremap! <BS> 
noremap 
noremap! 

" BackSpaceの挙動変更
" http://fantatchi.info/archives/201
set backspace=indent,eol,start
