" カラーテーマ
set cmdheight=1
set background=dark
colorscheme hybrid
" colorscheme molokai
" colorscheme jellybeans

" ウインドウサイズ
set columns=200
set lines=50

" フォント
set guifont=Menlo:h14
set guifontwide=ヒラギノ丸ゴ\ ProN\ W4:h14
" set guifont=Lucida\ Console:h12
" set guifont=DejaVu\ Sans\ Mono:h12
" set guifont=Source\ Code\ Pro \Regular:h12
" set guifont=Inconsolata:h14

" バックスラッシュ入力
noremap! ¥ \

" タブのラベルファイル名のみ
set guitablabel=%t

" -------------------------------------------
" 表示・非表示
" http://nanabit.net/blog/2007/11/01/vim-fullscreen/
" -------------------------------------------
" ツールバー非表示
set guioptions-=T

" 左スクロールバー非表示
set guioptions-=l
set guioptions-=L

" 右スクロールバー非表示
set guioptions-=r
set guioptions-=R

" カーソル行表示
set cursorline

" IME
set imdisableactivate

" -------------------------------------------
" 透過
" http://nanabit.net/blog/2007/11/01/vim-fullscreen/
" -------------------------------------------
set transparency=20

" Vimにフォーカスが当たっていない場合は透けさせる
" http://vim-users.jp/2011/10/hack234/
augroup hack234
  autocmd!
  if has('mac')
    autocmd FocusGained * set transparency=20
    autocmd FocusLost * set transparency=100
  endif
augroup END
