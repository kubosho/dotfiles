"ウインドウサイズ
set columns=200
set lines=50

"フォント
set guifont=DejaVu\ Sans\ Mono:h12
" set guifont=Source\ Code\ Pro \Regular:h13
" set guifont=Inconsolata:h13

"バックスラッシュ入力
noremap! ¥ \

"タブのラベルファイル名のみ
set guitablabel=%t

"ツールバー非表示
set guioptions-=T

"カーソル行表示
set cursorline

"kayoriyaのgvimrcで上書きされるのでもう一回設定
set cmdheight=1
set background=light
" set background=dark
" colorscheme railscasts
colorscheme jellybeans
" colorscheme pyte
" colorscheme solarized
" colorscheme newspaper

"IME
set imdisableactivate

"Vimにフォーカスが当たっていない場合は透けさせる
"http://vim-users.jp/2011/10/hack234/
augroup hack234
  autocmd!
  if has('mac')
    autocmd FocusGained * set transparency=20
    autocmd FocusLost * set transparency=100
  endif
augroup END
